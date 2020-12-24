#!/usr/bin/env python3
from pathlib import Path
import json
import re
import shutil
import subprocess
import sys
import fnmatch

mbsyncFile = Path("~/.mbsyncrc").expanduser()

imapnotifyConfigFolder = Path("~/.config/imapnotify/").expanduser()
imapnotifyConfigFolder.mkdir(exist_ok=True)
imapnotifyConfigFilename = "notify.conf"

imapnotifyDefault = {
    "host": "",
    "port": 993,
    "tls": True,
    "tlsOptions": {"rejectUnauthorized": True},
    "onNewMail": "",
    "onNewMailPost": "if mu index --lazy-check; then test -f /tmp/mu_reindex_now && rm /tmp/mu_reindex_now; else touch /tmp/mu_reindex_now; fi",
}


def stripQuotes(string):
    if string[0] == '"' and string[-1] == '"':
        return string[1:-1].replace('\\"', '"')


mbsyncInotifyMapping = {
    "Host": (str, "host"),
    "Port": (int, "port"),
    "User": (str, "username"),
    "Password": (str, "password"),
    "PassCmd": (stripQuotes, "passwordCmd"),
    "Patterns": (str, "_patterns"),
}

oldAccounts = [d.name for d in imapnotifyConfigFolder.iterdir() if d.is_dir()]

currentAccount = ""
currentAccountData = {}

successfulAdditions = []


def processLine(line):
    newAcc = re.match(r"^IMAPAccount ([^#]+)", line)

    linecontent = re.sub(r"(^|[^\\])#.*", "", line).split(" ", 1)
    if len(linecontent) != 2:
        return

    parameter, value = linecontent

    if parameter == "IMAPAccount":
        if currentAccountNumber > 0:
            finaliseAccount()
        newAccount(value)
    elif parameter in mbsyncInotifyMapping.keys():
        parser, key = mbsyncInotifyMapping[parameter]
        currentAccountData[key] = parser(value)
    elif parameter == "Channel":
        currentAccountData["onNewMail"] = f"mbsync --pull --new {value}:'%s'"


def newAccount(name):
    global currentAccountNumber
    global currentAccount
    global currentAccountData
    currentAccountNumber += 1
    currentAccount = name
    currentAccountData = {}
    print(f"\n\033[1;32m{currentAccountNumber}\033[0;32m - {name}\033[0;37m")


def accountToFoldername(name):
    return re.sub(r"[^A-Za-z0-9]", "", name)


def finaliseAccount():
    if currentAccountNumber == 0:
        return

    global currentAccountData
    try:
        currentAccountData["boxes"] = getMailBoxes(currentAccount)
    except subprocess.CalledProcessError as e:
        print(
            f"\033[1;31mError:\033[0;31m failed to fetch mailboxes (skipping): "
            + f"`{' '.join(e.cmd)}' returned code {e.returncode}\033[0;37m"
        )
        return
    except subprocess.TimeoutExpired as e:
        print(
            f"\033[1;31mError:\033[0;31m failed to fetch mailboxes (skipping): "
            + f"`{' '.join(e.cmd)}' timed out after {e.timeout:.2f} seconds\033[0;37m"
        )
        return

    if "_patterns" in currentAccountData:
        currentAccountData["boxes"] = applyPatternFilter(
            currentAccountData["_patterns"], currentAccountData["boxes"]
        )

    # strip not-to-be-exported data
    currentAccountData = {
        k: currentAccountData[k] for k in currentAccountData if k[0] != "_"
    }

    parametersSet = currentAccountData.keys()
    currentAccountData = {**imapnotifyDefault, **currentAccountData}
    for key, val in currentAccountData.items():
        valColor = "\033[0;33m" if key in parametersSet else "\033[0;37m"
        print(f"  \033[1;37m{key:<13} {valColor}{val}\033[0;37m")

    if (
            len(currentAccountData["boxes"]) > 15
            and "@gmail.com" in currentAccountData["username"]
    ):
        print(
            "  \033[1;31mWarning:\033[0;31m Gmail raises an error when more than"
            + "\033[1;31m15\033[0;31m simultanious connections are attempted."
            + "\n           You are attempting to monitor "
            + f"\033[1;31m{len(currentAccountData['boxes'])}\033[0;31m mailboxes.\033[0;37m"
        )

    configFile = (
        imapnotifyConfigFolder
        / accountToFoldername(currentAccount)
        / imapnotifyConfigFilename
    )
    configFile.parent.mkdir(exist_ok=True)

    json.dump(currentAccountData, open(configFile, "w"), indent=2)
    print(f" \033[0;35mConfig generated and saved to {configFile}\033[0;37m")

    global successfulAdditions
    successfulAdditions.append(accountToFoldername(currentAccount))


def getMailBoxes(account):
    boxes = subprocess.run(
        ["mbsync", "--list", account], check=True, stdout=subprocess.PIPE, timeout=10.0
    )
    return boxes.stdout.decode("utf-8").strip().split("\n")


def applyPatternFilter(pattern, mailboxes):
    patternRegexs = getPatternRegexes(pattern)
    return [m for m in mailboxes if testPatternRegexs(patternRegexs, m)]


def getPatternRegexes(pattern):
    def addGlob(b):
        blobs.append(b.replace('\\"', '"'))
        return ""

    blobs = []
    pattern = re.sub(r' ?"([^"]+)"', lambda m: addGlob(m.groups()[0]), pattern)
    blobs.extend(pattern.split(" "))
    blobs = [
        (-1, fnmatch.translate(b[1::])) if b[0] == "!" else (1, fnmatch.translate(b))
        for b in blobs
    ]
    return blobs


def testPatternRegexs(regexCond, case):
    for factor, regex in regexCond:
        if factor * bool(re.match(regex, case)) < 0:
            return False
    return True


def processSystemdServices():
    keptAccounts = [acc for acc in successfulAdditions if acc in oldAccounts]
    freshAccounts = [acc for acc in successfulAdditions if acc not in oldAccounts]
    staleAccounts = [acc for acc in oldAccounts if acc not in successfulAdditions]

    if keptAccounts:
        print(f"\033[1;34m{len(keptAccounts)}\033[0;34m kept accounts:\033[0;37m")
        restartAccountSystemdServices(keptAccounts)

    if freshAccounts:
        print(f"\033[1;32m{len(freshAccounts)}\033[0;32m new accounts:\033[0;37m")
        enableAccountSystemdServices(freshAccounts)
    else:
        print(f"\033[0;32mNo new accounts.\033[0;37m")

    notActuallyEnabledAccounts = [
        acc for acc in successfulAdditions if not getAccountServiceState(acc)["enabled"]
    ]
    if notActuallyEnabledAccounts:
        print(
            f"\033[1;32m{len(notActuallyEnabledAccounts)}\033[0;32m accounts need re-enabling:\033[0;37m"
        )
        enableAccountSystemdServices(notActuallyEnabledAccounts)

    if staleAccounts:
        print(f"\033[1;33m{len(staleAccounts)}\033[0;33m removed accounts:\033[0;37m")
        disableAccountSystemdServices(staleAccounts)
    else:
        print(f"\033[0;33mNo removed accounts.\033[0;37m")


def enableAccountSystemdServices(accounts):
    for account in accounts:
        print(f" \033[0;32m - \033[1;37m{account:<18}", end="\033[0;37m", flush=True)
        if setSystemdServiceState(
                "enable", f"goimapnotify@{accountToFoldername(account)}.service"
        ):
            print("\033[1;32m enabled")


def disableAccountSystemdServices(accounts):
    for account in accounts:
        print(f" \033[0;33m - \033[1;37m{account:<18}", end="\033[0;37m", flush=True)
        if setSystemdServiceState(
                "disable", f"goimapnotify@{accountToFoldername(account)}.service"
        ):
            print("\033[1;33m disabled")


def restartAccountSystemdServices(accounts):
    for account in accounts:
        print(f" \033[0;34m - \033[1;37m{account:<18}", end="\033[0;37m", flush=True)
        if setSystemdServiceState(
                "restart", f"goimapnotify@{accountToFoldername(account)}.service"
        ):
            print("\033[1;34m restarted")


def setSystemdServiceState(state, service):
    try:
        enabler = subprocess.run(
            ["systemctl", "--user", state, service, "--now"],
            check=True,
            stderr=subprocess.DEVNULL,
            timeout=5.0,
        )
        return True
    except subprocess.CalledProcessError as e:
        print(
            f" \033[1;31mfailed\033[0;31m to {state}, `{' '.join(e.cmd)}'"
            + f"returned code {e.returncode}\033[0;37m"
        )
    except subprocess.TimeoutExpired as e:
        print(f" \033[1;31mtimed out after {e.timeout:.2f} seconds\033[0;37m")
        return False


def getAccountServiceState(account):
    return {
        state: bool(
            1
            - subprocess.run(
                [
                    "systemctl",
                    "--user",
                    f"is-{state}",
                    "--quiet",
                    f"goimapnotify@{accountToFoldername(account)}.service",
                ],
                stderr=subprocess.DEVNULL,
            ).returncode
        )
        for state in ("enabled", "active", "failing")
    }


def getAccountServiceStates(accounts):
    for account in accounts:
        enabled, active, failing = getAccountServiceState(account).values()
        print(f"  - \033[1;37m{account:<18}\033[0;37m ", end="", flush=True)
        if not enabled:
            print("\033[1;33mdisabled\033[0;37m")
        elif active:
            print("\033[1;32mactive\033[0;37m")
        elif failing:
            print("\033[1;31mfailing\033[0;37m")
        else:
            print("\033[1;35min an unrecognised state\033[0;37m")


if len(sys.argv) > 1:
    if sys.argv[1]   in ["-e", "--enable"]:
        enableAccountSystemdServices(oldAccounts)
        exit()
    elif sys.argv[1] in ["-d", "--disable"]:
        disableAccountSystemdServices(oldAccounts)
        exit()
    elif sys.argv[1] in ["-r", "--restart"]:
        restartAccountSystemdServices(oldAccounts)
        exit()
    elif sys.argv[1] in ["-s", "--status"]:
        getAccountServiceStates(oldAccounts)
        exit()
    elif sys.argv[1] in ["-h", "--help"]:
        print("""\033[1;37mMbsync to IMAP Notify config generator.\033[0;37m

Usage: mbsync-imapnotify [options]

Options:
    -e, --enable       enable all services
    -d, --disable      disable all services
    -r, --restart      restart all services
    -s, --status       fetch the status for all services
    -h, --help         show this help
""", end='')
        exit()
    else:
        print(f"\033[0;31mFlag {sys.argv[1]} not recognised, try --help\033[0;37m")
        exit()


mbsyncData = open(mbsyncFile, "r").read()

currentAccountNumber = 0

totalAccounts = len(re.findall(r"^IMAPAccount", mbsyncData, re.M))


def main():
    print("\033[1;34m:: MbSync to Go IMAP notify config file creator ::\033[0;37m")

    shutil.rmtree(imapnotifyConfigFolder)
    imapnotifyConfigFolder.mkdir(exist_ok=False)
    print("\033[1;30mImap Notify config dir purged\033[0;37m")

    print(f"Identified \033[1;32m{totalAccounts}\033[0;32m accounts.\033[0;37m")

    for line in mbsyncData.split("\n"):
        processLine(line)

    finaliseAccount()

    print(
        f"\nConfig files generated for \033[1;36m{len(successfulAdditions)}\033[0;36m"
        + f" out of \033[1;36m{totalAccounts}\033[0;37m accounts.\n"
    )

    processSystemdServices()


if __name__ == "__main__":
    main()
