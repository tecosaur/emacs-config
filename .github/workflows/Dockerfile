FROM ubuntu:22.04
MAINTAINER TEC <tec@tecosaur.com>

ENV LANG=C.UTF-8
ENV DEBIAN_FRONTEND=noninteractive

RUN echo "\033[1;34mUpdate apt repo.\033[0m" &&\
  apt-get update

RUN echo "\033[1;34mTeX install.\033[0m" &&\
  apt-get update &&\
# Modified version of <https://github.com/thomasWeise/docker-texlive-full>
  printf 'path-exclude /usr/share/doc/*\npath-include /usr/share/doc/*/copyright\npath-exclude /usr/share/man/*\npath-exclude /usr/share/groff/*\npath-exclude /usr/share/info/*\npath-exclude /usr/share/lintian/*\npath-exclude /usr/share/linda/*\npath-exclude=/usr/share/locale/*' > /etc/dpkg/dpkg.cfg.d/01_nodoc &&\
# remove doc files and man pages already installed
  rm -rf /usr/share/groff/* /usr/share/info/* &&\
  rm -rf /usr/share/lintian/* /usr/share/linda/* /var/cache/man/* &&\
  rm -rf /usr/share/man &&\
  mkdir -p /usr/share/man &&\
  find /usr/share/doc -depth -type f ! -name copyright -delete &&\
  find /usr/share/doc -type f -name "*.pdf" -delete &&\
  find /usr/share/doc -type f -name "*.gz" -delete &&\
  find /usr/share/doc -type f -name "*.tex" -delete &&\
  (find /usr/share/doc -type d -empty -delete || true) &&\
  mkdir -p /usr/share/doc &&\
  mkdir -p /usr/share/info &&\
# install TeX Live and ghostscript as well as other tools (inkscape for svg)
  apt-get install -y texlive-base texlive-latex-recommended texlive-fonts-extra latexmk inkscape &&\
# delete Tex Live sources and other potentially useless stuff
  echo "\033[1;34mDelete TeX Live sources and other useless stuff.\033[0m" &&\
  (rm -rf /usr/share/texmf/source || true) &&\
  (rm -rf /usr/share/texlive/texmf-dist/source || true) &&\
  find /usr/share/texlive -type f -name "readme*.*" -delete &&\
  find /usr/share/texlive -type f -name "README*.*" -delete &&\
  (rm -rf /usr/share/texlive/release-texlive.txt || true) &&\
  (rm -rf /usr/share/texlive/doc.html || true) &&\
  (rm -rf /usr/share/texlive/index.html || true) &&\
  find /usr/share/doc -depth -type f ! -name copyright -delete &&\
  find /usr/share/doc -type f -name "*.pdf" -delete &&\
  find /usr/share/doc -type f -name "*.gz" -delete &&\
  find /usr/share/doc -type f -name "*.tex" -delete

RUN echo "\033[1;34mInstalling Emacs 29.\033[0m" &&\
  apt-get install -y apt-utils software-properties-common &&\
  add-apt-repository -y ppa:ubuntuhandbook1/emacs &&\
  apt-get update &&\
  apt-get install -y emacs emacs-common

RUN echo "\033[1;34mInstalling utilities.\033[0m" &&\
  apt-get install -y sudo git gcc sqlite sqlite3 wget ncurses-term ripgrep xclip curl cairosvg

RUN echo "\033[1;36mRemoving extranious files.\033[0m" &&\
  apt-get clean -y &&\
  rm -rf /var/lib/apt/lists/* &&\
  rm -f /etc/ssh/ssh_host_* &&\
  rm -rf /usr/share/groff/* /usr/share/info/* &&\
  rm -rf /usr/share/lintian/* /usr/share/linda/* /var/cache/man/* &&\
  rm -rf /usr/share/man &&\
  mkdir -p /usr/share/man &&\
  (find /usr/share/doc -type d -empty -delete || true) &&\
  mkdir -p /usr/share/doc &&\
  mkdir -p /usr/share/info &&\
  rm -rf /var/cache/apt/archives &&\
  mkdir -p /var/cache/apt/archives &&\
  rm -rf /tmp/* /var/tmp/* &&\
  (find /usr/share/ -type f -empty -delete || true) &&\
  (find /usr/share/ -type d -empty -delete || true) &&\
  mkdir -p /usr/share/texmf/source &&\
  mkdir -p /usr/share/texlive/texmf-dist/source


RUN echo "\033[1;36mDon't check github's SSH key\033[0m" &&\
  printf "Host github.com\n  StrictHostKeyChecking no\n" >> /etc/ssh/ssh_config

RUN echo "\033[1;36mAdd non-root user.\033[0m" &&\
  groupadd runner &&\
  useradd -m -g runner -G sudo -u 1000 -c "Docker image runner" runner &&\
  echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers &&\
  echo "Add command to run command with user 'runner'" &&\
  printf '#!/usr/bin/env sh\nsudo -i -u runner bash <<EOF\n$@\nEOF\n' > /bin/runner &&\
  chmod +x /bin/runner

USER runner
