function copyPreToClipdord(btn) {
    const pre = btn.parentElement.parentElement.getElementsByTagName("PRE")[0];
    const range = document.createRange();
    range.selectNode(pre);
    window.getSelection().addRange(range);
    var successful = document.execCommand('copy');
    window.getSelection().removeRange(range);
}
