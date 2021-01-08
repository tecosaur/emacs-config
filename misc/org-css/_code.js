function copyPreToClipbord(btn) {
    const pre = btn.parentElement.parentElement.getElementsByTagName("PRE")[0];
    const range = document.createRange();
    range.selectNodeContents(pre);
    range.setEnd(pre.childNodes[pre.childNodes.length-1], 0);
    window.getSelection().addRange(range);
    var successful = document.execCommand('copy');
    window.getSelection().removeRange(range);
}
