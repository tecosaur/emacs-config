// @license magnet:?xt=urn:btih:d3d9a9a6595521f9666a5e94cc830dab83b65699&dn=expat.txt Expat
function copyPreToClipbord(btn) {
    const pre = btn.parentElement.parentElement.getElementsByTagName("PRE")[0];
    const range = document.createRange();
    range.selectNodeContents(pre);
    range.setEnd(pre.childNodes[pre.childNodes.length-1], 0);
    window.getSelection().addRange(range);
    var successful = document.execCommand('copy');
    window.getSelection().removeRange(range);
}
window.addEventListener('DOMContentLoaded', () => {
    if (document.getElementById("text-table-of-contents")) {
        const sections = document.querySelectorAll('h1[id],h2[id],h3[id],h4[id],h5[id],h6[id]');
        const activate = (entry) => {
            entry.classList.add('active');
            if (["LI", "UL"].includes(entry.parentElement.tagName)) {
                activate(entry.parentElement);
            }
        };
        const activateLast = () => {
            document.querySelectorAll('#text-table-of-contents li.active, #text-table-of-contents ul.active').forEach(a => {
                a.classList.remove('active')
            });
            let mostRecent = { section: sections[0], bottom: -Infinity };
            const windowHeight = window.innerHeight;
            sections.forEach((section) => {
                const bounds = section.getBoundingClientRect()
                if ( bounds.bottom > mostRecent.bottom && bounds.top < windowHeight ) {
                    mostRecent = { section, bottom: bounds.bottom };
                }
            })
            activate(document.querySelector(`#text-table-of-contents li a[href="#${mostRecent.section.getAttribute('id')}"]`).parentElement);
        }
        const observer = new IntersectionObserver(entries => {
            activateLast();
        });
        sections.forEach((section) => {
            observer.observe(section);
        });}
});
// @license-end
