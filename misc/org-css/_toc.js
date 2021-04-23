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
