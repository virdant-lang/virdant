/**
 * Shrink the font size of code blocks (literalinclude & co.) so that
 * long lines fit the available width, instead of overflowing horizontally.
 *
 * On each (re)fit, every block is first reset to its natural font size,
 * then shrunk step by step while the content is wider than the container.
 * Widening the window therefore restores the original size, and shrinking
 * re-fits again. Font size never drops below MIN_FONT_SIZE (px); below
 * that the block falls back to the usual horizontal scrollbar.
 */

(function() {
    const MIN_FONT_SIZE = 10; // px
    const STEP = 1;           // px

    function fitBlock(block) {
        const pre = block.querySelector('pre');
        if (!pre) return;

        const available = block.clientWidth;
        if (!available) return; // hidden or zero-width; leave alone

        // Reset to the natural size (respecting theme/media-query rules)
        // so we measure a true baseline.
        pre.style.removeProperty('font-size');
        const base = parseFloat(getComputedStyle(pre).fontSize);
        if (!base || !isFinite(base)) return;

        // The site's global `code, pre { font-size: ... !important }` rules
        // override inherited sizes, so our size must also carry !important.
        pre.style.setProperty('font-size', base + 'px', 'important');

        let size = base;
        while (size > MIN_FONT_SIZE && pre.scrollWidth > block.clientWidth) {
            size = Math.max(MIN_FONT_SIZE, size - STEP);
            pre.style.setProperty('font-size', size + 'px', 'important');
        }
    }

    function fitAll() {
        document.querySelectorAll('div.highlight').forEach(fitBlock);
    }

    document.addEventListener('DOMContentLoaded', function() {
        fitAll();

        // Re-fit once web fonts have loaded; glyph widths can change.
        if (document.fonts && document.fonts.ready) {
            document.fonts.ready.then(fitAll);
        }
    });

    let resizeTimer;
    window.addEventListener('resize', function() {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(fitAll, 150);
    });
})();
