/**
 * Hover "current time cursor" and value popup for VCD waveform figures
 * (.vcd-svg).
 *
 * Each .. vcd:: figure is an inline SVG rendered by vcd_ext.py with:
 *   - a topmost <g class="vcd-waveform"> holding the waveform, translated
 *     right by the width of the signal-name gutter,
 *   - data-waveform-x0 / data-waveform-x1: the waveform area's x range in
 *     SVG user units (excluding the gutter and right padding),
 *   - data-waveform-y0 / data-waveform-y1: the y range spanned by the
 *     signal rows,
 *   - data-ticks: the number of ticks on the waveform grid,
 *   - data-values: a JSON array of per-signal runs, one entry per signal
 *     row in drawing order, each {"n": name, "runs": [[startTick, endTick,
 *     fullValueText], ...]}.  The value text is the signal's full value --
 *     the inline label may be abbreviated with an ellipsis, but the popup
 *     always shows the whole thing.
 *
 * Hovering over the waveform area draws a vertical cursor under the
 * pointer and a floating popup anchored above the first signal row,
 * listing every signal's value at that instant.  The popup stays anchored
 * to the waveform while the page (or an overflowing figure box) scrolls:
 * it is repositioned on every scroll event rather than scrolling away.
 * The cursor is hidden while the pointer is over the signal-name gutter
 * or outside the figure.
 */

document.addEventListener('DOMContentLoaded', function () {
    document.querySelectorAll('.vcd-svg').forEach(function (svg) {
        if (!svg.querySelector('g.vcd-waveform')) return;

        const x0 = parseFloat(svg.getAttribute('data-waveform-x0'));
        const x1 = parseFloat(svg.getAttribute('data-waveform-x1'));
        const y0 = parseFloat(svg.getAttribute('data-waveform-y0'));
        const y1 = parseFloat(svg.getAttribute('data-waveform-y1'));
        if ([x0, x1, y0, y1].some(isNaN) || x1 <= x0 || y1 <= y0) return;

        const nTicks = parseInt(svg.getAttribute('data-ticks'), 10);
        let rows = [];
        try {
            rows = JSON.parse(svg.getAttribute('data-values') || '[]');
        } catch (e) {
            rows = [];
        }
        const hasPopup = !isNaN(nTicks) && nTicks > 0 && rows.length > 0;

        // A direct child of the <svg> (not of the translated waveform
        // <g>), appended last so it draws on top of the waveform.  All its
        // coordinates are plain SVG-root user units -- the same space the
        // data-* attributes are expressed in -- so no transform math is
        // ever needed.
        const cursor = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        cursor.setAttribute('class', 'vcd-cursor');
        cursor.setAttribute('x1', x0);
        cursor.setAttribute('x2', x0);
        cursor.setAttribute('y1', y0);
        cursor.setAttribute('y2', y1);
        cursor.style.display = 'none';
        svg.appendChild(cursor);

        // The value popup: a fixed-position div (not inside the SVG, so it
        // is not clipped by the figure's scrollable box and never scales
        // with CSS figure scaling).  Appended to the body so it can follow
        // the cursor anywhere in the viewport.
        const tip = document.createElement('div');
        tip.className = 'vcd-tip';
        tip.style.display = 'none';
        document.body.appendChild(tip);

        // Values are user data (signal names and decoded values), not
        // trusted HTML -- e.g. an undecodable type value renders literally
        // as "<invalid>".  Escape everything interpolated into the popup.
        function escapeHtml(text) {
            const div = document.createElement('div');
            div.textContent = text;
            return div.innerHTML;
        }

        function hide() {
            cursor.style.display = 'none';
            tip.style.display = 'none';
            lastX = null;
        }

        // Position the popup for the cursor's x (SVG user units), leaving
        // its contents alone.  Called both from show() and from the scroll
        // reposition handler below.
        function placeTip(x) {
            const rect = svg.getBoundingClientRect();
            // The renderer sizes the SVG to match its viewBox exactly, so
            // this scale is normally 1; compute it anyway so the cursor
            // stays correct if the figure is ever CSS-scaled or scrolled.
            const scale = rect.width / svg.viewBox.baseVal.width;

            // Anchor the popup above the waveform's first signal row
            // (data-waveform-y0), so it never covers the signal text it
            // describes; horizontally it follows the cursor, clamped to
            // the viewport: flip to the cursor's left when the popup would
            // run off the right edge.  When the figure is at the very top
            // of the viewport there is no room above the signal rows, so
            // the popup clamps to the viewport top -- it then overlaps the
            // figure's head line only, which is figure metadata, not
            // signal text.
            const tipRect = tip.getBoundingClientRect();
            const gap = 6;
            let left = rect.left + x * scale + gap;
            if (left + tipRect.width > window.innerWidth - 4) {
                left = rect.left + x * scale - tipRect.width - gap;
            }
            left = Math.max(4, Math.min(left, window.innerWidth - tipRect.width - 4));
            let top = rect.top + y0 * scale - tipRect.height - gap;
            top = Math.max(4, top);
            tip.style.left = left + 'px';
            tip.style.top = top + 'px';
        }

        function show(clientX) {
            const rect = svg.getBoundingClientRect();
            // The renderer sizes the SVG to match its viewBox exactly, so
            // this scale is normally 1; compute it anyway so the cursor
            // stays correct if the figure is ever CSS-scaled or scrolled.
            const scale = rect.width / svg.viewBox.baseVal.width;
            const x = (clientX - rect.left) / scale;

            // Only over the waveform itself (not the name gutter, not the
            // right padding).
            if (x < x0 || x > x1) {
                hide();
                return;
            }
            cursor.setAttribute('x1', x);
            cursor.setAttribute('x2', x);
            cursor.style.display = '';

            if (!hasPopup) return;

            // The tick grid is uniform (both the plain and the stretched
            // grid are), so the tick under the cursor is exact arithmetic.
            const tick = Math.floor((x - x0) * nTicks / (x1 - x0));
            let html = '';
            rows.forEach(function (row) {
                let run = null;
                for (const r of row.runs) {
                    if (tick >= r[0] && tick < r[1]) { run = r; break; }
                }
                if (!run) run = row.runs[row.runs.length - 1];
                const value = run ? run[2] : '';
                html += `<div class="vcd-tip-row"><span class="vcd-tip-name">${escapeHtml(row.n)}</span>`
                      + `<span class="vcd-tip-val">${escapeHtml(value)}</span></div>`;
            });
            tip.innerHTML = html;
            tip.style.display = '';
            lastX = x;
            placeTip(x);
        }

        // The popup is position:fixed, so it would sit still while the
        // page (or an enclosing scrollable figure box) scrolls under it --
        // detaching from the waveform tick it annotates.  While it is
        // visible, re-anchor it on every scroll so it stays fixed relative
        // to the waveform: above the same first signal row, at the same
        // cursor x.  The pointer does not have to keep moving for this, so
        // a plain mouse-wheel scroll (which fires no mousemove) must be
        // caught too -- hence these listeners, not a mousemove-only hook.
        // The capture phase is needed because scroll events do not bubble:
        // without it, scrolling a .vcd-body overflow box inside the page
        // would never reach a plain window-level listener.
        let lastX = null;
        let rafPending = false;
        function reposition() {
            if (lastX === null) return;
            if (rafPending) return;
            rafPending = true;
            requestAnimationFrame(function () {
                rafPending = false;
                if (lastX === null) return;
                placeTip(lastX);
            });
        }
        document.addEventListener('scroll', reposition, true);
        window.addEventListener('resize', reposition);

        svg.addEventListener('mousemove', function (e) {
            show(e.clientX);
        });
        svg.addEventListener('mouseleave', hide);
    });
});
