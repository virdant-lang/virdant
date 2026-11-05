/**
 * Hover "current time cursor" for VCD waveform figures (.vcd-svg).
 *
 * Each .. vcd:: figure is an inline SVG rendered by vcd_ext.py with:
 *   - a topmost <g class="vcd-waveform"> holding the waveform, translated
 *     right by the width of the signal-name gutter,
 *   - data-waveform-x0 / data-waveform-x1: the waveform area's x range in
 *     SVG user units (excluding the gutter and right padding),
 *   - data-waveform-y0 / data-waveform-y1: the y range spanned by the
 *     signal rows.
 *
 * Hovering over the waveform area draws a vertical cursor under the
 * pointer, so the reader can see exactly which instant in time the line
 * marks and read the signal values there.  The cursor is hidden while the
 * pointer is over the signal-name gutter or outside the figure.
 */

document.addEventListener('DOMContentLoaded', function () {
    document.querySelectorAll('.vcd-svg').forEach(function (svg) {
        if (!svg.querySelector('g.vcd-waveform')) return;

        const x0 = parseFloat(svg.getAttribute('data-waveform-x0'));
        const x1 = parseFloat(svg.getAttribute('data-waveform-x1'));
        const y0 = parseFloat(svg.getAttribute('data-waveform-y0'));
        const y1 = parseFloat(svg.getAttribute('data-waveform-y1'));
        if ([x0, x1, y0, y1].some(isNaN) || x1 <= x0 || y1 <= y0) return;

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

        function hide() {
            cursor.style.display = 'none';
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
        }

        svg.addEventListener('mousemove', function (e) {
            show(e.clientX);
        });
        svg.addEventListener('mouseleave', hide);
    });
});
