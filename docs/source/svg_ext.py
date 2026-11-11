# Inline SVG embedding, shared by any hand-authored .svg figures.
#
# `.. image::` renders the file as an <img> tag, which cannot inherit the
# page's CSS (no var(--text-main), no dark-mode switch).  Embedding the SVG
# inline keeps it styleable, so figures can use the same typography and
# theming variables as the surrounding text.
#
# Usage:
#
#     .. inline-svg:: halfadder_addition.svg
#         :align: center
#
# The path is relative to the current document's directory (the same
# resolution `.. image::` uses). A leading '/' makes it relative to the
# Sphinx source root instead.

import os

from docutils import nodes
from docutils.parsers.rst import Directive, directives
from sphinx.util import logging

logger = logging.getLogger(__name__)


class InlineSvgDirective(Directive):
    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = False
    option_spec = {
        'align': directives.unchanged,
    }

    def run(self):
        env = self.state.document.settings.env
        filename = self.arguments[0]

        # Resolve relative to the doc's directory (like `.. image::`),
        # or the source root if the path starts with '/'.
        _, path = env.relfn2path(filename)
        if not os.path.isfile(path):
            logger.warning(
                f'inline-svg: file not found: {filename}',
                location=(env.docname, self.lineno))
            para = nodes.paragraph()
            para += nodes.literal(filename, filename)
            return [para]

        with open(path, encoding='utf-8') as f:
            svg = f.read()

        # Safety check so an accidental non-SVG file doesn't inject HTML.
        if '<svg' not in svg[:200]:
            logger.warning(
                f'inline-svg: {filename} does not look like an SVG file',
                location=(env.docname, self.lineno))
            para = nodes.paragraph()
            para += nodes.literal(filename, filename)
            return [para]

        align = self.options.get('align', 'center').strip().lower()
        # text-align centers the svg block (via margin auto) and the caption;
        # display:block + margin:auto is the reliable method for a fixed-width
        # svg element.
        html = (
            f'<div class="inline-svg inline-svg--{align}">'
            f'{svg}'
            f'</div>')
        return [nodes.raw('', html, format='html')]


def setup(app):
    app.add_directive('inline-svg', InlineSvgDirective)
    app.add_css_file('inline_svg.css')
