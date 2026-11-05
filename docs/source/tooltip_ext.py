from html import escape

from docutils import nodes


def tooltip_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    """
    Custom role for tooltips.
    Usage: :tooltip:`SPI (Serial Peripheral Interface)`
    Renders "SPI" with a hover tooltip showing "Serial Peripheral Interface".

    Note: this is intentionally a separate role from the `.. tip::`
    admonition, with its own CSS class, so the two never share styling.
    """
    # Parse "term (tooltip)" format
    if '(' not in text or not text.endswith(')'):
        # Fallback: just show the text as-is
        node = nodes.inline(text, text)
        return [node], []

    term, rest = text.split('(', 1)
    term = term.rstrip()
    tooltip_text = rest.rstrip(')').strip()

    html = '<span class="tooltip-inline" data-tooltip="%s">%s</span>' % (
        escape(tooltip_text, quote=True),
        escape(term),
    )
    node = nodes.raw('', html, format='html')
    return [node], []


def setup(app):
    app.add_role('tooltip', tooltip_role)
