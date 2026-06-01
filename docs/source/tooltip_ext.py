from docutils import nodes


def tip_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    """
    Custom role for tooltips.
    Usage: :tip:`SPI (Serial Peripheral Interface)`
    Renders "SPI" with a hover tooltip showing "Serial Peripheral Interface".
    """
    # Parse "term (tooltip)" format
    if '(' not in text or not text.endswith(')'):
        # Fallback: just show the text as-is
        node = nodes.inline(text, text)
        return [node], []

    term, rest = text.split('(', 1)
    term = term.rstrip()
    tooltip_text = rest.rstrip(')').strip()

    html = '<span class="tip" data-tooltip="%s">%s</span>' % (
        tooltip_text.replace('"', '&quot;'),
        term,
    )
    node = nodes.raw('', html, format='html')
    return [node], []


def setup(app):
    app.add_role('tip', tip_role)
