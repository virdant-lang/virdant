from html import escape

import os
import re

from docutils import nodes
from docutils.parsers.rst import Directive, directives

from sphinx.directives.code import LiteralInclude


class TryDirective(Directive):
    """
    Custom directive for "Try it" buttons.
    Usage:

        .. try::
            :project: buffer
            :label: Try Buffer

    Renders a green button that opens the Try dock on the given seeded
    project (via `virtry.load` — see trydock-boot.js) and resets it
    unconditionally.

    `:project:` is required and must name a guide project directory
    (docs/source/guide/<project>); unknown names are rejected at
    build time so typos never ship as dead buttons.  `:label:` is
    optional and defaults to "Try <Project>".  Directive content, if
    given, is also accepted as the label.
    """

    has_content = True
    option_spec = {
        'project': directives.unchanged_required,
        'label': directives.unchanged,
    }

    def run(self):
        env = self.state.document.settings.env
        project = (self.options.get('project') or '').strip()
        label = (self.options.get('label') or '').strip()
        if self.content:
            label = label or '\n'.join(self.content).strip()

        if not project:
            raise self.error('.. try:: needs a :project: option naming a guide project')
        if not os.path.isdir(os.path.join(env.srcdir, 'guide', project)):
            raise self.error(
                f'.. try:: :project: {project} — no guide project "guide/{project}"')

        if not label:
            label = f'Try {project[0].upper()}{project[1:]}'

        html = (
            '<button type="button" class="try-button"'
            ' data-virtry-project="%s">%s</button>'
        ) % (escape(project, quote=True), escape(label))
        return [nodes.raw('', html, format='html')]


class VirGotoLiteralInclude(LiteralInclude):
    """`literalinclude` plus a green "Try" button for .vir sources.

    For every include of a ``.vir`` file, a green button is pinned to
    the lower right of the code block.  Clicking it calls
    `virtry.goto_source("<FILE>.vir:<LINE>")` (see trydock-boot.js):
    FILE is the included file's basename — matching the Try dock's
    seeded project file names — and LINE is the first line of the
    ``:lines:`` window (1 when no line number is given, i.e. the whole
    file is shown).  A failed goto opens the dock's reset prompt, as
    if the user had clicked the dock's Reset button: the session's
    sources no longer match the docs' example.

    Assumes ``:lines:`` is the only line-selection mechanism in use.
    """

    def run(self):
        result = super().run()
        path = self.arguments[0].strip()
        base = os.path.basename(path)
        if not base.endswith('.vir'):
            return result

        project = self._snippet_project(path)
        if not project:
            return result  # no seed project derivable: no button

        # First line of the ``:lines:`` window ("8-9" → 8; "8" → 8);
        # no ``:lines:`` (or an unparseable spec) → line 1.
        m = re.match(r'\s*(\d+)', str(self.options.get('lines', '')))
        lineno = int(m.group(1)) if m else 1

        btn = nodes.raw('', (
            '<button type="button" class="try-button vir-goto-btn"'
            ' data-virtry-goto="%s" data-virtry-project="%s"'
            ' title="Open this snippet in the Try dock">Try</button>'
        ) % (escape(f'{base}:{lineno}', quote=True),
             escape(project, quote=True)), format='html')

        # Pin the button to the code block itself — not to a caption
        # that may sit above it: wrap the literal_block in a positioned
        # container, wherever it sits in the doctree (bare, or inside
        # the caption wrapper Sphinx builds for :caption:).  The nodes
        # are not attached to the document yet, so this reshapes the
        # directive result directly instead of using replace_self.
        def wrap(block):
            wrapper = nodes.container('', classes=['vir-goto-wrap'])
            wrapper += block
            wrapper += btn
            return wrapper

        # Case 1: the block is the result itself (no :caption:).
        # Case 2: :caption: — Sphinx wraps block + caption in a
        # container; swap the block within it.  (The nodes are not
        # attached to the document yet, so the doctree is not
        # reachable from here — restructure the result directly.)
        for i, node in enumerate(list(result)):
            if isinstance(node, nodes.literal_block):
                result[i] = wrap(node)
                break
            for j, child in enumerate(list(node.children)):
                if isinstance(child, nodes.literal_block):
                    wrapper = wrap(child)
                    node.children[j] = wrapper
                    wrapper.parent = node
                    break
            else:
                continue
            break
        return result

    def _snippet_project(self, path):
        """The seed project a snippet belongs to.

        Derived from the include path — ``/guide/<project>/...`` or
        ``/examples/<project>/...`` — or, for relative paths, from the
        including page's location (``guide/<project>/index``).
        """
        m = re.search(r'/(?:guide|examples)/([^/]+)/', '/' + path.strip('/'))
        if m:
            return m.group(1)
        env = self.state.document.settings.env
        parts = env.docname.split('/')
        if len(parts) >= 2 and parts[0] == 'guide':
            return parts[1]
        return None


def setup(app):
    app.add_directive('try', TryDirective)
    # Our literalinclude only decorates; the original directive still
    # does all the work (see VirGotoLiteralInclude).
    app.add_directive('literalinclude', VirGotoLiteralInclude, override=True)
