from html import escape

import os

from docutils import nodes
from docutils.parsers.rst import Directive, directives


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


def setup(app):
    app.add_directive('try', TryDirective)
