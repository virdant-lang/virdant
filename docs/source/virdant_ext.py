from sphinx.domains import Domain
from docutils.parsers.rst import roles

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import get_lexer_by_name
from docutils import nodes


class VirdantLexer(RegexLexer):
    name = 'Virdant'
    aliases = ['virdant']
    filenames = ['*.vir']

    KEYWORDS = [
        'builtin', 'case', 'ciso', 'client', 'cosi', 'else', 'enum',
        'export', 'ext', 'fn', 'if', 'import', 'incoming',
        'match', 'mod', 'of', 'on', 'outgoing', 'reg',
        'server', 'socket', 'struct', 'type', 'union',
        'unused', 'width', 'wire',
    ]

    BUILTINS = ['Bit', 'Word', 'Vec', 'tuple', 'Nat', 'Shape', 'io']

    tokens = {
        'root': [
            # Multi-character operators (must come before single-character ones)
            (r':=:', Punctuation),
            (r'=>', Punctuation),
            (r'<=', Punctuation),
            (r':=', Punctuation),
            (r'\.\.', Punctuation),
            (r'::', Punctuation),
            (r'->', Punctuation),
            (r'!=', Punctuation),
            (r'==', Punctuation),
            (r'&&', Punctuation),
            (r'\|\|', Punctuation),
            (r'\^\^', Punctuation),
            (r'>=', Punctuation),
            # Single-character operators and punctuation
            (r'!', Punctuation),
            (r'=', Punctuation),
            (r':', Punctuation),
            (r'\.', Punctuation),
            (r'<', Punctuation),
            (r'>', Punctuation),
            (r'[-+()<>{}\[\],;]', Punctuation),
            (r'[&|^~]', Punctuation),
            (r'[@#\$\?]', Punctuation),
            # Number literals with width suffix (must come before plain versions)
            (r'0b[0-1]+w[0-9]+', Number.Integer),
            (r'0x[0-9a-fA-F]+w[0-9]+', Number.Integer),
            (r'[0-9]+w[0-9]+', Number.Integer),
            # Plain binary and hex literals (must come before plain decimal)
            (r'0b[0-1]+', Number.Integer),
            (r'0x[0-9a-fA-F]+', Number.Integer),
            # Regular numbers
            (r'[0-9]+', Number.Integer),
            # Special 'it' keyword (context reference)
            (r'\bit\b', Keyword.Pseudo),
            # Boolean literals
            (r'\b(true|false)\b', Keyword.Constant),
            # Keywords
            (words(KEYWORDS, suffix=r'\b'), Keyword),
            # Special @ prefix (for union constructors)
            (r'@\b([a-zA-Z_][a-zA-Z_0-9]*)\b', Name.Decorator),
            # Whitespace
            (r'\s+', Text),
            # Comments
            (r'//.*$', Comment.Single),
            # Type names (capitalized identifiers)
            (r'\b([A-Z_][a-zA-Z_0-9]*)\b', Name.Class),
            # Enum variants with # prefix
            (r'#[a-zA-Z_][_a-zA-Z_0-9]*', Name.Constant),
            # Question mark patterns
            (r'\?[a-zA-Z_][_a-zA-Z_0-9]*', Name.Constant),
            # Constants
            (words(BUILTINS, suffix=r'\b'), Name.Builtin),
            # Regular identifiers
            (r'\b([a-zA-Z_][_a-zA-Z_0-9]*)\b', Name.Variable),
        ],
    }


class VirdantDomain(Domain):
    name = 'virdant'
    label = 'Virdant Language'

    directives = {}

    roles = {}


def vir_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    """
    Custom role for inline Virdant code with syntax highlighting.
    Usage: :vir:`mod` or just `mod` if it's the default role.
    """
    lexer = VirdantLexer()
    # Use nowrap=True to get just the span elements without the surrounding div
    formatter = HtmlFormatter(nowrap=True, cssclass='vir-inline')
    highlighted = highlight(text, lexer, formatter)
    # Strip newlines to prevent awkward line breaks in inline code
    highlighted = highlighted.replace('\n', '').strip()
    # Create a raw HTML node
    node = nodes.raw('', highlighted, format='html')
    # Add the 'code' class so it gets styled like other inline code
    node['classes'] = ['code', 'vir-inline']
    return [node], []


def verilog_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    """
    Custom role for inline Verilog code with syntax highlighting.
    Usage: :verilog:`input`
    """
    lexer = get_lexer_by_name('verilog')
    # Use nowrap=True to get just the span elements without the surrounding div
    formatter = HtmlFormatter(nowrap=True, cssclass='verilog-inline')
    highlighted = highlight(text, lexer, formatter)
    # Strip newlines to prevent awkward line breaks in inline code
    highlighted = highlighted.replace('\n', '').strip()
    # Create a raw HTML node
    node = nodes.raw('', highlighted, format='html')
    # Add the 'code' class so it gets styled like other inline code
    node['classes'] = ['code', 'verilog-inline']
    return [node], []


def setup(app):
    app.add_domain(VirdantDomain)
    # Register the vir role for inline Virdant syntax highlighting
    app.add_role('vir', vir_role)
    # Register the verilog role for inline Verilog syntax highlighting
    app.add_role('verilog', verilog_role)
