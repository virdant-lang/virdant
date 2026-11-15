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
        'builtin', 'bus', 'client', 'cosi', 'default', 'dependson',
        'dontcare', 'dyn', 'else', 'enum', 'ext', 'for', 'if', 'import',
        'incoming', 'initial', 'it', 'let', 'match', 'mod', 'net', 'of',
        'on', 'outgoing', 'platform', 'pub', 'reg', 'server', 'soci',
        'socket', 'struct', 'tap', 'type', 'union', 'unused', 'when',
        'where', 'wire', 'with',
    ]

    BUILTINS = ['Bit', 'Word', 'Vec', 'tuple', 'Nat', 'Shape', 'io']

    tokens = {
        'root': [
            # Multi-character operators (must come before single-character ones)
            (r':=:', Punctuation),
            (r'#=#', Punctuation),
            (r'#=', Punctuation),
            (r'#-', Punctuation),
            (r'\+\.', Punctuation),
            (r'-\.', Punctuation),
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
            (r'[-+()<>{}\[\],;~]', Punctuation),
            (r'[&|^~]', Punctuation),
            (r'[#\$\?]', Punctuation),
            # Annotations (@name, @name(...)) must come before plain @
            (r'@[a-zA-Z_][_a-zA-Z_0-9]*', Name.Decorator),
            (r'@', Punctuation),
            # String literals (only double-quoted; see syntax/token.rs)
            (r'"', String.Double, 'string'),
            # Number literals with width suffix (must come before plain versions)
            (r'0b[0-1][0-1_]*w[0-9][0-9_]*', Number.Integer),
            (r'0x[0-9a-fA-F][0-9a-fA-F_]*w[0-9][0-9_]*', Number.Integer),
            (r'[0-9][0-9_]*w[0-9][0-9_]*', Number.Integer),
            # Plain binary and hex literals (must come before plain decimal)
            (r'0b[0-1][0-1_]*', Number.Integer),
            (r'0x[0-9a-fA-F][0-9a-fA-F_]*', Number.Integer),
            # Regular numbers
            (r'[0-9][0-9_]*', Number.Integer),
            # Special 'it' keyword (context reference)
            (r'\bit\b', Keyword.Pseudo),
            # Boolean literals
            (r'\b(true|false)\b', Keyword.Constant),
            # Keywords
            (words(KEYWORDS, suffix=r'\b'), Keyword),
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
        'string': [
            (r'"', String.Double, '#pop'),
            # Single-character escapes: \a \b \e \f \n \r \t \v \\ \' \" \?
            (r'\\[abefnrtv\\\'"?]', String.Escape),
            # Hex escapes: \xNN (exactly two hex digits)
            (r'\\x[0-9a-fA-F]{2}', String.Escape),
            # Octal escapes: \0 through \777
            (r'\\[0-7]{1,3}', String.Escape),
            # Fallback for invalid escapes (still highlight, don't break the string)
            (r'\\.', String.Escape),
            # Any bytes except quote and backslash, verbatim
            (r'[^"\\]+', String.Double),
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
