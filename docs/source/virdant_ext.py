from sphinx.domains import Domain
from docutils.parsers.rst import roles

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *
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
            # Number literals (Word literals with width)
            (r'[0-9][0-9_]*w[0-9][0-9]*', Number.Integer),
            (r'[0-9]b[0-1_]*w[0-9][0-9]*', Number.Integer),
            (r'[0-9]x[0-9a-fA-F_]*w[0-9][0-9]*', Number.Integer),
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
            (r'/\*', Comment.Multiline, 'block_comment'),
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
        'block_comment': [
            (r'.*?\*/', Comment.Multiline, '#pop'),
            (r'.*$', Comment.Multiline),
        ],
    }


class VirdantDomain(Domain):
    name = 'virdant'
    label = 'Virdant Language'

    directives = {}

    roles = {}


def setup(app):
    app.add_domain(VirdantDomain)
