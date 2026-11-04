from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *


# Keywords from the `Tokens:` section of GRAMMAR.txt.
KEYWORDS = [
    'builtin', 'bus', 'client', 'cosi', 'default', 'dependson',
    'dontcare', 'dyn', 'else', 'enum', 'ext', 'for', 'if', 'import',
    'incoming', 'initial', 'it', 'let', 'match', 'mod', 'net', 'of',
    'on', 'outgoing', 'platform', 'pub', 'reg', 'server', 'soci',
    'socket', 'struct', 'tap', 'type', 'union', 'unused', 'when',
    'wire', 'with',
]

# Lexical token names, which appear as quoted terminals in GRAMMAR.txt.
TOKEN_NAMES = [
    'DocBang', 'DocComment', 'Ident', 'Nat', 'Str', 'Word',
]


class GrammarLexer(RegexLexer):
    name = 'Grammar'
    aliases = ['grammar']
    filenames = ['*.ebnf']

    tokens = {
        'root': [
            # Line comments
            (r'//.*$', Comment.Single),

            # Quoted terminals which are keywords (must come before generic strings)
            (words(KEYWORDS, prefix=r'"', suffix=r'"'), Keyword),

            # Boolean literals
            (r'"(true|false)"', Keyword.Constant),

            # Lexical token names like "DocComment", "Ident", "Nat"
            (words(TOKEN_NAMES, prefix=r'"', suffix=r'"'), Name.Builtin),

            # String literals (double-quoted)
            (r'"([^"\\]|\\.)*"', String),

            # Character ranges like a-z, A-Z, 0-9
            (r'([a-zA-Z0-9])(-)([a-zA-Z0-9])',
             bygroups(Name, Operator, Name)),

            # Repetition operators attached to uppercase non-terminals
            # Must come before plain non-terminals
            (r'\b([A-Z][a-zA-Z0-9]*)([*+?])', bygroups(Name.Class, Punctuation)),

            # Repetition operators attached to lowercase non-terminals
            (r'\b([a-z][a-zA-Z0-9]*)([*+?])', bygroups(Name, Punctuation)),

            # Non-terminal names (CamelCase, starting with uppercase)
            (r'\b[A-Z][a-zA-Z0-9]*\b', Name.Class),

            # Non-terminal names (lowercase, like 'ident', 'nat')
            (r'\b[a-z][a-zA-Z0-9]*\b', Name),

            # Standalone repetition operators (after closing paren, etc.)
            (r'[*+?]', Punctuation),

            # The assignment operator
            (r':=', Operator),

            # Multi-character operators and punctuation
            # (must come before single-character rules)
            (r':=:', Punctuation),
            (r'#=#', Punctuation),
            (r'#-', Punctuation),
            (r'#=', Punctuation),
            (r'::', Punctuation),
            (r'\.\.', Punctuation),
            (r'->', Punctuation),
            (r'<=', Punctuation),
            (r'>=', Punctuation),
            (r'==', Punctuation),
            (r'!=', Punctuation),
            (r'&&', Punctuation),
            (r'\|\|', Punctuation),
            (r'\^\^', Punctuation),
            (r'\+\.', Punctuation),
            (r'-\.', Punctuation),
            (r'\.\.\.', Punctuation),

            # The alternation operator
            (r'\|', Operator),

            # Arrow
            (r'=>', Punctuation),

            # Single-character punctuation
            (r'[(){}\[\],.;<>$#@~&^!-]', Punctuation),

            # Colon
            (r':', Punctuation),

            # Digits (standalone, like '0', '9' in char class ranges)
            (r'[0-9]', Number),

            # Whitespace
            (r'\s+', Text),
        ],
    }
