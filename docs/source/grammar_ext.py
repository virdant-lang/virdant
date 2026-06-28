from pygments.lexer import RegexLexer, bygroups
from pygments.token import *


class GrammarLexer(RegexLexer):
    name = 'Grammar'
    aliases = ['grammar']
    filenames = ['*.ebnf']

    tokens = {
        'root': [
            # Line comments
            (r'//.*$', Comment.Single),

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

            # Ellipsis
            (r'\.\.\.', Punctuation),

            # Colon (for productions like "ident:")
            (r':', Punctuation),

            # Hyphen (in character ranges like a-z, or standalone)
            (r'-', Punctuation),

            # The alternation operator
            (r'\|', Operator),

            # Arrow
            (r'=>', Punctuation),

            # Grouping parentheses
            (r'[()]', Punctuation),

            # Digits (standalone, like '0', '9' in char class ranges)
            (r'[0-9]', Number),

            # Whitespace
            (r'\s+', Text),
        ],
    }