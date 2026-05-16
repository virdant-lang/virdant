from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *


class VerilogLexer(RegexLexer):
    name = 'Verilog'
    aliases = ['verilog', 'v']
    filenames = ['*.v', '*.sv']

    # Verilog/SystemVerilog keywords
    KEYWORDS = [
        'always', 'and', 'assign', 'begin', 'buf', 'bufif0', 'bufif1',
        'case', 'casex', 'casez', 'cmos', 'deassign', 'default', 'defparam',
        'disable', 'edge', 'else', 'end', 'endcase', 'endfunction', 'endgenerate',
        'endmodule', 'endprimitive', 'endspecify', 'endtable', 'endtask',
        'event', 'for', 'force', 'forever', 'fork', 'function', 'generate',
        'genvar', 'highz0', 'highz1', 'if', 'ifnone', 'initial', 'inout',
        'input', 'integer', 'join', 'large', 'localparam', 'macromodule',
        'medium', 'module', 'nand', 'negedge', 'nmos', 'nor', 'not', 'notif0',
        'notif1', 'or', 'output', 'parameter', 'pmos', 'posedge', 'primitive',
        'pull0', 'pull1', 'pulldown', 'pullup', 'rcmos', 'real', 'realtime',
        'reg', 'release', 'repeat', 'rnmos', 'rpmos', 'rtran', 'rtranif0',
        'rtranif1', 'scalared', 'small', 'specify', 'specparam', 'strong0',
        'strong1', 'supply0', 'supply1', 'table', 'task', 'time', 'tran',
        'tranif0', 'tranif1', 'tri', 'tri0', 'tri1', 'triand', 'trior',
        'trireg', 'vectored', 'wait', 'wand', 'weak0', 'weak1', 'while',
        'wire', 'wor', 'xnor', 'xor',
    ]

    # SystemVerilog additional keywords
    SV_KEYWORDS = [
        'alias', 'always_comb', 'always_ff', 'always_latch', 'assert', 'assume',
        'automatic', 'before', 'bind', 'bins', 'binsof', 'bit', 'break', 'byte',
        'chandle', 'class', 'clocking', 'const', 'constraint', 'context', 'continue',
        'cover', 'covergroup', 'coverpoint', 'cross', 'dist', 'do', 'endclass',
        'endclocking', 'endgroup', 'endinterface', 'endpackage', 'endprogram',
        'endproperty', 'endsequence', 'enum', 'expect', 'export', 'extends',
        'extern', 'final', 'first_match', 'foreach', 'forkjoin', 'iff', 'ignore_bins',
        'illegal_bins', 'import', 'inside', 'int', 'interface', 'intersect',
        'join_any', 'join_none', 'local', 'logic', 'longint', 'matches', 'modport',
        'new', 'null', 'package', 'packed', 'priority', 'program', 'property',
        'protected', 'pure', 'rand', 'randc', 'randcase', 'randsequence', 'ref',
        'return', 'sequence', 'shortint', 'shortreal', 'solve', 'static', 'string',
        'struct', 'super', 'tagged', 'this', 'throughout', 'timeprecision',
        'timeunit', 'type', 'typedef', 'union', 'unique', 'var', 'virtual', 'void',
        'wait_order', 'wildcard', 'with', 'within',
    ]

    # Built-in system tasks and functions
    SYSTEM_TASKS = [
        'display', 'write', 'monitor', 'strobe', 'finish', 'stop', 'time',
        'realtime', 'random', 'readmemb', 'readmemh', 'dumpfile', 'dumpvars',
        'dumpon', 'dumpoff', 'dumpall', 'value$plusargs',
    ]

    # Data types
    TYPES = [
        'reg', 'wire', 'integer', 'real', 'time', 'realtime',
        'bit', 'logic', 'byte', 'shortint', 'int', 'longint',
        'string', 'void',
    ]

    tokens = {
        'root': [
            # Whitespace
            (r'\s+', Text),

            # Comments
            (r'//.*$', Comment.Single),
            (r'/\*', Comment.Multiline, 'block_comment'),

            # Compiler directives (preprocessor)
            (r'`\w+', Comment.Preproc),

            # System tasks (starting with $)
            (r'\$[a-zA-Z_]\w*', Name.Builtin),

            # Escaped identifiers (SystemVerilog: \top::Top, etc.)
            (r'\\[^\s]+', Name),

            # Number literals
            # Time literals (must come before regular numbers)
            (r'[0-9]+\s*(s|ms|us|ns|ps|fs)\b', Number),
            # Sized numbers: 32'hDEADBEEF, 8'b10101010, 16'd1234
            (r"[0-9]+('[bBoOdDhH])[0-9a-fA-FxXzZ?_]+", Number.Hex),
            # Unsized numbers with base
            (r"'[bBoOdDhH][0-9a-fA-FxXzZ?_]+", Number.Hex),
            # Regular decimal numbers
            (r'[0-9_]+(\.[0-9_]+)?([eE][+-]?[0-9_]+)?', Number),

            # String literals
            (r'"([^"\\]|\\.)*"', String),

            # Keywords
            (words(KEYWORDS + SV_KEYWORDS, suffix=r'\b'), Keyword),

            # Operators
            (r'[+\-*/%&|^~!]', Operator),
            (r'[<>=!]=?', Operator),
            (r'&&|\|\||<<|>>|>>>|\*\*', Operator),
            (r'\?|:', Operator),

            # Punctuation
            (r'[{}()\[\];,.]', Punctuation),
            (r'[@#]', Punctuation),

            # Identifiers (must come after keywords)
            (r'[a-zA-Z_]\w*', Name),
        ],
        'block_comment': [
            (r'.*?\*/', Comment.Multiline, '#pop'),
            (r'.*$', Comment.Multiline),
        ],
    }
