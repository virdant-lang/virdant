// A minimal CodeMirror StreamLanguage for Virdant (.vir) sources.
// Highlights comments (including the `//!` and `//>` docstring
// forms), strings, numeric literals (decimal, `0x…`, `0b…`, and
// `<value>w<width>` word literals), and the keywords from the lexer
// (virdant/src/syntax/token.rs `KEYWORDS`).

import { StreamLanguage } from '@codemirror/language';
import { tags } from '@lezer/highlight';

const KEYWORDS = [
  'pub', 'export', 'import', 'mod', 'ext', 'builtin',
  'type', 'union', 'struct', 'enum', 'fn',
  'width', 'incoming', 'outgoing', 'bus', 'net', 'tap',
  'reg', 'wire', 'submod', 'on', 'of',
  'socket', 'client', 'server', 'cosi', 'soci',
  'if', 'it', 'else', 'when',
  'match', 'case', 'unused', 'initial',
  'true', 'false', 'dontcare', 'dependson',
  'platform', 'for', 'default',
];

const KEYWORD_RE = new RegExp(`(?:${KEYWORDS.join('|')})\\b`);

export const virdantLanguage = StreamLanguage.define({
  name: 'virdant',
  // Map the custom `self` token (the special `it` keyword) onto the
  // builtin-variable tag so it gets its bold color in the theme.
  tokenTable: { self: tags.self },
  token(stream) {
    // Comments: line comments and both docstring forms.  Note: regex
    // patterns here — `stream.match` with a string is a literal match,
    // so `'.*'` would never match anything.
    if (stream.match(/^\/\/\>.*/) || stream.match(/^\/\/\!.*/) || stream.match(/^\/\/.*/)) {
      return 'comment';
    }
    // Strings.
    if (stream.match('"')) {
      while (!stream.eol()) {
        if (stream.match(/^\\./)) continue;
        if (stream.match('"')) return 'string';
        stream.next();
      }
      return 'string';
    }
    // Numeric literals: hex, binary, word literals (`<val>w<width>`),
    // plain decimals.
    if (stream.match(/0[xX][0-9a-fA-F_]+/)) return 'number';
    if (stream.match(/0[bB][01_]+/)) return 'number';
    if (stream.match(/[0-9][0-9_]*w[0-9]+/)) return 'number';
    if (stream.match(/[0-9][0-9_]*/)) return 'number';
    // Identifiers and keywords.
    if (stream.match(/^[A-Za-z_][A-Za-z0-9_]*/)) {
      const word = stream.current();
      if (word === 'it') return 'self';
      if (word === 'true' || word === 'false') return 'bool';
      return KEYWORD_RE.test(word) ? 'keyword' : 'variableName';
    }
    stream.next();
    return null;
  },
});
