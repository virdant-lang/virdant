# PLAN: `grammar-spec` — a parser/checker for the `GRAMMAR.ideal.txt` format

Written 2025-11-16. Co-designed with the operator; the data model below is
settled. Green-light pending on the three decision points in §6.

## 1. Goal

A small, dependency-free tool that parses the grammar-spec format (the file
format of `grammar/GRAMMAR.ideal.txt`) into the settled data model, validates
it semantically, and reports diagnostics. It becomes the co-design instrument
for the Virdant grammar: spec bugs surface mechanically instead of by hand.

**Non-goals (later phases, not this plan):** generating `.lalrpop` from the
spec, ambiguity/LALR-constructability analysis, parsing `.vir` files.

## 2. The load-bearing format property (verified 2025-11-16)

Productions span multiple lines and continuation lines can *start* with
quoted tokens (`ModDef`'s `"}"` line, `SubmoduleStmt`'s
`"mod" "ident" …` line). So a standalone quoted line is ambiguous between
"continuation term" and "token declaration" — unless blank lines terminate
productions. Verified invariant in the current file:

- **89 rule heads**, each preceded by a blank line (or start-of-content after
  the leading comment banner) — 0 violations
- **0 blank lines inside productions** (89 rules ↔ 89 blank-line gaps)
- **89 token-declaration lines** after the last rule

So: `//` comments are skipped entirely (operator instruction — they are
nowhere in the data model); **blank lines are structurally significant**
(production terminators). This is the one subtle rule the parser encodes.

## 3. Data model (settled)

```rust
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize, // 1-indexed
    pub col: usize,  // 1-indexed
}

pub struct GrammarFile {
    pub rules: Vec<Rule>,
    pub tokens: Vec<Token>,
}

pub struct Token {
    pub name: String, // quoted names as written: `"on"` includes the "'s
    pub span: Span,
}

pub struct Rule {
    pub lhs: String,
    pub alts: Vec<RuleAlt>,
    pub params: Vec<String>, // ["T"] for macro definitions; else empty
    pub span: Span,
}

pub struct RuleAlt {
    pub terms: Vec<Term>,
    pub span: Span,
}

pub struct Term {
    pub atom: Atom,
    pub postfix: Option<Postfix>, // capped at one
    pub span: Span,
}

pub enum Postfix {
    Star,     // *
    Plus,     // +
    Optional, // ?
}

pub enum Atom {
    /// Quoted terminal, name as written: `"on"` (quotes included,
    /// matching `Token.name`)
    Token(String),
    /// Bare word: rule reference, macro use (`CommaList<Param>`), or an
    /// unquoted slip (`dyn`) — resolved/flagged by the semantic layer
    Nonterminal(String, Vec<Atom>),
    /// `( … )` group
    Group(Vec<Term>),
}
```

Settled decisions folded in: no comment/section items; no `Hole` variant (a
stray `...` parses as `Nonterminal("...")` and gets flagged as unresolved);
quotes kept in `Atom::Token` and `Token.name`; at most one postfix per term.

## 4. Architecture

Single new crate, zero dependencies (hand-written lexer + recursive-descent
parser; no logos, no lalrpop — using a grammar tool to parse a grammar spec
is overkill here).

1. **Lexer** — span-carrying tokens over the line structure:
   - `Word` (bare identifiers, includes things like `dyn` — resolution is
     semantic), `Quoted` (strict `"` delimiters, name kept as written incl.
     quotes), `Assign` (`:=`), `Bar`, `Star`, `Plus`, `Optional`,
     `LParen`, `RParen`, `Lt`, `Gt`, `Comma`
   - Skips `//…` comment lines and horizontal whitespace; emits `Blank`
     markers on blank lines (the production terminators)
2. **Parser** — recursive descent, block-oriented:
   - After BOF/`Blank`/comment-run: `Word [<Word>] :=` starts a `Rule`; its
     terms accumulate across lines until the next `Blank`/EOF; `|` starts a
     new `RuleAlt`
   - A `Quoted`-only line *outside* a rule block is a `Token` declaration
   - Postfix: at most one per term; `X*?` is a parse error
   - Produces exactly the §3 types, spans on everything
3. **Semantic layer** (separate pass, each check independent, all spanned):
   - **S1 duplicate `lhs`** — would have caught the old double-`DriverStmt`
     bug
   - **S2 unresolved references** — `Nonterminal` names matching no rule and
     no macro parameter: flags the unquoted `dyn` slip; would have flagged
     `ExecStmt`, `InitialStmt`, `BidirectionalDriverStmt` in the old file
   - **S3 macro arity/typing** — use-site arg count vs. definition `params`;
     type-args on non-macros; macros used bare
   - **S4 token cross-check** — every `Atom::Token` declared in `tokens`;
     flag undeclared and unused token declarations
4. **CLI** — bin `specheck` (mirrors `grammarcheck`):
   - `specheck check <file>` — default; exit 0 clean, 1 diagnostics,
     2 usage
   - `specheck dump <file>` — print rules (name, params, alt count) + token
     list; the view used in co-design discussions

## 5. Milestones

| # | Deliverable | Verification |
|---|---|---|
| M1 | Crate scaffold + lexer | dump token stream of the real file; spot-check spans |
| M2 | Parser → `GrammarFile` | parses `GRAMMAR.ideal.txt`: exactly 89 rules + 89 tokens; dump output eyeballed against the file |
| M3 | Semantic checks S1–S4 | run on current file: expect a precise, small diagnostic list (e.g. `dyn` unresolved; any token anomalies) — reviewed with the operator |
| M4 | CLI + workspace wiring | `specheck check grammar/GRAMMAR.ideal.txt` → exit code per findings |
| M5 | Findings writeup | the diagnostic list becomes the next co-design agenda |

## 6. Decision points for the operator

1. **Crate location/name**: new standalone workspace crate `grammar-spec/`
   (recommended — keeps `grammar/` = the `.vir` reference parser) **vs.** a
   bin inside `grammar/`. Note: a new crate means adding it to root
   `Cargo.toml` members (that file already carries intended modifications
   from the earlier session).
2. **Binary name**: `specheck`, or something else?
3. **`dump` mode now or later?** (Recommended: build it at M2 anyway — it is
   nearly free and is the co-design view.)

## 7. Risks / notes

- The blank-line invariant is empirical, not guaranteed. If a future spec
  edit puts a blank line inside a production, the parser will mis-split it —
  the checker should detect the symptom (an "unexpected `:=`" or
  orphan-quoted-line diagnostic) rather than silently mis-parsing.
- Per project rules: no `#[test]`-style tests; verification is via CLI runs
  against the real file (golden files only if the operator later asks).
