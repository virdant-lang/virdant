# GRAMMAR_REPORT.md

*The ideal grammar of Virdant: findings, opinions, and recommendations
from writing `vir-parse`.*

This report accompanies two artifacts:

* `vir-parse/` — a standalone binary that parses a `.vir` file with the
  **ideal grammar** (`vir-parse/src/syntax/ideal.lalrpop`) and builds a
  **redesigned AST** (`vir-parse/src/syntax/payload.rs`), rendered to
  `IDEAL_GRAMMAR.txt` by `make ideal-grammar`.
* `IDEAL_GRAMMAR.txt` — the ideal grammar, rendered by the same
  lalrpop fork (v0.23.1, `virdant-lang/lalrpop`) and with the same
  stripping options as `GRAMMAR.txt`, so the two files can be read
  side by side.

The ideal grammar is a thought experiment with two halves:

1. **Grammar.** What does the Virdant grammar look like if every
   production has to describe something the user can write, and
   nothing else? The production grammar
   (`virdant/src/syntax/virdant.lalrpop`) deliberately does not
   satisfy that standard — several productions exist only so that
   wrong programs parse far enough to be rejected with a good error
   message.
2. **Nodes.** What does the AST look like if every node has to earn
   its place? The production node structure carries a lot of
   parse-history: three different node kinds for what is grammatically
   the same brace block, wrapper nodes that only ever wrap one child,
   an always-present-but-often-empty annotations wrapper, `if`/`match`
   arms that exist only as *counts* of children, and name-shaped
   things (`Path`, `Ofness`, `Kind`) modeled as childless nodes.

Node-for-node parity with the production AST was an early scaffolding
device and was then dropped on purpose: **the production node
structure is part of the problem this experiment examines.** The
redesign is described in §3; the grammar simplifications are in §4.

---

## 1. How to verify all of this

```sh
./check.py                  # builds vir-parse, then checks every .vir in tests/check/
./check.py --tree           # also dumps the AST of each file
vir-parse [--tree] FILE.vir
make ideal-grammar          # re-render IDEAL_GRAMMAR.txt
```

`check.py` is deliberately **not** part of the test framework (no
`test.toml`, not run by `make test`); it exists for exactly this kind
of grammar experimentation. It requires every file in `tests/check/`
to parse with the ideal grammar.

Result: **37/37 files in `tests/check/` parse.** As a broader check,
`vir-parse` was run over *every* `.vir` file in the repository (453
files: `lib/`, `tests/`, `assets/`, `docs/`, `socket_loop/`): **448/453
parse**. The 5 exceptions are all files in `tests/errors/` whose
`test.toml` says `expect = "error"`; see §5 for what happens to them
and why those differences are the *point* of the exercise.

`make test` passes unchanged (403 passed, 4 pre-existing skips).

### The size of the trees

The redesigned nodes are not just prettier — they are materially
smaller. Node counts on representative files (production-parser counts
in parentheses, measured while the ideal parser still mirrored
production):

| file                          | ideal nodes | production nodes |
|-------------------------------|------------:|-----------------:|
| `tests/check/alu`             |         415 |              620 |
| `tests/check/memory/basic`   |         161 |              252 |
| `tests/check/generics_types` |         182 |              271 |
| `tests/check/reg_initial`    |          72 |              113 |
| `tests/check/ifs`            |          66 |              100 |

A consistent ~33% fewer nodes, from deleting wrappers, name-subtrees,
and duplicate block nodes — with no information lost (§3.4 explains
the one apparent exception, the annotations wrapper).

---

## 2. What the production grammar actually contains

Reading `virdant.lalrpop` critically, its ~100 productions fall into
four categories:

1. **Language** — productions that describe Virdant programs.
2. **Error recovery** — three `<error:!>` productions (in
   `PackageStmt`, `ModDefStmtCore`, `MemoryBlockStmt`) that let the
   parser swallow a bad region, record it as an `Error` AST node, and
   keep going so the user sees many errors per compile.
3. **Diagnostic traps** — productions that accept slightly-wrong
   programs purely so a later pass can reject them with a better
   message than "unexpected token". The entire `ArmBlock*` apparatus
   (8 nonterminals) plus `BinOpCompareNoLe` is here: it exists so that
   `when a { true }` (an expression arm without `=>`) parses, and the
   terminator-style pass can report `ArmBlockNeedsArrow` — "For
   expression-based conditionals, use `=>` instead of `{ ... }`" —
   instead of a parse error. The grammar's own comment says so:
   *"This form exists only so that `when c { expr }` parses and can
   be reported as `ArmBlockNeedsArrow`."*
4. **Accidental duplication** — one production per keyword spelling
   and per list shape, where one production plus a small keyword table
   would do.

The ideal grammar keeps category 1, drops categories 2 and 3
entirely, and collapses category 4.

### The numbers

|                                    | production | ideal |
|------------------------------------|-----------:|------:|
| grammar source lines               |      1,530 | 1,098 |
| nonterminal definitions (source)   |         98 |    90 |
| alternatives with actions (source) |        337 |   268 |
| nonterminals (rendered grammar)    |         48 |    56 |
| alternatives (rendered grammar)    |        133 |    88 |

(The rendered nonterminal count goes *up* because the ideal grammar's
two list macros and a few one-line helpers are counted as
nonterminals; the meaningful currency is alternatives, which drop 34%
in the rendered grammar — and everything that remains is language.
The ideal grammar's source is also heavily documented: roughly a third
of its lines are comments explaining each construct.)

---

## 3. The node structure, redesigned

The production payload enum has **73 node kinds**, of which 4 are dead
(`FnDef`, `ExprWord`, `ExprZext`, `ExprSext` — no production ever
constructs them) and 1 (`Error`) exists only for parse recovery. The
ideal grammar has **66 kinds, every one of them live** — but the
count undersells the change, which is structural:

### 3.1 One block, not three

The production grammar produces three node kinds for brace-delimited
statement lists:

* `ModDefStmtBlock` — it-blocks, `on`-region bodies;
* `ArmBlock` — `if`/`match`/`let` arm bodies;
* …wrapped in an `It` node when they are a component's body.

So `incoming x : T { unused it }` produced `It → ModDefStmtBlock → …`,
and an arm body produced a structurally identical `ArmBlock`. What a
block *means* was being encoded three times in the node vocabulary
when it is fully determined by the block's parent. The ideal grammar
has **one `Block` node**; the component's body is simply its `Block`
child, an arm's body is the arm's `Block` child. This deletes the
`It` wrapper — a node whose only content was another node — and the
`ArmBlock`/`ModDefStmtBlock` duplication.

### 3.2 Arms are nodes, not counts

In the production grammar, `IfArm`/`MatchArm` return only a *count*
(`when`-arm = 2 children, `else`-arm = 1), and the `If` node records
the sum. The actual arm grouping is reconstructed by the
parent-fixup's "children are the most recently created nodes" trick.
The ideal grammar makes arms real nodes:

```
If       children: IfArm*
IfArm    children: guard Expr?, body        (no guard means else arm)
Match    children: scrutinee, MatchArm*
MatchArm children: Pat?, body               (no pattern means else arm)
```

Self-describing, trivially walkable, and the grammar needs no
count-summing actions. This also removes the one place where the
production scheme cannot express a tree without an out-of-band
convention.

### 3.3 Names are data, not subtrees

The production grammar models names as childless nodes, wrapped in
yet more nodes:

* `ExprReference → Path` — a reference is a node wrapping a node
  wrapping interned text;
* `Type → Ofness` — a type's name is a childless child;
* `DependsonList → Path*` — a list of names is a list of childless
  nodes;
* `KindParam → Kind` — a kind is just a name.

The ideal grammar's rule: **a path or a (possibly qualified) name is a
leaf; it lives in the payload as interned text.** Concretely:

* `ExprReference { path }`, `ExprTap { path }`, `Driver { lhs }`,
  `IndexedDriver { lhs }`, `Unused { path }`, `Initial { lhs }`,
  `BidirectionalDriver { lhs, rhs }` — paths in payloads;
* `Type { package?, name }`, `ExprFn { callee }`,
  `Submodule { target }`, `SocketInstance { target }`,
  `ModDef { for_clause }` — qualified names (`Ofness`) in payloads;
* `DependsonList { paths }`, `KindParam { kind }` — likewise.

This deletes the `Path`, `Ofness`, and `Kind` node kinds entirely and
shrinks every tree that mentions a signal — which is all of them.

### 3.4 No empty wrappers

* The production `Annotations` node is **always** present, even with
  zero annotations — every statement in every file carries an
  often-empty wrapper as its first child. In the ideal grammar,
  `Annotation` nodes are the statement's direct children; a statement
  without annotations simply has none. (The annotations were already
  child-like: they are variable in number, and their argument
  expressions are nodes.)
* `DefaultClause` and `OnClause` are kept as nodes — unlike the cases
  above, a clause *marks a decision* (which clauses are present), and
  component children like `[Type, DependsonList?, DefaultClause?,
  OnClause?, Block?]` must stay self-describing when clauses are
  absent. A bare `Expr` child in an optional slot could not be told
  apart from another clause's bare `Expr`. The rule the redesign
  follows: **a node exists when it marks a decision; a wrapper that
  only relabels its single child, or exists even when empty, does
  not.**

### 3.5 Uniform children per node kind

* **`Memory` gets its own node.** The production grammar models a
  `memory` declaration as `Component { kind: Memory }` — but a
  memory's children (depth `Expr`, element `Type`, clock, body) share
  nothing with a port's children (`Type`, `dependson`, `default`,
  `on`, `async`, body). One node kind with kind-dependent child
  shapes is exactly the "bad place" the node structure is in. The
  ideal grammar has `Component` (eight port kinds, uniform children)
  and `Memory` (uniform children of its own).
* **`IndexedDriver` always has two children** — the index `Expr` and
  the value `Expr`. The production grammar alternates between two
  children (constant index carried in the payload) and three
  (dynamic index as a child). In the ideal grammar a constant index
  is simply an `ExprWordLit` child, and `is_dyn` in the payload keeps
  the distinction explicit.
* `Submodule`'s kind string in production is `"Module"` — the same
  word as a mod *definition*, for a node that is an *instance*. The
  ideal grammar calls it `Submodule`, and calls the socket instance
  `SocketInstance` (production's `"Socket"` collides with the socket
  *definition* reading).
* One-symbol wrapper nodes are gone: `ForClause` (the `for` clause is
  `ModDef` payload now), `EnumValue`-style tails, and the `Kind` node
  (§3.3).

### 3.6 Dead kinds found

Reading the payload enum as if it had to justify itself: `FnDef`,
`ExprWord`, `ExprZext`, `ExprSext` are constructed by **no production
at all**. They are either planned features whose grammar never landed,
or leftovers; either way they are not part of the language today.

---

## 4. The grammar simplifications, one by one

### 4.1 Error recovery removed

The three `<error:!>` productions and the `Error` payload are gone. A
malformed file now stops at the first syntax error with a precise
location, the offending token, and the set of expected terminals:

```
$ vir-parse tests/errors/syntax/parser_errors/src/parser_errors.vir
...:7:20: unexpected token `@`. Expected one of: "Ident"
```

This is the main *behavioral* difference between the two parsers, and
it is intentional: recovery is a compiler feature, not a language
feature. (See §5 for the consequences.)

### 4.2 The `ArmBlock` apparatus removed, and arm bodies unified

Production grammar, for arm bodies alone:

```
IfArm    := "when" Expr "=>" Expr | "when" Expr ArmBlock | "when" Expr If | "when" Expr Match | "when" Expr Let
         |  "else" "=>" Expr |  "else" ArmBlock |  "else" If |  "else" Match |  "else" Let      (10 alternatives)
MatchArm := ... the same ten, with "with" Pat ...                                               (10 alternatives)
Let      := "let" bindings ArmBlock | "let" bindings "=>" Expr | "let" bindings If | "let" bindings Match  (4)
ArmBlock := "{" ModDefStmt* "}" | "{" ArmBlockExpr "}"                                          (2)
```

…plus the eight diagnostic-trap nonterminals (`ArmBlockExpr`,
`ArmBlockCompare`, `ArmBlockAdditive`, `ArmBlockUnOp`,
`ArmBlockAscription`, `ArmBlockPrimary`, `ArmBlockAtom`,
`BinOpCompareNoLe`) that mirror the whole expression grammar in a
restricted form so that `{ expr }` blocks parse-but-warn.

The ideal grammar has **one** nonterminal:

```
ArmBody := "=>" Expr | Block | If | Match | Let
IfArm    := "when" Expr ArmBody | "else" ArmBody
MatchArm := "with" Pat ArmBody | "else" ArmBody
Let      := "let" CommaList<LetBinding> ArmBody
```

24 alternatives plus 8 helper nonterminals become 5 + 2 + 2 + 1. The
language is unchanged except in one spot: `when a { true }` is now a
parse error instead of a style diagnostic (see §5). One tiny
over-acceptance falls out of the unification: a `let` body may now be
another `let` (`let x = a let y = b { ... }`), which the production
grammar's four-way `Let` did not list.

The `BinOpCompareNoLe` nonterminal — `<=` removed from the comparison
operators, solely so `when a <= b { ... }` would parse as a latched
driver rather than a comparison — dies with the rest of the apparatus.
The comparison tier is now just `BinOpCompare`, all six operators,
which is what the language actually means.

### 4.3 One component declaration instead of eight

`ModDefStmtComponent` had eight productions, one per keyword spelling
(`incoming`, `outgoing`, `bus`, `net`, `wire`, `reg`,
`outgoing wire`, `outgoing reg`), each repeating the full clause list
with small variations. The ideal grammar:

```
ComponentStmt  := DocAnn ComponentKind Ident ":" Type DependsonList? DefaultClause? OnClause? "async"? Block?
ComponentKind  := "incoming" | "outgoing" | "bus" | "net" | "wire" | "reg"
               |  "outgoing" "wire" | "outgoing" "reg"
```

The one real change: **which clauses each component kind admits is now
a semantic question, not a syntactic one.** The production grammar
encodes restrictions like "only `outgoing*` carries `default`",
"`outgoing reg` has no `async`", "`bus`/`net` have no `on` clause"
syntactically; the ideal grammar accepts, say, `bus x : T default e`
and lets the analysis passes reject it. This is the one place where
the ideal grammar deliberately over-accepts relative to production —
we think it is the right call (the clause matrix is semantics, and
encoding it in the grammar costs eight productions and makes the
clause *order* hard to see), but it is a language-design decision and
it is called out here rather than hidden.

The same treatment unifies `cosi`/`soci` (`ChannelDir`) and
`client`/`server` (`SocketRole`) into tiny keyword tables, and merges
`@name` / `@name(args)` annotations into one production.

### 4.4 Driver operators as tables

```
DriverStmt          := Path DriverOp Expr            (was 4 productions)
BidirectionalDriverStmt := Path BiDriverOp Path      (was 2 productions)
IndexedDriverStmt   := Path "[" IndexSpec "]" IndexedDriverOp Expr   (was 4 productions)
DriverOp        := ":=" | "<=" | "#=" | "#-"
IndexedDriverOp := "<=" | ":="
IndexSpec       := "dyn" Expr | Nat
```

Two discoveries here:

* **`#=` and `:=` carry byte-identical payloads** (`Continuous`,
  `Strong`) in the production grammar. Either that is a bug, or `#=`
  (token name: `OpStrongDriver`) is meant to differ in strength or
  attach semantics somewhere downstream and the parse-level payload
  loses the distinction. Worth a look.
* The indexed driver keeps its own two-operator table (only `<=` and
  `:=`), because letting `#=`/`#-` index a memory word would be
  meaningless. This is one of the few places the ideal grammar stays
  deliberately finer-grained.

### 4.5 One comma-list shape (well, two)

The production grammar has eight hand-rolled list productions with
*four different trailing-comma rules*:

| list               | trailing comma? | may be empty?  |
|--------------------|-----------------|----------------|
| `ArgList`          | yes             | via `ArgList?` |
| `PatList`          | yes             | yes            |
| `AssignList`       | yes             | yes            |
| `Params`           | no              | via `Params?`  |
| `GenericsArgList`  | no              | no             |
| `KindParamList`    | no              | no             |
| `DependsonPathList`| no              | no             |
| `LetBindingList`   | no              | no             |

The ideal grammar has two macro shapes:

```
CommaList<T>      := (T ",")* T          -- one or more, no trailing comma
CommaListTrail<T> := (T ",")* T ","?     -- the same, plus optional trailing comma
```

with `CommaListTrail` used only inside `(`/`[`/`{`. **The production
grammar's inconsistency turns out to be load-bearing**: a trailing
comma is only unambiguous when a closing delimiter follows. At
statement level, `dependson a, b,` is genuinely ambiguous — is the
final comma trailing, or does `c` on the next line begin a driver
statement (`c := x`)? A statement-level list *must* forbid the
trailing comma to stay LR(1), which is exactly what the production
grammar does for `DependsonPathList`, `Params`, `GenericsArgList`,
and `LetBindingList` — it just does it incidentally, differently in
each production, with no comment explaining why. The ideal grammar
makes the rule explicit: *trailing commas live inside delimiters,
nowhere else.* The accepted language is identical to production's.

### 4.6 Small merges

* `Enumerant`: 3 productions → 1 (`name ParamList? ("=" Expr)?`),
  same four shapes.
* `LetBinding`: 2 → 1 (`name (":" Type)? "=" Expr`).
* `Pat`: 7 → 6 (`@Ident` and `@Ident(PatList)` merged).
* `Path`: 2 (`Ident…` / `it…`) → 1 with a two-token `PathStart`.
* `ExprUnOpPrefix` merged into `ExprUnOp`;
  `ExprAtom`/`ExprAtomPath`/`ExprAtomOther` merged into one `Atom`;
  `ExprPrimaryWith<Atom>` — parameterized only so the deleted
  `ArmBlockPrimary` could reuse it — inlined into `ExprPrimary`.

### 4.7 Dead code found and removed

Reading the grammar as if it had to justify itself surfaced several
pieces of pure dead weight in the *production* grammar:

* **Nonterminals `Width` and `Str` are never referenced** by any
  production. (`Str` is especially confusing: the *token* `"Str"` is
  used directly in the string-literal atom, and there is also a
  `Str` nonterminal that nothing calls.)
* **The extern token table lists tokens no production uses**: `;`
  (`Semi`), `export`, `fn`, `width`, `submod`, `/*`, `*/`, and
  `ERROR` (needed only by the deleted error productions). The `enum
  Token` block also declares `"memory" => Token::KwMemory` **twice**.
* The docstring at the top of `virdant/src/syntax/parsing.rs` shows
  example input with semicolons (`incoming inp : Bit;`) — Virdant has
  no statement terminators; the example is stale.

The ideal grammar's extern table lists only the tokens its
productions reference. (A stray `;` in a source file still lexes; the
ideal parser reports it as an unexpected token, which is honest.)

---

## 5. What changes for users of the ideal grammar

Three behavior deltas, all deliberate:

1. **No error recovery.** Invalid files stop at the first syntax
   error instead of producing a tree full of `Error` nodes. E.g.
   `tests/errors/syntax/docstring_no_space` — production swallows the
   malformed `//!` line via recovery and lets the docstring lint
   report it; the ideal parser rejects it at that token. For a
   *language* definition this is a feature: the ideal grammar's
   accepted language is exactly what its productions say, with no
   side channel.
2. **`when a { expr }` / `let x = a { expr }` are parse errors.** The
   production parser accepts these (in a restricted form costing 8
   nonterminals) so the style pass can say "For expression-based
   conditionals, use `=>` instead of `{ ... }`". The ideal parser
   rejects them at the expression token. Whether the production's
   friendlier message is worth 8 nonterminals and a shadow copy of
   the expression grammar is a judgment call; see §7.
3. **Component clause admissibility is semantic** (§4.3). E.g.
   `bus x : T default e` now parses and must be rejected downstream.

---

## 6. On the split of statement types

The one place the ideal grammar resists unification is the statement
sets, and it is worth being precise about why, because the split
*looks* like the same disease as the `ArmBlock` apparatus but is not.

The natural wish is a single `Stmt` used in every body. That is
impossible with the current surface syntax, for one specific reason:
a declaration's optional `on <CLOCK>` clause and a memory body's
`on <CLOCK> { ... }` region begin with the same token. If both could
follow a declaration, then

```
reg r : T on clk { ... }
```

would have two parses — a declaration with a clock clause and an
it-block, or a declaration followed by an `on`-region statement — and
LR(1) cannot choose between them (the decision point is the `on`
right after the type, and both parses consume `on clk { ... }`
identically). The production grammar resolves this by giving memory
bodies their own statement set that excludes declarations; that is
also why `on`-regions live only in memory bodies. The split is forced
by the language, not by grammar style.

What the ideal grammar *does* change is the shape of the split. The
production grammar writes the two statement sets out in full
(`ModDefStmtCore` with 8 alternatives, `MemoryBlockStmt` with 8
alternatives, six of them duplicated). The ideal grammar factors them
so the duplication is visible and zero:

```
ExecStmt := DriverStmt | BidirectionalDriverStmt | IndexedDriverStmt
          | UnusedStmt | InitialStmt | If | Match | Let     -- the one true statement set
DeclStmt := ComponentStmt | MemoryStmt | SubmoduleStmt | SocketStmt
ModStmt  := DeclStmt | ExecStmt        -- module bodies, it-blocks, arm blocks
MemStmt  := ExecStmt | OnRegion        -- memory bodies
```

Read as a sentence: *a statement is an executable statement; module
bodies additionally admit declarations; memory bodies additionally
admit clocked regions; and no body admits both extensions, because
the language would be ambiguous if it did.* Two two-alternative
dispatchers replace sixteen written-out alternatives.

If the split itself is to go away, the *language* has to change
first. The cheapest options, in ascending order of disruption:

1. Give the clocked region a distinctive second token, e.g.
   `on clk write { ... }` — then `on` after a type unambiguously
   continues the declaration, and one `Stmt` set suffices.
2. Move the declaration's clock clause before the colon
   (`reg on clk r : T { ... }`).
3. Drop per-declaration clock clauses and require an explicit clock
   context block everywhere.

All three change real programs (the `memory/multiclock` and
`asclock` tests exercise the current forms), so the ideal grammar
keeps the current syntax and confines the split to the four
productions above.

---

## 7. Opinions and recommendations

1. **The production grammar should keep its error recovery and its
   diagnostic traps — but mark them.** The ideal grammar demonstrates
   that the *language* is about a third smaller than the production
   grammar suggests. That difference is fine (diagnostics are a
   feature), but it should be visible: a section banner comment in
   `virdant.lalrpop` separating "language" productions from
   "diagnostic" productions. Anyone auditing the grammar today has to
   rediscover which productions are load-bearing for the language and
   which exist for error messages.
2. **Reconsider the arm-body arrow convention.** The `=>`-for-
   expressions-only rule costs more grammar (and more reader
   confusion) than anything else in the language. Two cleaner worlds
   are available: require `=>` before *every* arm body (uniform, but
   noisy for the common block case), or forbid `=>` entirely and make
   expression bodies parenthesized/braced. The current hybrid is the
   only shape that requires a shadow expression grammar; that is a
   real cost, and the ideal grammar's parse errors for
   `when a { true }` are honestly not bad.
3. **Move the per-kind component clause matrix to the semantic layer,
   or generate it.** Eight productions encoding "which clauses this
   kind admits" is a lot of grammar to maintain for checks that the
   analysis passes are better at explaining anyway. If the
   syntax-level errors are prized, the eight productions could be
   generated from a table instead of hand-maintained.
4. **Investigate `#=` vs `:=`.** They produce identical payloads
   today. If `#=` is meant to be a distinct attach/strong-driver
   form, the payload should say so; if not, one of them should go.
5. **Delete the dead code** from the production grammar and payload
   (`Width`, `Str`; `FnDef`, `ExprWord`, `ExprZext`, `ExprSext`;
   `;`, `export`, `fn`, `width`, `submod`, `/*`, `*/`, the duplicate
   `"memory"` extern entry). Dead terminals also pollute lalrpop's
   "expected one of" lists in some diagnostics.
6. **Adopt the node redesign in the production parser.** The changes
   in §3 are independent and each is a strict improvement over the
   production shapes: arms as nodes (deleting the only count-based
   child convention in the codebase), one `Block` node (deleting the
   `It` wrapper and the `ArmBlock`/`ModDefStmtBlock` split),
   names-as-payload (deleting `Path`/`Ofness`/`Kind` nodes), and no
   empty `Annotations` wrappers. The ideal grammar is a working
   prototype for all of them; `vir-parse --tree` shows the target
   shape on any file. The one behavior to preserve in migration:
   `DefaultClause`/`OnClause` stay as clause-marking nodes (§3.4).
7. **If a stricter frontend is ever wanted** (an LSP that wants
   fail-fast parsing, a formatter that wants no `Error` nodes in the
   tree, a tutorial), the ideal grammar is that frontend's parser,
   already written.

---

## 8. Summary

The ideal grammar is the production grammar with the mirror removed.
It drops 3 error-recovery productions, the entire 8-nonterminal
`ArmBlock`/`BinOpCompareNoLe` diagnostic apparatus, two dead
nonterminals, and eight dead extern tokens; it collapses the
eight component spellings, six driver productions, four indexed-driver
productions, ten-plus-ten arm alternatives, the four-way `let`, and
eight inconsistent list shapes into single productions plus small
keyword tables and two list macros.

The node structure is redesigned around two rules — *a node exists
when it marks a decision; names are data, not subtrees* — which
deletes the `It` wrapper, the `ArmBlock`/`ModDefStmtBlock` split, the
`Path`/`Ofness`/`Kind` name-nodes, the always-empty `Annotations`
wrappers, and the count-based arm children, replacing them with one
`Block` kind, real `IfArm`/`MatchArm` nodes, and name-bearing
payloads. Trees shrink by about a third on every file measured.

The one place the grammar cannot be unified — declaration statements
versus `on`-regions — is forced by the language's surface syntax, not
by grammar style; the ideal grammar factors the split down to its
minimum (two two-alternative dispatchers over one shared statement
set) and documents exactly which language change would remove it. On
every valid file in the repository the ideal grammar parses — 448 of
453, with the 5 exceptions being files that are supposed to fail.
