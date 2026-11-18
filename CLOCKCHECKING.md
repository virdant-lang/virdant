# Virdant Clock Domains and Clock Checking — Reference Specification

This document specifies how Virdant assigns clock domains to components and
expressions, and how the compiler checks them. It is authoritative for the
behavior of `vir check` with respect to clocking. Where this document and the
implementation disagree, this document is to be corrected or the implementation
is to be treated as buggy; consult the *Implementation notes* for the source
locations this specification was derived from.

---

## Conventions

**[normative-language]** The keywords **must**, **must not**, **may**,
**need not**, and **shall be valid** are to be interpreted as normative
requirements on the language and its checker.

**[clause-format]** A *clause* is a single, self-contained requirement tagged
with a stable slug in brackets (`[slug]`). Cross-references cite the slug:
`[slug](n)` cites item *n* of a clause's numbered list. Adding, removing, or
reordering clauses renumbers nothing; slugs name concepts, not positions.
Prose between clauses is explanatory and non-normative.

**[diagnostic-names]** Diagnostic names (e.g. `DomainMismatch`) name the
concrete diagnostics listed in *Diagnostic catalog*. Emitting a diagnostic is
a requirement on the checker; the *severity* (error vs. warning) is fixed by
the catalog.

**[notation]**

* `k` ranges over *clocks* — clock classes ([clock-classes]) and the abstract
  clock variable of a flexible module ([clock-variable]); `d, e` over
  *domains* (*The domain lattice*).
* `d₁ ⊑ d₂` means "`d₁` may be used where `d₂` is expected" (the lattice
  order, [upcast-order]). It is read "d₁ upcasts to d₂".
* `d₁ ⊔ d₂` is the least upper bound (join); `d₁ ⊓ d₂` the greatest lower
  bound (meet) ([join], [meet]).
* `D(expr)` is the domain inferred for an expression; `D(comp)` is the domain
  assigned to a component in a given module view (*Declaration-level domain
  assignment*, *Clocks across boundaries*).

---

## Clocks of a module

**[incoming-clock-ports]** A module's *incoming clock ports* are exactly:

1. its declared components of kind `incoming` whose type is the builtin
   type `Clock`, and
2. the *receiving-side* channels of its socket instances whose type is
   `Clock` (the `cosi` channel of a `server` socket instance, the `soci`
   channel of a `client` socket instance).

**[clock-carriers]** Components that merely *carry* a clock — a
`wire`/`outgoing` component typed `Clock`, an outward-driven socket clock
channel — are not incoming clock ports. A submodule instance's clock *output*
port and a platform-synthesized Clock-typed port are likewise not incoming
clock ports, but they are clock sources ([clock-sources]) and contribute to
the clock count ([clock-count]).

**[inherited-components]** The ports and sub-socket channels of a submodule
instance are *inherited components*: they are not incoming clock ports of
the enclosing module, and the parent never clocks them — their domains are
assigned by the submodule's clocking and mapped into the parent's view
([domain-mapping]); the parent binds them only by driving the instance's
clock ports ([clock-binding]).

### Clock classes

**[opaque-clocks]** `Clock` is an *opaque* type: a clock value cannot be
inspected, tested, or combined in combinational logic. The only things a
design may do with a clock are associate components with it (an `on` clause)
and pass the value around. Consequently, a driver of a Clock-typed component
does not transfer a value; it asserts an *identity* — "this name is that
clock." Clocks are therefore *defined*, not assigned. Clock opacity is
enforced by the type system: clocks flow only to contexts that expect a
clock (`Clock`-typed ports and channels, `on` clauses, and memory `on`
regions), and nothing may be computed from them.

**[clock-sources]** A *clock source* is a construct that introduces a fresh
clock identity. Clocks coming from the outside are each distinct sources —
they have the *potential* to differ, regardless of how the platform
ultimately wires them, so a module's clock structure never depends on its
instantiation context:

1. each incoming clock port ([incoming-clock-ports]) is itself a source;
2. a platform-synthesized Clock-typed port is itself a source — but only
   once the module declares it: an undeclared synthesized port cannot be
   referenced at all (`UseUndeclaredPlatformPort`), so it sources nothing
   in the module's view;
3. a Clock-typed outgoing port that declares no clock `dependson`
   ([clock-provenance]) is itself a source: the module's (necessarily
   undriven) outgoing port, or a submodule or `ext mod` instance's clock
   output port, identified by its instance path (e.g. `pll.clkout`). A port
   that *does* declare a clock `dependson` is an alias of the named clock,
   not a source;
4. a call to a clock-generating builtin (none today; the removal of
   `asclock` does not preclude future ones).

**[clock-classes]** Clock-typed things denote *clock classes*: the
equivalence classes of Clock-typed components and expressions under the
relation generated by the sources of [clock-sources] and by *drivers*.
A driver whose target is a Clock-typed component and whose driving
expression is a plain reference to a Clock-typed thing makes the two
denote the same class — unconditionally, and also when it is *guarded*
(its enclosing guard stack, [the-walk], is non-empty) provided its
enclosing conditional is single-class ([conditional-one-domain]); a
driver beneath a conditional that mixes clock classes contributes
nothing ([conditional-one-domain] diagnoses it).  Clock identity is
static: it is fixed by the module's structure, never by runtime
behavior, and never by instantiation-site usage.

**[conditional-one-domain]** *One clock domain per conditional.* The
whole of a statement-position `if` or `match` acts on a single clock
domain: its conditions (a `match`'s subject) and the clock drivers in
its arms — an `else` arm included, which is guarded by the negations of
the preceding conditions and so denotes their domains — must all denote
one clock class.  A conditional that denotes more than one class is
reported `DomainMismatch` and contributes no class relation; a
single-class conditional is equivalent to its clock drivers written
unconditionally (their targets join the driving clock's class), and
each guarded clock driver warns `GuardedClockDriver`, since a
conditional cannot change which clock a target denotes.  The warning is
suppressed when the conditional is mixed, which is an error in its own
right.

**[clock-provenance]** *Clock provenance declaration.* An outgoing
Clock-typed port (or a socketdef's Clock-typed channel) declares its
provenance with a `dependson` clause naming an incoming clock port declared
in the same item:

* with `dependson <clock>` declared, the port is an *alias*: it must carry
  exactly one definition rooted at the named clock's class — an
  unconditional driver, or a guarded driver under a single-class
  conditional ([conditional-one-domain]). The clause is singleton — a clock that
  depends on two clocks merges two classes and is reported `DomainMismatch`
  (see *Diagnostic catalog*). For a module with a visible body this is
  verified; for an `ext mod` it is trusted.
* with no `dependson` declared, the port is a *fresh source*
  ([clock-sources](3)) and **must not** be driven; driver analysis's
  no-driver rule does not apply to it. A visible definition rooted at an
  incoming clock without the declaration is reported
  `MissingClockDependson` (see *Diagnostic catalog*). A definition rooted at
  an internal source (e.g. `clkout := pll.clkout`) needs no declaration:
  outside the module it is treated as a fresh source, which is conservative.

**[one-source-per-class]** Every *concrete* clock class has exactly one
source ([clock-sources]). (The abstract clock variables of the system — a
flexible module's clock variable ([clock-variable]) and the unbound
clock of an instance clock port ([instance-clock-domain]) — are not concrete
classes: they are abstract clocks, never sourced.) A class with zero sources or two or more sources is not
well-formed; for a module's locally declared components this is precluded by
driver
analysis's exactly-one-driver rule, and for instance clock ports by the
definition rule of [driver-check]. A class's *canonical name* is its source.

**[clock-count]** A module's *clock count* is the number of clock classes
with a source in the module's view ([clock-sources]). It is not the number of
incoming clock ports: a module may acquire clocks internally, and such clocks
count.

**[unique-clock]** A module's *unique clock* exists iff its clock count is
exactly one; it is that one clock class, and it may arise from any kind of
source ([clock-sources]): an incoming clock port, a platform-synthesized
port, an outgoing clock port, or a generating builtin. The unique clock
needs no naming: components of the module may omit the `on` clause and are
clocked by it anyway ([redundant-on-clause], [unique-clock-inference]). With
clock count zero or two or more, the module has no unique clock. A
*unique-clock module* has clock count one; a *multi-clock module*, two or
more; a *zero-clock module*, zero.

---

## The domain lattice

### Domains

**[domains]** A *domain* is one of:

* `Sync(k)` — clocked by the clock `k` ([clock-classes]): a concrete clock
  class, identified by its canonical source ([one-source-per-class]) within
  the enclosing module's component analysis, or the abstract clock variable
  of a flexible module ([clock-variable]). Domains never cross module
  boundaries (*Clocks across boundaries*).
* `Async` — not bound to any clock.
* `Constant` — independent of every clock (literals, enumerants, holes, and
  expressions built only from such values).

### Upcast order

**[upcast-order]** The upcast relation `⊑` is the least relation satisfying:

```
Constant ⊑ d            for every domain d
Sync(k)  ⊑ Sync(k)
Sync(k)  ⊑ Async
Async    ⊑ Async
```

The abstract clock variables of the system — a flexible module's clock
variable ([clock-variable]) and the unbound clock of an instance clock
port ([instance-clock-domain]) — are ordinary clock classes in every respect
but one: they are never sourced ([one-source-per-class]) and never compare
equal to a concrete class. Conditional clock drivers need no domain rule:
the definition rule of [driver-check] catches them syntactically, in every
module view.

**[incomparable-domains]** Two distinct clock classes give incomparable
domains: `Sync(k₁)` does not upcast to `Sync(k₂)` for `k₁ ≠ k₂`. Consequently:

* A `Sync(k)` value **must not** be used where `Sync(k′)`, `k ≠ k′`, is
  expected (a clock-domain-crossing violation).
* A value on any non-constant domain **must not** be used where a `Sync(k)`
  domain is expected (downcasting is never allowed). In particular, an
  `Async` value **must not** drive a clocked component.
* A `Constant` value may be used where any domain is expected.
* A flexible module's clock variable behaves as an ordinary clock class in
  the module's view; it is substituted at the instantiation site
  ([pinning]), and no check inside the module depends on how
  ([clock-classes]).

### Partial order

**[partial-order]** `⊑` extended by `(d, d)` is a partial order:
`d ⊑ e ∧ e ⊑ d` iff `d = e`; `d ⊑ e ∧ e ⊑ f` implies `d ⊑ f`; `d ⊑ e` and
`e ⊑ d` hold simultaneously only for `d = e`.

### Join and meet

**[join]** The *join* `d ⊔ e` (least domain both may upcast to) is:

```
d ⊔ d           = d
Constant ⊔ e    = e                 (symmetric)
Sync(k)  ⊔ Var  = k                 (unification; symmetric)
Sync(k₁) ⊔ Sync(k₂) = Async,  k₁ ≠ k₂
Sync(k)  ⊔ Async = Async            (symmetric)
otherwise       = join failure      ([join-failure])
```

Joining an *unbound* abstract clock variable with a concrete class
*unifies* the variable with it: the usage commits the variable to that
clock.  Joining two distinct unbound variables fails — two independent
generative contexts cannot share a domain until both are pinned.

**[meet]** The *meet* `d ⊓ e` (greatest domain upcasting to both) is:

```
d ⊓ d           = d
Async    ⊓ e    = e                 (symmetric)
otherwise       = Constant
```

---

## Declaration-level domain assignment

The component analysis *registers* every component visible in a module's
view — local declarations, the channels of the module's socket instances,
inherited components under their instance paths, and
platform-synthesized ports — and assigns a domain to each of them. This
assignment is context-free; the clock checker adds the parent-view domains
of inherited components and checks expression and driver rules.
Components whose domain is left unset by this section are assigned by the
unique-clock and flexible inferences ([unique-clock-inference],
[clock-variable]) or remain unset (multi-clock/zero-clock/unsafe cases,
*The clock-checking pass*).

**[component-provenance]** Every component's domain is assigned by the clock
analysis of the item that declares it. A component is *local* when it is
declared in the module's item (its ports, wires, regs, and memories) or is a
channel of one of the module's socket instances: a socketdef has no clock
analysis, so there is nothing to inherit — its channels' domains are
assigned by this module ([socket-channel-domains]). A component is
*inherited* when it is declared in an instantiated definition — a submodule
or `ext mod` — and registered under an instance path (`<instance>.<port>`):
its domain is assigned by that definition's clocking and mapped into the
parent's view ([inherited-components], [domain-mapping]); the parent never
clocks it, it only binds the instance's clocks ([clock-binding]).
Platform-synthesized ports are registered on behalf of the `platform` item
([clock-sources](2)). The clauses of this section and of *The clock-checking
pass* that speak of "local components" mean local in this sense.

### Declaration-fixed domains

**[declaration-fixed-domains]** A component declared with the `async`
modifier has domain `Async`; this is fixed by the declaration alone. A
component typed `Clock` has domain `Sync(k)` where `k` is its clock class —
the class the component itself denotes ([clock-classes]). Clock classes are
computed by clock-class resolution ([phase-order]), which is purely
syntactic, so these domains are available before any other phase. A
Clock-typed component whose class is not well-formed ([one-source-per-class])
has no domain; the failure is diagnosed.

**[async-register]** A component declared `reg`, `outgoing reg`, or `memory`
**must not** be declared `async`; the checker emits `AsyncRegister` (see
*Diagnostic catalog*). No declaration fixes its domain to `Async`; clocked
components always live on a `Sync` domain.

### `on` clauses

**[on-clause]** A component with an explicit `on <CLOCK>` clause has domain
`Sync(k)` where `k` is the clock class of the component named by the clause;
the clause is
resolved against the module's components and **must** name a Clock-typed
component. In a zero-clock module the clause names nothing and the domain
stays unset (the clock checker diagnoses the module).

**[on-clause-scope]** A platform-synthesized port or a socketdef channel with
an `on <CLOCK>` clause resolves its clause *within the declaring item*: the
written
name must denote a Clock-typed sibling port/channel of the same `platform` or
`socket` item. The clause is not expanded into the instantiating module.
An unresolved clause leaves the component's domain unset.

**[redundant-on-clause]** In a unique-clock module, an explicit `on` clause
naming the module's unique clock is redundant, and the checker emits the
`RedundantOnClause` warning — except in a module annotated `@unsafe_clocks`,
where an explicit clause is load-bearing (it is the safety net required by
[fast-path]) and is never reported redundant.

### Socket channels

**[socket-channel-domains]** The channels of a socket instance take their
domains from the *socketdef's* clock structure:

1. A channel typed `Clock` has domain `Sync(k)` for its clock class
   ([declaration-fixed-domains]).
2. A channel declared `async` has domain `Async`.
3. A channel with an explicit `on` clause resolves it per [on-clause-scope].
4. Otherwise, with exactly one Clock channel in the socketdef, every other
   channel has domain `Sync(c)` where `c` is the socket instance's Clock
   channel (the socket's unique clock), subject to
   [socket-clock-binding] rebinding.
5. Otherwise, with zero Clock channels, every channel is clock-generic: it
   has domain `Sync(α)` under a clock variable of the socketdef,
   resolved per [pinning] at the instantiation site.
6. Otherwise (two or more Clock channels), a clause-less non-Clock channel
   has no domain; the socketdef itself is diagnosed
   ([socketdef-multi-clock]).

**[socketdef-multi-clock]** A socketdef with two or more Clock channels
requires every non-Clock, non-`async` channel to carry an explicit `on` or
`async` clause; the checker emits `MissingDomainClause` at the socketdef
(reason: "the socket has multiple Clock channels"). A socketdef channel whose
`on` clause names the socketdef's unique Clock channel is reported as
`RedundantOnClause` (clause-less channels are treated as `on` it).

### Memory clock slot

**[memory-clock-slot]** Every `memory` component has a *clock slot*: the
clock class named by its `on` clause if present, else the module's unique
clock ([unique-clock]) if clock count is one, else unresolved. The slot is
used by the memory-write rules of *Memory clocking*. A `memory` carrying an
image annotation (`@image`, `@image_hex`, `@image_inline`) never writes and
is exempt from requiring a clock slot.

### Unique-clock and flexible inferences

**[unique-clock-inference]** In a unique-clock module, every local component
([component-provenance]) whose domain is still unset after
[declaration-fixed-domains]–[socket-channel-domains] has domain
`Sync(<the module's unique clock>)`. This includes the channels of the
module's sockets.

**[clock-variable]** A zero-clock module (clock count zero, [clock-count])
that declares no `async` component is *clock-polymorphic*: the component
analysis gives it a *clock variable* `α` — an abstract clock,
fresh for the module's view, distinct from every clock class and never
sourced. Every local component ([component-provenance]) whose domain is
still unset after [declaration-fixed-domains]–[socket-channel-domains] has
domain `Sync(α)` — except `reg`, `outgoing reg`, and non-image `memory`
components, which [missing-on-clause] has already diagnosed and which take
no domain: a flexible module is clock-generic in its combinational structure
only. The rule covers the channels of the module's sockets. Inside the
module's view, `Sync(α)` is checked exactly like a unique clock: drivers,
guards, and joins among `Sync(α)` values succeed as they would against a
single concrete clock. The variable is substituted at each instantiation
site ([pinning]).

**[zero-clock-with-async]** A zero-clock module that *does* declare an
`async` component gets no clock variable ([clock-variable]): a local
component with no clause and no `async` modifier has no domain, and the
clock checker diagnoses it ([zero-clock-coverage]).

### Missing clauses

**[missing-on-clause]** In a module whose clock count is not one, a `reg` or
`outgoing reg` (and any non-image `memory`) with neither an `on` clause nor
an `async` modifier — the latter impossible, since [async-register] prohibits
it — is reported `MissingOnClause` by the component analysis. In a
zero-clock module this admits no escape: there is no clock to name, so a
zero-clock module cannot contain a register or a non-image memory.

---

## The clock-checking pass

### Applicability

**[clocking]** For every `mod` definition, the checker builds a *clocking*:
an assignment of a domain (or *unknown*) to every subexpression, the
parent-view domains of inherited components ([clock-binding]), and a
set of clock diagnostics. `ext mod` definitions are not clock-checked.

**[after-typechecking]** The pass runs after typechecking; expressions whose
types failed to resolve still participate in clock checking to the extent
their structure is known.

### Skip rules

**[unsafe-clocks-skip]** A module annotated `@unsafe_clocks` is not
expression-checked and not driver-checked: no domain is assigned to any
subexpression, no `DomainMismatch` or `DriverDomainMismatch` is reported for
it. The annotations are then the sole safety net: every declared top-level
component (except Clock-typed components, which are clocked by their clock
classes, [declaration-fixed-domains]) **must**
carry an explicit `on` or `async` clause; the checker emits
`MissingDomainClause` (reason: "the module is marked `@unsafe_clocks`") for
each that does not.

**[zero-clock-coverage]** In a zero-clock module that declares an `async`
component, every declared top-level component (except Clock-typed ones) must
carry an explicit `on` or `async` clause; the checker emits
`MissingDomainClause` (reason: "the module has no clocks") for each that does
not. (Registers without a clause were already reported as `MissingOnClause`
by [missing-on-clause]; the checker diagnoses the remaining kinds here.) If
the zero-clock module declares no `async` component, nothing is flagged: the
module is clock-polymorphic and is checked under its clock variable
([clock-variable]).

**[multi-clock-coverage]** In a multi-clock module, every declared top-level
component (except Clock-typed ones) must carry an explicit `on` or `async`
clause; the checker emits `MissingDomainClause` (reason: "the module has
multiple clocks") for each that does not. (Registers without a clause were
already reported as `MissingOnClause` by [missing-on-clause]; the checker
diagnoses the remaining kinds here.)

**[fast-path]** *Single-domain fast path.* A module that (a) has clock count
≤ 1, (b) declares no `async` component, and (c) instantiates no submodule, is
not expression-checked and not driver-checked; no clock diagnostic other than
those of *Declaration-level domain assignment* may arise from it. In a
unique-clock module every local component is then `Sync(<the module's unique
clock>)` ([unique-clock-inference]) and no local join or driver can fail; in
a zero-clock module every local component is `Sync(α)` under the module's
clock
variable ([clock-variable]).

**[boundary-drivers]** A module with any submodule instance never takes the
fast path ([fast-path](c)): boundary drivers must be checked against the
parent-view domains produced by [clock-binding]. Modules with socket
instances may take the fast path; their channels' parent-view domains are
already fixed by [socket-channel-domains] and [socket-clock-binding].

### Order of the checked phases

**[phase-order]** For every `mod` definition — skipped or not — the checker
first performs *clock-class resolution* ([clock-classes]): it computes the
module's clock classes, their sources, and the clock count ([clock-count]).
The phase is purely syntactic (driver targets, driving references, guard
nesting, and `dependson` clauses; it reads no data domains), and it precedes
component analysis, whose clock count, unique-clock inferences, and
Clock-typed domains consume it ([unique-clock-inference],
[clock-variable], [declaration-fixed-domains]). Then, for a module that
is not skipped, the checker performs, in order:

1. *Cross-boundary binding* ([clock-binding], [socket-clock-binding]): bind
   submodule and socket clocks to clock classes and pin flexible instances
   ([pinning]). This runs before every other checked phase; all
   later phases observe the bound parent-view domains.
2. *Declaration checks* ([async-register] → `AsyncRegister`).
3. *Clause coverage* ([unsafe-clocks-skip], [zero-clock-coverage],
   [multi-clock-coverage] as applicable).
4. *The walk* (*Expression-domain inference*, *Driver, guard, and connection
   checking*): color every subexpression and check every driver, guard,
   indexed write, and connection.

**[report-once]** The clock checker emits a diagnostic for a given error site
at most once; downstream sites that would repeat it are suppressed
([initial-drivers], [connection-check], [region-clock]).

---

## Clocks across boundaries

### Submodule clock binding

**[child-clock-variables]** Each incoming clock of a submodule instance — a
clock port, or an inward Clock channel of one of the instance's sockets — is
a free variable in the submodule's definition: the submodule's clock
analysis phrases its internal `Sync` domains in terms of that variable; in
the submodule's view, the port's domain is `Sync(α)` for that free
variable ([instance-clock-domain]). Driving the instance's clock port
instantiates the variable: the driving expression, a plain reference to a
Clock-typed component of the parent, *is* the concrete clock.

**[clock-binding]** For each submodule instance, the checker collects the
*bindings*: each *definition* ([clock-classes]) whose target is one of the
instance's incoming Clock-typed ports and whose driving expression is a plain
reference to a Clock-typed thing of the parent's view — a local component,
or
an inherited clock output such as `pll.clkout` — binds that port to the
*clock class* of that reference. A guarded driver contributes no binding (it
is an assignment, and [driver-check] has already diagnosed it). Conflicting
re-definitions are driver analysis's concern (multiple drivers), not the
clock checker's.

**[instance-clock-domain]** In the parent's view, an instance's incoming
Clock-typed port has domain `Sync(k)` where `k` is the class bound to it
([clock-binding]). If the port is unbound, its domain is `Sync(α)` for the
instance's abstract clock variable — the same construct as a flexible
module's clock variable ([clock-variable]) — incomparable with every
concrete class. An unbound port admits no drivers by construction
(definitions would bind it), so only guard comparisons can reach the
abstract domain, and they fail: a guard may never gate the clock of an
unbound instance.

**[domain-mapping]** Every inherited component of the instance whose
declared (defining-view) domain is set is mapped onto its parent-side
instance path `<instance>.<port>`:

1. a child domain of `Async` or `Constant` maps to itself;
2. a child domain of `Sync(α)`, where `α` is the child's clock
   variable ([clock-variable]), maps to the parent-side resolution of that
   variable for this instance ([pinning]): `Sync(k)` if pinned, `Async` if
   unbound;
3. a child domain of `Sync(c)` maps to the binding of the parent-side
   component named by the child clock's path — i.e. if the child's clock `c`
   is the child component named by path `p`, the mapped domain is the clock
   class bound to `<instance>.p` ([clock-binding]).

**[unbound-child-clocks]** A child clock the parent never *bound* — undriven,
or driven only by guarded assignments ([clock-classes]) — leaves the child
components clocked by it unmapped; unmapped components are not checked in the
parent. An inherited component whose domain is unset in the defining
module's view (e.g. an unclad component of a multi-clock submodule) is
likewise unmapped.

**[parent-view-domains]** In the parent's view, the domain of an inherited
component is its mapped domain ([domain-mapping]); components absent from
that map have no parent-view domain and are not checked.

### Socket clock binding

**[socket-clock-binding]** The unique Clock channel of a single-Clock-channel
socket instance is bound the way a submodule's clock port is bound
([clock-binding]): a driver targeting that channel
(`it.clk := <clock reference>`) binds every sibling channel whose declared
domain is `Sync(<the socket's Clock channel>)` to the referenced clock's
*class* ([clock-classes]). Bindings are contributed by definitions only, as
in [clock-binding].

**[socket-unique-clock]** A socket's Clock channel that is the module's
unique clock (an inward `cosi` channel of a `server` instance) is an
incoming clock port ([incoming-clock-ports]) — hence a clock source
([clock-sources]) — and needs no binding; the channels treated as `on` it
already have `Sync(<its class>)` as their parent-view domain.

**[undriven-outward-clock]** An outward clock channel that carries no
definition ([clock-classes]) leaves the socket's Clock-channel class without
a source ([one-source-per-class]); it is diagnosed (by driver analysis's
no-driver rule where applicable), and its sibling channels are not checked.
There is no benign undriven case.

### Instance pinning

**[pinning]** A submodule instance of a zero-clock, no-`async` module — or a
socket instance of a zero-Clock-channel socketdef — instantiates the clock
variable `α` of its definition ([clock-variable]). The checker resolves each
such instance's mode from the parent's usage:

1. Every driver, guard, indexed write, and connection in the parent whose
   contributing expressions (driven target, driving expression, indexed
   write's index, enclosing guards, connection sides) contain a `Sync(k)`
   domain *and* whose source region contains a reference to a component of
   the instance records `k` for that instance.
2. An instance touched by exactly one clock class `k` is *pinned*: `α` is
   substituted with `k`, and every flexible component of the instance takes
   domain `Sync(k)` in the parent's view.
3. An instance touched by no clock leaves `α` unbound — the *all-async
   mode*: the instance's components keep the enclosing view's ambient
   clock variable, which accepts any driving domain and unifies with
   whatever clock reads them ([upcast-order], [join]).
4. An instance touched by two or more distinct clock classes is reported
   `FlexibleInstanceClockConflict` and leaves `α` unbound (so checking does
   not cascade). One instantiation of a flexible module resolves to a single
   domain; an instance cannot serve two clocks.

**[pinning-before-walk]** Pinning runs before the walk ([phase-order]) and
shares the expression-inference memo table; the clock sets it records are
those of the parent's drivers and connections exactly as the walk will check
them (including the guard-propagation rule of [guard-propagation]).

---

## Expression-domain inference

### Judgment

**[inference-judgment]** The checker infers a judgment `D(expr) = d`,
assigning a domain to every expression root it reaches through the walk.
Inference is *bottom-up and context-free*: a subexpression's domain does not
depend on where it occurs. Inference is memoized per AST node; each node is
visited at most once per phase that consults it, and a node's recorded domain
is never changed once recorded.

**[self-reference]** A degenerate self-reference (e.g. `let x = x`) yields
*unknown*; the checker must not loop.

**[unknown-participation]** An expression whose domain is *unknown*
participates in no check: drivers on it, and guards expressed by it, are not
diagnosed. The checker reports *unknown* rather than a spurious error
whenever an operand could not be resolved.

### Inference rules

**[literal-domains]** Literal forms — bit literals, word literals, string
literals, enumerant expressions, holes (`?`), and dontcares (`—`) — have
domain `Constant`.

**[parenthesized-domain]** A parenthesized expression has the domain of its
subject.

**[reference-domain]** A reference has, in order of applicability:

1. the declared parent-view domain of the component it names
   (*Declaration-level domain assignment*, *Clocks across boundaries*);
2. otherwise, if it names a `let`-bound variable, the domain of the
   binding's right-hand side (beta-reduction);
3. otherwise, if it names a variable bound by a `match` pattern, the domain
   of the innermost enclosing `match`'s subject — where "innermost" excludes
   the match whose subject the reference is reached through;
4. otherwise, the domain of the component of that name resolved by simple
   name (covers references the component analysis did not register, e.g. the
   RHS of an `initial` statement).

Lexical shadowing between pattern bindings and `let` bindings is resolved by
the same rule the typechecker applies.

**[projection-domains]** Projection forms — field access `x->f`, indexing
`x[i]`, slice `x[hi..lo]`, unary operators, and `as`-casts — have the domain
of their subject.

**[bus-tap-domain]** `bus.tap` has the domain of the tapped component.

**[joining-operators]** A binary operator expression, a concatenation
(`word(...)`), and zero/sign extension (`zext(...)`, `sext(...)`) have domain
`d₁ ⊔ ⋯ ⊔ dₙ`, the join of their operands' domains. Each operand pair with
distinct clock domains reports `DomainMismatch` (see *Diagnostic catalog*)
exactly once, at the joining expression ([join-failure]).

**[call-domains]** A function call has domain `Sync(k)` for a fresh clock
source ([clock-sources](4)) if the callee is the builtin `asclock`;
`asclock(e)` produces a clock value and does not constrain its subject's
domain (the subject is still inferred, for its separate checks). Every other
call
(`mux`, `any`, `all`, `word`, …) has the join of its arguments' domains
([joining-operators]).

**[dynamic-read-domain]** A dynamic memory read `mem[dyn i]` joins the
subject's and the index's domains ([joining-operators]); if the join
succeeds, the expression's domain is the subject's domain.

**[struct-domain]** A struct construction `${f = e, …}` and a constructor
application have the join of their field/argument expressions' domains
([joining-operators]).

### Control-flow joins

**[if-join]** An `if`/`when` expression has the join of: every guard's domain
and every arm *result expression*'s domain. Arms that are statement blocks
contribute no value and do not join, but their guards do.

**[match-join]** A `match` expression has the join of: the subject's domain
and every arm *result expression*'s domain. Patterns are not expressions and
do not participate (their bound variables take the subject's domain,
[reference-domain](3)).

**[let-domain]** A `let` expression: each binding's right-hand side is an
independent expression with a separately inferred domain (not joined); the
`let`'s domain is its body's domain. A statement-position `let` (or an
arm-block body) has no value domain.

**[arm-block-domain]** A single-expression arm block `{ e }` has the domain
of `e`. Any other arm block has no value domain (its style is diagnosed
elsewhere).

### Join failure

**[join-failure]** A join of two distinct clock classes reports
`DomainMismatch` once, at the joining expression, and evaluates to *unknown*
(the subtree's other checks are suppressed; [report-once]).

---

## Driver, guard, and connection checking

### The walk

**[the-walk]** The walk visits every expression-bearing node of the module
and establishes the roots handed to *Expression-domain inference*: driver
right-hand sides and indexed write indices, `on`-clause clocks, `if` guards,
`match` subjects, `let` bindings, and arm bodies. The walk tracks an
*it-context* (the component whose it-block or memory body encloses the
current position) and a stack of *enclosing guards* (see *Guards and match
subjects*).

### Driver checking

**[driver-check]** For every driver statement `t := e`, `t <= e`, `it := e`,
`it <= e` (and `initial` statements), the checker resolves the target
component `t` (through the it-context when written as `it`/`it.…`; the
written path is resolved directly when the target was not registered as a
reference, as with `initial`). The driving expression's domain `D(e)` **must**
upcast to the target's parent-view domain `D(t)`:

```
D(e) ⊑ D(t)          — else: DriverDomainMismatch
```

A driver whose target is Clock-typed is subject to one further rule: its
driving expression **must** be a plain reference to a Clock-typed thing
([clock-binding]); anything else — a computed clock — is reported
`ConditionalClockDriver` (see *Diagnostic catalog*).  Clocks are defined,
not assigned ([opaque-clocks]): a computed driver makes a clock's identity
depend on runtime behavior, which is meaningless for an opaque clock.  A
guarded driver of a plain clock reference is legal but reported
`GuardedClockDriver` as a warning: the whole conditional acts on a single
clock domain ([conditional-one-domain]), so the conditional cannot change
which clock the target denotes, and the driver joins the target into the
driving clock's class exactly as an unconditional driver would.  The
warning is suppressed when the target's class is unresolved — the mixed
conditional (or an unsourced driving clock) is already reported as an
error.  When the
conditional is mixed ([conditional-one-domain]), it is reported
`DomainMismatch` and contributes no class; the per-driver checks do not
cascade.  Guard checking ([guard-check]) applies to a Clock-typed target
exactly when the target's class is resolved: a guard on a foreign clock
is reported `DriverDomainMismatch`, and a driver that binds an instance
clock port is checked definitionally — the upcast above holds by
construction, and the binding's content is the structural requirement of
[clock-binding].

**[memory-write-check]** For every indexed memory write `mem[dyn i] <= e`,
both the index's domain `D(i)` and the written data's domain `D(e)` **must**
upcast to the memory's parent-view domain; each violation reports
`DriverDomainMismatch`.

**[unset-targets]** A target whose parent-view domain is unset (a missing
clause already diagnosed, or a clock class that failed to resolve) is not
driver-checked; the checker does not cascade.

**[reg-drivers]** A `reg`'s parent-view domain is always a `Sync` domain
([upcast-order], [async-register]); consequently a register may only be
driven from its clock's domain — or by `Constant` expressions — and an
`Async` or foreign-clock driving expression is a `DriverDomainMismatch`.

**[initial-drivers]** `initial` statements drive their target and are
driver-checked exactly like other drivers ([driver-check]).

### Guards and match subjects

**[guard-check]** A statement-position `when` guard gates every driver in its
arms; the guard is partly a driver of the gated value. A guard's domain,
together with the driver's domain, **must** upcast to the driven
component's parent-view domain:

```
D(guard) ⊑ D(t)        — else: DriverDomainMismatch (at the guard)
```

**[match-subject-check]** A statement-position `match`'s subject selects the
driven value and constrains the drivers in its arms exactly like a guard
([guard-check]).

**[guard-propagation]** Guard propagation: an arm's enclosing guards are the
parent's guards extended with the arm's guard (and, for `match`, the
subject) — but only when the arm's joined domain is known
([if-join]/[join-failure]). When an `if`'s guards conflict among themselves,
they do not constrain the drivers beneath it, or every driver would cascade
the same error ([report-once]).

**[guard-judged-once]** Each (guard, driven target) pair is judged once: one
bad guard is reported once per component it wrongly gates, not once per
driver beneath it.

### Connections

**[connection-check]** A bidirectional connection `a :=: b` joins its two
sides to one domain (a connection is bidirectional; neither side is
"expected"). If the two sides' parent-view domains are distinct `Sync`
domains, the checker reports `DomainMismatch` at the connection. Nets on
distinct clocks cannot be connected.

### Memory regions in the walk

**[region-clock]** An `on` region's clock expression is inferred but never
joined with its surroundings — the region clock is an ordinary clock value,
`Sync(k)` for its class ([declaration-fixed-domains]), not a domain
constraint; the memory-write checks of *Memory clocking* govern the
region-clock correctness.

---

## Memory clocking

**[memory-write-context]** A memory write is legal only in a *clocked write
context*:

1. a module-level write site (a driver outside the memory's body) is legal
   iff the memory's clock slot is resolved ([memory-clock-slot]);
2. within a memory body, top-level writes and an `on <CLOCK> { … }` region
   are mutually exclusive: a body uses one or the other;
3. inside an `on` region, only writes to the region's memory are legal
   (`if`/`match` guard structure and `unused` are also allowed as wrappers);
   any other statement is reported `MemoryOnRegionNonWrite`.

A write outside a clocked write context is reported
`MemoryWriteOutsideOnRegion`.

**[region-clock-mismatch]** An `on <CLOCK>` region inside a memory body
**must** name the memory's clock slot; any other clock is reported
`MemoryOnRegionClockMismatch` at the region's clock. A memory whose slot is
unresolved (no clause, zero or ≥2 clocks — `MissingOnClause` already
reported) does not additionally check its regions.

**[memory-assign-style]** A memory word write uses `<=` (an indexed driver);
using `:=` is a warning (`MemoryAssignInsteadOfLassign`) accepted so that
fixing it never changes legality.

**[memory-read-domain]** A memory read `mem[dyn i]` participates in
expression checking ([dynamic-read-domain]): the index's domain joins with
the memory's, and the read's domain is the memory's.

---

## Diagnostic catalog

All diagnostics in this table are **errors** unless marked otherwise. The
*Message* column is the exact rendered text, with the substituted arguments.

| Diagnostic | Reported by | Message |
|---|---|---|
| `MissingOnClause` | component analysis ([missing-on-clause]) | `Missing on clause for reg <component>` |
| `RedundantOnClause` *(warning)* | component analysis ([redundant-on-clause]), socketdef check ([socketdef-multi-clock]) | ``Redundant `on` clause: `<component>` `` |
| `DomainMismatch` | clock checker ([join-failure], [connection-check]); component analysis ([clock-provenance]) | `Domain mismatch: cannot combine <left> with <right>` |
| `MissingDomainClause` | clock checker ([unsafe-clocks-skip], [zero-clock-coverage], [multi-clock-coverage]), socketdef check ([socketdef-multi-clock]) | `Missing \`on\` or \`async\` clause for <component>: <reason>` |
| `AsyncRegister` | clock checker ([async-register]) | `` `<component>` cannot be `async`: registers and memories are clocked `` |
| `FlexibleInstanceClockConflict` | clock checker ([pinning](4)) | `Instance `<instance>` of a clockless module is used on both <left> and <right>; a clockless module resolves to a single domain per instantiation` |
| `DriverDomainMismatch` | clock checker ([driver-check], [memory-write-check], [guard-check]) | `Domain mismatch: cannot drive `<target>` (<target domain>) with <expr domain>` |
| `ConditionalClockDriver` | clock checker ([driver-check]) | `` `<target>` is a clock and may only be driven by a reference to a clock: clocks are defined, not assigned `` |
| `GuardedClockDriver` *(warning)* | clock checker ([driver-check]) | `` `<target>` is a clock: a conditional cannot change which clock it denotes — drive it unconditionally `` |
| `MissingClockDependson` | component analysis ([clock-provenance]) | `` outgoing clock `<component>` is driven from `<clock>`; declare `dependson <clock>` `` |
| `MemoryWriteOutsideOnRegion` | driver analysis ([memory-write-context]) | `write to memory <component> outside a clocked write context (an \`on\` region, or a module with exactly one \`Clock\`)` |
| `MemoryOnRegionClockMismatch` | driver analysis ([region-clock-mismatch]) | `` `on` region in memory <component> must name the memory's clock <clock> `` |
| `MemoryOnRegionNonWrite` | driver analysis ([memory-write-context]) | `a statement other than a memory write appeared inside a memory body's \`on\` region` |

**Domain rendering.** For diagnostics, a domain renders as follows:
`Sync(k)` as ``clocked by `<the class's canonical source>` `` (or
`<?clock?>` if the class cannot be resolved); `Async` as `async`; `Constant`
as `constant`.

---

## Implementation notes (non-normative)

This specification was derived from the following sources:

* `virdant/src/analysis/domain.rs` — the `Domain` lattice, `upcasts_to`,
  `lub`, `glb` (*The domain lattice*).
* `virdant/src/analysis/component.rs` — declaration-level domain assignment,
  unique-clock and flexible inference (implemented as `implicit_clock`),
  socket channel rules, memory clock
  slots, `incoming_clock_ports` (*Clocks of a module*,
  *Declaration-level domain assignment*).
* `virdant/src/queries/clockcheck.rs`, `virdant/src/clocks.rs`,
  `clocks/check.rs`, `clocks/infer.rs` — the checking engine: skip rules,
  binding, flexible resolution, the walk, driver/guard/connection checks,
  expression inference (*The clock-checking pass* through *Driver, guard, and
  connection checking*).
* `virdant/src/queries/check.rs` — where the pass runs relative to other
  checks and where socketdef domain rules are checked
  (*Declaration-level domain assignment*, *The clock-checking pass*).
* `virdant/src/analysis/drivers.rs` — memory write contexts and region
  clock checks (*Memory clocking*).
* `virdant/src/diagnostics.rs` — diagnostic payloads and messages
  (*Diagnostic catalog*).
* Tests: `tests/check/{clock_color,multi_clock,zero_clock,asclock,socket_clock,socketdef_clock_structure,clockcheck_submodule_skip}`,
  `tests/errors/clocks/*`, `tests/warnings/{redundant_on_clause,guarded_clock_driver}`.

* Amendments (clock-class design discussion): clock classes, sources, and
  definitions ([clock-classes], [clock-sources]); the `dependson`
  clock-provenance declaration ([clock-provenance]); the `Clock` domain and
  the revised lattice (*The domain lattice*); the class-based clock count and
  unique clock ([clock-count], [unique-clock]); class-based bindings
  (*Clocks across boundaries*); the definition rule for clock drivers and the
  `ConditionalClockDriver` diagnostic ([driver-check], *Diagnostic catalog*);
  the clock-variable refactor: `Flexible` removed from the domain lattice in
  favor of the clock variable ([clock-variable], [pinning]); the
  removal of the `Clock` domain: Clock-typed components are clocked by their
  clock classes ([declaration-fixed-domains], [instance-clock-domain]).
