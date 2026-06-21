# Syntax Changes

This section details all syntax changes needed for clock domain tracking in Virdant.

## Overview of Changes

1. **Add `async` keyword** after type in component declarations
2. **Add `on <clock>` clause** after type in component declarations
3. **Both annotations are optional** and inferred when possible
4. **Add `sync(x)` and `async(x)` primitive expressions**

## Component Declaration Syntax

### Current Syntax (Before Changes)

```virdant
mod Example {
    incoming clock : Clock
    incoming reset : Reset
    incoming data : Bit
    outgoing result : Bit on clock

    reg state : State on clock
    wire temp : Bit
}
```

### New Syntax (After Changes)

```virdant
mod Example {
    incoming clock : Clock
    incoming reset : Reset
    incoming data : Bit async          // Explicit async declaration
    outgoing result : Bit on clock     // Explicit clocked declaration

    reg state : State on clock         // Unchanged
    wire temp : Bit                    // Inferred from usage
}
```

### Concrete Syntax Examples

**Correct Examples:**

```virdant
// Async input (external signal, not synchronized)
incoming button : Bit async

// Clocked output (synchronized to local clock)
outgoing clean : Bit on clock

// Reg with explicit clock (existing syntax)
reg state : DebState on clock

// Wire with explicit clock
wire synced : Bit on clock

// Wire without annotation (inferred)
wire temp : Bit

// Multiple clocks in same module
incoming clk_a : Clock
incoming clk_b : Clock
reg data_a : Bit on clk_a
reg data_b : Bit on clk_b
```

**Incorrect Examples:**

```virdant
// ERROR: Both async and on specified
incoming x : Bit async on clock

// ERROR: 'async' on a Clock type is meaningless
incoming clk : Clock async

// ERROR: 'async' on a Reg (regs must be clocked)
reg state : Bit async

// ERROR: Clock 'unknown_clock' not in scope
wire data : Bit on unknown_clock

// ERROR: 'clock_name' is not a Clock type
incoming clock_name : Bit
wire data : Bit on clock_name
```

## Grammar Changes (EBNF-like)

### Modified Productions

The existing grammar already supports `OnClause` for `reg` and `wire` declarations.
We extend this to all component kinds and add the `async` keyword.

```
// BEFORE (existing production for reg)
ModDefStmtComponent =
    <docstring:DocString?>
    "reg" <ident:Ident> ":" <typ:Type>
    <on:OnClause?> <it:ItBlock?>

// AFTER (new production for all component kinds)
ModDefStmtComponent =
    ComponentKind <ident:Ident> ":" <typ:Type>
    <clock_domain:ClockDomain?> <it:ItBlock?>

ClockDomain =
    "async"
    | "on" <clock_ref:Expr>
    | /* empty - inferred */

ComponentKind =
    "incoming"
    | "outgoing"
    | "outgoing" "wire"
    | "outgoing" "reg"
    | "wire"
    | "reg"
```

### Clock Domain Annotation Ordering

The `async` keyword and `on` clause are **mutually exclusive**:

```
ClockDomain =
    "async"           // Signal is asynchronous
    | "on" Expr       // Signal is synchronized to specified clock
    | /* nothing */   // Clock domain will be inferred
```

### Clock Reference Validation

The expression in `on <expr>` must resolve to a component with type `Clock`:

```
// Valid: 'clock' is an incoming port with type Clock
incoming clock : Clock
wire data : Bit on clock

// Valid: 'pll_clk' is another component with type Clock
incoming pll_clk : Clock
reg fast_data : Bit on pll_clk

// Invalid: 'some_signal' has type Bit, not Clock
incoming some_signal : Bit
wire data : Bit on some_signal  // ERROR
```

## Primitive Expression Syntax

### `sync(x)` - Synchronize Signal

```virdant
wire btn_sync : Bit on clock = sync(button)
```

**Syntax:** `sync` `(` Expr `)`

**Semantics:**
- Input type: `T` in any clock domain (async or clocked)
- Output type: `T` in the **expected** clock domain (checked context)
- Allocates a double-flop synchronizer in generated Verilog

**Examples:**

```virdant
// Sync async Bit to local clock
wire synced : Bit on clock = sync(async_input)

// Sync from one clock domain to another
wire data_b : Bit on clk_b = sync(data_a)

// ERROR: Cannot infer expected clock domain
// (sync result must be assigned to a target with known clock)
reg x : Bit = sync(button)  // Missing 'on clock' on reg
```

### `async(x)` - Mark as Asynchronous

```virdant
wire data_async : Bit async = async(synced_data)
```

**Syntax:** `async` `(` Expr `)`

**Semantics:**
- Input type: `T` in any clock domain
- Output type: `T async`
- No hardware generated - type-level operation only

**Examples:**

```virdant
// Convert clocked signal to async for cross-module use
wire async_data : Bit async = async(local_data)

// No-op: async signal to async
wire also_async : Bit async = async(other_async)
```

## Grammar Changes for Primitives

Add new productions for primitive expressions:

```
ExprPrimary =
    | ...existing productions...
    | "sync" "(" <arg:Expr> ")"
    | "async" "(" <arg:Expr> ")"
```

These follow the same pattern as existing primitives like `mux`, `sext`, `zext`:

```virdant
// Existing primitive examples
mux(sel, a, b)      // Multiplexer
sext(x)             // Sign extend
zext(x)             // Zero extend
word(a, b, c)       // Concatenate into word

// New clock domain primitives
sync(x)             // Synchronize to expected clock
async(x)            // Mark as asynchronous
```

## Edge Cases and Constraints

### Constraint 1: `async` on Register Declarations

Registers **must** have a clock. The `async` keyword is invalid on `reg`:

```virdant
// INVALID: Reg must be clocked
reg state : Bit async

// VALID: Reg with explicit clock
reg state : Bit on clock

// VALID: Reg with inferred clock
reg state : Bit
```

### Constraint 2: Clock Type Cannot Be Async or Clocked

The `Clock` and `Reset` types cannot have clock domain annotations:

```virdant
// INVALID: Clock cannot be async
incoming clk : Clock async

// INVALID: Clock cannot be 'on' another clock
incoming clk : Clock on other_clk

// VALID: Clock has no domain annotation
incoming clk : Clock
```

This will be enforced as a semantic error during type checking.

### Constraint 3: Mutual Exclusivity

`async` and `on <clock>` cannot both appear:

```virdant
// INVALID: Cannot specify both
incoming x : Bit async on clock
```

### Constraint 4: Clock Reference Must Be In Scope

```virdant
mod Parent {
    incoming clock : Clock

    mod child of Child
    // child.clock := clock  // Parent's clock

    // INVALID: 'clock' not visible inside Child's port assignments
    // unless explicitly passed in
}

mod Child {
    // 'clock' must be declared here to use in 'on' clauses
    incoming clock : Clock
    wire data : Bit on clock
}
```

### Constraint 5: Expression Position for `on` Clause

The `on` clause uses an `Expr` production (currently), which allows:

```virdant
// Current grammar allows expressions
wire x : Bit on clock
wire y : Bit on clock  // simple identifier

// But complex expressions would be unusual:
wire z : Bit on some_complex_expr  // Technically parseable
```

**Recommendation:** During semantic analysis, validate that the expression
is a simple path reference (no operators, no function calls).

### Constraint 6: External Module Ports

External module declarations require explicit clock domain annotations:

```virdant
ext mod ExternalUart {
    incoming clk : Clock
    incoming tx : Bit async
    outgoing rx : Bit on clk
}
```

The `on` clause must specify which clock port the signal relates to.

## Summary of Grammar Changes

| Change | Location | Description |
|--------|----------|-------------|
| `async` keyword | After type in component | Marks signal as asynchronous |
| `on <clock>` clause | After type in component | Specifies clock domain (already exists for reg) |
| `sync(x)` expression | ExprPrimary | Synchronize signal to expected clock |
| `async(x)` expression | ExprPrimary | Convert signal to async type |

## Parsing Order

The grammar unambiguously parses clock domain annotations:

1. Parse component kind (`incoming`, `outgoing`, `wire`, `reg`)
2. Parse identifier
3. Parse `:` separator
4. Parse type
5. Parse optional clock domain (`async` or `on <expr>` or nothing)
6. Parse optional it-block

The lexer distinguishes `async` from other keywords.
The parser rejects `async on` as two separate annotations (mutual exclusivity).

## Validation During Parsing

The following are **parse-time errors** (rejected by the grammar):

- `async` followed by `on`: `incoming x : Bit async on clock`

The following are **semantic errors** (checked after parsing):

- `async` on a `reg` declaration
- `async` or `on` on `Clock` or `Reset` type
- Clock reference not in scope
- Clock reference does not have type `Clock`

---

# Type System Changes

This section details how the type system must be extended to track clock domains.

## Fundamental Design

Every value in Virdant has **two** orthogonal properties:

1. **Type** - The shape of the data (`Bit`, `Word[8]`, `DebState`, etc.)
2. **ClockDomain** - The timing domain (`async` or `on <clock>`)

These properties are **completely independent**: a `Bit` can exist in any clock domain,
and a clock domain can contain values of any type.

## ClockDomain Enum

```rust
/// Represents the clock domain of a value.
///
/// Every value in Virdant has an associated ClockDomain that determines
/// its timing relationship to other values in the design.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClockDomain {
    /// Signal is asynchronous - no timing guarantees.
    /// External inputs that may change at any time relative to any clock.
    /// Cannot be used directly in sequential logic without synchronization.
    Async,

    /// Signal is synchronized to a specific clock.
    /// The Identifier refers to a component with type `Clock` that is in scope.
    /// Two OnClock values are in the same domain iff their identifiers refer
    /// to the same Clock component declaration.
    OnClock(Identifier),

    /// Used during type inference when the domain is not yet determined.
    /// This variant should never appear in the final, fully-typed AST.
    /// Attempting to output a value with Unknown domain is a bug.
    Unknown,
}
```

### Identifier Type

The `Identifier` in `OnClock(Identifier)` refers to the **symbol ID** of the
clock component, not a string name:

```rust
/// A reference to a declared clock component.
/// This is the symbol ID of the `incoming clock : Clock` declaration.
pub type Identifier = SymbolId;
```

Using `SymbolId` ensures that:
- Two references to `clock` in the same scope refer to the same domain
- We can resolve clock references across module boundaries
- We avoid string comparison for domain equality

### ClockDomain Equality Rules

Two `ClockDomain` values are equal if and only if:

```rust
impl PartialEq for ClockDomain {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Async equals Async
            (ClockDomain::Async, ClockDomain::Async) => true,

            // OnClock equals OnClock if their identifiers refer to the
            // same clock component declaration
            (ClockDomain::OnClock(id1), ClockDomain::OnClock(id2)) => id1 == id2,

            // Unknown never equals anything (used during inference only)
            // This ensures we keep propagating constraints
            (ClockDomain::Unknown, _) | (_, ClockDomain::Unknown) => false,

            // Async and OnClock are never equal
            _ => false,
        }
    }
}
```

**Important:** `Unknown` is not equal to itself. This ensures that during inference,
unknown domains continue to be constrained until resolved.

## TypedExpr Structure

Instead of modifying `TypedExpr` directly, we introduce a new wrapper struct that
associates a type with its clock domain:

```rust
/// A value with both a type and a clock domain.
///
/// This is the fundamental unit of type information during and after inference.
/// Every expression in a well-typed program has a `Typed` value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Typed {
    /// The type of the value (Bit, Word[n], etc.)
    pub typ: Type,

    /// The clock domain of the value (async or on <clock>)
    pub clock: ClockDomain,
}

impl Typed {
    /// Create a new typed value.
    pub fn new(typ: Type, clock: ClockDomain) -> Self {
        Typed { typ, clock }
    }

    /// Create an async value of the given type.
    pub fn async_of(typ: Type) -> Self {
        Typed::new(typ, ClockDomain::Async)
    }

    /// Create a clocked value of the given type.
    pub fn on_clock(typ: Type, clock: SymbolId) -> Self {
        Typed::new(typ, ClockDomain::OnClock(clock))
    }

    /// Create a typed value with unknown domain (for inference).
    pub fn unknown_domain(typ: Type) -> Self {
        Typed::new(typ, ClockDomain::Unknown)
    }
}
```

### Why a New Struct?

We introduce `Typed` rather than modifying `Type` because:

1. **Orthogonality:** Type and ClockDomain are independent. A `Word[8]` can exist
   in any clock domain, and adding `ClockDomain` to `Type` would create an explosion
   of type combinations.

2. **Separation of Concerns:** Type checking (is this `Bit` compatible with that `Bit`?)
   is independent of domain checking (are these in the same clock domain?).

3. **Inference Separation:** Type inference and domain inference can run in parallel
   or sequentially, with domain inference potentially being a separate pass.

## Changes to Typing Struct

The `Typing` struct currently stores type annotations. We extend it to store
clock domain annotations as well:

```rust
#[derive(Debug)]
pub struct Typing {
    item: Symbol,
    exprroot: ExprRoot,

    /// Type annotations for each AST node (existing)
    typs: IndexMap<AstNodeId, Type>,

    /// Clock domain annotations for each AST node (new)
    clocks: IndexMap<AstNodeId, ClockDomain>,

    diagnostics: Vec<Diagnostic>,
    use_locations: IndexMap<BString, Vec<Location>>,
    tags: IndexMap<Location, Tag>,
}

impl Typing {
    /// Get both type and clock domain for a node.
    pub fn typed_at(&self, id: AstNodeId) -> Option<Typed> {
        let typ = self.typs.get(&id)?.clone();
        let clock = self.clocks.get(&id).cloned().unwrap_or(ClockDomain::Unknown);
        Some(Typed::new(typ, clock))
    }

    /// Get type for a node (existing method, unchanged signature).
    pub fn type_at(&self, id: AstNodeId) -> Option<&Type> {
        self.typs.get(&id)
    }

    /// Get clock domain for a node (new method).
    pub fn clock_at(&self, id: AstNodeId) -> Option<&ClockDomain> {
        self.clocks.get(&id)
    }

    /// Annotate a node with both type and clock domain.
    pub fn annotate_typed(&mut self, node: &AstNode<'_>, typed: &Typed) {
        let previous_typ = self.typs.insert(node.id(), typed.typ.clone());
        let previous_clock = self.clocks.insert(node.id(), typed.clock.clone());

        if previous_typ.is_some() || previous_clock.is_some() {
            panic!(
                "Node {:?} already annotated at {:?}",
                node.id(),
                node.region()
            );
        }
    }

    /// Annotate a node with type only (clock remains Unknown or existing).
    pub fn annotate_type(&mut self, node: &AstNode<'_>, typ: &Type) {
        let previous = self.typs.insert(node.id(), typ.clone());
        if let Some(previous_typ) = previous {
            panic!(
                "Node already has type {previous_typ:?}, can't annotate with {typ:?} at {:?}",
                node.region()
            );
        }
    }

    /// Annotate a node with clock domain only.
    pub fn annotate_clock(&mut self, node: &AstNode<'_>, clock: &ClockDomain) {
        self.clocks.insert(node.id(), clock.clone());
    }
}
```

## Changes to TypingContext

The `TypingContext` tracks bindings. We extend each binding to include clock domain:

```rust
#[derive(Debug, Clone)]
pub struct TypingContext {
    /// Stack of variable bindings.
    /// Each binding maps a name to a referent and its full type information.
    context: Vec<(BString, Binding)>,
}

/// A binding in the typing context.
#[derive(Debug, Clone)]
pub struct Binding {
    /// What the name refers to (component or local variable).
    pub referent: Referent,

    /// The full type information including clock domain.
    /// None if type resolution failed (error already reported).
    pub typed: Option<Typed>,
}

impl TypingContext {
    pub fn new() -> TypingContext {
        TypingContext { context: vec![] }
    }

    /// Look up a binding by name.
    pub fn get(&self, name: &BStr) -> Option<&Binding> {
        for (name_, binding) in self.context.iter().rev() {
            if name.as_bstr() == name_.as_bstr() {
                return Some(binding);
            }
        }
        None
    }

    /// Push a component binding onto the context.
    pub fn push_component(
        mut self,
        name: BString,
        component_id: ComponentId,
        typed: Typed,
    ) -> TypingContext {
        self.context.push((
            name,
            Binding {
                referent: Referent::Component(component_id),
                typed: Some(typed),
            },
        ));
        self
    }

    /// Push a local variable binding onto the context.
    pub fn push_local(
        mut self,
        name: BString,
        location: Location,
        typed: Typed,
    ) -> TypingContext {
        self.context.push((
            name,
            Binding {
                referent: Referent::Local(location),
                typed: Some(typed),
            },
        ));
        self
    }

    /// Push a binding with unresolved type (for error recovery).
    pub fn push_unresolved(
        mut self,
        name: BString,
        referent: Referent,
    ) -> TypingContext {
        self.context.push((
            name,
            Binding {
                referent,
                typed: None,
            },
        ));
        self
    }
}
```

## Display and Debug Implementations

### ClockDomain Display

```rust
impl std::fmt::Display for ClockDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClockDomain::Async => write!(f, "async"),
            ClockDomain::OnClock(id) => write!(f, "on <clock:{}>", id),
            ClockDomain::Unknown => write!(f, "<unknown domain>"),
        }
    }
}
```

**Note:** The Display for `OnClock` shows the symbol ID for debugging purposes.
In error messages, we should resolve the symbol ID to the actual clock name.

### Typed Display

```rust
impl std::fmt::Display for Typed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.clock {
            ClockDomain::Async => write!(f, "{} async", self.typ),
            ClockDomain::OnClock(_) => write!(f, "{} on <clock>", self.typ),
            ClockDomain::Unknown => write!(f, "{}", self.typ),
        }
    }
}
```

For error messages, we'll have a helper that resolves clock names:

```rust
impl Typed {
    /// Display with resolved clock name for error messages.
    pub fn display_with(&self, builder: &Builder) -> String {
        match &self.clock {
            ClockDomain::Async => format!("{} async", self.typ),
            ClockDomain::OnClock(id) => {
                let clock_name = builder.get_symbol(*id).name();
                format!("{} on {}", self.typ, clock_name)
            }
            ClockDomain::Unknown => format!("{} (unknown domain)", self.typ),
        }
    }
}
```

## Type Checking Rules

### Rule 1: Combinational Operations Preserve Domain

All binary and unary operators require operands in the **same domain**, and the
result is in that domain:

```
Γ ⊢ e1 : T1 @ d1    Γ ⊢ e2 : T2 @ d2    d1 == d2
------------------------------------------------
    Γ ⊢ e1 + e2 : (T1 + T2) @ d1
```

Where `e : T @ d` means expression `e` has type `T` in domain `d`.

### Rule 2: Driver LHS and RHS Must Match Domain

```
Γ ⊢ target : T @ d1    Γ ⊢ expr : T @ d2    d1 == d2
-----------------------------------------------------
        Γ ⊢ target := expr : ok
```

The target and expression must have the **same type** and the **same domain**.

### Rule 3: `sync(x)` Changes Domain

```
    Γ ⊢ x : T @ d1    expected : T @ d2
------------------------------------------
    Γ ⊢ sync(x) : T @ d2
```

The `sync(x)` primitive:
- Preserves the type `T`
- Changes domain to the **expected** domain in the surrounding context
- Produces a warning if `d1 == d2` (no-op sync)

### Rule 4: `async(x)` Removes Domain

```
    Γ ⊢ x : T @ d1
----------------------
    Γ ⊢ async(x) : T @ Async
```

The `async(x)` primitive:
- Preserves the type `T`
- Sets domain to `Async` regardless of input domain
- Produces a warning (this operation discards timing information)

### Rule 5: Match Subject and Arms Must Match Domain

```
Γ ⊢ subject : T @ d
Γ ⊢ pat1 => body1 : U @ d
...
Γ ⊢ patn => bodyn : U @ d
---------------------------------
Γ ⊢ match subject { ... } : U @ d
```

All arms and the result must be in the same domain as the subject.

### Rule 6: When Arms Must Match Domain

```
Γ ⊢ guard1 : Bit @ d
Γ ⊢ body1 : T @ d
...
Γ ⊢ guardn : Bit @ d
Γ ⊢ bodyn : T @ d
---------------------------------
Γ ⊢ when { ... } : T @ d
```

All guards and bodies must be in the same domain.

## Domain Inference Algorithm

### Overview

Domain inference uses a **constraint collection + unification** approach:

1. **Collection Phase:** Walk the AST, collecting constraints on domains
2. **Unification Phase:** Solve constraints, detecting conflicts
3. **Reporting Phase:** Report any unresolvable domains or conflicts

### Constraint Types

```rust
enum DomainConstraint {
    /// Node's domain must equal the given domain
    MustBe(AstNodeId, ClockDomain),

    /// Two nodes must have the same domain
    SameDomain(AstNodeId, AstNodeId),

    /// Node's domain must be Async (for async primitive)
    MustBeAsync(AstNodeId),

    /// Node is the result of sync(), domain comes from expected context
    FromExpected(AstNodeId),
}
```

### Collection Rules

1. **Explicit Annotations:** If a component has `async` or `on <clock>`,
   add `MustBe(node_id, domain)` constraint.

2. **Binary Operators:** For `e1 op e2`, add `SameDomain(e1, e2)` and
   `SameDomain(e1, result)`.

3. **Drivers:** For `target := expr`, add `SameDomain(target, expr)`.

4. **Match:** For `match subject { ... }`, add `SameDomain(subject, each_arm)`
   and `SameDomain(each_arm, result)`.

5. **Primitives:**
   - `sync(x)` is special-cased during checking, not inference
   - `async(x)` adds `MustBe(result, ClockDomain::Async)`

### Unification

Use union-find on domain variables:

```rust
struct DomainInference {
    /// Map from node ID to its domain (possibly Unknown)
    domains: IndexMap<AstNodeId, ClockDomain>,

    /// Union-find structure for equating domains
    uf: UnionFind<AstNodeId>,

    /// Collected constraints
    constraints: Vec<DomainConstraint>,

    /// Diagnostics generated during inference
    diagnostics: Vec<Diagnostic>,
}
```

### Handling Conflicts

When unification finds a conflict (e.g., trying to unify `Async` with `OnClock(id)`):

1. Record the conflict location
2. Generate a diagnostic with:
   - The two conflicting domains
   - Where each domain was established
   - A suggestion to use `sync()` or `async()`

## Integration with Existing Type Checking

### Orthogonal Passes

Domain inference can run **after** type inference:

```rust
pub(crate) fn typecheck(builder: &mut Builder, symbol_id: SymbolId) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    // ... existing type checking ...

    // New: Domain inference pass
    diagnostics.extend(infer_domains(builder, symbol_id));

    Arc::new(diagnostics)
}
```

### Combined Pass (Alternative)

Alternatively, domain checking can be integrated into the existing type check:

```rust
impl Typing {
    fn check(&mut self, builder: &mut Builder, context: &TypingContext,
             node: &AstNode<'_>, expected: &Type) -> Result<Typed, ()> {
        match node.payload() {
            AstNodePayload::ExprBinOp(binop) => {
                let left = node.child(0);
                let right = node.child(1);

                let left_typed = self.check(builder, context, &left, expected)?;
                let right_typed = self.check(builder, context, &right, expected)?;

                // Type check (existing)
                // ... type compatibility logic ...

                // Domain check (new)
                if left_typed.clock != right_typed.clock {
                    self.diagnostics.push(diagnostics::DomainMismatch {
                        region: node.region(),
                        left_region: left.region(),
                        left_domain: left_typed.clock.clone(),
                        right_region: right.region(),
                        right_domain: right_typed.clock.clone(),
                    }.into());
                    return Err(());
                }

                let result_typed = Typed::new(result_type, left_typed.clock);
                self.annotate_typed(node, &result_typed);
                Ok(result_typed)
            }
            // ... other cases ...
        }
    }
}
```

The combined approach is likely cleaner since type and domain checks often
need the same context.

## Summary of Type System Changes

| Component | Change | Purpose |
|-----------|--------|--------|
| `ClockDomain` | New enum | Represents async/on-clock domains |
| `Typed` | New struct | Bundles Type + ClockDomain |
| `Typing` | Add `clocks` field | Store domain annotations |
| `TypingContext` | Extend `Binding` | Include domain in bindings |
| Type rules | Domain constraints | Preserve domain through operations |
| Inference | New pass | Solve domain constraints |

---

# Analysis Changes

This section details changes to the analysis module for clock domain tracking.

## Component Struct Changes

### Current Component Definition (component.rs)

The current `Component` struct lacks clock domain information:

```rust
#[derive(Debug, Clone)]
pub struct Component {
    id: ComponentId,
    path: BString,
    location: Location,
    flow: Flow,
    kind: Option<ComponentKind>,
    typ: Option<Type>,
}
```

### New Component Definition

Add a `clock` field to track the component's clock domain:

```rust
use crate::types::ClockDomain;

#[derive(Debug, Clone)]
pub struct Component {
    id: ComponentId,
    path: BString,
    location: Location,
    flow: Flow,
    kind: Option<ComponentKind>,
    typ: Option<Type>,

    /// The clock domain of this component.
    /// None if the domain couldn't be determined (error already reported).
    /// For Clock/Reset types, this is always None (they don't have domains).
    clock: Option<ClockDomain>,
}

impl Component {
    pub fn clock(&self) -> Option<&ClockDomain> {
        self.clock.as_ref()
    }

    /// Returns true if this component is asynchronous.
    pub fn is_async(&self) -> bool {
        matches!(self.clock, Some(ClockDomain::Async))
    }

    /// Returns true if this component is synchronized to a clock.
    pub fn is_clocked(&self) -> bool {
        matches!(self.clock, Some(ClockDomain::OnClock(_)))
    }
}
```

### Rationale

- `Option<ClockDomain>` allows for error recovery when domain inference fails
- `None` is distinct from `Some(ClockDomain::Unknown)` - the latter means "not yet inferred"
- Clock and Reset types have `clock: None` because they don't participate in clock domain tracking

### Updating Component Construction

All places that construct `Component` must provide the clock domain:

```rust
// In build_component_analysis()
let clock_domain = self.infer_component_clock(builder, stmt, component);

let component = Component {
    id,
    path: path.clone(),
    location: stmt.location(),
    typ,
    flow,
    kind: Some(component_kind),
    clock: clock_domain,
};
```

## ComponentAnalysis Changes

### Tracking Clocks in Scope

The `ComponentAnalysis` struct must track which clock components are in scope:

```rust
#[derive(Debug)]
pub struct ComponentAnalysis {
    moddef: SymbolId,
    components: Vec<(BString, Component)>,
    diagnostics: Vec<Diagnostic>,
    references: IndexMap<Location, (ComponentId, ReferenceKind)>,

    /// Clock components available in this module.
    /// Maps clock name to its SymbolId for domain resolution.
    clocks_in_scope: IndexMap<BString, SymbolId>,
}
```

### Clock Discovery

During component analysis, discover all clocks in the module:

```rust
impl ComponentAnalysis {
    fn discover_clocks(&mut self, builder: &mut Builder, stmts: &[AstNode]) {
        for stmt in stmts {
            if let AstNodePayload::Component(component) = stmt.payload() {
                let typ = stmt.typ().and_then(|t| builder.get_type_at(t.location()).ok());

                if matches!(typ, Some(Type::Clock)) {
                    let name = stmt.parsing().string(component.name).to_owned();

                    // Clocks must be incoming ports
                    if component.kind != ComponentKind::Incoming {
                        self.diagnostics.push(diagnostics::ClockNotInput {
                            region: stmt.region(),
                            name: name.clone(),
                        }.into());
                        continue;
                    }

                    // Store in clocks_in_scope
                    let symbol = self.resolve_component_symbol(builder, &name);
                    if let Some(symbol_id) = symbol {
                        self.clocks_in_scope.insert(name, symbol_id);
                    }
                }
            }
        }
    }
}
```

### Inferring Component Clock Domain

Add a method to infer a component's clock domain from its declaration:

```rust
impl ComponentAnalysis {
    fn infer_component_clock(
        &mut self,
        builder: &mut Builder,
        stmt: &AstNode<'_>,
        component: &payload::Component,
    ) -> Option<ClockDomain> {
        let parsing = stmt.parsing();
        let children = stmt.children();

        // Find the OnClause or async keyword in the component declaration
        for child in &children {
            match child.payload() {
                // Explicit async: "incoming x : Bit async"
                AstNodePayload::Kind(kind) => {
                    if parsing.string(kind.name) == b"async" {
                        return Some(ClockDomain::Async);
                    }
                }

                // Explicit clock: "wire x : Bit on clock"
                AstNodePayload::OnClause => {
                    return self.resolve_on_clause(builder, child);
                }

                _ => {}
            }
        }

        // No explicit annotation - domain will be inferred later
        Some(ClockDomain::Unknown)
    }

    fn resolve_on_clause(
        &mut self,
        builder: &mut Builder,
        on_clause: &AstNode<'_>,
    ) -> Option<ClockDomain> {
        // The on clause contains an expression (the clock reference)
        let clock_expr = on_clause.child(0);

        // Resolve to a symbol
        let resolved = self.resolve_clock_reference(builder, &clock_expr)?;

        // Verify it's a Clock type
        let symbol = builder.get_symboltable().symbol(resolved);
        if !self.is_clock_type(builder, resolved) {
            self.diagnostics.push(diagnostics::OnClauseNotClock {
                region: clock_expr.region(),
                name: symbol.name().to_owned(),
            }.into());
            return None;
        }

        Some(ClockDomain::OnClock(resolved))
    }
}
```

### Clock Reference Validation

Validate that `on <clock>` references resolve to valid clocks:

```rust
impl ComponentAnalysis {
    fn resolve_clock_reference(
        &mut self,
        builder: &mut Builder,
        expr: &AstNode<'_>,
    ) -> Option<SymbolId> {
        match expr.payload() {
            AstNodePayload::ExprReference => {
                let path = expr.path()?;
                let name = expr.parsing().string(path);

                // Check if it's in clocks_in_scope
                if let Some(symbol_id) = self.clocks_in_scope.get(name.as_bstr()) {
                    return Some(*symbol_id);
                }

                // Not found - report error
                self.diagnostics.push(diagnostics::UnknownClock {
                    region: expr.region(),
                    name: name.clone(),
                }.into());
                None
            }

            // For now, only simple references are allowed
            _ => {
                self.diagnostics.push(diagnostics::ComplexClockRef {
                    region: expr.region(),
                }.into());
                None
            }
        }
    }

    fn is_clock_type(&self, builder: &mut Builder, symbol_id: SymbolId) -> bool {
        let symbol = builder.get_symboltable().symbol(symbol_id);
        if let Some(typ_location) = symbol.typ_location() {
            if let Ok(typ) = builder.get_type_at(typ_location) {
                return matches!(typ, Type::Clock);
            }
        }
        false
    }
}
```

## Clock Domain Validation Rules

### Rule 1: Clock Types Cannot Have Domains

Clock and Reset types cannot have `async` or `on` annotations:

```rust
fn validate_clock_type_domains(&mut self, stmt: &AstNode<'_>) {
    let payload::Component { kind, .. } = match stmt.payload() {
        AstNodePayload::Component(c) => c,
        _ => return,
    };

    let typ = stmt.typ().and_then(|t| builder.get_type_at(t.location()).ok());

    if matches!(typ, Some(Type::Clock) | Some(Type::Reset)) {
        // Check for async or on clause
        if self.has_async_annotation(stmt) || self.has_on_clause(stmt) {
            self.diagnostics.push(diagnostics::ClockTypeDomain {
                region: stmt.region(),
                typ: typ.unwrap(),
            }.into());
        }
    }
}
```

### Rule 2: Registers Must Be Clocked

Register declarations cannot be `async`:

```rust
fn validate_reg_clock(&mut self, stmt: &AstNode<'_>) {
    let payload::Component { kind, .. } = match stmt.payload() {
        AstNodePayload::Component(c) if c.kind == ComponentKind::Reg => c,
        _ => return,
    };

    if self.has_async_annotation(stmt) {
        self.diagnostics.push(diagnostics::AsyncReg {
            region: stmt.region(),
        }.into());
    }
}
```

### Rule 3: Valid Clock References

The `on <clock>` clause must reference a Clock type:

```rust
fn validate_on_clause_clock(&mut self, stmt: &AstNode<'_>, on_clause: &AstNode<'_>) {
    let clock_expr = on_clause.child(0);

    // Must resolve to a symbol
    let symbol_id = match self.resolve_clock_reference(builder, &clock_expr) {
        Some(id) => id,
        None => return, // Error already reported
    };

    // Must have Clock type
    if !self.is_clock_type(builder, symbol_id) {
        self.diagnostics.push(diagnostics::OnClauseNotClock {
            region: clock_expr.region(),
            name: builder.get_symbol(symbol_id).name().to_owned(),
        }.into());
    }
}
```

## Module Signature Changes

### Current Port Signature

The current `Port` struct in `ports.rs`:

```rust
#[derive(Debug, Clone)]
pub struct Port {
    pub name: BString,
    pub dir: PortDir,
    pub width: Width,
}
```

### New Port Signature with Clock Domain

```rust
use crate::types::ClockDomain;

#[derive(Debug, Clone)]
pub struct Port {
    pub name: BString,
    pub dir: PortDir,
    pub width: Width,

    /// Clock domain of this port.
    /// None for Clock/Reset types or when uninferrable.
    pub clock: Option<ClockDomain>,
}

impl Port {
    /// Create a new port with clock domain.
    pub fn new(name: BString, dir: PortDir, width: Width, clock: Option<ClockDomain>) -> Self {
        Port { name, dir, width, clock }
    }

    /// Create an async port.
    pub fn async_port(name: BString, dir: PortDir, width: Width) -> Self {
        Port::new(name, dir, width, Some(ClockDomain::Async))
    }

    /// Create a clocked port.
    pub fn clocked_port(name: BString, dir: PortDir, width: Width, clock: SymbolId) -> Self {
        Port::new(name, dir, width, Some(ClockDomain::OnClock(clock)))
    }
}
```

### Port Signature Building

Update `build_ports_of` to include clock domain:

```rust
pub(crate) fn build_ports_of(builder: &mut Builder, symbol_id: SymbolId) -> Arc<Vec<Port>> {
    let component_analysis = builder.get_component_analysis(symbol_id);
    let mut ports = vec![];

    for (path, component) in component_analysis.components() {
        // Only include actual ports (not internal wires/regs)
        if !matches!(component.kind(), Some(ComponentKind::Incoming) | Some(ComponentKind::Outgoing)) {
            continue;
        }

        // Skip submodule ports
        if path.contains(&b'.') {
            continue;
        }

        let typ = match component.typ() {
            Some(t) => t,
            None => continue,
        };

        let width = type_width(&typ);
        let dir = match component.kind() {
            Some(ComponentKind::Incoming) => PortDir::Input,
            Some(ComponentKind::Outgoing) => PortDir::Output,
            _ => continue,
        };

        // Get clock domain (None for Clock/Reset types)
        let clock = if matches!(typ, Type::Clock | Type::Reset) {
            None
        } else {
            component.clock().cloned()
        };

        ports.push(Port::new(path.clone(), dir, width, clock));
    }

    Arc::new(ports)
}
```

## Submodule Instantiation Changes

### Validating Port Connections

When connecting submodule ports, validate clock domain compatibility:

```rust
fn validate_submodule_connection(
    &mut self,
    builder: &mut Builder,
    parent_symbol: SymbolId,
    submod_name: &BStr,
    port_name: &BStr,
    expr: &AstNode<'_>,
) {
    // Get the submodule's port signature
    let submod_symbol = self.resolve_submodule(builder, submod_name)?;
    let submod_ports = builder.get_ports_of(submod_symbol.id());

    // Find the specific port
    let port = submod_ports.iter()
        .find(|p| p.name.as_bstr() == port_name)
        .expect("port should exist");

    // Get the expression's clock domain
    let expr_clock = self.infer_expr_clock(builder, expr);

    // Validate compatibility
    match (&port.clock, &expr_clock) {
        // Both async: OK
        (Some(ClockDomain::Async), Some(ClockDomain::Async)) => {}

        // Both on same clock: OK
        (Some(ClockDomain::OnClock(p1)), Some(ClockDomain::OnClock(p2))) if p1 == p2 => {}

        // Port clocked, expr async: ERROR
        (Some(ClockDomain::OnClock(_)), Some(ClockDomain::Async)) => {
            self.diagnostics.push(diagnostics::AsyncToClockedPort {
                region: expr.region(),
                port_name: port_name.to_owned(),
                submod_name: submod_name.to_owned(),
            }.into());
        }

        // Port async, expr clocked: WARN (safe but unusual)
        (Some(ClockDomain::Async), Some(ClockDomain::OnClock(_))) => {
            self.diagnostics.push(diagnostics::ClockedToAsyncPort {
                region: expr.region(),
                port_name: port_name.to_owned(),
                submod_name: submod_name.to_owned(),
            }.into());
        }

        // Any unknown: error cascade, skip
        (_, None) | (None, _) => {}
    }
}
```

### Cross-Module Clock Domain Propagation

When a module instantiates another, clock domains must be checked:

```rust
fn check_cross_module_domain(
    &mut self,
    builder: &mut Builder,
    parent_component: &Component,
    child_port: &Port,
    connection_expr: &AstNode<'_>,
) {
    // Get the domain of the connection expression
    let expr_domain = self.get_expr_domain(builder, connection_expr);

    // Check if child port has explicit domain requirement
    if let Some(ref child_domain) = child_port.clock {
        match (child_domain, &expr_domain) {
            // Same domain: OK
            (d1, d2) if d1 == d2 => {}

            // Child async, parent clocked: OK (safe)
            (ClockDomain::Async, ClockDomain::OnClock(_)) => {}

            // Child clocked, parent async: ERROR
            (ClockDomain::OnClock(child_clk), ClockDomain::Async) => {
                self.diagnostics.push(diagnostics::DomainCrossingError {
                    region: connection_expr.region(),
                    from_domain: expr_domain.clone(),
                    to_domain: child_domain.clone(),
                    suggestion: "Use sync() to synchronize the signal".into(),
                }.into());
            }

            // Different clocks: ERROR
            (ClockDomain::OnClock(c1), ClockDomain::OnClock(c2)) => {
                self.diagnostics.push(diagnostics::DomainCrossingError {
                    region: connection_expr.region(),
                    from_domain: expr_domain.clone(),
                    to_domain: child_domain.clone(),
                    suggestion: "Use sync() to cross clock domains".into(),
                }.into());
            }

            // Unknown: skip (error cascade)
            _ => {}
        }
    }
}
```

## External Module Changes

### Requiring Clock Annotations

External module declarations must have explicit clock domain annotations:

```rust
fn validate_ext_mod_ports(&mut self, stmt: &AstNode<'_>) {
    let payload::ModDef { is_ext, .. } = match stmt.payload() {
        AstNodePayload::ModDef(m) if m.is_ext => m,
        _ => return,
    };

    // For ext modules, all ports must have explicit clock domain
    for child in stmt.children() {
        if let AstNodePayload::Component(component) = child.payload() {
            let has_explicit_domain = self.has_async_annotation(&child)
                || self.has_on_clause(&child);

            // Skip Clock/Reset types
            let typ = child.typ().and_then(|t| builder.get_type_at(t.location()).ok());
            if matches!(typ, Some(Type::Clock) | Some(Type::Reset)) {
                continue;
            }

            if !has_explicit_domain {
                self.diagnostics.push(diagnostics::ExtModMissingDomain {
                    region: child.region(),
                    port_name: child.parsing().string(component.name).to_owned(),
                    mod_name: stmt.parsing().string(stmt.name().unwrap()).to_owned(),
                }.into());
            }
        }
    }
}
```

## New Diagnostics

### Diagnostic Definitions (diagnostics.rs)

```rust
// Clock domain errors

#[derive(Debug, Clone)]
pub struct UnknownClock {
    pub region: Region,
    pub name: BString,
}

#[derive(Debug, Clone)]
pub struct OnClauseNotClock {
    pub region: Region,
    pub name: BString,
}

#[derive(Debug, Clone)]
pub struct ClockNotInput {
    pub region: Region,
    pub name: BString,
}

#[derive(Debug, Clone)]
pub struct AsyncReg {
    pub region: Region,
}

#[derive(Debug, Clone)]
pub struct ClockTypeDomain {
    pub region: Region,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct ComplexClockRef {
    pub region: Region,
}

// Cross-module errors

#[derive(Debug, Clone)]
pub struct AsyncToClockedPort {
    pub region: Region,
    pub port_name: BString,
    pub submod_name: BString,
}

#[derive(Debug, Clone)]
pub struct ClockedToAsyncPort {
    pub region: Region,
    pub port_name: BString,
    pub submod_name: BString,
}

#[derive(Debug, Clone)]
pub struct DomainCrossingError {
    pub region: Region,
    pub from_domain: ClockDomain,
    pub to_domain: ClockDomain,
    pub suggestion: BString,
}

#[derive(Debug, Clone)]
pub struct ExtModMissingDomain {
    pub region: Region,
    pub port_name: BString,
    pub mod_name: BString,
}
```

### Diagnostic Implementations

```rust
impl Diagnostic for UnknownClock {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("Unknown clock '{}' in 'on' clause", self.name)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }
}

impl Diagnostic for OnClauseNotClock {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("'{}' is not a Clock type; cannot use in 'on' clause", self.name)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }
}

impl Diagnostic for AsyncReg {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        "Register cannot be async; registers must be clocked".into()
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }
}

impl Diagnostic for ClockTypeDomain {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("{} type cannot have clock domain annotation", self.typ)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }
}

impl Diagnostic for DomainCrossingError {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!(
            "Clock domain crossing: cannot assign {} to {}",
            self.from_domain, self.to_domain
        )
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some(self.suggestion.to_string())
    }
}
```

## Summary of Analysis Changes

| Component | Change | Purpose |
|-----------|--------|---------|
| `Component` | Add `clock: Option<ClockDomain>` | Track domain per component |
| `ComponentAnalysis` | Add `clocks_in_scope` | Track available clocks |
| `Port` | Add `clock: Option<ClockDomain>` | Domain in port signature |
| `build_ports_of` | Include clock domain | Propagate domain info |
| New validation functions | Multiple | Enforce domain rules |
| New diagnostics | Multiple | Report domain errors |

## Files to Modify

1. `virdant/src/analysis/component.rs` - Component struct, ComponentAnalysis
2. `virdant/src/analysis/ports.rs` - Port struct, build_ports_of
3. `virdant/src/diagnostics.rs` - New diagnostic types
4. `virdant/src/types/typedef.rs` - Import ClockDomain for Component
5. `virdant/src/db.rs` - Update queries if needed

## Testing Strategy

1. **Unit tests for clock discovery:** Test that incoming Clock ports are identified
2. **Unit tests for domain inference:** Test explicit `async` and `on` clauses
3. **Integration tests for cross-module:** Test submodule port connection validation
4. **Error case tests:** Verify each diagnostic is produced correctly

---

# Verilog Generation Changes

This section details how Verilog code generation must be updated to handle the
`sync(x)` and `async(x)` primitives.

## Overview

The key insight is that `sync(x)` **allocates hardware** (double-flop
synchronizer) while `async(x)` is a **type-level operation** that generates no
hardware.

## sync(x) Primitive

### Phase 1 Constraint: Bit Type Only

For Phase 1, `sync(x)` is only valid when `x` has type `Bit`. This ensures:
- Simplicity of implementation
- Safe CDC (multi-bit CDC requires gray coding or other patterns)
- Clear error message if misused

### Generated Verilog Structure

For a `sync(x)` expression targeting a specific result:

```virdant
wire btn : Bit on clock := sync(button_async)
```

Generates:

```verilog
reg \temp$sync$0 ;
reg btn;

always @(posedge clock) begin
    \temp$sync$0 <= button_async;
    btn <= \temp$sync$0 ;
end
```

### Name Mangling for Temporaries

The temporary registers use a **gensym'd** naming scheme:
- Pattern: `\temp$sync$N` where N is a counter
- The backslash prefix allows `$` in Verilog identifiers
- Counter is reset per-module

```rust
struct SyncInstance {
    /// Source expression (the argument to sync)
    source: Expr,
    /// The expected result location
    target: Expr,
    /// Gensym'd temporary register name
    temp_name: String,
    /// Counter for uniqueness
    counter: usize,
}
```

### Instance Collection During Conversion

During Verilog conversion, we must **collect** all `sync()` instances before
generating the module:

```rust
pub(crate) fn convert_moddef(
    builder: &mut Builder,
    symbol_id: SymbolId,
) -> Arc<VerilogModule> {
    let mut sync_instances: Vec<SyncInstance> = vec![];

    // First pass: collect all sync() calls
    for stmt in &stmts {
        collect_sync_instances(builder, stmt, &mut sync_instances);
    }

    // Second pass: generate module elements
    // ... existing conversion logic ...

    // Add sync temporaries as separate reg declarations
    for inst in &sync_instances {
        module.elements.push(Element::Reg(Reg {
            name: inst.temp_name.clone(),
            width: 1, // Bit type only in Phase 1
            expr: None,
        }));
    }
}
```

### Handling Multiple sync() Calls

When multiple `sync(x)` calls target the same source, we **reuse** the temporary:

```virdant
wire a : Bit on clock := sync(button)
wire b : Bit on clock := sync(button)  // Reuses same temp
```

Generated Verilog:

```verilog
reg \temp$sync$0 ;
reg a;
reg b;

always @(posedge clock) begin
    \temp$sync$0 <= button;
    a <= \temp$sync$0 ;
    b <= \temp$sync$0 ;  // Same temp, different target
end
```

Implementation:

```rust
fn find_or_create_sync_temp(
    &mut self,
    source_name: &str,
    instances: &mut Vec<SyncInstance>,
) -> String {
    // Check if we already have a sync for this source
    for inst in instances.iter() {
        if inst.source_name == source_name {
            return inst.temp_name.clone();
        }
    }

    // Create new sync instance
    let temp_name = format!("\\temp$sync${}", self.sync_counter);
    self.sync_counter += 1;

    instances.push(SyncInstance {
        source_name: source_name.to_owned(),
        temp_name: temp_name.clone(),
        // ... other fields ...
    });

    temp_name
}
```

## async(x) Primitive

### No Hardware Generation

The `async(x)` primitive is a **type-level operation only**. It:
- Generates **no Verilog code**
- Only affects type checking
- Is essentially erased during conversion

```virdant
wire async_data : Bit async := sync(clocked_data)
```

Generated Verilog (async primitive disappears):

```verilog
wire async_data = clocked_data;
```

### Warning for async(x)

Since `async(x)` discards timing information, generate a **compiler warning**:

```
warning: async() discards clock domain information
  --> example.vir:10:5
   |
10 |     wire async_data : Bit async := async(clocked_data)
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: The signal 'clocked_data' was synchronized to a clock, but is now marked async
   = help: This is safe if the signal crosses to another module in a different clock domain
```

## Integration with Existing Always Blocks

### Current Approach

The existing Verilog converter generates separate `always` blocks for each
driver. The `sync()` primitive must integrate with this approach.

### New Element Type: SyncReg

Add a new element type to track sync registers:

```rust
#[derive(Debug)]
pub enum Element {
    Wire(Wire),
    Reg(Reg),
    Assign(Assign),
    Always(Always),
    Initial(Initial),
    Submodule(Submodule),
    SyncReg(SyncReg),  // NEW
}

#[derive(Debug)]
pub struct SyncReg {
    /// The gensym'd temporary register name
    pub temp_name: String,
    /// The source signal being synchronized
    pub source: String,
    /// The clock signal
    pub clock: String,
    /// Width (1 for Bit in Phase 1)
    pub width: Width,
}
```

### Code Generation for SyncReg

When emitting the Verilog module:

```rust
fn emit_element(element: &Element, out: &mut impl Write) -> io::Result<()> {
    match element {
        Element::SyncReg(sync_reg) => {
            // Emit the temporary register declaration
            writeln!(out, "reg {} ;", sync_reg.temp_name)?;
        }
        Element::Always(always) => {
            // Existing logic, but may need to include sync logic
            // ...
        }
        // ... other cases ...
    }
}
```

### Combining with Existing Drivers

The sync registers are driven in a **separate always block** from other logic:

```verilog
// Sync register (separate always block)
always @(posedge clock) begin
    \temp$sync$0 <= button_async;
    btn <= \temp$sync$0 ;
end

// Other sequential logic (different always block)
always @(posedge clock) begin
    if (reset) begin
        state <= 2'b01;
    end else begin
        // ...
    end
end
```

This separation ensures:
- Clear synthesis constraints
- No interference with existing reset logic
- Easy to identify sync chains in generated code

## Changes to conversion.rs

### New AST Node Handling

Add handlers for the new primitive expressions:

```rust
fn convert_expr(builder: &mut Builder, node: &AstNode<'_>) -> Expr {
    match node.payload() {
        // ... existing cases ...

        AstNodePayload::ExprSync => {
            // This should not be reached during normal expression conversion
            // sync() is handled at the driver level
            panic!("sync() should be handled at driver level");
        }

        AstNodePayload::ExprAsync => {
            // async() just passes through its argument
            let inner = node.child(0);
            convert_expr(builder, &inner)
        }

        _ => { /* ... */ }
    }
}
```

### Driver-Level Handling

At the driver statement level, handle `sync()` specially:

```rust
fn convert_driver(
    builder: &mut Builder,
    node: &AstNode<'_>,
    module: &mut Module,
) {
    let target = node.child(0);  // LHS path
    let expr = node.child(1);   // RHS expression

    // Check if the RHS is a sync() primitive
    if is_sync_primitive(&expr) {
        let source = expr.child(0);
        let target_name = path_to_string(&target);
        let source_name = path_to_string(&source);
        let clock = get_clock_for_target(builder, &target);

        // Find or create sync temporary
        let temp_name = find_or_create_sync_temp(
            module,
            &source_name,
            &clock,
        );

        // Generate: target <= temp
        module.add_assignment(
            target_name,
            Expr::Reference(expr::Reference::Named(temp_name)),
        );

        return;
    }

    // ... existing driver conversion logic ...
}
```

### Helper Functions

```rust
fn is_sync_primitive(node: &AstNode<'_>) -> bool {
    matches!(node.payload(), AstNodePayload::ExprSync)
}

fn get_clock_for_target(
    builder: &mut Builder,
    target: &AstNode<'_>,
) -> String {
    // Look up the clock domain of the target component
    // Return the clock signal name
    let path = target.path().expect("target should have a path");
    let name = target.parsing().string(path);

    // Get component's clock domain from analysis
    let component = builder.get_component_analysis(...)
        .resolve(name.as_bstr())
        .expect("target should resolve");

    match component.clock() {
        Some(ClockDomain::OnClock(clock_id)) => {
            let clock_symbol = builder.get_symbol(*clock_id);
            clock_symbol.name().to_string()
        }
        _ => panic!("sync() target must be clocked"),
    }
}
```

## Handling No-Op Sync Warning

When `sync(x)` is called on a signal that is **already in the expected clock
domain**:

```rust
fn check_sync_necessity(
    builder: &mut Builder,
    source_clock: &ClockDomain,
    target_clock: &ClockDomain,
) {
    if source_clock == target_clock {
        builder.add_diagnostic(diagnostics::UnnecessarySync {
            region: node.region(),
            domain: source_clock.clone(),
        }.into());
    }
}
```

Diagnostic:

```rust
#[derive(Debug, Clone)]
pub struct UnnecessarySync {
    pub region: Region,
    pub domain: ClockDomain,
}

impl Diagnostic for UnnecessarySync {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Warning }

    fn message(&self) -> String {
        format!(
            "sync() on signal already in expected clock domain ({})",
            self.domain
        )
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }
}
```

## Files to Modify

1. `virdant/src/verilog.rs` - Add `SyncReg` element type
2. `virdant/src/verilog/conversion.rs` - Add sync/async handling
3. `virdant/src/syntax/payload.rs` - Add `ExprSync` and `ExprAsync` variants
4. `virdant/src/syntax/virdant.lalrpop` - Grammar for sync/async primitives
5. `virdant/src/diagnostics.rs` - New diagnostics for sync warnings

## Testing Strategy for Verilog Generation

1. **Unit tests for temp generation:** Test that sync produces correct temp names
2. **Unit tests for reuse:** Test that duplicate sync(x) calls reuse temps
3. **Integration tests:** Test generated Verilog matches expected output
4. **Warning tests:** Test that no-op sync produces warnings
5. **Error tests:** Test that sync on Word[n] produces errors (Phase 1)

## Example Transformation

### Input Virdant

```virdant
mod Debounce {
    incoming clock : Clock
    incoming reset : Bit
    incoming button : Bit async
    outgoing clean : Bit on clock

    wire btn : Bit on clock := sync(button)

    reg state : DebState on clock
    reg count : Word[4] on clock

    state <= when {
        case reset => #Idle
        else match state {
            case #Idle => when {
                case btn != clean => #Count
                else => #Idle
            }
            case #Count => when {
                case btn == clean => #Idle
                case count >= 15 => #Idle
                else => #Count
            }
        }
    }

    count <= when {
        case reset => 0
        case state == #Count => count + 1
        else => 0
    }

    clean <= when {
        case reset => false
        case state == #Count && count >= 15 => btn
        else => clean
    }
}
```

### Expected Output Verilog

```verilog
module Debounce (
    input clock,
    input reset,
    input button,
    output reg clean
);
    reg \temp$sync$0 ;
    reg btn;
    reg [1:0] state;
    reg [3:0] count;

    // Synchronizer for button input
    always @(posedge clock) begin
        \temp$sync$0 <= button;
        btn <= \temp$sync$0 ;
    end

    // State machine
    always @(posedge clock) begin
        if (reset) begin
            state <= 2'b01;
        end else begin
            case (state)
                2'b01: begin  // Idle
                    if (btn != clean)
                        state <= 2'b10;
                    else
                        state <= 2'b01;
                end
                2'b10: begin  // Count
                    if (btn == clean)
                        state <= 2'b01;
                    else if (count >= 15)
                        state <= 2'b01;
                    else
                        state <= 2'b10;
                end
            endcase
        end
    end

    // Counter
    always @(posedge clock) begin
        if (reset) begin
            count <= 0;
        end else if (state == 2'b10) begin
            count <= count + 1;
        end else begin
            count <= 0;
        end
    end

    // Output
    always @(posedge clock) begin
        if (reset) begin
            clean <= 0;
        end else if (state == 2'b10 && count >= 15) begin
            clean <= btn;
        end else begin
            clean <= clean;
        end
    end
endmodule
```

---

# Diagnostics

This section defines all new error messages and warnings for clock domain checking.

## Error Diagnostics

### UnknownClock

**When it triggers:** The identifier in an `on <clock>` clause does not resolve to any declared component.

**Example code:**
```virdant
mod Example {
    incoming clk : Clock
    wire data : Bit on unknown_clock
}
```

**Example error:**
```
error[E0701]: unknown clock 'unknown_clock' in 'on' clause
 --> example.vir:3:19
  |
3 |     wire data : Bit on unknown_clock
  |                   ^^^^^^^^^^^^^ unknown clock
  |
  = note: 'unknown_clock' was not declared as an incoming Clock port
  = help: declare 'incoming unknown_clock : Clock' or check the spelling
```

**Rust struct definition:**
```rust
/// Clock reference in 'on' clause does not exist.
#[derive(Debug, Clone)]
pub struct UnknownClock {
    /// Source location of the clock reference.
    pub region: Region,
    /// The name that could not be resolved.
    pub name: BString,
}

impl Diagnostic for UnknownClock {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("unknown clock '{}' in 'on' clause", self.name)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some(format!(
            "declare 'incoming {} : Clock' or check the spelling",
            self.name
        ))
    }
}
```

---

### OnClauseNotClock

**When it triggers:** The identifier in an `on <clock>` clause resolves to a component, but that component does not have type `Clock`.

**Example code:**
```virdant
mod Example {
    incoming some_signal : Bit
    wire data : Bit on some_signal
}
```

**Example error:**
```
error[E0702]: 'some_signal' is not a Clock type; cannot use in 'on' clause
 --> example.vir:3:19
  |
3 |     wire data : Bit on some_signal
  |                   ^^^^^^^^^^^ not a Clock
  |
  = note: 'some_signal' has type 'Bit', expected 'Clock'
  = help: only Clock-type signals can be used in 'on' clauses
```

**Rust struct definition:**
```rust
/// Clock reference in 'on' clause does not have Clock type.
#[derive(Debug, Clone)]
pub struct OnClauseNotClock {
    /// Source location of the clock reference.
    pub region: Region,
    /// The name that was resolved.
    pub name: BString,
}

impl Diagnostic for OnClauseNotClock {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("'{}' is not a Clock type; cannot use in 'on' clause", self.name)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some("only Clock-type signals can be used in 'on' clauses".to_string())
    }
}
```

---

### ClockNotInput

**When it triggers:** A signal is declared with type `Clock` but is not an incoming port.

**Example code:**
```virdant
mod Example {
    outgoing my_clock : Clock
}
```

**Example error:**
```
error[E0703]: Clock 'my_clock' must be an incoming port
 --> example.vir:2:5
  |
2 |     outgoing my_clock : Clock
  |     ^^^^^^^^^^^^^^^^^^^^^^^^ Clock must be incoming
  |
  = note: Clock signals can only be module inputs, not outputs or internal wires
```

**Rust struct definition:**
```rust
/// Clock signal is not an incoming port.
#[derive(Debug, Clone)]
pub struct ClockNotInput {
    /// Source location of the Clock declaration.
    pub region: Region,
    /// The name of the Clock signal.
    pub name: BString,
}

impl Diagnostic for ClockNotInput {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("Clock '{}' must be an incoming port", self.name)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some("Clock signals can only be module inputs".to_string())
    }
}
```

---

### AsyncReg

**When it triggers:** A register is declared with the `async` keyword.

**Example code:**
```virdant
mod Example {
    incoming clock : Clock
    reg state : Bit async
}
```

**Example error:**
```
error[E0704]: register cannot be async; registers must be clocked
 --> example.vir:3:5
  |
3 |     reg state : Bit async
  |     ^^^^^^^^^^^^^^^^^^^ async not allowed on reg
  |
  = note: registers represent state that updates on clock edges
  = help: use 'reg state : Bit on clock' instead
```

**Rust struct definition:**
```rust
/// Register declared with async keyword.
#[derive(Debug, Clone)]
pub struct AsyncReg {
    /// Source location of the reg declaration.
    pub region: Region,
}

impl Diagnostic for AsyncReg {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        "register cannot be async; registers must be clocked".to_string()
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some("use 'reg name : Type on clock' instead".to_string())
    }
}
```

---

### ClockTypeDomain

**When it triggers:** A Clock or Reset type is declared with an `async` or `on <clock>` annotation.

**Example code:**
```virdant
mod Example {
    incoming clk : Clock async
    incoming rst : Reset on clk
}
```

**Example error:**
```
error[E0705]: Clock type cannot have clock domain annotation
 --> example.vir:2:5
  |
2 |     incoming clk : Clock async
  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^ unexpected 'async'
  |
  = note: Clock and Reset types represent timing signals, not data

error[E0705]: Reset type cannot have clock domain annotation
 --> example.vir:3:5
  |
3 |     incoming rst : Reset on clk
  |     ^^^^^^^^^^^^^^^^^^^^^^^^^ unexpected 'on' clause
  |
  = note: Clock and Reset types represent timing signals, not data
```

**Rust struct definition:**
```rust
/// Clock or Reset type with domain annotation.
#[derive(Debug, Clone)]
pub struct ClockTypeDomain {
    /// Source location of the declaration.
    pub region: Region,
    /// The type (Clock or Reset).
    pub typ: Type,
    /// What kind of annotation was present.
    pub annotation_kind: DomainAnnotationKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DomainAnnotationKind {
    Async,
    OnClause,
}

impl Diagnostic for ClockTypeDomain {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("{} type cannot have clock domain annotation", self.typ)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some("Clock and Reset types represent timing signals, not data".to_string())
    }
}
```

---

### ComplexClockRef

**When it triggers:** The expression in an `on <expr>` clause is not a simple identifier reference.

**Example code:**
```virdant
mod Example {
    incoming clocks : Word[2]
    wire data : Bit on clocks[0]
}
```

**Example error:**
```
error[E0706]: complex expression in 'on' clause; expected simple identifier
 --> example.vir:3:19
  |
3 |     wire data : Bit on clocks[0]
  |                   ^^^^^^^^^^^^ too complex
  |
  = note: 'on' clause must be a simple clock identifier, not an expression
  = help: use 'wire data : Bit on my_clock' where 'my_clock' is declared as 'incoming my_clock : Clock'
```

**Rust struct definition:**
```rust
/// Complex expression used in 'on' clause.
#[derive(Debug, Clone)]
pub struct ComplexClockRef {
    /// Source location of the expression.
    pub region: Region,
}

impl Diagnostic for ComplexClockRef {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        "complex expression in 'on' clause; expected simple identifier".to_string()
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some("use 'on my_clock' where 'my_clock' is declared as 'incoming my_clock : Clock'".to_string())
    }
}
```

---

### DomainCrossing

**When it triggers:** A binary operation or driver combines values from different clock domains without explicit synchronization.

**Example code:**
```virdant
mod Example {
    incoming clk_a : Clock
    incoming clk_b : Clock
    incoming data_a : Bit on clk_a
    incoming data_b : Bit on clk_b
    wire combined : Bit := data_a && data_b
}
```

**Example error:**
```
error[E0707]: clock domain crossing: cannot combine values from different domains
 --> example.vir:6:23
  |
6 |     wire combined : Bit := data_a && data_b
  |                       ^^^^^^^^^^^^^^^^^^^^^
  |
  = note: 'data_a' is in clock domain 'clk_a'
  = note: 'data_b' is in clock domain 'clk_b'
  = help: use sync() to cross clock domains
  = help: sync(data_a) will synchronize 'data_a' to the expected clock domain
```

**Rust struct definition:**
```rust
/// Combining values from different clock domains.
#[derive(Debug, Clone)]
pub struct DomainCrossing {
    /// Source location of the problematic expression.
    pub region: Region,
    /// Location and domain of the left operand.
    pub left_region: Region,
    pub left_domain: ClockDomain,
    /// Location and domain of the right operand.
    pub right_region: Region,
    pub right_domain: ClockDomain,
}

impl Diagnostic for DomainCrossing {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        "clock domain crossing: cannot combine values from different domains".to_string()
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn notes(&self) -> Vec<String> {
        vec![
            format!("'left operand' is in clock domain '{}'", self.left_domain),
            format!("'right operand' is in clock domain '{}'", self.right_domain),
        ]
    }

    fn hint(&self) -> Option<String> {
        Some("use sync() to cross clock domains".to_string())
    }
}
```

---

### UninferrableClock

**When it triggers:** A signal's clock domain cannot be inferred from context and lacks an explicit annotation.

**Example code:**
```virdant
mod Example {
    incoming clk : Clock
    wire standalone : Bit
}
```

**Example error:**
```
error[E0708]: cannot infer clock domain for 'standalone'
 --> example.vir:3:5
  |
3 |     wire standalone : Bit
  |     ^^^^^^^^^^^^^^^^^^^^ no driver to infer from
  |
  = note: add an explicit annotation: 'wire standalone : Bit on clk'
  = help: or 'wire standalone : Bit async' for asynchronous signals
```

**Rust struct definition:**
```rust
/// Clock domain could not be inferred.
#[derive(Debug, Clone)]
pub struct UninferrableClock {
    /// Source location of the declaration.
    pub region: Region,
    /// The name of the signal.
    pub name: BString,
}

impl Diagnostic for UninferrableClock {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }

    fn message(&self) -> String {
        format!("cannot infer clock domain for '{}'", self.name)
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some("add an explicit annotation: 'on <clock>' or 'async'".to_string())
    }
}
```

---

## Warning Diagnostics

### NoOpSync

**When it triggers:** `sync(x)` is called on a signal that is already in the expected clock domain.

**Example code:**
```virdant
mod Example {
    incoming clk : Clock
    incoming data : Bit on clk
    wire synced : Bit on clk := sync(data)
}
```

**Example warning:**
```
warning[W0751]: sync() on signal already in expected clock domain
 --> example.vir:4:27
  |
4 |     wire synced : Bit on clk := sync(data)
  |                           ^^^^^^^^^^^ unnecessary
  |
  = note: 'data' is already on 'clk', same as expected domain
  = help: remove the sync() call or verify this was intentional
```

**Rust struct definition:**
```rust
/// sync() called on signal already in expected domain.
#[derive(Debug, Clone)]
pub struct NoOpSync {
    /// Source location of the sync() call.
    pub region: Region,
    /// The clock domain the signal is already in.
    pub domain: ClockDomain,
}

impl Diagnostic for NoOpSync {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Warning }

    fn message(&self) -> String {
        format!(
            "sync() on signal already in expected clock domain ({})",
            self.domain
        )
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some("remove the sync() call or verify this was intentional".to_string())
    }
}
```

---

### AsyncToClockedPort

**When it triggers:** An async signal is connected to a clocked port without using `sync()`.

**Example code:**
```virdant
mod Child {
    incoming clk : Clock
    incoming data : Bit on clk
}

mod Parent {
    incoming clk : Clock
    incoming async_data : Bit async
    mod child of Child
    child.clk := clk
    child.data := async_data  // Warning: missing sync
}
```

**Example warning:**
```
warning[W0752]: connecting async signal to clocked port without sync()
 --> example.vir:11:5
  |
11|     child.data := async_data
  |     ^^^^^^^^^^^^^^^^^^^^^^^^^
  |
  = note: 'async_data' is async, but 'child.data' expects clock domain 'clk'
  = help: use 'child.data := sync(async_data)'
```

**Rust struct definition:**
```rust
/// Async signal connected to clocked port without sync.
#[derive(Debug, Clone)]
pub struct AsyncToClockedPort {
    /// Source location of the connection.
    pub region: Region,
    /// Name of the port being connected.
    pub port_name: BString,
    /// Name of the submodule.
    pub submod_name: BString,
    /// Expected clock domain.
    pub expected_domain: ClockDomain,
}

impl Diagnostic for AsyncToClockedPort {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Warning }

    fn message(&self) -> String {
        format!(
            "connecting async signal to clocked port '{}' of '{}' without sync()",
            self.port_name, self.submod_name
        )
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn hint(&self) -> Option<String> {
        Some(format!(
            "use '{}.{} := sync(<signal>)'",
            self.submod_name, self.port_name
        ))
    }
}
```

---

### MultiBitSync

**When it triggers:** `sync(x)` is called on a multi-bit value (Phase 1 restriction).

**Example code:**
```virdant
mod Example {
    incoming clk : Clock
    incoming data : Word[8] async
    wire synced : Word[8] on clk := sync(data)
}
```

**Example warning:**
```
warning[W0753]: sync() on multi-bit value is unsafe for clock domain crossing
 --> example.vir:4:33
  |
4 |     wire synced : Word[8] on clk := sync(data)
  |                                 ^^^^^^^^^^^^^
  |
  = note: 'sync()' on Word[8] may cause metastability on individual bits
  = note: bits may arrive at different times, corrupting the value
  = help: use gray coding for counters, or handshake/FIFO for data
  = help: Phase 1 supports sync() only for Bit type
```

**Rust struct definition:**
```rust
/// sync() on multi-bit value (unsafe CDC).
#[derive(Debug, Clone)]
pub struct MultiBitSync {
    /// Source location of the sync() call.
    pub region: Region,
    /// The type being synchronized.
    pub typ: Type,
}

impl Diagnostic for MultiBitSync {
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Warning }

    fn message(&self) -> String {
        format!(
            "sync() on multi-bit value {} is unsafe for clock domain crossing",
            self.typ
        )
    }

    fn source(&self) -> Option<Region> { Some(self.region.clone()) }

    fn notes(&self) -> Vec<String> {
        vec![
            "sync() on Word[n] may cause metastability on individual bits".to_string(),
            "bits may arrive at different times, corrupting the value".to_string(),
        ]
    }

    fn hint(&self) -> Option<String> {
        Some("use gray coding for counters, or handshake/FIFO for data".to_string())
    }
}
```

---

## Summary Table

| Code | Name | Level | Description |
|------|------|-------|-------------|
| E0701 | UnknownClock | Error | Clock reference not found |
| E0702 | OnClauseNotClock | Error | Referenced signal is not type Clock |
| E0703 | ClockNotInput | Error | Clock is not an incoming port |
| E0704 | AsyncReg | Error | Register cannot be async |
| E0705 | ClockTypeDomain | Error | Clock/Reset cannot have domain annotation |
| E0706 | ComplexClockRef | Error | Complex expression in 'on' clause |
| E0707 | DomainCrossing | Error | Combining different clock domains |
| E0708 | UninferrableClock | Error | Cannot infer clock domain |
| W0751 | NoOpSync | Warning | sync() on signal already in target domain |
| W0752 | AsyncToClockedPort | Warning | Async to clocked port without sync |
| W0753 | MultiBitSync | Warning | sync() on multi-bit value |

---

## Implementation in diagnostics.rs

All diagnostics should be added to `virdant/src/diagnostics.rs`:

```rust
// Add to the existing diagnostics module

// Error codes E07xx for clock domain errors
pub use self::clock_domain::*;

mod clock_domain {
    use super::*;
    use crate::types::ClockDomain;
    use crate::types::Type;

    #[derive(Debug, Clone)]
    pub struct UnknownClock {
        pub region: Region,
        pub name: BString,
    }

    #[derive(Debug, Clone)]
    pub struct OnClauseNotClock {
        pub region: Region,
        pub name: BString,
    }

    #[derive(Debug, Clone)]
    pub struct ClockNotInput {
        pub region: Region,
        pub name: BString,
    }

    #[derive(Debug, Clone)]
    pub struct AsyncReg {
        pub region: Region,
    }

    #[derive(Debug, Clone)]
    pub struct ClockTypeDomain {
        pub region: Region,
        pub typ: Type,
        pub annotation_kind: DomainAnnotationKind,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum DomainAnnotationKind {
        Async,
        OnClause,
    }

    #[derive(Debug, Clone)]
    pub struct ComplexClockRef {
        pub region: Region,
    }

    #[derive(Debug, Clone)]
    pub struct DomainCrossing {
        pub region: Region,
        pub left_region: Region,
        pub left_domain: ClockDomain,
        pub right_region: Region,
        pub right_domain: ClockDomain,
    }

    #[derive(Debug, Clone)]
    pub struct UninferrableClock {
        pub region: Region,
        pub name: BString,
    }

    // Warning codes W07xx for clock domain warnings

    #[derive(Debug, Clone)]
    pub struct NoOpSync {
        pub region: Region,
        pub domain: ClockDomain,
    }

    #[derive(Debug, Clone)]
    pub struct AsyncToClockedPort {
        pub region: Region,
        pub port_name: BString,
        pub submod_name: BString,
        pub expected_domain: ClockDomain,
    }

    #[derive(Debug, Clone)]
    pub struct MultiBitSync {
        pub region: Region,
        pub typ: Type,
    }

    // Implement Diagnostic trait for all...
    // (Implementations shown above)
}
```

---

## Testing Diagnostics

Each diagnostic should have test cases in `tests/errors/` and `tests/warnings/`:

```
tests/
  errors/
    clock_domain/
      unknown_clock.vir
      on_clause_not_clock.vir
      clock_not_input.vir
      async_reg.vir
      clock_type_domain.vir
      complex_clock_ref.vir
      domain_crossing.vir
      uninferrable_clock.vir
  warnings/
    clock_domain/
      no_op_sync.vir
      async_to_clocked_port.vir
      multi_bit_sync.vir
```

Each test file should include a comment with the expected error code and message pattern.

---

# Test Plan

This section covers the testing strategy for clock domain tracking and synchronization primitives.

## Test Directory Structure

```
tests/
  clock_domain/           # New top-level directory for clock domain tests
    unit/                 # Unit tests
      clock_domain.rs     # ClockDomain enum tests
      typed.rs           # Typed struct tests
    parser/               # Parser tests
      async_annotation.vir
      on_clause.vir
      sync_expr.vir
      async_expr.vir
      errors/
        async_and_on.vir
        async_on_clock.vir
        on_non_clock.vir
    typecheck/            # Type checking tests
      domain_inference.vir
      crossing_errors.vir
      module_boundary.vir
    verilog/              # Verilog generation tests
      sync_double_flop.vir
      sync_temp_reuse.vir
      async_no_op.vir
    integration/          # Full integration tests
      debounce_sync.vir
      multi_clock.vir
      ext_module.vir
```

---

## 1. Unit Tests for ClockDomain Enum

**File:** `tests/clock_domain/unit/clock_domain.rs`

### Test Categories

#### 1.1 Equality and Comparison

```rust
#[cfg(test)]
mod clock_domain_tests {
    use virdant::types::ClockDomain;
    use virdant::analysis::symbols::SymbolId;

    #[test]
    fn async_equals_async() {
        assert_eq!(ClockDomain::Async, ClockDomain::Async);
    }

    #[test]
    fn on_clock_equality() {
        let id = SymbolId::new(42);
        assert_eq!(
            ClockDomain::OnClock(id),
            ClockDomain::OnClock(id)
        );
    }

    #[test]
    fn on_clock_different_ids_not_equal() {
        let id1 = SymbolId::new(1);
        let id2 = SymbolId::new(2);
        assert_ne!(
            ClockDomain::OnClock(id1),
            ClockDomain::OnClock(id2)
        );
    }

    #[test]
    fn async_not_equal_on_clock() {
        let id = SymbolId::new(1);
        assert_ne!(ClockDomain::Async, ClockDomain::OnClock(id));
    }

    #[test]
    fn unknown_not_equal_anything() {
        let id = SymbolId::new(1);
        assert_ne!(ClockDomain::Unknown, ClockDomain::Unknown);
        assert_ne!(ClockDomain::Unknown, ClockDomain::Async);
        assert_ne!(ClockDomain::Unknown, ClockDomain::OnClock(id));
    }
}
```

**Expected:** All 6 tests pass.

#### 1.2 Display Formatting

```rust
#[test]
fn display_async() {
    assert_eq!(format!("{}", ClockDomain::Async), "async");
}

#[test]
fn display_on_clock() {
    let id = SymbolId::new(42);
    let domain = ClockDomain::OnClock(id);
    // Display shows symbol ID for debugging
    assert!(format!("{}", domain).contains("clock"));
}

#[test]
fn display_unknown() {
    assert_eq!(
        format!("{}", ClockDomain::Unknown),
        "<unknown domain>"
    );
}
```

**Expected:** All 3 tests pass.

#### 1.3 Domain Matching

```rust
#[test]
fn same_clock_matches() {
    let id = SymbolId::new(1);
    let d1 = ClockDomain::OnClock(id);
    let d2 = ClockDomain::OnClock(id);
    assert!(d1.matches(&d2));
}

#[test]
fn different_clock_no_match() {
    let d1 = ClockDomain::OnClock(SymbolId::new(1));
    let d2 = ClockDomain::OnClock(SymbolId::new(2));
    assert!(!d1.matches(&d2));
}

#[test]
fn async_no_match_with_clock() {
    let d1 = ClockDomain::Async;
    let d2 = ClockDomain::OnClock(SymbolId::new(1));
    assert!(!d1.matches(&d2));
}
```

**Expected:** All 3 tests pass.

---

## 2. Parser Tests for New Syntax

### 2.1 Async Annotation

**File:** `tests/clock_domain/parser/async_annotation.vir`

```virdant
// Test: async annotation on various component kinds

mod TestAsync {
    incoming clk : Clock
    incoming button : Bit async
    outgoing status : Bit async
    wire temp : Bit async
}
```

**Expected:** Parses successfully, no errors.

**Test case structure:**
```rust
#[test]
fn parse_async_annotation() {
    let source = include_str!("parser/async_annotation.vir");
    let result = parse_package(source);
    assert!(result.is_ok());

    // Verify async annotations are captured
    let module = result.unwrap().items()[0];
    let button = find_component(module, "button");
    assert_eq!(button.clock_domain, Some(ClockDomain::Async));
}
```

### 2.2 On Clause

**File:** `tests/clock_domain/parser/on_clause.vir`

```virdant
// Test: on clause with clock reference

mod TestOnClause {
    incoming clk : Clock
    incoming pll_clk : Clock

    incoming data : Bit on clk
    reg state : Bit on clk
    wire fast : Bit on pll_clk
}
```

**Expected:** Parses successfully, all clock domains resolved.

### 2.3 sync(x) Expression

**File:** `tests/clock_domain/parser/sync_expr.vir`

```virdant
// Test: sync() primitive expression

mod TestSync {
    incoming clk : Clock
    incoming async_input : Bit async

    wire synced : Bit on clk {
        it := sync(async_input)
    }
}
```

**Expected:** Parses successfully, `sync()` recognized as primitive.

### 2.4 async(x) Expression

**File:** `tests/clock_domain/parser/async_expr.vir`

```virdant
// Test: async() primitive expression

mod TestAsyncExpr {
    incoming clk : Clock
    incoming data : Bit on clk

    wire async_data : Bit async {
        it := async(data)
    }
}
```

**Expected:** Parses successfully.

### 2.5 Error Cases

#### Error: Both async and on specified

**File:** `tests/clock_domain/parser/errors/async_and_on.vir`

```virdant
// Error: Cannot specify both async and on

mod TestError {
    incoming clk : Clock
    incoming x : Bit async on clk  // ERROR
}
```

**Expected error:** E0705 or similar, "Cannot specify both 'async' and 'on'"

**Test case:**
```rust
#[test]
fn reject_async_and_on() {
    let source = include_str!("parser/errors/async_and_on.vir");
    let result = parse_package(source);
    assert!(result.is_err());
    assert_error_code(result, "E0705");
}
```

#### Error: async on Clock type

**File:** `tests/clock_domain/parser/errors/async_on_clock.vir`

```virdant
// Error: Clock cannot have domain annotation

mod TestError {
    incoming clk : Clock async  // ERROR
}
```

**Expected error:** E0705, "Clock type cannot have clock domain annotation"

#### Error: on references non-clock

**File:** `tests/clock_domain/parser/errors/on_non_clock.vir`

```virdant
// Error: 'data' is not a Clock type

mod TestError {
    incoming clk : Clock
    incoming data : Bit
    wire x : Bit on data  // ERROR
}
```

**Expected error:** E0702, "'data' is not a Clock type"

---

## 3. Type Checking Tests

### 3.1 Domain Inference from Drivers

**File:** `tests/clock_domain/typecheck/domain_inference.vir`

```virdant
// Test: Clock domain inference from driver context

mod TestInference {
    incoming clk : Clock
    incoming button : Bit async

    wire synced : Bit on clk := sync(button)

    // 'temp' clock domain inferred from RHS
    wire temp : Bit
    temp := synced && true

    // Verify temp is 'on clk'
}
```

**Expected:** All domains successfully inferred.

**Test case structure:**
```rust
#[test]
fn infer_domain_from_driver() {
    let diagnostics = typecheck_file("typecheck/domain_inference.vir");
    assert!(diagnostics.is_empty());

    // Verify 'temp' has domain 'on clk'
    let temp_domain = get_component_domain("TestInference", "temp");
    assert!(matches!(temp_domain, Some(ClockDomain::OnClock(_))));
}
```

### 3.2 Domain Crossing Errors

**File:** `tests/clock_domain/typecheck/crossing_errors.vir`

```virdant
// Test: Errors when crossing clock domains

mod TestErrors {
    incoming clk_a : Clock
    incoming clk_b : Clock

    wire a : Bit on clk_a := true
    wire b : Bit on clk_b := true

    // ERROR: Combining different domains
    wire c : Bit
    c := a && b
}
```

**Expected error:** E0707, "Cannot combine values from different clock domains"

**Test case:**
```rust
#[test]
fn detect_domain_crossing() {
    let diagnostics = typecheck_file("typecheck/crossing_errors.vir");
    assert_error_count(diagnostics, 1);
    assert_error_code(diagnostics[0], "E0707");
    assert_error_message_contains(diagnostics[0], "different clock domains");
}
```

### 3.3 Module Boundary Propagation

**File:** `tests/clock_domain/typecheck/module_boundary.vir`

```virdant
// Test: Clock domain propagation across module boundaries

mod Child {
    incoming clk : Clock
    incoming data : Bit on clk
    outgoing result : Bit on clk

    result := data
}

mod Parent {
    incoming clk : Clock
    incoming input_data : Bit on clk

    mod child of Child
    child.clk := clk
    child.data := input_data

    wire output : Bit on clk
    output := child.result
}
```

**Expected:** No errors, domains propagate correctly.

**Test case:**
```rust
#[test]
fn propagate_domain_across_modules() {
    let diagnostics = typecheck_file("typecheck/module_boundary.vir");
    assert!(diagnostics.is_empty());

    // Verify child.data has domain 'on clk' in Parent's context
    let child_data_domain = get_submodule_port_domain(
        "Parent", "child", "data"
    );
    assert!(matches!(child_data_domain, Some(ClockDomain::OnClock(_))));
}
```

---

## 4. Verilog Generation Tests

### 4.1 sync() Generates Double-Flop

**File:** `tests/clock_domain/verilog/sync_double_flop.vir`

```virdant
mod TestSync {
    incoming clk : Clock
    incoming async_input : Bit async

    wire synced : Bit on clk {
        it := sync(async_input)
    }
}
```

**Expected Verilog:**
```verilog
module TestSync (
    input clk,
    input async_input,
    output synced
);
    reg \temp$sync$0 ;
    reg synced;

    always @(posedge clk) begin
        \temp$sync$0 <= async_input;
        synced <= \temp$sync$0 ;
    end
endmodule
```

**Test case structure:**
```rust
#[test]
fn sync_generates_double_flop() {
    let verilog = compile_to_verilog("verilog/sync_double_flop.vir");

    // Verify structure
    assert!(verilog.contains("reg \\"));
    assert!(verilog.contains("temp$sync$"));
    assert!(verilog.contains("always @(posedge clk)"));

    // Count temporary registers (should be exactly 1)
    let temp_count = count_sync_temps(&verilog);
    assert_eq!(temp_count, 1);
}
```

### 4.2 Multiple sync() Calls Reuse Temps

**File:** `tests/clock_domain/verilog/sync_temp_reuse.vir`

```virdant
mod TestSyncReuse {
    incoming clk : Clock
    incoming button : Bit async

    wire a : Bit on clk := sync(button)
    wire b : Bit on clk := sync(button)  // Should reuse temp
}
```

**Expected Verilog:**
```verilog
module TestSyncReuse (
    input clk,
    input button,
    output a,
    output b
);
    reg \temp$sync$0 ;
    reg a;
    reg b;

    always @(posedge clk) begin
        \temp$sync$0 <= button;
        a <= \temp$sync$0 ;
        b <= \temp$sync$0 ;
    end
endmodule
```

**Test case:**
```rust
#[test]
fn sync_reuses_temp_for_same_source() {
    let verilog = compile_to_verilog("verilog/sync_temp_reuse.vir");

    // Should only have ONE temp register
    let temp_count = count_sync_temps(&verilog);
    assert_eq!(temp_count, 1);

    // Both 'a' and 'b' should be driven from same temp
    assert!(verilog.contains("a <= \\"));
    assert!(verilog.contains("b <= \\"));
}
```

### 4.3 async() Generates No Code

**File:** `tests/clock_domain/verilog/async_no_op.vir`

```virdant
mod TestAsyncOp {
    incoming clk : Clock
    incoming data : Bit on clk

    wire async_data : Bit async {
        it := async(data)
    }
}
```

**Expected Verilog:**
```verilog
module TestAsyncOp (
    input clk,
    input data,
    output async_data
);
    wire async_data = data;  // No double-flop, direct connection
endmodule
```

**Test case:**
```rust
#[test]
fn async_generates_no_hardware() {
    let verilog = compile_to_verilog("verilog/async_no_op.vir");

    // Should NOT contain any sync temporaries
    assert!(!verilog.contains("temp$sync$"));

    // Should be direct connection
    assert!(verilog.contains("assign async_data = data")
        || verilog.contains("wire async_data = data"));
}
```

---

## 5. Integration Tests

### 5.1 Full Debounce Example with sync()

**File:** `tests/clock_domain/integration/debounce_sync.vir`

```virdant
// Integration test: Complete debounce module using sync()

enum type DebState {
    Idle = 0b01w2
    Count = 0b10
}

mod Debounce {
    incoming clock : Clock
    incoming reset : Bit
    incoming button : Bit async
    outgoing clean : Bit on clock

    // Synchronize the asynchronous button input
    wire btn : Bit on clock := sync(button)

    reg state : DebState on clock
    reg count : Word[4] on clock
    reg clean : Bit on clock

    state <= when {
        case reset => #Idle
        else match state {
            case #Idle => when {
                case btn != clean => #Count
                else => #Idle
            }
            case #Count => when {
                case btn == clean => #Idle
                case count >= 15 => #Idle
                else => #Count
            }
        }
    }

    count <= when {
        case reset => 0
        case state == #Count => count + 1
        else => 0
    }

    clean <= when {
        case reset => false
        case state == #Count && count >= 15 => btn
        else => clean
    }
}
```

**Expected:** Compiles successfully, generates valid Verilog.

**Test case:**
```rust
#[test]
fn integration_debounce() {
    let verilog = compile_to_verilog("integration/debounce_sync.vir");

    // Verify sync() generated double-flop for 'button'
    assert!(verilog.contains("temp$sync$"));

    // Verify the module structure is correct
    assert!(verilog.contains("module Debounce"));
    assert!(verilog.contains("input clock"));
    assert!(verilog.contains("input button"));
    assert!(verilog.contains("output clean"));

    // Simulate if possible (future work)
}
```

### 5.2 Multi-Clock Crossing

**File:** `tests/clock_domain/integration/multi_clock.vir`

```virdant
// Integration test: Crossing between two clock domains

mod ClockCrossing {
    incoming clk_a : Clock
    incoming clk_b : Clock
    incoming data_a : Bit on clk_a

    // Step 1: Convert to async
    wire async_data : Bit async := async(data_a)

    // Step 2: Sync to target clock
    wire data_b : Bit on clk_b := sync(async_data)
}
```

**Expected:** Compiles successfully with warnings.

**Test case:**
```rust
#[test]
fn integration_multi_clock_crossing() {
    let diagnostics = typecheck_file("integration/multi_clock.vir");

    // Should have warnings about async() discarding timing info
    assert!(diagnostics.iter().any(|d| d.code() == "W07xx"));

    let verilog = compile_to_verilog("integration/multi_clock.vir");

    // Verify sync() creates proper CDC
    assert!(verilog.contains("clk_b"));
    assert!(verilog.contains("temp$sync$"));
}
```

### 5.3 External Module Connections

**File:** `tests/clock_domain/integration/ext_module.vir`

```virdant
// Integration test: External module with explicit clock domains

ext mod ExternalUart {
    incoming clk : Clock
    incoming tx_data : Bit async
    outgoing rx_data : Bit on clk
}

mod Top {
    incoming clk : Clock
    incoming tx : Bit async

    mod uart of ExternalUart
    uart.clk := clk
    uart.tx_data := tx

    wire rx : Bit on clk
    rx := uart.rx_data
}
```

**Expected:** Compiles successfully.

**Test case:**
```rust
#[test]
fn integration_external_module() {
    let verilog = compile_to_verilog("integration/ext_module.vir");

    // Verify external module instantiation
    assert!(verilog.contains("ExternalUart"));
    assert!(verilog.contains("uart"));

    // Verify port connections
    assert!(verilog.contains(".clk(clk)"));
    assert!(verilog.contains(".tx_data(tx)"));
}
```

---

## 6. Running Tests

### Using Make

```bash
# Run all tests
make test

# Run only clock domain tests
cargo test clock_domain

# Run a specific test file
cargo test --test clock_domain_unit

# Run with verbose output
cargo test -- --nocapture clock_domain
```

### Expected Test Count Increase

| Category | New Tests | Files |
|----------|-----------|-------|
| ClockDomain unit tests | 12 | 1 |
| Parser tests | 8 + 3 errors | 7 |
| Typecheck tests | 6 | 3 |
| Verilog tests | 5 | 3 |
| Integration tests | 4 | 3 |
| **Total** | **~41 new tests** | **17 files** |

### Test File Naming Convention

- Unit tests: `tests/clock_domain/unit/<feature>.rs`
- Parser tests: `tests/clock_domain/parser/<feature>.vir`
- Parser error tests: `tests/clock_domain/parser/errors/<error_type>.vir`
- Typecheck tests: `tests/clock_domain/typecheck/<feature>.vir`
- Verilog tests: `tests/clock_domain/verilog/<feature>.vir`
- Integration tests: `tests/clock_domain/integration/<feature>.vir`

---

## 7. Pass/Fail Criteria

### Parser Tests

| Test | Pass Condition |
|------|----------------|
| `async_annotation.vir` | Parses with no errors |
| `on_clause.vir` | Parses with no errors, clock refs resolved |
| `sync_expr.vir` | Parses, `sync()` recognized |
| `async_expr.vir` | Parses, `async()` recognized |
| `async_and_on.vir` | Error E0705 |
| `async_on_clock.vir` | Error E0705 |
| `on_non_clock.vir` | Error E0702 |

### Typecheck Tests

| Test | Pass Condition |
|------|----------------|
| `domain_inference.vir` | No errors, domains inferred correctly |
| `crossing_errors.vir` | Error E0707 (domain mismatch) |
| `module_boundary.vir` | No errors, domains propagate across modules |

### Verilog Tests

| Test | Pass Condition |
|------|----------------|
| `sync_double_flop.vir` | Contains `temp$sync$`, double-flop structure |
| `sync_temp_reuse.vir` | Exactly 1 temp register for 2 sync() calls |
| `async_no_op.vir` | No `temp$sync$`, direct connection |

### Integration Tests

| Test | Pass Condition |
|------|----------------|
| `debounce_sync.vir` | Compiles, valid Verilog output, correct module structure |
| `multi_clock.vir` | Compiles with warnings, correct CDC structure |
| `ext_module.vir` | Compiles, correct instantiation |

---

## 8. Continuous Integration

Add to `.github/workflows/test.yml`:

```yaml
- name: Run clock domain tests
  run: cargo test clock_domain --verbose

- name: Verify no regressions
  run: cargo test --all
```

Ensure CI fails on any test failure or new warnings.

---

# Implementation Order

This section outlines the implementation order with dependencies, timeline, and risk mitigation.

## Phase Breakdown

### Phase 1: Parsing (Estimated: 2-3 days)

**Goal:** Parse all new syntax and produce valid AST nodes.

**Tasks:**

1. **Add ClockDomain enum** (`types/typ.rs`)
   - Define `ClockDomain::Async`, `ClockDomain::OnClock(Identifier)`, `ClockDomain::Unknown`
   - Implement `PartialEq`, `Eq`, `Hash`, `Clone`, `Debug`
   - Implement `Display` for error messages

2. **Update grammar** (`syntax/virdant.lalrpop`)
   - Add `async` keyword to lexer (reserved word)
   - Extend `ModDefStmtComponent` to accept optional `ClockDomain` annotation
   - Add `ClockDomain` production: `"async" | "on" Expr | /* empty */`
   - Add `ExprSync` and `ExprAsync` to `Expr` production
   - Ensure grammar rejects `async on` combination

3. **Add AST node payloads** (`syntax/payload.rs`)
   - Add `ExprSync` and `ExprAsync` variants to `AstNodePayload`
   - No additional fields needed (single child: the argument expression)

4. **Write parser tests** (`tests/clock_domain/parser/`)
   - Test valid `async` annotation
   - Test valid `on <clock>` clause
   - Test valid `sync(x)` expression
   - Test valid `async(x)` expression
   - Test error: `async on` combination
   - Test error: `async` on Clock type

**Deliverables:**
- Grammar changes compile without conflicts
- All parser tests pass
- New AST nodes generated correctly

**Dependencies:** None (can start immediately)

---

### Phase 2: Analysis (Estimated: 3-4 days)

**Goal:** Populate clock domain information for all components.

**Tasks:**

1. **Extend Component struct** (`analysis/component.rs`)
   - Add `clock: Option<ClockDomain>` field
   - Add accessor methods: `clock()`, `is_async()`, `is_clocked()`
   - Update all `Component` construction sites

2. **Add clock discovery** (`analysis/component.rs`)
   - Add `clocks_in_scope: IndexMap<BString, SymbolId>` to `ComponentAnalysis`
   - Implement `discover_clocks()` method
   - Validate that Clock types are incoming ports only

3. **Implement clock domain inference** (`analysis/component.rs`)
   - `infer_component_clock()` - parse explicit `async` or `on` annotations
   - `resolve_on_clause()` - resolve clock reference to SymbolId
   - Validate clock references are Clock type

4. **Extend Port struct** (`analysis/ports.rs`)
   - Add `clock: Option<ClockDomain>` field
   - Update `build_ports_of()` to include clock domain

5. **Write analysis tests** (`tests/clock_domain/unit/`)
   - Test clock discovery
   - Test explicit domain annotation
   - Test domain inference
   - Test clock reference validation

**Deliverables:**
- All components have clock domain populated (or Unknown)
- Port signatures include clock domain
- Clock references validated

**Dependencies:** Phase 1 complete

---

### Phase 3: Type Checking (Estimated: 4-5 days)

**Goal:** Verify all clock domain constraints and infer unknown domains.

**Tasks:**

1. **Extend Typing struct** (`types/typing.rs`)
   - Add `clocks: IndexMap<AstNodeId, ClockDomain>` field
   - Add `clock_at()`, `annotate_clock()` methods
   - Add `typed_at()` returning `Typed` struct

2. **Extend TypingContext** (`types/context.rs`)
   - Update `Binding` to include full `Typed` info
   - Update `push_component()`, `push_local()` signatures

3. **Implement domain inference algorithm** (`types/typing/infer.rs`)
   - Collect constraints from:
     - Explicit annotations
     - Binary operators (same domain)
     - Drivers (LHS = RHS)
     - Match arms (all same domain)
   - Unify constraints using union-find
   - Detect and report conflicts

4. **Implement domain checking** (`types/typing/check.rs`)
   - Check binary operators: both operands same domain
   - Check drivers: LHS and RHS same domain
   - Check `sync(x)`: any domain -> expected domain
   - Check `async(x)`: any domain -> async

5. **Add new diagnostics** (`diagnostics.rs`)
   - E0701: UnknownClock
   - E0702: OnClauseNotClock
   - E0707: DomainCrossing
   - E0708: UninferrableClock
   - W0751: NoOpSync
   - W0753: MultiBitSync

6. **Write type checking tests** (`tests/clock_domain/typecheck/`)
   - Test domain inference from drivers
   - Test domain crossing errors
   - Test sync() type rules
   - Test async() type rules
   - Test module boundary propagation

**Deliverables:**
- All expressions have inferred clock domains
- All domain violations produce errors
- All diagnostics have test coverage

**Dependencies:** Phase 2 complete

---

### Phase 4: Verilog Generation (Estimated: 2-3 days)

**Goal:** Generate correct Verilog for sync() primitive.

**Tasks:**

1. **Add SyncReg element** (`verilog.rs`)
   - Add `Element::SyncReg(SyncReg)` variant
   - Define `SyncReg` struct: `temp_name`, `source`, `clock`, `width`

2. **Implement sync() code generation** (`verilog/conversion.rs`)
   - Collect all `sync()` instances during conversion pass
   - Detect and reuse temps for same source
   - Generate double-flop always block
   - Generate gensym'd temporary names

3. **Implement async() code generation** (`verilog/conversion.rs`)
   - Erase `async()` calls during conversion
   - Direct connection, no hardware

4. **Write Verilog tests** (`tests/clock_domain/verilog/`)
   - Test sync() generates double-flop
   - Test sync() temp reuse for same source
   - Test async() generates no hardware
   - Test generated Verilog is valid

**Deliverables:**
- sync() produces double-flop synchronizer
- async() produces direct connection
- Temporaries are reused correctly

**Dependencies:** Phase 3 complete

---

### Phase 5: Integration (Estimated: 2-3 days)

**Goal:** Validate complete feature with real examples.

**Tasks:**

1. **Update debounce.vir example**
   - Replace manual double-flop with sync()
   - Verify output Verilog matches expected

2. **Add multi-clock example**
   - Create example showing clock domain crossing
   - Document recommended patterns

3. **Update documentation**
   - Update tutorial with sync() usage
   - Document clock domain tracking feature
   - Add FAQ for common CDC patterns

4. **Run full test suite**
   - Verify no regressions in existing tests
   - Verify all new tests pass
   - Check for new warnings

**Deliverables:**
- Updated examples using new feature
- Documentation complete
- All tests passing

**Dependencies:** Phase 4 complete

---

## Dependency Graph

```
Phase 1: Parsing
    │
    ▼
Phase 2: Analysis
    │
    ▼
Phase 3: Type Checking
    │
    ▼
Phase 4: Verilog Generation
    │
    ▼
Phase 5: Integration
```

**Critical Path:** Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5

**Total Duration:** 13-18 days

---

## Parallel Work Opportunities

While the phases are sequential, some parallel work is possible:

### During Phase 1 (Parsing)
- **Parallel:** Write diagnostic message templates
- **Parallel:** Design test cases for later phases
- **Parallel:** Review existing Verilog generation code

### During Phase 2 (Analysis)
- **Parallel:** Implement ClockDomain unit tests
- **Parallel:** Design type checking constraint collection
- **Parallel:** Review existing type checking code

### During Phase 3 (Type Checking)
- **Parallel:** Design Verilog generation strategy
- **Parallel:** Write Verilog test expectations
- **Parallel:** Update documentation drafts

### During Phase 4 (Verilog Generation)
- **Parallel:** Write integration test cases
- **Parallel:** Prepare example updates
- **Parallel:** Review documentation

### During Phase 5 (Integration)
- **Sequential:** All tasks must be completed
- **Parallel:** Documentation review by stakeholders

---

## Risk Areas and Mitigation

### Risk 1: Grammar Conflicts

**Risk:** Adding `async` keyword and new annotations may conflict with existing grammar rules.

**Probability:** Medium

**Impact:** High (blocks Phase 1)

**Mitigation:**
- Test grammar changes incrementally
- Use LALRPOP's conflict resolution hints if needed
- Consider using `async` as contextual keyword (only valid after type)

**Contingency:** If conflicts arise, use `async` as identifier and parse based on position.

---

### Risk 2: Type Inference Complexity

**Risk:** Domain inference algorithm may have edge cases or exponential behavior.

**Probability:** Medium

**Impact:** Medium (delays Phase 3)

**Mitigation:**
- Start with simple constraint collection
- Use union-find with path compression
- Add cycle detection early
- Test with complex module hierarchies

**Contingency:** Fall back to requiring explicit annotations if inference is too complex.

---

### Risk 3: Verilog Temp Reuse Bugs

**Risk:** Reusing sync temps incorrectly may cause subtle Verilog bugs.

**Probability:** Low

**Impact:** High (incorrect hardware)

**Mitigation:**
- Implement conservative temp reuse (same source only)
- Add extensive Verilog output verification
- Add visual diff tests for generated code

**Contingency:** Disable temp reuse in Phase 1, add optimization later.

---

### Risk 4: Cross-Module Propagation Errors

**Risk:** Clock domain propagation across module boundaries may have edge cases.

**Probability:** Medium

**Impact:** Medium (incorrect type checking)

**Mitigation:**
- Write comprehensive module boundary tests
- Test with nested submodules
- Test with external modules

**Contingency:** Require explicit annotations on all port connections.

---

### Risk 5: Breaking Existing Code

**Risk:** New type checking rules may reject previously valid code.

**Probability:** Low

**Impact:** High (regression)

**Mitigation:**
- Run full test suite after each phase
- Check all examples compile
- Add compatibility test cases

**Contingency:** If regressions found, add gradual migration path with deprecation warnings.

---

### Risk 6: Multi-Bit Sync Unsafe Usage

**Risk:** Users may try sync() on multi-bit values and get unsafe CDC.

**Probability:** Medium

**Impact:** High (unsafe hardware)

**Mitigation:**
- Phase 1: Emit ERROR for sync() on non-Bit types
- Documentation: Clearly document CDC safety
- Future: Add safe multi-bit CDC primitives

**Contingency:** None needed - errors prevent unsafe usage.

---

## Milestone Checkpoints

### Milestone 1: Parser Complete (Day 3)

**Criteria:**
- [ ] Grammar changes compile without conflicts
- [ ] All parser tests pass (8 valid cases, 3 error cases)
- [ ] Manual testing: parse example.vir with new syntax

**Gate:** Cannot proceed to Phase 2 until parser is stable.

**Review:** Check AST output for new node types.

---

### Milestone 2: Analysis Complete (Day 7)

**Criteria:**
- [ ] All components have clock domain populated
- [ ] Port signatures include clock domain
- [ ] Clock reference validation works
- [ ] All analysis unit tests pass (6 tests)

**Gate:** Cannot proceed to Phase 3 until analysis is complete.

**Review:** Check ComponentAnalysis output for sample modules.

---

### Milestone 3: Type Checking Complete (Day 12)

**Criteria:**
- [ ] Domain inference works for simple modules
- [ ] Domain crossing errors detected
- [ ] sync() and async() type checking works
- [ ] All typecheck tests pass (10 tests)
- [ ] New diagnostics have test coverage

**Gate:** Cannot proceed to Phase 4 until type checking is correct.

**Review:** Verify error messages are clear and helpful.

---

### Milestone 4: Verilog Generation Complete (Day 15)

**Criteria:**
- [ ] sync() generates double-flop synchronizer
- [ ] async() generates direct connection
- [ ] Temp reuse works correctly
- [ ] All Verilog tests pass (5 tests)
- [ ] Generated Verilog is valid (passes linting)

**Gate:** Cannot proceed to Phase 5 until Verilog is correct.

**Review:** Visual inspection of generated Verilog for sample modules.

---

### Milestone 5: Feature Complete (Day 18)

**Criteria:**
- [ ] debounce.vir updated and working
- [ ] Multi-clock example added
- [ ] Documentation updated
- [ ] Full test suite passes (no regressions)
- [ ] All new tests pass (41 tests)

**Gate:** Feature ready for merge.

**Review:** Final code review, documentation review, test coverage review.

---

## Implementation Tracking

Use the following checklist to track progress:

```
Phase 1: Parsing [ ]
  [ ] ClockDomain enum
  [ ] Grammar updates
  [ ] AST payloads
  [ ] Parser tests

Phase 2: Analysis [ ]
  [ ] Component extension
  [ ] Clock discovery
  [ ] Port extension
  [ ] Analysis tests

Phase 3: Type Checking [ ]
  [ ] Typing extension
  [ ] Context extension
  [ ] Domain inference
  [ ] Domain checking
  [ ] Diagnostics
  [ ] Typecheck tests

Phase 4: Verilog Generation [ ]
  [ ] SyncReg element
  [ ] sync() generation
  [ ] async() generation
  [ ] Verilog tests

Phase 5: Integration [ ]
  [ ] Update examples
  [ ] Documentation
  [ ] Full test suite
```

---

## Summary

| Phase | Duration | Dependencies | Deliverables |
|-------|----------|--------------|-------------|
| 1. Parsing | 2-3 days | None | Grammar, AST, parser tests |
| 2. Analysis | 3-4 days | Phase 1 | Component clock info, analysis tests |
| 3. Type Checking | 4-5 days | Phase 2 | Domain inference, checking, diagnostics |
| 4. Verilog Generation | 2-3 days | Phase 3 | sync() code gen, Verilog tests |
| 5. Integration | 2-3 days | Phase 4 | Examples, docs, full tests |
| **Total** | **13-18 days** | | **41 tests, 17 files** |

# Design Decisions

This section documents key design decisions made for clock domain tracking.

## 1. Combinational modules require a clock if they have clocked signals

**Decision:** Modules with any clocked signals must have at least one `Clock` input.

**Rationale:**
This ensures all `on <clock>` references can be resolved and eliminates uninferrable
clock domains. A module without a `Clock` input can still exist, but all its signals
would be implicitly `async`.

**Example:**
```virdant
// Valid: all signals async, no clock needed
mod Combinational {
    incoming a : Bit
    incoming b : Bit
    outgoing c : Bit
    c := a && b
}

// Valid: clock present, signals can be clocked
mod Sequential {
    incoming clk : Clock
    incoming a : Bit async
    outgoing c : Bit on clk
    reg state : Bit on clk
    // ...
}

// Invalid: no clock but clocked signal
mod Bad {
    incoming a : Bit
    outgoing c : Bit on clk  // Error: 'clk' not in scope
}
```

---

## 2. Reset is synchronous, same domain rules as Bit

**Decision:** `Reset` type is treated identically to `Bit` for clock domain purposes.
Both require explicit domain annotation.

**Rationale:**
Explicit domain annotation makes intent clear and avoids special-case handling in
the type system. Users can choose synchronous or asynchronous reset based on their
needs:

```virdant
// Synchronous reset
incoming reset : Bit on clock

// Asynchronous reset
incoming reset : Bit async
```

---

## 3. sync(x) restricted to Bit type in Phase 1

**Decision:** `sync(x)` only accepts `Bit` type arguments. Other types produce an error.

**Rationale:**
Single-bit double-flop synchronization is safe. Multi-bit synchronization is unsafe
because bits may arrive at different times, corrupting the value. Safe multi-bit CDC
requires domain-specific patterns (gray coding, handshakes, FIFOs).

**Error message:**
```
error[E0753]: sync() only supports Bit type
  |
  |     wire synced : Word[8] on clock := sync(data)
  |                                       ^^^^^^^^^^
  |
  = note: Multi-bit CDC requires domain-specific patterns
  = help: Use gray coding for counters, handshake/FIFO for data
```

**Future:** May add multi-bit support with warning once safe CDC primitives are available.

---

## 4. Clock gating deferred to future work

**Decision:** Clock gating is not supported in Phase 1.

**Rationale:**
Clock gating is a power optimization technique requiring specialized cells and timing
analysis. Phase 1 focuses on basic CDC safety with simple clock trees.

---

## 5. Socket channels must share a clock domain

**Decision:** All channels in a socket must be on the same clock domain.

**Rationale:**
Sockets group related signals that travel together. Mixed-clock sockets would require
individual channel synchronization, violating the socket abstraction.

**Syntax:**
```virdant
socket Mem {
    cosi addr : Word[16]
    soci data : Word[8]
}

// All channels on 'clk'
client socket mem of Mem on clk
```

**Error case:**
```
error[E07xx]: socket channels must share a clock domain
  |
  |     client socket mem of Mem on clk_a  // addr on clk_a
  |     mem.data := data_b                   // data on clk_b
  |              ^^^^^^^^^
  |
  = note: Cannot mix clock domains within a socket
```

---

## 6. Strict mode deferred to future work

**Decision:** No strict mode in Phase 1. Domain inference is the default.

**Rationale:**
Inference provides good ergonomics for common cases. If users encounter issues,
a strict mode requiring explicit annotations can be added later as an opt-in feature.

---

# Appendix

This appendix provides reference material for the clock domain tracking feature.

## A. Full Example: debounce.vir with Clock Domains

```virdant
//> A counter-based debouncer for mechanical switches.
//>
//> When the button changes state, the debouncer waits for
//> 16 consecutive stable samples before accepting the change.
//> This filters out the mechanical bounce of physical switches.

enum type DebState {
    Idle = 0b01w2
    Count = 0b10
}

mod Debounce {
    incoming clock : Clock
    incoming reset : Bit
    incoming button : Bit async
    outgoing clean : Bit on clock

    // Synchronize the asynchronous button input to prevent metastability.
    wire btn : Bit on clock {
        it := sync(button)
    }

    // The stable debounced output.
    reg clean : Bit on clock

    // Debounce state machine.
    reg state : DebState on clock
    reg count : Word[4] on clock

    state <= when {
        case reset => #Idle
        else match state {
            case #Idle => when {
                case btn != clean => #Count
                else => #Idle
            }
            case #Count => when {
                case btn == clean => #Idle
                case count >= 15 => #Idle
                else => #Count
            }
        }
    }

    count <= when {
        case reset => 0
        case state == #Count => count + 1
        else => 0
    }

    clean <= when {
        case reset => false
        case state == #Count && count >= 15 => btn
        else => clean
    }
}
```

**Key points:**
1. `incoming button : Bit async` - Input is asynchronous
2. `wire btn : Bit on clock` - Synchronized version
3. `it := sync(button)` - Double-flop synchronizer
4. All other signals are `on clock`

---

## B. Full Example: Multi-Clock Crossing

```virdant
// Multi-clock domain crossing example
// Shows safe patterns for crossing between clock domains

mod ClockDomainA {
    incoming clk_a : Clock
    incoming data_in : Bit on clk_a
    outgoing data_out : Bit on clk_a

    data_out := data_in
}

mod ClockDomainB {
    incoming clk_b : Clock
    incoming data_in : Bit on clk_b
    outgoing data_out : Bit on clk_b

    data_out := data_in
}

mod ClockCrossing {
    incoming clk_a : Clock
    incoming clk_b : Clock

    // Data originates in domain A
    incoming data_a : Bit on clk_a

    // Step 1: Convert to async (erases timing information)
    wire async_data : Bit async {
        it := async(data_a)
    }

    // Step 2: Synchronize to domain B
    wire data_b : Bit on clk_b {
        it := sync(async_data)
    }

    // Now safe to use in domain B
    outgoing result : Bit on clk_b
    result := data_b
}

mod Top {
    incoming clk_a : Clock
    incoming clk_b : Clock
    incoming input_data : Bit on clk_a

    mod domain_a of ClockDomainA
    domain_a.clk_a := clk_a
    domain_a.data_in := input_data

    mod crossing of ClockCrossing
    crossing.clk_a := clk_a
    crossing.clk_b := clk_b
    crossing.data_a := domain_a.data_out

    mod domain_b of ClockDomainB
    domain_b.clk_b := clk_b
    domain_b.data_in := crossing.result

    outgoing final_output : Bit on clk_b
    final_output := domain_b.data_out
}
```

**Key points:**
1. Data starts in `clk_a` domain
2. `async()` converts to async (discards timing)
3. `sync()` synchronizes to `clk_b` domain
4. Safe to use in `clk_b` domain after crossing

---

## C. Reference: All New Rust Types

### C.1 ClockDomain Enum

**File:** `virdant/src/types/typ.rs`

```rust
use crate::analysis::symbols::SymbolId;

/// Represents the clock domain of a value.
///
/// Every value in Virdant belongs to exactly one clock domain.
/// The domain determines when the value is valid and can be sampled.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ClockDomain {
    /// Value is not synchronized to any clock.
    /// May change at any time unrelated to clock edges.
    /// Cannot be used in sequential logic without synchronization.
    Async,

    /// Value is synchronized to a specific clock signal.
    /// The SymbolId identifies the Clock-typed component.
    OnClock(SymbolId),

    /// Domain could not be inferred.
    /// Used internally during type checking.
    /// Invalid as a final result - produces an error.
    Unknown,
}

impl ClockDomain {
    /// Returns true if this is an async domain.
    pub fn is_async(&self) -> bool {
        matches!(self, ClockDomain::Async)
    }

    /// Returns true if this is a clocked domain.
    pub fn is_clocked(&self) -> bool {
        matches!(self, ClockDomain::OnClock(_))
    }

    /// Returns the clock symbol if this is a clocked domain.
    pub fn clock_symbol(&self) -> Option<SymbolId> {
        match self {
            ClockDomain::OnClock(id) => Some(*id),
            _ => None,
        }
    }

    /// Checks if two domains are compatible.
    /// Unknown never matches anything (including itself).
    pub fn matches(&self, other: &ClockDomain) -> bool {
        match (self, other) {
            (ClockDomain::Async, ClockDomain::Async) => true,
            (ClockDomain::OnClock(a), ClockDomain::OnClock(b)) => a == b,
            _ => false,
        }
    }
}

impl std::fmt::Display for ClockDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClockDomain::Async => write!(f, "async"),
            ClockDomain::OnClock(id) => write!(f, "on clock({:?})", id),
            ClockDomain::Unknown => write!(f, "<unknown domain>"),
        }
    }
}
```

---

### C.2 Typed Struct

**File:** `virdant/src/types/typed.rs` (new file)

```rust
use crate::types::{Type, ClockDomain};
use crate::analysis::Location;

/// A value with both a type and a clock domain.
///
/// Every expression in Virdant produces a Typed value.
/// This struct captures the complete type information for a value.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Typed {
    /// The data type (Bit, Word[n], struct, enum, union).
    pub typ: Type,

    /// The clock domain (Async, OnClock, or Unknown).
    pub clock: ClockDomain,

    /// Source location for error messages.
    pub location: Location,
}

impl Typed {
    /// Create a new Typed value.
    pub fn new(typ: Type, clock: ClockDomain, location: Location) -> Self {
        Typed { typ, clock, location }
    }

    /// Create a typed Bit value.
    pub fn bit(clock: ClockDomain, location: Location) -> Self {
        Typed::new(Type::Bit, clock, location)
    }

    /// Create a typed Word[n] value.
    pub fn word(width: u16, clock: ClockDomain, location: Location) -> Self {
        Typed::new(Type::Word(width), clock, location)
    }

    /// Check if this value is async.
    pub fn is_async(&self) -> bool {
        self.clock.is_async()
    }

    /// Check if this value is clocked.
    pub fn is_clocked(&self) -> bool {
        self.clock.is_clocked()
    }

    /// Check if the domains match between two values.
    pub fn domain_matches(&self, other: &Typed) -> bool {
        self.clock.matches(&other.clock)
    }
}
```

---

### C.3 Component Extension

**File:** `virdant/src/analysis/component.rs`

Add to existing `Component` struct:

```rust
#[derive(Debug, Clone)]
pub struct Component {
    id: ComponentId,
    path: BString,
    location: Location,
    flow: Flow,
    kind: Option<ComponentKind>,
    typ: Option<Type>,

    // NEW: Clock domain of this component
    clock: Option<ClockDomain>,
}

impl Component {
    // ... existing methods ...

    /// Returns the clock domain of this component.
    /// Returns None for Clock/Reset types (no domain).
    pub fn clock(&self) -> Option<&ClockDomain> {
        self.clock.as_ref()
    }

    /// Returns true if this component is asynchronous.
    pub fn is_async(&self) -> bool {
        matches!(self.clock, Some(ClockDomain::Async))
    }

    /// Returns true if this component is clocked.
    pub fn is_clocked(&self) -> bool {
        matches!(self.clock, Some(ClockDomain::OnClock(_)))
    }
}
```

---

### C.4 Port Extension

**File:** `virdant/src/analysis/ports.rs`

Add to existing `Port` struct:

```rust
#[derive(Debug, Clone)]
pub struct Port {
    pub name: BString,
    pub dir: PortDir,
    pub width: Width,
    pub kind: PortKind,

    // NEW: Clock domain of this port
    pub clock: Option<ClockDomain>,
}

impl Port {
    /// Create the port signature string including clock domain.
    pub fn signature(&self) -> String {
        let domain_str = match &self.clock {
            Some(ClockDomain::Async) => " async".to_string(),
            Some(ClockDomain::OnClock(id)) => format!(" on clock({:?})", id),
            Some(ClockDomain::Unknown) => " <unknown>".to_string(),
            None => "".to_string(),
        };

        format!(
            "{} {} : {}{}{}",
            self.dir_str(),
            self.name,
            self.type_str(),
            domain_str,
            self.kind_suffix()
        )
    }
}
```

---

## D. Reference: All Grammar Changes

### D.1 Reserved Words

**File:** `virdant/src/syntax/token.rs`

Add `async` to reserved words:

```rust
pub enum Reserved {
    // ... existing reserved words ...
    Async,  // NEW
}
```

---

### D.2 Component Declaration

**File:** `virdant/src/syntax/virdant.lalrpop`

**Current:**
```
ModDefStmtComponent: AstNodeId = {
    <docstring:DocString?> <ll:@L>
    ("incoming" | "outgoing" | "wire" | "reg") <ident:Ident> ":" <typ:Type>
    <on:OnClause?> <it:ItBlock?>
    <rr:@R> => { ... }
}
```

**New:**
```
ModDefStmtComponent: AstNodeId = {
    <docstring:DocString?> <ll:@L>
    ("incoming" | "outgoing" | "wire" | "reg") <ident:Ident> ":" <typ:Type>
    <domain:ClockDomain?> <it:ItBlock?>
    <rr:@R> => { ... }
}

ClockDomain: ClockDomainAnnotation = {
    "async" => ClockDomainAnnotation::Async,
    "on" <clock:Expr> => ClockDomainAnnotation::On(clock),
}
```

---

### D.3 Expressions

**File:** `virdant/src/syntax/virdant.lalrpop`

Add to `Expr` production:

```
Expr: AstNodeId = {
    // ... existing expression rules ...

    // NEW: sync() primitive
    <ll:@L> "sync" "(" <arg:Expr> ")" <rr:@R> => {
        let payload = AstNodePayload::ExprSync;
        let span = parser.span(ll, rr);
        let num_children = 1;
        parser.add_node(payload, span, num_children)
    }

    // NEW: async() primitive
    <ll:@L> "async" "(" <arg:Expr> ")" <rr:@R> => {
        let payload = AstNodePayload::ExprAsync;
        let span = parser.span(ll, rr);
        let num_children = 1;
        parser.add_node(payload, span, num_children)
    }
}
```

---

### D.4 AST Payloads

**File:** `virdant/src/syntax/payload.rs`

Add new variants:

```rust
#[derive(Debug, Clone)]
pub enum AstNodePayload {
    // ... existing variants ...

    /// sync() primitive expression - synchronizes value to local clock
    ExprSync,

    /// async() primitive expression - converts clocked value to async
    ExprAsync,
}
```

---

### D.5 Clock Domain Annotation

**File:** `virdant/src/syntax/payload.rs`

Add new payload:

```rust
/// Represents a clock domain annotation on a component declaration.
#[derive(Clone, Debug)]
pub enum ClockDomainAnnotation {
    /// `async` keyword - value is not synchronized to any clock
    Async,

    /// `on <clock>` - value is synchronized to named clock signal
    On(AstNodeId),  // References the clock expression
}
```

---

## E. Diagnostic Codes Reference

| Code | Name | Level | Message |
|------|------|-------|---------|
| E0701 | UnknownClock | Error | Clock reference not found in scope |
| E0702 | OnClauseNotClock | Error | Referenced signal is not type Clock |
| E0703 | ClockNotInput | Error | Clock must be an incoming port |
| E0704 | AsyncReg | Error | Register cannot be async |
| E0705 | ClockTypeDomain | Error | Clock/Reset cannot have domain annotation |
| E0706 | ComplexClockRef | Error | Complex expression in 'on' clause |
| E0707 | DomainCrossing | Error | Cannot combine different clock domains |
| E0708 | UninferrableClock | Error | Cannot infer clock domain |
| W0751 | NoOpSync | Warning | sync() on signal already in target domain |
| W0752 | AsyncToClockedPort | Warning | Async to clocked port without sync |
| W0753 | MultiBitSync | Warning | sync() on multi-bit value (unsafe) |

---

## F. File Modification Checklist

### New Files to Create

- [ ] `virdant/src/types/typed.rs` - Typed struct definition
- [ ] `tests/clock_domain/` - Test directory structure
- [ ] `tests/clock_domain/unit/clock_domain.rs` - Unit tests
- [ ] `tests/clock_domain/parser/*.vir` - Parser test cases
- [ ] `tests/clock_domain/typecheck/*.vir` - Type checking test cases
- [ ] `tests/clock_domain/verilog/*.vir` - Verilog generation test cases
- [ ] `tests/clock_domain/integration/*.vir` - Integration test cases

### Files to Modify

- [ ] `virdant/src/types/typ.rs` - Add ClockDomain enum
- [ ] `virdant/src/types/typing.rs` - Add clock domain to Typing struct
- [ ] `virdant/src/types/context.rs` - Extend TypingContext
- [ ] `virdant/src/analysis/component.rs` - Add clock field to Component
- [ ] `virdant/src/analysis/ports.rs` - Add clock field to Port
- [ ] `virdant/src/syntax/virdant.lalrpop` - Grammar updates
- [ ] `virdant/src/syntax/payload.rs` - New AST payloads
- [ ] `virdant/src/syntax/token.rs` - Add async keyword
- [ ] `virdant/src/diagnostics.rs` - New diagnostic types
- [ ] `virdant/src/verilog.rs` - SyncReg element type
- [ ] `virdant/src/verilog/conversion.rs` - sync() code generation
- [ ] `virdant/src/db.rs` - New query functions

### Documentation to Update

- [ ] `docs/source/tutorial/debounce.rst` - Use sync() primitive
- [ ] `docs/source/reference/sync.rst` - New page for sync primitive
- [ ] `examples/debounce.vir` - Update to use sync()
- [ ] `AGENTS.md` - Add clock domain guidance

---

## G. Migration Guide

### For Existing Code

If you have existing Virdant code with manual synchronization:

**Before:**
```virdant
reg meta : Bit on clock
reg sync : Bit on clock
meta <= button
sync <= meta
wire btn : Bit on clock
btn := sync
```

**After:**
```virdant
wire btn : Bit on clock {
    it := sync(button)
}
```

### Breaking Changes

1. **Modules without clocks:** Modules with any clocked signals must now have at least one `Clock` input.

2. **Uninferrable domains:** All clock domains must be inferrable or explicitly annotated. Previous code with unclear timing will produce errors.

3. **Mixed domains:** Combining signals from different clock domains now produces an error. Must use explicit `sync()` and `async()`.

---

## H. Glossary

| Term | Definition |
|------|------------|
| CDC | Clock Domain Crossing - transferring data between clock domains |
| Double-flop | Two-register synchronizer for single-bit signals |
| Metastability | Unstable state when setup/hold times violated |
| Async | Not synchronized to any clock |
| Clocked | Synchronized to a specific clock |
| Domain | Clock domain - the timing context for a signal |
| Sync primitive | Built-in sync(x) function for CDC |
| Async primitive | Built-in async(x) function for domain erasure |

