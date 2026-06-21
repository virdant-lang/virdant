Please go through the code and remove all references to E0708, E0707, and any other "error
codes" that were mentioned in the plan. Those were not authorized. The actual errors do
not have error codes. I *think* this only affected the comments.
Similarly, E0701-E0708, W0751-W0753, etc.

There should be an AST helper for the driver block.


> 2. The `ClockDomain` enum design was sound
> The decision to use `SymbolId` (not a string) in `OnClock(SymbolId)`
> meant that clock identity is structural:

This sounds a little suspicious and should be investigated.


The clanker made these ExprAtoms:
  "sync" "(" Expr ")"
| "async" "(" Expr ")"
This is in violation of what I asked.
However, the clanker did try to point out that `async` is being overloaded as a keyword.
I need to resolve this.


virdant/src/types/typ.rs
I don't like `ClockDomain` having an `Unknown` variant.
It should be tracked in the state instead.
I also wonder if this is not appropriate for `typ.rs` since it's not about types.


virdant/src/diagnostics.rs
Certain things in the diagnostics look weird.
The numbers the robot hallucinated piss me off.
I'll leave notes for each one:


/// Clock reference in `on` clause does not exist. (E0701)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownClock {
    pub region: Region,
    pub name: BString,
}

**NOTE** pointless. it should just be an unknown reference

/// Clock reference in `on` clause does not have Clock type. (E0702)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OnClauseNotClock {
    pub region: Region,
    pub name: BString,
}

**NOTE** Pointless. There's already an alternative for this.

/// Clock signal is not an incoming port. (E0703)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClockNotInput {
    pub region: Region,
    pub name: BString,
}

**NOTE** I thought we discussed this is not in scope.

/// Register declared with the `async` keyword. (E0704)
/// Registers must be clocked.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsyncReg {
    pub region: Region,
}

**NOTE** GOOD

/// Clock or Reset type with a clock-domain annotation. (E0705)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClockTypeDomain {
    pub region: Region,
    pub typ: BString,
}

**NOTE** Reasonable but needs a better name.

/// Complex expression used in `on` clause; expected simple identifier. (E0706)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComplexClockRef {
    pub region: Region,
}

**NOTE** This is a smell and hints that the design is under-baked.

/// Combining values from different clock domains. (E0707)
#[derive(Debug, Clone)]
pub struct DomainCrossing {
    pub region: Region,
    pub left_region: Region,
    pub left_domain: ClockDomain,
    pub right_region: Region,
    pub right_domain: ClockDomain,
}

**NOTE** Needs a better name. I think the error message for this sucks too

/// Clock domain could not be inferred. (E0708)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UninferrableClock {
    pub region: Region,
    pub name: BString,
}

**NOTE** GREAT

/// `sync()` called on signal already in expected clock domain. (W0751)
#[derive(Debug, Clone)]
pub struct NoOpSync {
    pub region: Region,
    pub domain: ClockDomain,
}

**NOTE** Great, needs a better name, should be a warning.

/// Async signal connected to clocked port without `sync()`. (W0752)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsyncToClockedPort {
    pub region: Region,
    pub port_name: BString,
    pub submod_name: BString,
}

**NOTE** Needs a better name.

/// `sync()` on multi-bit value is unsafe for clock domain crossing. (W0753)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MultiBitSync {
    pub region: Region,
    pub typ: BString,
}

**NOTE** I'm not sure where this is used. I don't think we need this yet.
May suggest a bug in the typechecking for sync(x) async(x) (should only work for Bit types).



virdant/src/sim/expr.rs
Simulation should technically use thread-safe primitives to implement sync.
But for now, an `id` function is fine.
