local db = open_file("arbiter.vir")
local sim = db:sim("arbiter::Arbiter")

sim.attach_clock("top.clock", Clock.with_period_ps(1000))

sim.run(function()
    -- During reset, grants are always false regardless of requests.
    sim.set("top.reset", true)
    sim.set("top.req0", true)
    sim.set("top.req1", true)
    assert(sim.get("top.grant0") == false, "grant0 must be false during reset")
    assert(sim.get("top.grant1") == false, "grant1 must be false during reset")

    -- Tick once while in reset: priority_for_client_0 latches true.
    sim.wait("top.clock")

    -- Deassert reset; client 0 now has priority (priority_for_client_0 = true).
    sim.set("top.reset", false)

    -- Only client 0 requests.
    sim.set("top.req0", true)
    sim.set("top.req1", false)
    assert(sim.get("top.grant0") == true,  "grant0: only req0, priority=0")
    assert(sim.get("top.grant1") == false, "grant1: only req0, priority=0")

    -- Only client 1 requests.
    sim.set("top.req0", false)
    sim.set("top.req1", true)
    assert(sim.get("top.grant0") == false, "grant0: only req1, priority=0")
    assert(sim.get("top.grant1") == true,  "grant1: only req1, priority=0")

    -- Both request: client 0 wins (has priority).
    sim.set("top.req0", true)
    sim.set("top.req1", true)
    assert(sim.get("top.grant0") == true,  "grant0: both req, priority=0")
    assert(sim.get("top.grant1") == false, "grant1: both req, priority=0")

    -- Neither requests.
    sim.set("top.req0", false)
    sim.set("top.req1", false)
    assert(sim.get("top.grant0") == false, "grant0: no req, priority=0")
    assert(sim.get("top.grant1") == false, "grant1: no req, priority=0")

    -- Tick: priority_for_client_0 flips to false; client 1 now has priority.
    sim.wait("top.clock")

    -- Only client 0 requests.
    sim.set("top.req0", true)
    sim.set("top.req1", false)
    assert(sim.get("top.grant0") == true,  "grant0: only req0, priority=1")
    assert(sim.get("top.grant1") == false, "grant1: only req0, priority=1")

    -- Only client 1 requests.
    sim.set("top.req0", false)
    sim.set("top.req1", true)
    assert(sim.get("top.grant0") == false, "grant0: only req1, priority=1")
    assert(sim.get("top.grant1") == true,  "grant1: only req1, priority=1")

    -- Both request: client 1 wins (has priority).
    sim.set("top.req0", true)
    sim.set("top.req1", true)
    assert(sim.get("top.grant0") == false, "grant0: both req, priority=1")
    assert(sim.get("top.grant1") == true,  "grant1: both req, priority=1")

    -- Neither requests.
    sim.set("top.req0", false)
    sim.set("top.req1", false)
    assert(sim.get("top.grant0") == false, "grant0: no req, priority=1")
    assert(sim.get("top.grant1") == false, "grant1: no req, priority=1")
end)

print("PASS")
