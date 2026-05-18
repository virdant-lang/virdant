-- Test Lua script for Virdant simulation
-- Verifies LFSR output matches expected Galois-LFSR sequence using coroutines

local examples_dir = "../../examples"

-- Open the LFSR example
local db = open_file(examples_dir .. "/lfsr.vir")
local sim = db:sim("lfsr::Lfsr")

-- Expected LFSR output sequence (time_ps, value)
-- Polynomial 0x1D, reset value 0xFF
-- These are the values observed at the rising edge of each clock cycle
local expected = {
    { t = 5000,  v = 255 },  -- First clock edge, reset still high, out = 0xFF
    { t = 15000, v = 227 },  -- Second edge, reset low, LFSR runs
    { t = 25000, v = 219 },
    { t = 35000, v = 171 },
    { t = 45000, v = 75 },
    { t = 55000, v = 150 },
    { t = 65000, v = 49 },
    { t = 75000, v = 98 },
    { t = 85000, v = 196 },
    { t = 95000, v = 149 },
}

-- Track observed values
local observed = {}

-- Add clock with 10ns period (10,000 ps)
sim.attach_clock("top.clock", Clock.with_period_ps(10000))

print("Running LFSR simulation with Lua coroutines...")

sim.run(function()
    -- Set up initial values - reset high
    sim.set("top.reset", true)

    -- Wait for first clock edge and observe (reset still high)
    sim.wait("top.clock")
    local t = sim.now()
    local v = sim.get("top.out")
    print(string.format("[t=%dps] out = %d", t, v))
    table.insert(observed, { t = t, v = v })

    -- Deassert reset after first clock cycle
    sim.set("top.reset", false)

    -- Run for 9 more clock cycles, observing the output
    for i = 2, 10 do
        sim.wait("top.clock")
        t = sim.now()
        v = sim.get("top.out")
        print(string.format("[t=%dps] out = %d", t, v))
        table.insert(observed, { t = t, v = v })
    end

    print(string.format("[t=%dps] Simulation ended", sim.now()))
    sim.finish()
end)

-- Verify results
print(string.format("Verifying %d observations against expected sequence...", #observed))

assert(#observed == #expected,
    string.format("Expected %d observations, got %d", #expected, #observed))

for i = 1, #expected do
    local exp = expected[i]
    local obs = observed[i]
    assert(obs.t == exp.t,
        string.format("Observation %d: expected t=%d, got t=%d", i, exp.t, obs.t))
    assert(obs.v == exp.v,
        string.format("Observation %d at t=%d: expected v=%d, got v=%d", i, obs.t, exp.v, obs.v))
end

print(string.format("All %d observations matched expected LFSR sequence!", #expected))
print("PASS")
