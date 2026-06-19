-- Unified test for mux2.vir, decoder4.vir, and decoder4_alt.vir.
-- All four modules are purely combinational, so no clock or sim.run is needed.

-- Run the shared Mux2 assertions against any sim built from either mux variant.
local function test_mux(sim)
    -- select = false: output follows b
    sim.set("top.a",      42)
    sim.set("top.b",      99)
    sim.set("top.select", false)
    assert(sim.get("top.out") == 99, "out should be b when select=false")

    -- select = true: output follows a
    sim.set("top.select", true)
    assert(sim.get("top.out") == 42, "out should be a when select=true")

    -- boundary values
    sim.set("top.a",      0)
    sim.set("top.b",      255)
    sim.set("top.select", false)
    assert(sim.get("top.out") == 255, "out should be b=255 when select=false")
    sim.set("top.select", true)
    assert(sim.get("top.out") == 0, "out should be a=0 when select=true")
end

-- Run the shared Decoder4 assertions against any sim built from either decoder variant.
local function test_decoder(sim)
    sim.set("top.inp", 0)
    assert(sim.get("top.out") == 1, "inp=0 should give out=0b0001")
    sim.set("top.inp", 1)
    assert(sim.get("top.out") == 2, "inp=1 should give out=0b0010")
    sim.set("top.inp", 2)
    assert(sim.get("top.out") == 4, "inp=2 should give out=0b0100")
    sim.set("top.inp", 3)
    assert(sim.get("top.out") == 8, "inp=3 should give out=0b1000")
end

do
    local db = open_file("mux2.vir")
    local sim = db:sim("mux2::Mux2")
    test_mux(sim)
end

do
    local db = open_file("decoder4.vir")
    local sim = db:sim("decoder4::Decoder4")
    test_decoder(sim)
end

do
    local db = open_file("decoder4_alt.vir")
    local sim = db:sim("decoder4_alt::Decoder4")
    test_decoder(sim)
end

print("PASS")
