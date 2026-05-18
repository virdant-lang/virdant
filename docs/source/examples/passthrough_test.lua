local db = open_file("passthrough.vir")
local sim = db:sim("passthrough::Passthrough")

sim.run(function()
    for inp = 0, 255 do
        sim.set("top.inp", inp)
        local out = sim.get("top.out")
        assert(inp == out, string.format("out = %d, but expected %d", out, inp))
    end
end)

print("PASS")
