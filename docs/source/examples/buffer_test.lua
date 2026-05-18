local db = open_file("buffer.vir")
local sim = db:sim("buffer::Buffer")

sim.attach_clock("top.clock", Clock.with_period_ps(1000))

sim.run(function()
    for inp = 0, 1025 do
        sim.set("top.inp", inp)
        sim.wait("top.clock")
        local out = sim.get("top.out")
        assert(out == inp, string.format("out = %d, but expected %d", out, inp))
    end
end)

print("PASS")
