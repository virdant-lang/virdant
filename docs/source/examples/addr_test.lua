local db = open_file("adder.vir")
local sim = db:sim("adder::Adder4")

local expected = {
    {  0,  0, false,  0, false },
    {  1,  1, false,  2, false },
    {  0,  1, false,  1, false },
    {  1,  0, false,  1, false },
    {  2,  3, false,  5, false },
    {  8,  8, false,  0,  true },
    { 15, 15, false,  14, true },
    { 15, 15,  true,  15, true },
}

sim.run(function()
    for _, values in ipairs(expected) do
        local a, b, carry_in, sum_expected, carry_out_expected = table.unpack(values)
        sim.set("top.a", a)
        sim.set("top.b", b)
        sim.set("top.carry_in", carry_in)

        local sum_actual = sim.get("top.sum")
        local carry_out_actual = sim.get("top.carry_out")

        assert(sum_actual == sum_expected, string.format("sum = %d, but expected %d", sum_actual, sum_expected))
    end
end)

print("PASS")
