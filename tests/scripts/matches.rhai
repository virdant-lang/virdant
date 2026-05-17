let examples_dir = "../../examples";
let db = open_file(examples_dir + "/matches.vir");
let sim = db.sim("matches::Top");

sim.assert(sim.get("top.enum_match") == 0, "enum_match expected to be 0, but was " + sim.get("top.enum_match"));
