let examples_dir = "../../examples";
let db = open_file(examples_dir + "/matches.vir");
let sim = db.sim("matches::Top");

sim.assert(sim.get("top.enum_match") == 0, "top.enum_match expected to be 0, but was " + sim.get("top.enum_match"));
sim.assert(sim.get("top.parity") == false, "top.parity expected to be false, but was " + sim.get("top.parity"));
sim.assert(sim.get("top.union_match_is_baz"), "top.union_match_is_baz expected to be true");
