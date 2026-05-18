// Test Rhai script for Virdant simulation
// Verifies LFSR output matches expected Galois-LFSR sequence

let examples_dir = "../../examples";

// Open the LFSR example
let db = open_file(examples_dir + "/lfsr.vir");
let sim = db.sim("lfsr::Lfsr");

// Expected LFSR output sequence (time_ps, value)
// Polynomial 0x1D, reset value 0xFF
let expected = [
    #{ t: 5000,  v: 255 },
    #{ t: 15000, v: 227 },
    #{ t: 25000, v: 219 },
    #{ t: 35000, v: 171 },
    #{ t: 45000, v: 75 },
    #{ t: 55000, v: 150 },
    #{ t: 65000, v: 49 },
    #{ t: 75000, v: 98 },
    #{ t: 85000, v: 196 },
    #{ t: 95000, v: 149 },
];

// Track observed values (auto-captured as shared reference by closures)
let observed = [];

// Add clock with 10ns period (10,000 ps)
sim.attach_clock("top.clock", Clock::with_period_ps(10000));

// Set up initial values
sim.set("top.reset", true);

// Deassert reset after 10ns
sim.after(10000, |sim| {
    sim.set("top.reset", false);
});

// Watch the output and record observations
sim.on_change("top.out", |sim| {
    let t = sim.now();
    let v = sim.get("top.out");
    print("[t=" + t + "ps] out = " + v);
    observed.push(#{ t: t, v: v });
});

// Finish after 100ns
sim.after(100000, |sim| {
    print("[t=" + sim.now() + "ps] Simulation ended");
    sim.finish();
});

// Run the simulation
print("Running LFSR simulation...");
sim.run();

// Verify results
print("Verifying " + observed.len() + " observations against expected sequence...");

sim.assert(observed.len() == expected.len(),
    "Expected " + expected.len() + " observations, got " + observed.len());

for i in 0..expected.len() {
    let exp = expected[i];
    let obs = observed[i];
    sim.assert(obs.t == exp.t,
        "Observation " + i + ": expected t=" + exp.t + ", got t=" + obs.t);
    sim.assert(obs.v == exp.v,
        "Observation " + i + " at t=" + obs.t + ": expected v=" + exp.v + ", got v=" + obs.v);
}

print("All " + expected.len() + " observations matched expected LFSR sequence!");
print("PASS");
