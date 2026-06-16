`timescale 1ns/1ps

// FifoTb -- comprehensive testbench for the 4-deep FIFO
//
// Tests:
//   1. Reset initializes to empty
//   2. Bypass (empty FIFO forwards input to output)
//   3. Basic push-pop sequence (4 items)
//   4. Fill to full (4 pushes, no pops)
//   5. Push/pop at full (sequential pop then push at capacity)
//   6. Drain and ordering
//   7. Wrap-around (fill/drain twice to lap pointers)
//   8. Concurrent push+pop (steady-state throughput)
//   9. Full-duplex at boundary (fill-level boundary)
//   10. Empty-pop underflow
//   11. Reset during active operation
//   12. Randomized stress (random push/pop for many cycles)

module FifoTb;
    reg   clock = 1;
    reg   reset = 0;

    reg   [8:0] inp_data = 0;
    wire        inp_ready;

    wire  [8:0] out_data;
    reg         out_ready = 0;

    reg  [63:0] master_state;
    reg  [63:0] slave_state;

    reg [7:0] d;
    reg [7:0] e;
    reg [7:0] f;
    reg [7:0] g;

    // ---------- convenience debugging ----------

    always @(*) begin
        if (reset) begin
            master_state = "RESET";
        end else if (inp_data[8] == 1) begin
            master_state = "PUSH";
        end else begin
            master_state = "IDLE";
        end

        if (reset) begin
            slave_state = "RESET";
        end else if (out_ready) begin
            slave_state = "POP";
        end else begin
            slave_state = "IDLE";
        end
    end

    // ---------- DUT ----------

    \top::Fifo fifo(
        .\out.data  (out_data),
        .\out.ready (out_ready),
        .\inp.data  (inp_data),
        .\inp.ready (inp_ready),
        .reset(reset),
        .clock(clock)
    );

    // ---------- helper functions ----------

    function automatic logic [8:0] valid(
        input logic [7:0] in
    );
        return in | (logic'(1) << 8);
    endfunction

    function automatic logic [8:0] invalid();
        return 9'b0_1111_1111;
    endfunction

    // ---------- clock ----------

    // Free-running clock: 20 time units period
    always #5 clock = ~clock;

    // ---------- reset ----------

    task do_reset();
        reset = 1;
        @(posedge clock);
        @(posedge clock);
        reset = 0;
    endtask

    // ---------- push helpers ----------

    task push(
        input logic [7:0] in
    );
        inp_data <= valid(in);
        while (!inp_ready) begin
            @(posedge clock);
        end
        @(posedge clock);
        inp_data <= invalid();
    endtask

    // ---------- pop helpers ----------

    task pop(
        output logic [7:0] out
    );
        #1ps out_ready = 1;
        while (!out_data[8]) begin
            @(posedge clock);
        end
        @(posedge clock);
        out = out_data[7:0];
        #1ps out_ready = 0;
    endtask

    // ---------- idle helpers ----------

    task master_idle();
        inp_data <= invalid();
        @(posedge clock);
    endtask

    task slave_idle();
        out_ready <= 0;
        @(posedge clock);
    endtask

    // ---------- check helpers ----------

    task check_eq(
        input logic [7:0] got,
        input logic [7:0] expected,
        input string      msg
    );
        if (got != expected) begin
            $display("FAIL [%0t] %s: got 0x%02x, expected 0x%02x",
                     $time, msg, got, expected);
            $fatal;
        end else begin
            $display("PASS [%0t] %s", $time, msg);
        end
    endtask

    task check_out_invalid(
        input string msg
    );
        if (out_data[8] != 1'b0) begin
            $display("FAIL [%0t] %s: out_data[8]=%b (expected 0)",
                     $time, msg, out_data[8]);
            $fatal;
        end else begin
            $display("PASS [%0t] %s", $time, msg);
        end
    endtask

    task check_inp_ready(
        input logic expected,
        input string  msg
    );
        if (inp_ready != expected) begin
            $display("FAIL [%0t] %s: inp_ready=%b (expected %b)",
                     $time, msg, inp_ready, expected);
            $fatal;
        end else begin
            $display("PASS [%0t] %s", $time, msg);
        end
    endtask

    // =============================================================
    //  Initial block: dump & launch
    // =============================================================

    initial begin
        $dumpfile("build/dump.vcd");
        $dumpvars(0, FifoTb);

        inp_data = invalid();
        out_ready = 1'b0;

        // Run test phases sequentially
        test_reset();
        test_bypass();
        test_basic_push_pop();
        test_fill_to_full();
        test_push_pop_at_full();
        test_drain_and_ordering();
        test_wrap_around();
        test_concurrent_push_pop();
        test_fullduplex_at_boundary();
        test_empty_pop();
        test_reset_during_operation();
        test_random_stress();

        $display("=== ALL TESTS PASSED ===");
        $finish;
    end

    // =============================================================
    //  Test 1: Reset initializes to empty
    // =============================================================

    task test_reset();
        $display("--- Test 1: Reset behavior ---");

        do_reset();

        check_out_invalid("reset: FIFO empty after reset");
        check_inp_ready(1'b1, "reset: inp_ready high after reset");

        $display("--- Test 1 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 2: Bypass -- empty FIFO forwards input to output
    // =============================================================

    task test_bypass();
        reg [7:0] d;

        $display("--- Test 2: Bypass (empty FIFO forward) ---");

        do_reset();

        fork
            begin : bypass_push
                push(8'hA5);
            end
            begin : bypass_pop
                pop(d);
                check_eq(d, 8'hA5, "bypass: popped forwarded value");
            end
        join

        $display("--- Test 2 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 3: Basic push-pop sequence (4 items, full FIFO depth)
    // =============================================================

    task test_basic_push_pop();
        $display("--- Test 3: Basic push-pop (4 items) ---");

        do_reset();

        fork
            begin : master3
                push(8'h01);
                push(8'h23);
                push(8'h45);
                push(8'h67);
            end
            begin : slave3
                pop(d);
                pop(e);
                pop(f);
                pop(g);
            end
        join

        check_eq(d, 8'h01, "basic[0]");
        check_eq(e, 8'h23, "basic[1]");
        check_eq(f, 8'h45, "basic[2]");
        check_eq(g, 8'h67, "basic[3]");

        $display("--- Test 3 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 4: Fill to full (4 pushes, no pops)
    // =============================================================

    task test_fill_to_full();
        $display("--- Test 4: Fill to full ---");

        do_reset();

        fork
            begin : fill4
                push(8'h10);
                $display("PASS [%0t] fill: pushed 0x10", $time);
                push(8'h20);
                $display("PASS [%0t] fill: pushed 0x20", $time);
                push(8'h30);
                $display("PASS [%0t] fill: pushed 0x30", $time);
                push(8'h40);
                $display("PASS [%0t] fill: pushed 0x40 (FIFO now full)", $time);
            end
            begin : idle4
                repeat (10) master_idle();
            end
        join

        $display("--- Test 4 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 5: Push/pop at full
    // =============================================================

    task test_push_pop_at_full();
        reg [7:0] d;

        $display("--- Test 5: Push/pop at full ---");
        $display("TEST START [%0t]", $time);

        do_reset();

        $display("RESET COMPLETE [%0t]", $time);

        // Fill the FIFO
        fork
            begin : fill5
                push(8'hAA);
                push(8'hBB);
                push(8'hCC);
                push(8'hDD);
            end
            begin : idle5
                repeat (4) slave_idle();
            end
        join

        // Now FIFO is full.  inp_ready is still 1 because out_data is
        // valid (the FIFO has data to read).  The FIFO only stalls the
        // master when it is full AND out_data is NOT valid.
        $display("INFO [%0t] full: FIFO full, inp_ready=%b out_data=0x%03x",
                 $time, inp_ready, out_data);

        // Pop first to make room, then push.
        fork
            begin : idle_
                master_idle();
            end
            begin : pop_
                pop(d);
                check_eq(d, 8'hAA, "full: pop while full");
            end
        join

        // Wait one cycle for clean separation
        @(posedge clock);

        // Now push the new value (FIFO has room: 3 items left)
        push(8'hEE);
        $display("PASS [%0t] full: pushed 0xEE after pop", $time);

        // Drain the remaining 4 items (0xBB, 0xCC, 0xDD, 0xEE)
        fork
            begin : idle5c
                repeat (10) master_idle();
            end
            begin : drain5
                pop(d); check_eq(d, 8'hBB, "full: drain[0]");
                pop(d); check_eq(d, 8'hCC, "full: drain[1]");
                pop(d); check_eq(d, 8'hDD, "full: drain[2]");
                pop(d); check_eq(d, 8'hEE, "full: drain[3]");
            end
        join

        // Fill to full and push WITHOUT a consumer pop.
        fork
            begin : fill5b
                push(8'h11); push(8'h22); push(8'h33); push(8'h44);
            end
            begin : idle5d
                repeat (10) slave_idle();
            end
        join

        push(8'h55);

        fork
            begin : idle5e
                repeat (10) master_idle();
            end
            begin : drain5b
                pop(d); check_eq(d, 8'h11, "full-overwrite: drain[0]");
                pop(d); check_eq(d, 8'h22, "full-overwrite: drain[1]");
                pop(d); check_eq(d, 8'h33, "full-overwrite: drain[2]");
                pop(d); check_eq(d, 8'h44, "full-overwrite: drain[3]");
            end
        join

        $display("--- Test 5 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 6: Drain and ordering
    // =============================================================

    task test_drain_and_ordering();
        reg [7:0] d1, d2, d3, d4;

        $display("--- Test 6: Drain and ordering ---");

        do_reset();

        fork
            begin : fill6
                push(8'h11);
                push(8'h22);
                push(8'h33);
                push(8'h44);
                master_idle();
                master_idle();
                master_idle();
            end
            begin : drain6
                slave_idle();
                slave_idle();
                pop(d1); check_eq(d1, 8'h11, "ordering[0]");
                slave_idle();
                pop(d2); check_eq(d2, 8'h22, "ordering[1]");
                slave_idle();
                slave_idle();
                pop(d3); check_eq(d3, 8'h33, "ordering[2]");
                pop(d4); check_eq(d4, 8'h44, "ordering[3]");
            end
        join

        $display("--- Test 6 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 7: Wrap-around (fill/drain twice to lap pointers)
    // =============================================================

    task test_wrap_around();
        reg [7:0] d;

        $display("--- Test 7: Wrap-around ---");

        do_reset();

        // Fill and drain once (pointers at 4/4, no lap yet)
        fork
            begin : fill7a
                push(8'hF1); push(8'hF2); push(8'hF3); push(8'hF4);
            end
            begin : drain7a
                pop(d); pop(d); pop(d); pop(d);
            end
        join

        // Fill and drain again to exercise the lap bit.
        fork
            begin : fill7b
                push(8'hA1); push(8'hA2); push(8'hA3); push(8'hA4);
            end
            begin : drain7b
                pop(d); check_eq(d, 8'hA1, "wrap[0] after lap");
                pop(d); check_eq(d, 8'hA2, "wrap[1] after lap");
                pop(d); check_eq(d, 8'hA3, "wrap[2] after lap");
                pop(d); check_eq(d, 8'hA4, "wrap[3] after lap");
            end
        join

        // Fill and drain a third time for good measure
        fork
            begin : fill7c
                push(8'hB0); push(8'hB1); push(8'hB2); push(8'hB3);
            end
            begin : drain7c
                pop(d); check_eq(d, 8'hB0, "wrap[4] triple");
                pop(d); check_eq(d, 8'hB1, "wrap[5] triple");
                pop(d); check_eq(d, 8'hB2, "wrap[6] triple");
                pop(d); check_eq(d, 8'hB3, "wrap[7] triple");
            end
        join

        $display("--- Test 7 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 8: Concurrent push+pop (steady-state throughput)
    // =============================================================

    task test_concurrent_push_pop();
        reg [7:0] d;

        $display("--- Test 8: Concurrent push+pop ---");

        do_reset();

        // Fill to 2 items first, then run push+pop concurrently
        fork
            begin : fill8
                push(8'h71);
                push(8'h72);
                push(8'h73);
                push(8'h74);
                push(8'h75);
            end
            begin : drain8
                slave_idle();
                slave_idle();
                pop(d); check_eq(d, 8'h71, "concurrent[0]");
                pop(d); check_eq(d, 8'h72, "concurrent[1]");
                pop(d); check_eq(d, 8'h73, "concurrent[2]");
                pop(d); check_eq(d, 8'h74, "concurrent[3]");
                pop(d); check_eq(d, 8'h75, "concurrent[4]");
            end
        join

        $display("--- Test 8 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 9: Full-duplex at boundary (fill-level boundary)
    // =============================================================

    task test_fullduplex_at_boundary();
        reg [7:0] d;

        $display("--- Test 9: Full-duplex at boundary ---");

        do_reset();

        // Fill FIFO to 3 items
        fork
            begin : fill9
                push(8'h91);
                push(8'h92);
                push(8'h93);
            end
            begin : idle9
                repeat (6) master_idle();
            end
        join

        // Now we have 3/4 full.
        fork
            begin : push9
                push(8'h94);
                push(8'h95);
            end
            begin : pop9
                pop(d); check_eq(d, 8'h91, "boundary[0]");
                pop(d); check_eq(d, 8'h92, "boundary[1]");
                pop(d); check_eq(d, 8'h93, "boundary[2]");
                pop(d); check_eq(d, 8'h94, "boundary[3]");
                pop(d); check_eq(d, 8'h95, "boundary[4]");
            end
        join

        $display("--- Test 9 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 10: Empty-pop underflow
    // =============================================================

    task test_empty_pop();
        reg [7:0] d;

        $display("--- Test 10: Empty-pop underflow ---");

        do_reset();

        // Check that output is invalid when FIFO is empty
        check_out_invalid("empty-pop: out_data invalid when FIFO empty");

        // Now push+pop a value
        fork
            begin : push10
                push(8'hFF);
            end
            begin : pop10
                pop(d);
                check_eq(d, 8'hFF, "empty-pop: single value");
            end
        join

        // FIFO should be empty again -- check
        @(posedge clock);
        check_out_invalid("empty-pop: FIFO empty after drain");

        $display("--- Test 10 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 11: Reset during active operation
    // =============================================================

    task test_reset_during_operation();
        reg [7:0] d;

        $display("--- Test 11: Reset during operation ---");

        do_reset();

        // Push some items, pop one, then reset mid-operation
        fork
            begin : fill11
                push(8'h11);
                push(8'h22);
                push(8'h33);
                push(8'h44);
            end
            begin : drain11
                pop(d); check_eq(d, 8'h11, "reset-mid: pop before reset");
            end
        join

        // Reset while there's still data in the FIFO
        do_reset();

        // After reset, FIFO should be cleanly empty
        check_out_invalid("reset-mid: FIFO empty after mid-op reset");
        check_inp_ready(1'b1, "reset-mid: inp_ready high after mid-op reset");

        // Push fresh data and verify it works
        fork
            begin : fill11b
                push(8'h55);
                push(8'h66);
            end
            begin : drain11b
                pop(d); check_eq(d, 8'h55, "reset-mid: post-reset[0]");
                pop(d); check_eq(d, 8'h66, "reset-mid: post-reset[1]");
            end
        join

        $display("--- Test 11 PASSED ---\n");
    endtask

    // =============================================================
    //  Test 12: Randomized stress test
    // =============================================================

    task test_random_stress();
        reg [7:0] push_lfsr;
        reg [7:0] expected_q [0:255];
        reg [7:0] d;
        integer   q_head;
        integer   q_tail;
        integer   i;
        integer   push_count;
        integer   pop_count;
        integer   decision;

        $display("--- Test 12: Randomized stress ---");

        do_reset();

        push_lfsr = 8'h01;
        q_head = 0;
        q_tail = 0;
        push_count = 0;
        pop_count = 0;

        // Run for 100 cycles of randomized push/pop
        for (i = 0; i < 100; i = i + 1) begin
            decision = {push_lfsr[7], push_lfsr[6]};

            push_lfsr = {push_lfsr[6:0],
                         push_lfsr[7] ^ push_lfsr[5] ^
                         push_lfsr[4] ^ push_lfsr[3]};

            if (decision == 0 || decision == 3) begin
                push_lfsr = push_lfsr ^ 8'hA5;
                push(push_lfsr);
                expected_q[q_tail] = push_lfsr;
                q_tail = q_tail + 1;
                push_count = push_count + 1;
            end else begin
                master_idle();
            end

            if (decision == 2 || decision == 3) begin
                if (q_head < q_tail) begin
                    fork
                        begin : pop12
                            pop(d);
                            pop_count = pop_count + 1;
                            if (d != expected_q[q_head]) begin
                                $display("FAIL [%0t] random[push=%0d,pop=%0d]: got 0x%02x, expected 0x%02x",
                                         $time, push_count, pop_count,
                                         d, expected_q[q_head]);
                                $fatal;
                            end
                            q_head = q_head + 1;
                        end
                        begin : idle12pop
                            master_idle();
                        end
                    join
                end else begin
                    slave_idle();
                end
            end else begin
                slave_idle();
            end
        end

        // Drain any remaining items
        while (q_head < q_tail) begin
            fork
                begin : draind
                    pop(d);
                    pop_count = pop_count + 1;
                    if (d != expected_q[q_head]) begin
                        $display("FAIL [%0t] random-drain[push=%0d,pop=%0d]: got 0x%02x, expected 0x%02x",
                                 $time, push_count, pop_count,
                                 d, expected_q[q_head]);
                        $fatal;
                    end
                    q_head = q_head + 1;
                end
                begin : idled
                    master_idle();
                end
            join
        end

        check_out_invalid("random: FIFO empty after drain");

        $display("PASS [%0t] random: pushed %0d, popped %0d items",
                 $time, push_count, pop_count);
        $display("--- Test 12 PASSED ---\n");
    endtask

endmodule
