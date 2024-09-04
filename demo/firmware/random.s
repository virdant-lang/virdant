.section .text
.global _start

# Memory address where the random number will be written
ADDRESS = 0x100000

_start:
    # Initialize a seed for the LFSR; any non-zero value works
    li t0, 0xACE1        # Seed value (can be any non-zero value)
    li t4, ADDRESS
    li t2, 1010

loop:
    # LFSR random number generation
    # t0 is the LFSR state, generating pseudo-random values
    slli t2, t2, 1
    srli t3, t2, 11
    or t2, t2, t3
    # Write the random number to the memory address
    sb t2, 0(t4)

    call wait
    # Jump back to loop
    j loop

wait:
    li x31, 10000
.loop2:
    addi x31, x31, -1
    bnez x31, .loop2
    ret
