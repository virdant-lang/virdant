.section .text
.global _start



_start:
    la t0, msg
    li t1, 0x400000

.loop:
    lb t2, 0(t0)
    beqz t2, _start

    sb t2, 0(t1)
    addi t0, t0, 1
    call wait
    j .loop

wait:
    li t4, 1000
.loop2:
    addi t4, t4, -1
    bnez t4, .loop2
    ret

msg:
    .asciz "I love my Wifey!\n"
