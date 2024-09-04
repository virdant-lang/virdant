.section .text
.global _start

_start:
    li x4, 'A'
    li x2, 0x400000

.loop:
    sw x4, 0(x2)
    addi x4, x4, 1
    call wait
    j .loop

wait:
    li x3, 10000
.loop2:
    addi x3, x3, -1
    bnez x3, .loop2
    ret
