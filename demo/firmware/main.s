.section .text
.global _start

_start:
    li x4, 0b101
    li x5, 0b110
    li x2, 0x100000

.loop:
    sw x4, 0(x2)
    call wait
    sw x5, 0(x2)
    call wait
    j .loop

wait:
    li x3, 10000
.loop2:
    addi x3, x3, -1
    bnez x3, .loop2
    ret
