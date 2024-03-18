.text
.globl main
main:
addi sp, sp, -4
sw fp, 0(sp)
lw fp, 0(sp)
addi sp, sp, 4
jr ra

