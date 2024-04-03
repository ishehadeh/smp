.text
.globl main
main:
addi sp, sp, -8
sw ra, 0(sp)
sw fp, 4(sp)
lw ra, 0(sp)
lw fp, 4(sp)
addi sp, sp, 8
jr ra

