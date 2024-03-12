.text
.globl main
main:
addi sp, sp, -8
sw fp, 4(sp)
lw fp, 4(sp)
addi sp, sp, 8
jr ra

