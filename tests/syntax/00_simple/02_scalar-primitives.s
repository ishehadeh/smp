.text
.globl main
main:
addi sp, sp, -16
sw fp, 12(sp)
li a1, 0
li a2, 5
lw fp, 12(sp)
addi sp, sp, 16
jr ra

