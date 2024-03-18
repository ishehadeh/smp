.text
.globl main
main:
addi sp, sp, -4
sw fp, 0(sp)
li a1, 0
li a2, 5
lw fp, 0(sp)
addi sp, sp, 4
jr ra

