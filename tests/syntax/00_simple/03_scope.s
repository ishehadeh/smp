.text
.globl main
main:
addi sp, sp, -4
sw fp, 0(sp)
li a1, 1
li a2, 2
add a3, a2, a2
lw fp, 0(sp)
addi sp, sp, 4
jr ra

