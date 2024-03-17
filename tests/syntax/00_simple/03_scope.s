.text
.globl main
main:
addi sp, sp, -20
sw fp, 16(sp)
li a1, 1
li a2, 2
add a3, a2, a2
lw fp, 16(sp)
addi sp, sp, 20
jr ra

