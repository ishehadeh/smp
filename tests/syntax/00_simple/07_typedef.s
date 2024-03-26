.text
.globl add_A
add_A:
addi sp, sp, -12
sw s1, 0(sp)
sw ra, 4(sp)
sw fp, 8(sp)
add s1, a0, a1
mv a0, s1
lw s1, 0(sp)
lw ra, 4(sp)
lw fp, 8(sp)
addi sp, sp, 12
jr ra

