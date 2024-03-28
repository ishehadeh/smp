.text
.globl add
add:
addi sp, sp, -16
sw s2, 0(sp)
sw s1, 4(sp)
sw ra, 8(sp)
sw fp, 12(sp)
sub s1, a1, a0
add s2, a0, s1
mv a0, s2
lw s2, 0(sp)
lw s1, 4(sp)
lw ra, 8(sp)
lw fp, 12(sp)
addi sp, sp, 16
jr ra

