.text
.globl add
add:
addi sp, sp, -16
sw fp, -4(sp)
mv fp, sp
sw s1, -8(fp)
sw s2, -12(fp)
sw ra, -16(fp)
sub s1, a1, a0
add s2, a0, s1
mv a0, s2
lw s1, -8(fp)
lw s2, -12(fp)
lw ra, -16(fp)
addi sp, sp, 16
lw fp, -4(sp)
jr ra

