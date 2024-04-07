.text
.globl add_A
add_A:
addi sp, sp, -20
sw fp, -4(sp)
mv fp, sp
sw s1, -8(fp)
sw s2, -12(fp)
sw s3, -16(fp)
sw ra, -20(fp)
addi s1, zero, 100
sub s2, s1, a0
slt s3, s2, a1
beq s3, zero, .L3
add s1, a0, a1
mv s1, s1
j .L2
.L3:
li s1, 0
.L2:
mv a0, s1
lw s1, -8(fp)
lw s2, -12(fp)
lw s3, -16(fp)
lw ra, -20(fp)
addi sp, sp, 20
lw fp, -4(sp)
jr ra

