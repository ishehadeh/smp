.text
.globl main
main:
addi sp, sp, -20
sw fp, -4(sp)
mv fp, sp
sw s1, -8(fp)
sw s2, -12(fp)
sw s3, -16(fp)
sw ra, -20(fp)
addi s1, zero, 1
beq s1, zero, .L1
addi s1, zero, 1
addi s2, zero, 1
add s3, s1, s2
mv s1, s3
j .L0
.L1:
addi s1, zero, 1
addi s2, zero, 1
sub s3, s1, s2
mv s1, s3
.L0:
mv a0, s1
lw s1, -8(fp)
lw s2, -12(fp)
lw s3, -16(fp)
lw ra, -20(fp)
addi sp, sp, 20
lw fp, -4(sp)
jr ra

