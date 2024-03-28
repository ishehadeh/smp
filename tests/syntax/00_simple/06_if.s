.text
.globl main
main:
addi sp, sp, -20
sw s1, 0(sp)
sw s2, 4(sp)
sw s3, 8(sp)
sw ra, 12(sp)
sw fp, 16(sp)
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
lw s1, 0(sp)
lw s2, 4(sp)
lw s3, 8(sp)
lw ra, 12(sp)
lw fp, 16(sp)
addi sp, sp, 20
jr ra

