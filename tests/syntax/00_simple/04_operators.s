.text
.globl main
main:
addi sp, sp, -20
sw s2, 0(sp)
sw s3, 4(sp)
sw s1, 8(sp)
sw ra, 12(sp)
sw fp, 16(sp)
addi s1, zero, 1
addi s2, zero, 1
add s3, s1, s2
addi s1, zero, 2
addi s2, zero, 1
div s3, s1, s2
addi s1, zero, 2
addi s2, zero, 1
sub s3, s1, s2
addi s1, zero, 2
addi s2, zero, 1
mul s3, s1, s2
addi s1, zero, 1
addi s2, zero, 2
slt s3, s1, s2
addi s1, zero, 1
addi s2, zero, 1
sub s3, s1, s2
snez s3, s3
addi s1, zero, 1
addi s2, zero, 1
sub s3, s1, s2
snez s3, s3
mv a0, s3
lw s2, 0(sp)
lw s3, 4(sp)
lw s1, 8(sp)
lw ra, 12(sp)
lw fp, 16(sp)
addi sp, sp, 20
jr ra

