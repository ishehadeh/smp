.text
.globl add_A
add_A:
addi sp, sp, -20
sw s1, 0(sp)
sw s3, 4(sp)
sw s2, 8(sp)
sw ra, 12(sp)
sw fp, 16(sp)
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
lw s1, 0(sp)
lw s3, 4(sp)
lw s2, 8(sp)
lw ra, 12(sp)
lw fp, 16(sp)
addi sp, sp, 20
jr ra

