.text
.globl main
main:
addi sp, sp, -20
sw s1, 0(sp)
sw s3, 4(sp)
sw s2, 8(sp)
sw ra, 12(sp)
sw fp, 16(sp)
addi s1, zero, 1
addi s2, zero, 2
add s3, s1, s2
mv a0, s3
lw s1, 0(sp)
lw s3, 4(sp)
lw s2, 8(sp)
lw ra, 12(sp)
lw fp, 16(sp)
addi sp, sp, 20
jr ra

