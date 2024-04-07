.text
.globl add_tuple
add_tuple:
addi sp, sp, -20
sw fp, -4(sp)
mv fp, sp
sw s1, -8(fp)
sw s2, -12(fp)
sw s3, -16(fp)
sw ra, -20(fp)
lw s1, 0(a0)
lw s2, 4(a0)
add s3, s1, s2
mv a0, s3
lw s1, -8(fp)
lw s2, -12(fp)
lw s3, -16(fp)
lw ra, -20(fp)
addi sp, sp, 20
lw fp, -4(sp)
jr ra
.globl main
main:
addi sp, sp, -16
sw fp, -4(sp)
mv fp, sp
sw s1, -8(fp)
sw s2, -12(fp)
sw ra, -16(fp)
li s1, 1
li s2, 2
addi a0, sp, 0
sw s1, 0(a0)
sw s2, 4(a0)
call add_tuple
mv a0, a0
lw s1, -8(fp)
lw s2, -12(fp)
lw ra, -16(fp)
addi sp, sp, 16
lw fp, -4(sp)
jr ra

