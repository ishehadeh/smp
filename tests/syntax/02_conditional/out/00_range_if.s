.text
.globl big_number
big_number:
addi sp, sp, -8
sw fp, -4(sp)
mv fp, sp
sw ra, -8(fp)
addi a0, zero, 150
lw ra, -8(fp)
addi sp, sp, 8
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
call big_number
addi s1, zero, 100
slt s2, a0, s1
beq s2, zero, .L2
mv s1, a0
j .L1
.L2:
li s1, 0
.L1:
mv a0, s1
lw s1, -8(fp)
lw s2, -12(fp)
lw ra, -16(fp)
addi sp, sp, 16
lw fp, -4(sp)
jr ra

