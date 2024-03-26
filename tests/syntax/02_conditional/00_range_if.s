.text
.globl big_number
big_number:
addi sp, sp, -8
sw ra, 0(sp)
sw fp, 4(sp)
addi a0, zero, 150
lw ra, 0(sp)
lw fp, 4(sp)
addi sp, sp, 8
jr ra
.globl main
main:
addi sp, sp, -16
sw s1, 0(sp)
sw s2, 4(sp)
sw ra, 8(sp)
sw fp, 12(sp)
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
lw s1, 0(sp)
lw s2, 4(sp)
lw ra, 8(sp)
lw fp, 12(sp)
addi sp, sp, 16
jr ra

