.text
.globl get_number
get_number:
addi sp, sp, -8
sw ra, 0(sp)
sw fp, 4(sp)
addi a0, zero, 4
lw ra, 0(sp)
lw fp, 4(sp)
addi sp, sp, 8
jr ra
.globl main
main:
addi sp, sp, -12
sw s1, 0(sp)
sw ra, 4(sp)
sw fp, 8(sp)
call get_number
addi s1, zero, 5
slt a0, a0, s1
beq a0, zero, .L2
li s1, 1
j .L1
.L2:
li s1, 0
.L1:
mv a0, s1
lw s1, 0(sp)
lw ra, 4(sp)
lw fp, 8(sp)
addi sp, sp, 12
jr ra

