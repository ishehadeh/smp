.text
.globl get_number
get_number:
addi sp, sp, -8
sw fp, -4(sp)
mv fp, sp
sw ra, -8(fp)
addi a0, zero, 4
lw ra, -8(fp)
addi sp, sp, 8
lw fp, -4(sp)
jr ra
.globl main
main:
addi sp, sp, -12
sw fp, -4(sp)
mv fp, sp
sw s1, -8(fp)
sw ra, -12(fp)
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
lw s1, -8(fp)
lw ra, -12(fp)
addi sp, sp, 12
lw fp, -4(sp)
jr ra

