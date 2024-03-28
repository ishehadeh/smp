.text
.globl fib
fib:
addi sp, sp, -32
sw s1, 8(sp)
sw s2, 12(sp)
sw s3, 16(sp)
sw s4, 20(sp)
sw ra, 24(sp)
sw fp, 28(sp)
addi s1, zero, 1
slt s2, s1, a0
beq s2, zero, .L2
sw a0, 0(sp)
addi s1, zero, 2
sub s2, a0, s1
mv a0, s2
call fib
mv s2, a0
lw a0, 0(sp)
sw a0, 4(sp)
addi s1, zero, 1
sub s3, a0, s1
mv a0, s3
call fib
mv s3, a0
lw a0, 4(sp)
add s4, s2, s3
mv s1, s4
j .L1
.L2:
mv s1, a0
.L1:
mv a0, s1
lw s1, 8(sp)
lw s2, 12(sp)
lw s3, 16(sp)
lw s4, 20(sp)
lw ra, 24(sp)
lw fp, 28(sp)
addi sp, sp, 32
jr ra

