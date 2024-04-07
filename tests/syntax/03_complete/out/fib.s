.text
.globl fib
fib:
addi sp, sp, -32
sw fp, -12(sp)
mv fp, sp
sw s1, -16(fp)
sw s2, -20(fp)
sw s3, -24(fp)
sw s4, -28(fp)
sw ra, -32(fp)
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
lw s1, -16(fp)
lw s2, -20(fp)
lw s3, -24(fp)
lw s4, -28(fp)
lw ra, -32(fp)
addi sp, sp, 32
lw fp, -12(sp)
jr ra

