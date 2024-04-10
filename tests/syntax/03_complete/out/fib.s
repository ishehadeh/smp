.text
.globl fib
fib:
addi sp, sp, -64
sd fp, 0(sp)
addi fp, sp, 64
sd s1, -24(fp)
sd s2, -32(fp)
sd s3, -40(fp)
sd s4, -48(fp)
sd ra, -56(fp)
addi s1, zero, 1
slt s2, s1, a0
beq s2, zero, .L2
sd a0, -8(fp)
addi s1, zero, 2
sub s2, a0, s1
mv a0, s2
jal fib
mv s2, a0
ld a0, -8(fp)
sd a0, -16(fp)
addi s1, zero, 1
sub s3, a0, s1
mv a0, s3
jal fib
mv s3, a0
ld a0, -16(fp)
add s4, s2, s3
mv s1, s4
j .L1
.L2:
mv s1, a0
.L1:
mv a0, s1
ld s1, -24(fp)
ld s2, -32(fp)
ld s3, -40(fp)
ld s4, -48(fp)
ld ra, -56(fp)
ld fp, 0(sp)
addi sp, sp, 64
jr ra

