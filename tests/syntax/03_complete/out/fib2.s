.text
.globl fib_loop
fib_loop:
addi sp, sp, -56
sd fp, 0(sp)
addi fp, sp, 56
sd s1, -16(fp)
sd s2, -24(fp)
sd s3, -32(fp)
sd s4, -40(fp)
sd ra, -48(fp)
li t0, 0
sb t0, -8(fp)
li t0, 0
sb t0, -7(fp)
li t1, 1
sb t1, -4(fp)
li t1, 0
sb t1, -3(fp)
.L0:
addi t0, zero, 0
li t1, 4
mul t0, t0, t1
add t0, t0, fp
addi t1, zero, 1
li t2, 4
mul t1, t1, t2
add t1, t1, fp
ld s1, -8(t0)
ld s2, -8(t1)
add s3, s1, s2
addi t0, zero, 1
li t1, 4
mul t0, t0, t1
add t0, t0, fp
addi t0, zero, 0
li t1, 4
mul t0, t0, t1
add t0, t0, fp
ld t2, -8(t0)
sd t2, -8(t0)
addi t0, zero, 1
li t1, 4
mul t0, t0, t1
add t0, t0, fp
sd s3, -8(t0)
addi s1, zero, 0
addi s2, zero, 1
add s4, s1, s2
addi s1, zero, 0
addi s2, zero, 10
slt s3, s1, s2
beq s3, zero, .L0
addi a0, zero, 0
ld s1, -16(fp)
ld s2, -24(fp)
ld s3, -32(fp)
ld s4, -40(fp)
ld ra, -48(fp)
ld fp, 0(sp)
addi sp, sp, 56
jr ra
.globl fib_rec
fib_rec:
addi sp, sp, -64
sd fp, 0(sp)
addi fp, sp, 64
sd s1, -24(fp)
sd s2, -32(fp)
sd s3, -40(fp)
sd s4, -48(fp)
sd ra, -56(fp)
addi s1, zero, 2
slt s2, a0, s1
beq s2, zero, .L11
mv s1, a0
j .L10
.L11:
sd a0, -8(fp)
addi s1, zero, 2
sub s2, a0, s1
mv a0, s2
jal fib_rec
mv s2, a0
ld a0, -8(fp)
sd a0, -16(fp)
addi s1, zero, 1
sub s3, a0, s1
mv a0, s3
jal fib_rec
mv s3, a0
ld a0, -16(fp)
add s4, s2, s3
mv s1, s4
.L10:
mv a0, s1
ld s1, -24(fp)
ld s2, -32(fp)
ld s3, -40(fp)
ld s4, -48(fp)
ld ra, -56(fp)
ld fp, 0(sp)
addi sp, sp, 64
jr ra
.globl main
main:
addi sp, sp, -40
sd fp, 0(sp)
addi fp, sp, 40
sd s1, -16(fp)
sd s4, -24(fp)
sd ra, -32(fp)
addi a0, zero, 10
jal fib_rec
sd a0, -8(fp)
addi a0, zero, 10
jal fib_loop
mv s4, a0
ld a0, -8(fp)
sub s1, a0, s4
snez s1, s1
mv a0, s1
ld s1, -16(fp)
ld s4, -24(fp)
ld ra, -32(fp)
ld fp, 0(sp)
addi sp, sp, 40
jr ra

