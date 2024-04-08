.text
.globl main
main:
addi sp, sp, -40
sd fp, 0(sp)
addi fp, sp, 40
sd s1, -8(fp)
sd s2, -16(fp)
sd s3, -24(fp)
sd ra, -32(fp)
addi s1, zero, 1
beq s1, zero, .L1
addi s1, zero, 1
addi s2, zero, 1
add s3, s1, s2
mv s1, s3
j .L0
.L1:
addi s1, zero, 1
addi s2, zero, 1
sub s3, s1, s2
mv s1, s3
.L0:
mv a0, s1
ld s1, -8(fp)
ld s2, -16(fp)
ld s3, -24(fp)
ld ra, -32(fp)
ld fp, 0(sp)
addi sp, sp, 40
jr ra

