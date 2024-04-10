.text
.globl add_A
add_A:
addi sp, sp, -40
sd fp, 0(sp)
addi fp, sp, 40
sd s1, -8(fp)
sd s2, -16(fp)
sd s3, -24(fp)
sd ra, -32(fp)
addi s1, zero, 100
sub s2, s1, a0
slt s3, s2, a1
beq s3, zero, .L3
add s1, a0, a1
mv s1, s1
j .L2
.L3:
li s1, 0
.L2:
mv a0, s1
ld s1, -8(fp)
ld s2, -16(fp)
ld s3, -24(fp)
ld ra, -32(fp)
ld fp, 0(sp)
addi sp, sp, 40
jr ra

