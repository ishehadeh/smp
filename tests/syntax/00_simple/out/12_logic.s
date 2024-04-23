.text
.globl main
main:
addi sp, sp, -72
sd fp, 0(sp)
addi fp, sp, 72
sd s1, -8(fp)
sd s2, -16(fp)
sd s3, -24(fp)
sd s4, -32(fp)
sd s5, -40(fp)
sd s6, -48(fp)
sd s7, -56(fp)
sd ra, -64(fp)
addi s1, zero, 1
addi s2, zero, 1
sub s3, s1, s2
snez s3, s3
addi s1, zero, 1
addi s2, zero, 1
sgt s4, s1, s2
addi s1, zero, 1
addi s2, zero, 1
slt s5, s2, s1
addi s1, zero, 1
addi s2, zero, 1
sgt s6, s2, s1
addi s1, zero, 1
addi s2, zero, 1
slt s7, s1, s2
addi s1, zero, 1
addi s2, zero, 1
sub s8, s1, s2
snez s8, s8
mv a0, s8
ld s1, -8(fp)
ld s2, -16(fp)
ld s3, -24(fp)
ld s4, -32(fp)
ld s5, -40(fp)
ld s6, -48(fp)
ld s7, -56(fp)
ld ra, -64(fp)
ld fp, 0(sp)
addi sp, sp, 72
jr ra

