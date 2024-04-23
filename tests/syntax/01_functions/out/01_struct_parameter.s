.text
.globl add_tuple
add_tuple:
addi sp, sp, -40
sd fp, 0(sp)
addi fp, sp, 40
sd s1, -8(fp)
sd s2, -16(fp)
sd s3, -24(fp)
sd ra, -32(fp)
ld s1, 0(a0)
ld s2, 4(a0)
add s3, s1, s2
mv a0, s3
ld s1, -8(fp)
ld s2, -16(fp)
ld s3, -24(fp)
ld ra, -32(fp)
ld fp, 0(sp)
addi sp, sp, 40
jr ra
.globl main
main:
addi sp, sp, -32
sd fp, 0(sp)
addi fp, sp, 32
sd s1, -8(fp)
sd s2, -16(fp)
sd ra, -24(fp)
li s1, 0
li s2, 0
addi a0, sp, 0
sd s2, 4(a0)
sd s1, 0(a0)
jal add_tuple
mv a0, a0
ld s1, -8(fp)
ld s2, -16(fp)
ld ra, -24(fp)
ld fp, 0(sp)
addi sp, sp, 32
jr ra

