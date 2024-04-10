.text
.globl add
add:
addi sp, sp, -32
sd fp, 0(sp)
addi fp, sp, 32
sd s1, -8(fp)
sd s2, -16(fp)
sd ra, -24(fp)
sub s1, a1, a0
add s2, a0, s1
mv a0, s2
ld s1, -8(fp)
ld s2, -16(fp)
ld ra, -24(fp)
ld fp, 0(sp)
addi sp, sp, 32
jr ra

