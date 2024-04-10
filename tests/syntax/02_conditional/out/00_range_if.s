.text
.globl big_number
big_number:
addi sp, sp, -16
sd fp, 0(sp)
addi fp, sp, 16
sd ra, -8(fp)
addi a0, zero, 150
ld ra, -8(fp)
ld fp, 0(sp)
addi sp, sp, 16
jr ra
.globl main
main:
addi sp, sp, -32
sd fp, 0(sp)
addi fp, sp, 32
sd s1, -8(fp)
sd s2, -16(fp)
sd ra, -24(fp)
jal big_number
addi s1, zero, 100
slt s2, a0, s1
beq s2, zero, .L2
mv s1, a0
j .L1
.L2:
li s1, 0
.L1:
mv a0, s1
ld s1, -8(fp)
ld s2, -16(fp)
ld ra, -24(fp)
ld fp, 0(sp)
addi sp, sp, 32
jr ra

