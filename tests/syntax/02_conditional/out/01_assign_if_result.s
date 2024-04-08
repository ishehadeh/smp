.text
.globl get_number
get_number:
addi sp, sp, -16
sd fp, 0(sp)
addi fp, sp, 16
sd ra, -8(fp)
addi a0, zero, 4
ld ra, -8(fp)
ld fp, 0(sp)
addi sp, sp, 16
jr ra
.globl main
main:
addi sp, sp, -24
sd fp, 0(sp)
addi fp, sp, 24
sd s1, -8(fp)
sd ra, -16(fp)
jal get_number
addi s1, zero, 5
slt a0, a0, s1
beq a0, zero, .L2
li s1, 0
j .L1
.L2:
li s1, 0
.L1:
mv a0, s1
ld s1, -8(fp)
ld ra, -16(fp)
ld fp, 0(sp)
addi sp, sp, 24
jr ra

