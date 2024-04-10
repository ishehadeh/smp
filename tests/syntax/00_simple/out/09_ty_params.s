.text
.globl main
main:
addi sp, sp, -16
sd fp, 0(sp)
addi fp, sp, 16
sd ra, -8(fp)
addi a0, zero, 0
ld ra, -8(fp)
ld fp, 0(sp)
addi sp, sp, 16
jr ra

