.text
.globl main
main:
addi sp, sp, -40
sw s1, 24(sp)
sw s2, 28(sp)
sw ra, 32(sp)
sw fp, 36(sp)
li t0, 0
sb t0, 4(sp)
li t0, 0
sb t0, 5(sp)
li t1, 1
sb t1, 8(sp)
li t1, 0
sb t1, 9(sp)
li t2, 2
sb t2, 12(sp)
li t2, 0
sb t2, 13(sp)
li t3, 3
sb t3, 16(sp)
li t3, 0
sb t3, 17(sp)
li s1, 4
sb s1, 20(sp)
li s1, 0
sb s1, 21(sp)
li s2, 5
sb s2, 24(sp)
li s2, 0
sb s2, 25(sp)
lw a0, 0(sp)
lw s1, 24(sp)
lw s2, 28(sp)
lw ra, 32(sp)
lw fp, 36(sp)
addi sp, sp, 40
jr ra

