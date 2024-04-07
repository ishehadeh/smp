.text
.globl main
main:
addi sp, sp, -40
sw fp, -24(sp)
mv fp, sp
sw s1, -28(fp)
sw s2, -32(fp)
sw s3, -36(fp)
sw ra, -40(fp)
li t0, 0
sb t0, -20(fp)
li t0, 0
sb t0, -19(fp)
li t1, 1
sb t1, -16(fp)
li t1, 0
sb t1, -15(fp)
li t2, 2
sb t2, -12(fp)
li t2, 0
sb t2, -11(fp)
li t3, 3
sb t3, -8(fp)
li t3, 0
sb t3, -7(fp)
li s1, 4
sb s1, -4(fp)
li s1, 0
sb s1, -3(fp)
addi t0, zero, 0
li t1, 4
mul t0, t0, t1
add t0, t0, fp
addi t1, zero, 4
li t2, 4
mul t1, t1, t2
add t1, t1, fp
lw s1, -20(t0)
lw s2, -20(t1)
add s3, s1, s2
mv a0, s3
lw s1, -28(fp)
lw s2, -32(fp)
lw s3, -36(fp)
lw ra, -40(fp)
addi sp, sp, 40
lw fp, -24(sp)
jr ra

