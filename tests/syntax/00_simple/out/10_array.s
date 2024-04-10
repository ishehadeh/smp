.text
.globl main
main:
addi sp, sp, -60
sd fp, 0(sp)
addi fp, sp, 60
sd s1, -28(fp)
sd s2, -36(fp)
sd s3, -44(fp)
sd ra, -52(fp)
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
ld s1, -20(t0)
ld s2, -20(t1)
add s3, s1, s2
mv a0, s3
ld s1, -28(fp)
ld s2, -36(fp)
ld s3, -44(fp)
ld ra, -52(fp)
ld fp, 0(sp)
addi sp, sp, 60
jr ra

