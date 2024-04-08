.text
.globl main
main:
addi sp, sp, -32
sw fp, -12(sp)
mv fp, sp
sw s1, -16(fp)
sw s2, -20(fp)
sw s3, -24(fp)
sw s4, -28(fp)
sw ra, -32(fp)
li t0, 0
sb t0, -8(fp)
li t0, 0
sb t0, -7(fp)
li t1, 1
sb t1, -4(fp)
li t1, 0
sb t1, -3(fp)
.L0:
addi t0, zero, 0
li t1, 4
mul t0, t0, t1
add t0, t0, fp
addi t1, zero, 1
li t2, 4
mul t1, t1, t2
add t1, t1, fp
lw s1, -8(t0)
lw s2, -8(t1)
add s3, s1, s2
addi t0, zero, 1
li t1, 4
mul t0, t0, t1
add t0, t0, fp
addi t0, zero, 0
li t1, 4
mul t0, t0, t1
add t0, t0, fp
lw t2, -8(t0)
sw t2, -8(t0)
addi t0, zero, 1
li t1, 4
mul t0, t0, t1
add t0, t0, fp
sw s3, -8(t0)
addi s1, zero, 0
addi s2, zero, 1
add s4, s1, s2
addi s1, zero, 0
addi s2, zero, 10
slt s3, s1, s2
beq s3, zero, .L0
lw s1, -16(fp)
lw s2, -20(fp)
lw s3, -24(fp)
lw s4, -28(fp)
lw ra, -32(fp)
addi sp, sp, 32
lw fp, -12(sp)
jr ra

