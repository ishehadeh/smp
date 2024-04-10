.text
.globl main
main:
addi sp, sp, -16
sd fp, 0(sp)
addi fp, sp, 16
sd ra, -8(fp)
addi a0, zero, 0
addi a1, zero, 1
addi a2, zero, 1
addi a3, zero, 0
addi a4, zero, 1
addi a5, zero, 1
addi a6, zero, 0
addi a7, zero, 1
jal many_many_params
mv a0, a0
ld ra, -8(fp)
ld fp, 0(sp)
addi sp, sp, 16
jr ra
.globl __libc_start_main
__libc_start_main:
addi sp, sp, -16
sd fp, 0(sp)
addi fp, sp, 16
sd ra, -8(fp)
jal main
ld ra, -8(fp)
ld fp, 0(sp)
addi sp, sp, 16
jr ra
.globl many_many_params
many_many_params:
addi sp, sp, -64
sd fp, 0(sp)
addi fp, sp, 64
sd s1, -8(fp)
sd s2, -16(fp)
sd s3, -24(fp)
sd s4, -32(fp)
sd s5, -40(fp)
sd s6, -48(fp)
sd ra, -56(fp)
add s1, a5, a6
add s2, a4, s1
add s3, a3, s2
add s4, a2, s3
add s5, a1, s4
add s6, a0, s5
mv a0, s6
ld s1, -8(fp)
ld s2, -16(fp)
ld s3, -24(fp)
ld s4, -32(fp)
ld s5, -40(fp)
ld s6, -48(fp)
ld ra, -56(fp)
ld fp, 0(sp)
addi sp, sp, 64
jr ra

