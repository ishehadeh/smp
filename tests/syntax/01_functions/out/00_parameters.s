.text
.globl main
main:
addi sp, sp, -8
sw fp, -4(sp)
mv fp, sp
sw ra, -8(fp)
addi a0, zero, 0
addi a1, zero, 1
addi a2, zero, 1
addi a3, zero, 0
addi a4, zero, 1
addi a5, zero, 1
addi a6, zero, 0
addi a7, zero, 1
call many_many_params
mv a0, a0
lw ra, -8(fp)
addi sp, sp, 8
lw fp, -4(sp)
jr ra
.globl __libc_start_main
__libc_start_main:
addi sp, sp, -8
sw fp, -4(sp)
mv fp, sp
sw ra, -8(fp)
call main
lw ra, -8(fp)
addi sp, sp, 8
lw fp, -4(sp)
jr ra
.globl many_many_params
many_many_params:
addi sp, sp, -32
sw fp, -4(sp)
mv fp, sp
sw s1, -8(fp)
sw s2, -12(fp)
sw s3, -16(fp)
sw s4, -20(fp)
sw s5, -24(fp)
sw s6, -28(fp)
sw ra, -32(fp)
add s1, a5, a6
add s2, a4, s1
add s3, a3, s2
add s4, a2, s3
add s5, a1, s4
add s6, a0, s5
mv a0, s6
lw s1, -8(fp)
lw s2, -12(fp)
lw s3, -16(fp)
lw s4, -20(fp)
lw s5, -24(fp)
lw s6, -28(fp)
lw ra, -32(fp)
addi sp, sp, 32
lw fp, -4(sp)
jr ra

