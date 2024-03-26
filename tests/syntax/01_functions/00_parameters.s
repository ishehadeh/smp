.text
.globl main
main:
addi sp, sp, -8
sw ra, 0(sp)
sw fp, 4(sp)
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
lw ra, 0(sp)
lw fp, 4(sp)
addi sp, sp, 8
jr ra
.globl __libc_start_main
__libc_start_main:
addi sp, sp, -8
sw ra, 0(sp)
sw fp, 4(sp)
call main
lw ra, 0(sp)
lw fp, 4(sp)
addi sp, sp, 8
jr ra
.globl many_many_params
many_many_params:
addi sp, sp, -32
sw s4, 0(sp)
sw s5, 4(sp)
sw s1, 8(sp)
sw s3, 12(sp)
sw s2, 16(sp)
sw s6, 20(sp)
sw ra, 24(sp)
sw fp, 28(sp)
add s1, a5, a6
add s2, a4, s1
add s3, a3, s2
add s4, a2, s3
add s5, a1, s4
add s6, a0, s5
mv a0, s6
lw s4, 0(sp)
lw s5, 4(sp)
lw s1, 8(sp)
lw s3, 12(sp)
lw s2, 16(sp)
lw s6, 20(sp)
lw ra, 24(sp)
lw fp, 28(sp)
addi sp, sp, 32
jr ra

