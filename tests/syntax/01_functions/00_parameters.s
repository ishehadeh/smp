.text
.globl __libc_start_main
__libc_start_main:
addi sp, sp, -4
sw fp, 0(sp)
jal zero,main
lw fp, 0(sp)
addi sp, sp, 4
jr ra
.globl many_many_params
many_many_params:
addi sp, sp, -16
sw fp, 12(sp)
sw s7, 8(sp)
sw s6, 4(sp)
sw s5, 0(sp)
add s10, a5, a6
add s9, a4, s10
add s8, a3, s9
add s7, a2, s8
add s6, a1, s7
add s5, a0, s6
mv a0, s5
lw fp, 12(sp)
lw s7, 8(sp)
lw s6, 4(sp)
lw s5, 0(sp)
addi sp, sp, 16
jr ra
.globl main
main:
addi sp, sp, -4
sw fp, 0(sp)
li a1, 0
li a2, 1
li a3, 1
li a4, 0
li a5, 1
li a6, 1
li a7, 0
li s11, 1
mv a0, a1
mv a1, a2
mv a2, a3
mv a3, a4
mv a4, a5
mv a5, a6
mv a6, a7
mv a7, s11
jal zero,many_many_params
lw fp, 0(sp)
addi sp, sp, 4
jr ra

