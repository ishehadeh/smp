.text
.globl __libc_start_main
__libc_start_main:
addi sp, sp, -8
sw ra, 0(sp)
sw fp, 4(sp)
call main
call __hw_breakpoint
lw ra, 0(sp)
lw fp, 4(sp)
addi sp, sp, 8
jr ra
.globl add_tuple
add_tuple:
addi sp, sp, -20
sw s1, 0(sp)
sw s2, 4(sp)
sw s3, 8(sp)
sw ra, 12(sp)
sw fp, 16(sp)
lw s1, 0(a0)
lw s2, 4(a0)
add s3, s1, s2
mv a0, s3
lw s1, 0(sp)
lw s2, 4(sp)
lw s3, 8(sp)
lw ra, 12(sp)
lw fp, 16(sp)
addi sp, sp, 20
jr ra
.globl main
main:
addi sp, sp, -16
sw s1, 0(sp)
sw s2, 4(sp)
sw ra, 8(sp)
sw fp, 12(sp)
li s1, 1
li s2, 2
addi a0, fp, 0
sw s1, 0(a0)
sw s2, 4(a0)
call add_tuple
mv a0, a0
lw s1, 0(sp)
lw s2, 4(sp)
lw ra, 8(sp)
lw fp, 12(sp)
addi sp, sp, 16
jr ra

