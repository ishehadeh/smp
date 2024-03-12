.text
.globl add
add:
addi sp, sp, -28
sw fp, 24(sp)
sub a4, a0, a1
add a5, a0, a4
mv a0, a5
lw fp, 24(sp)
addi sp, sp, 28
jr ra

