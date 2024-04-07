.text
.globl main
main:
addi sp, sp, -8
sw fp, -4(sp)
mv fp, sp
sw ra, -8(fp)
lw ra, -8(fp)
addi sp, sp, 8
lw fp, -4(sp)
jr ra

