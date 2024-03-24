.text
.globl get_number
get_number:
li a0, 4
ret
.globl main
main:
jal get_number
li s1, 5
slt s2, a0, s1
beqz .L1, s2
j .L0
li s1, 1
.L1:
li s1, 0
.L0:
mv a0, s1
ret

