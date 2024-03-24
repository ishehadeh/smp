.text
.globl big_number
big_number:
li a0, 150
ret
.globl main
main:
jal big_number
li s1, 100
slt s2, a0, s1
beqz .L1, s2
j .L0
mv s1, a0
.L1:
li s1, 0
.L0:
mv a0, s1
ret

