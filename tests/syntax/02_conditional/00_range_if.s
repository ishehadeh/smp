.text
big_number:
li a0, 150
ret
main:
jal big_number
li s1, 100
slt a0, a0, s1
beqz .L1, a0
j .L0
mv s1, a0
.L1:
li s1, 0
.L0:
mv a0, s1
ret

