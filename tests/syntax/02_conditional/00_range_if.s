.text
big_number:
main:
jal big_number
li s1, 100
slt a0, a0, s1
beqz .L1, a0
j .L0
.L1:
.L0:

