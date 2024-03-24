.text
get_number:
main:
jal get_number
li s1, 5
slt a0, a0, s1
beqz .L1, a0
j .L0
li s1, 1
.L1:
li s1, 0
.L0:

