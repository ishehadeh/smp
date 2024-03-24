.text
.globl main
main:
li s1, 1
beqz .L1, s1
li s3, 1
li s4, 1
add s5, s3, s4
j .L0
mv s2, s5
.L1:
li s4, 1
li s6, 1
sub s7, s4, s6
mv s2, s7
.L0:
mv a0, s2
ret

