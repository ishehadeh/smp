.text
main:
li s1, 1
beqz .L1, s1
li s3, 1
li s4, 1
add s3, s3, s4
j .L0
mv s2, s3
.L1:
li s4, 1
li s5, 1
sub s4, s4, s5
mv s2, s4
.L0:
mv a0, s2
ret

