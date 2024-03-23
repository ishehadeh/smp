.text
main:
li s1, 1
beqz .L1, s1
li s2, 1
li s3, 1
add s2, s2, s3
j .L0
.L1:
li s3, 1
li s4, 1
sub s3, s3, s4
.L0:

