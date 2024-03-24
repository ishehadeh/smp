.text
.globl main
main:
li a0, 0
li a1, 1
li a2, 1
li a3, 0
li a4, 1
li a5, 1
li a6, 0
li a7, 1
jal many_many_params
mv a0, a0
ret
.globl __libc_start_main
__libc_start_main:
jal main
ret
.globl many_many_params
many_many_params:
add s1, a5, a6
add a6, a4, s1
add s1, a3, a6
add a6, a2, s1
add s1, a1, a6
add a6, a0, s1
mv a0, a6
ret

