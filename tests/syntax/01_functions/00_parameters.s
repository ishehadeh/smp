.text
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
__libc_start_main:
jal main
many_many_params:
add a5, a5, a6
add a4, a4, a5
add a3, a3, a4
add a2, a2, a3
add a1, a1, a2
add a0, a0, a1

