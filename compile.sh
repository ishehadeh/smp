#!/bin/sh

input="$1"
output_asm="$1.s"
output_bin="${1%.hlt}"

shift

cargo run --bin hltc --features=cli <"$input" >"$output_asm"

riscv64-linux-gnu-gcc  "$output_asm" -g -nodefaultlibs -static -o "$output_bin"
# spike "$@" /usr/riscv64-linux-gnu/bin/pk "$output_bin"
qemu-riscv64-static -g 1234 program-examples/simple &
riscv64-linux-gnu-gdb -ex 'target remote :1234' "$@"
