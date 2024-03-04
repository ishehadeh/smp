#!/bin/sh

input="$1"
output_asm="$1.s"
output_bin="${1%.reed}"

cargo run --bin reedc --features=cli <"$input" >"$output_asm"

riscv64-linux-gnu-gcc  "$output_asm" -nodefaultlibs -static -o "$output_bin"
spike /usr/riscv64-linux-gnu/bin/pk "$output_bin"
