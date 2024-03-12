#!/bin/sh
set -xe

RV_ARCH="${RV_ARCH:-riscv64}"
GCC_TARGET="${GCC_TARGET:-$RV_ARCH-linux-gnu}"
GDB="${GDB:-$GCC_TARGET-gdb}"
GCC="${GCC:-$GCC_TARGET-gcc}"
QEMU_USER="${QEMU_USER:-qemu-$RV_ARCH-static}"
GDB_PORT="${GDB_PORT:-1234}"


REPO_ROOT="${REPO_ROOT:-$(dirname $(dirname $(readlink -f "$0")))}"
HTLC_OUT_DIR="${HTLC_OUT_DIR:-$REPO_ROOT/out}"

input="$1"
shift

output_asm="$HTLC_OUT_DIR/$(basename "$input").s"
output_bin="$HTLC_OUT_DIR/$(basename "$input" .hlt)"


mkdir -p "$HTLC_OUT_DIR"
cargo run --features=cli --bin hltc "$@" <"$input" >"$output_asm"

$GCC "$output_asm" -g -nodefaultlibs -static -o "$output_bin"
$QEMU_USER -g "$GDB_PORT" "$output_bin" &
$GDB -ex 'target remote :'"$GDB_PORT" -ex 'b main'
