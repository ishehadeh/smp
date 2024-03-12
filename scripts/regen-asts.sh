#!/bin/sh
set -xeo

root="$(dirname $(dirname $(readlink -f "$0")))"

gen_ast() {
    out="$(dirname $1)/$(basename "$1" .hlt).json"
    "$root/target/debug/hlt-dbg-ast" --format json "$1" >"$out"
}

cargo build --bin hlt-dbg-ast --features cli,json

for file in $(find "$root/tests/syntax" -maxdepth 2 -iname '*.hlt'); do
    gen_ast "$file"
done
