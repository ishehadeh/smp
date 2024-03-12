#!/bin/sh

root="$(dirname $(readlink -f "$0"))"

gen_ast() {
    out="$(dirname $1)/$(basename "$1" .hlt).json"
    cargo run --bin hlt-dbg-ast --features cli,json --  --format json "$1" >"$out"
}


for file in $(find "$root" -maxdepth 2 -iname '*.hlt'); do
    gen_ast "$file"
done
