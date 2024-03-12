#!/bin/sh
set -eo

root="$(dirname $(dirname $(readlink -f "$0")))"

gen_ast() {
    basename="$(dirname $1)/$(basename "$1" .hlt)"
    "$root/target/debug/hlt-dbg-ast" --format json "$1" >"$basename.json" || echo "hlt-dbg-ast failed"
    "$root/target/debug/hltc" "$1" >"$basename.s" || echo "hltc failed"
}

cargo build --bin hlt-dbg-ast --features cli,json
cargo build --bin hltc --features cli

for file in $(find "$root/tests/syntax" -maxdepth 2 -iname '*.hlt'); do
    gen_ast "$file"
done
