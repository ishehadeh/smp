#!/bin/sh
set -e

root="$(dirname $(dirname $(readlink -f "$0")))"

gen_ast() {
    out_dir="$(dirname $1)/out/"
    mkdir -p "$out_dir"
    basename="$out_dir/$(basename "$1" .hlt)"
    if [ -z "$NO_AST" ]; then
        "$root/target/debug/hlt-dbg-ast" --pass type --format json "$1" >"$basename.json" || echo "[$1] hlt-dbg-ast failed"
    fi

    if [ -z "$NO_COMPILE" ]; then
        "$root/target/debug/hltc" "$1" >"$basename.s" || echo "[$1] hltc failed"
    fi
}

test -z "$NO_AST" && cargo build --bin hlt-dbg-ast --features cli,json
test -z "$NO_COMPILE" && cargo build --bin hltc --features cli

PAT="${1:-*}"
for file in $(find "$root/tests/syntax" -maxdepth 2 -iname "$PAT.hlt"); do
    gen_ast "$file"
done
