#!/bin/sh
set -e

root="$(dirname $(dirname $(readlink -f "$0")))"

gen_ast() {
    basename="$(dirname $1)/$(basename "$1" .hlt)"
    if [ -z "$NO_AST" ]; then
        "$root/target/debug/hlt-dbg-ast" --format json "$1" >"$basename.json" || echo "hlt-dbg-ast failed"
    fi

    if [ -z "$NO_COMPILE" ]; then
        "$root/target/debug/hltc" "$1" >"$basename.s" || echo "hltc failed"
    fi
}

test -z "$NO_AST" && cargo build --bin hlt-dbg-ast --features cli,json
test -z "$NO_COMPILE" && cargo build --bin hltc --features cli

PAT="${1:-*}"
for file in $(find "$root/tests/syntax" -maxdepth 2 -iname "$PAT.hlt"); do
    gen_ast "$file"
done
