tmpbib="${TEMP:-/tmp}/update-bib.sh__refs.bib"
bib="$(dirname $(readlink -f "$0"))/../report/refs.bib"
while true; do
    curl -s 'http://127.0.0.1:23119/better-bibtex/collection?/0/D5A2MV9C.biblatex' >"$tmpbib";
    if [ "$(sha1sum "$tmpbib" | awk '{ print $1 }')" != $(sha1sum "$bib" | awk '{ print $1 }') ]; then
        cp "$tmpbib" "$bib"
    fi
    sleep 1;
done
