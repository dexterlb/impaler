#!/bin/bash

shopt -s nullglob

cd "$(dirname "$(readlink -f "${0}")")"

OPTS=(-latexoption="-shell-escape" -halt-on-error -xelatex)

case "${1}" in
    "")
        "${0}" build main.tex
        ;;
    build)
        latexmk ${OPTS[@]} "${2}"
        ;;
    clean)
        latexmk -C
        for f in *.bbl *.run.xml *.aux{,lock} *.log *.fls *-figure*.{pdf,log,dpth,md5}; do
            rm -rvf "${f}"
        done
        ;;
    watch)
        if [[ -z "${2}" ]]; then
            f=main.tex
        else
            f="${2}"
        fi
        latexmk ${OPTS[@]} -pvc "${f}"
        ;;
    *)
        echo "say whaaa?"
        ;;
esac
