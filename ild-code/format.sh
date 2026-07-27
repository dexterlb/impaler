#!/usr/bin/env bash

set -euo pipefail

cdir="$(dirname "$(readlink -f "${0}")")"
rs_dir="${cdir}"/../rs-interpreter

if [ "$#" -eq 0 ]; then
    readarray -t files < <(find "${cdir}" -type f -name "*.ild")
else
    declare -a files
    for f in "${@}"; do
        files+=("$(readlink -f "${f}")")
    done
fi

cd "${rs_dir}"
exec cargo run --quiet --bin fmt_ild -- "${files[@]}"
