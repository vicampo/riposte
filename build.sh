#!/bin/bash

set -exuo pipefail

raco pkg remove riposte

raco pkg install -n riposte

cwd=$(pwd)

build_dir=$(mktemp -d)

find . -mindepth 1 -maxdepth 1 -name '*.rkt' -exec cp {} "${build_dir}" ';'

cd "${build_dir}"

raco exe --vv ++lang riposte ++lang racket/base ++lang brag riposte.rkt

mkdir dist

mkdir -p collects/riposte

find . -mindepth 1 -maxdepth 1 -name '*.rkt' -exec cp {} collects/riposte ';'

raco distribute ++collects-copy collects dist riposte

cp "${build_dir}"/dist/bin/riposte "${cwd}"

cd "${cwd}"
