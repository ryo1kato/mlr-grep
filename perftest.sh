#!/bin/bash
set -ue
trap 'echo "$0: Unexpected error at line $LINENO" >&2; exit 1;' ERR
testdata="${1:-test-result/test.data}"

MSG () { echo "$@"; }
mlgrep=(./aml ./hml)

if [[ $(grep --version) =~ GNU ]]
then
    localgrep=(e)
else
    if type gegrep >/dev/null 2>&1
    then
        localgrep=(ge)
    else
        localgrep=(e)
        MSG "WARNING: your version of grep is not GNU"
    fi
fi
cmdprefixes+=("${localgrep[@]}")
cmdprefixes+=("${mlgrep[@]}")

patterns=('zzzz' 'SPARSE$' 'Amazon' 'Ama[Zz]o*n' '^foo' 'ba[rz]')

MSG "Caching the file..."
cat "$testdata" > /dev/null

# This is bash specific. (TIMEFMT for zsh)
TIMEFORMAT="%R,%U,%S"

mkdir -p "test-result"

i=1
for pat in ${patterns[@]}
do
    {
    MSG "#---- Testing for /$pat/ ----"
    for cmd in ${cmdprefixes[@]}
    do
        echo -n "${cmd#*/}grep,"
        time "${cmd}grep" "$pat" "$testdata" > /dev/null || true
    done
    } 2>&1 | tee "test-result/log-$i"
    let i++
done
