#!/bin/zsh
# zsh is 3x faster than bash with the same code.
# 128MB in 20sec for zsh, 1min for bash

size_in_MB="${1:-256}"
outfile=${2:-test-result/test.data}

if [ "$outfile" != ${outfile%/*} ]
then
    mkdir -p ${outfile%/*}
fi

sparse_ratio=10000

words=(
    foo bar baz hoge piyo huga
    CIA FBI NSA Amazon Apple Google Microsoft Facebook Twitter
)

sparse_word=SPARSE

echo_log_entry () {
echo "
-------------------
counter=$counter
attr1=1234
attr2=5678
attr3=abc
attr4=def
ATTR5=$1
ATTR6=foo,bar,baz
comment=a quick brown fox jumped over lazy dog
s_p_a_r_s_e=$2
lkajeofijasdf"
}

##################################################################
MSG () { echo "$@" >&2; }

logsize=$(echo_log_entry 1234 dummy dummy | wc -c)
size_count_estimate=$(( size_in_MB * 1024 * 1024 / logsize ))

counter=0

MSG "Generating about ${size_in_MB}MB ($size_count_estimate entries) data to $outfile"
while true
do
    if (( counter % 787 == 0 ))
    then
        {
        printf '\r                                \r'
        printf '%10d / %10d (%3d%%)' $counter $size_count_estimate \
                $((counter * 100 / size_count_estimate))
        } >&2
    fi
    for w in ${words[@]}
    do
        (( counter < size_count_estimate )) || break 2
        if (( counter % sparse_ratio == 0 ))
        then
            echo_log_entry $w $sparse_word
        else
            echo_log_entry $w
        fi
        let counter++
    done
done > $outfile

