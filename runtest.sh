#!/bin/bash
print_help () {
cat <<EOF
Usage: $0 [OPTIONS]
$(b OPTIONS)
-s, --stop      Exit immediately after a test case failure.
-v, --verbose   Increase verbosity level.
-h, --help      Print this help.
EOF
}
set -ue

trap 'echo "$0: Unexpected error at line $LINENO ($BASH_COMMAND, ret=$?)" >&2; exit 1;' ERR
ERROR () { echo "ERROR: $0: $*" >&2; }
DIE () { ERROR "$*"; exit 1; }

b () { tput bold; echo -n "$*"; tput sgr0; } # bold
u () { tput smul; echo -n "$*"; tput rmul; } # underline
green () { tput setaf 2 ; echo -n "$*"; tput sgr0; } # bold
color_error () { tput setab 1; tput setaf 7; tput bold; echo -n "$*"; tput sgr0; } # bold


TERSE () { if (( opt_verbose == 0 )); then echo "$@"; fi; }
INFO () {   if (( opt_verbose > 0 ));  then echo "INFO: $0: $*"; fi; }
DEBUG () { if (( opt_verbose > 1 ));  then echo "DEBUG: $0: $*"; fi; }

#############################################################################
strip_ctrl () {
    # This doesn't work with BSD sed with --color option for awk
    # BSD sed appends '\n' if it's missing from input.
    #sed -E -e $'s/\033[^m]*m//g' -e $'s/\017//g' -e '$d'
    gawk -v RS='\n' '{ gsub("\033[^m]*m",""); gsub("\017", ""); printf("%s", $0); printf("%s", RT);}'
}

runtest () {
    name="$1"
    shift
    INFO "[$name] Testing: $*"
    TERSE -n '.'
    for cmd in "${cmds[@]}"
    do
        (
            set +e
            ./$cmd "$@" | strip_ctrl > "$t/$name.$cmd"
            echo $? > "$t/$name.$cmd.ret"
        )
    done
    reference_cmd="${cmds[0]}"
    for cmd in "${cmds[@]}"
    do
        if [ $cmd = $reference_cmd ]
        then
            continue
        fi
        INFO "  <$cmd>"
        if ! diff -u "$t/$name.$reference_cmd" "$t/$name.$cmd"
        then
            echo
            echo "====================================="
            echo "$(color_error Inconsistent results): ${reference_cmd} and ${cmd}"
            echo "Arguments were: $*"
            let nr_errors++ || true
        else
            TERSE -n "$(green .)"
            INFO "    $(b '-->') $(green 'results OK')"
        fi
        TERSE -n '.'
        if ! diff -u "$t/$name.$reference_cmd.ret" "$t/$name.$cmd.ret"
        then
            echo
            echo "====================================="
            echo "$(color_error "Inconsistent exit codes"): ${reference_cmd} and ${cmd}"
            echo "Arguments were: $*"
            let nr_errors++ || true
        else
            TERSE -n "$(green .)"
            INFO "    $(b '-->') $(green 'return code OK')"
        fi
    done
    if [ $opt_stop = 'yes' -a ${nr_errors} -gt 0 ]
    then
        DIE "exit now due to --stop option."
    else
        return 0
    fi
}


#############################################################################

opt_verbose=0
opt_stop='no'
while [ $# -ge 1 ]
do
case $1 in
    -[^-][^-]*)
        # expand '-abcd' to '-a -bcd' (and eventually '-a -b -c -d')
        firstarg="$1"; shift
        set -- "${firstarg:0:2}" "-${firstarg:2}" "$@"; continue;;
    --*=*)
        firstarg="$1"; shift;
        set -- "${firstarg%%=*}" "${firstarg#*=}" "$@"; continue;;
    -s|--stop)    opt_stop='yes';;
    -v|--verbose) ((++opt_verbose));;
    -h|--help)    print_help; exit 0;;
    --) shift; break;;
    -*) DIE "Unknown option '$1'";;
    *) break;;
esac
shift
done


#############################################################################

make

nr_errors=0
t="test-result"
mkdir -p "$t"

## -------- Basic features --------
cmds=(amlgrep hmlgrep pymlgrep rmlgrep gmlgrep jmlgrep)
runtest dot       . test/test1.txt
runtest foo1      foo test/test1.txt
runtest FOO1      FOO test/test1.txt
runtest foo2      foo test/test2.txt
runtest FOO2      FOO test/test2.txt
runtest notfound  zzzzzzzz test/test1.txt

runtest foo_re1   '^foo' test/test1.txt
runtest foo_re2   'foo$' test/test1.txt
runtest foo_re3   '^foo$' test/test1.txt
runtest foo_re1   '^foo' test/test2.txt
runtest foo_re2   'foo$' test/test2.txt
runtest foo_re3   '^foo$' test/test2.txt

runtest foo1rs    --rs='^----' foo test/test1.txt
runtest foo1rs2   --rs='----' foo test/test1.txt
runtest foo1rs3   --rs='----$' foo test/test1.txt
runtest ba1rs2    --rs '2014|Mon|Jan' ba test/date.txt

runtest rs1foo       --rs='^$'     'foo'     test/test1.txt
runtest rs1foobar    --rs='^$'     'foo bar' test/test1.txt
runtest rs2foo       --rs='^----$' 'foo'     test/test1.txt
runtest rs2foobar    --rs='^----$' 'foo bar' test/test1.txt
runtest rs3foo       --rs='^----$' 'foo'     test/test1.txt
runtest rs3foobar    --rs='^----$' 'foo bar' test/test1.txt

## -------- --datetime option --------
cmds=(amlgrep hmlgrep pymlgrep rmlgrep jmlgrep)
runtest date      -t foo test/date.txt
runtest datetime1 -t foo test/date.txt
runtest datetime2 -t 'logentry 2' test/date.txt


## -------- --count, --ignore-case options --------
runtest foo1_i    -i foo test/test1.txt
runtest foo2_i    -i foo test/test2.txt
runtest foo1_c    -c foo test/test1.txt
runtest foo2_c    -c foo test/test2.txt


## -------- multiple files --------
cmds=(amlgrep hmlgrep pymlgrep rmlgrep)
runtest multifile              foo test/test[23].txt
runtest multifile_count1 -c    foo test/test[12].txt
runtest multifile_count2 -c -i foo test/test[123].txt

runtest foo_re1   '^foo' test/test[12].txt
runtest foo_re2   'foo$' test/test[12].txt
runtest foo_re3   '^foo$' test/test[12].txt


## -------- multiple patterns, --and --------
cmds=(hmlgrep amlgrep)

runtest foo_multi1  FOO foo test/test1.txt
runtest foo_multi2  --and  FOO foo test/test1.txt
runtest foo_multi3  foo hoge test/test2.txt
runtest foo_multi3  --and foo hoge test/test2.txt
runtest foo_multi3  --and foo logentry test/date.txt
# only one pattern; should be same as without "--and"
runtest foo_multi4  --and foo test/test1.txt

runtest color_dot         --color . test/test1.txt
runtest color_foo1        --color foo test/test1.txt
runtest color_FOO1        --color FOO test/test1.txt
runtest color_FOO1        -i --color FOO test/test1.txt
runtest color_foo_multi3  --and foo logentry test/date.txt
runtest color_rs          --rs '2014' --and ba 2014 loge test/date.txt
runtest color_rs2         --rs '2014|Mon|Jan' ba test/date.txt

## -------- compressed files -------
gzip --force --keep test/test1.txt
bzip2 --force --keep test/test1.txt
xz -z --force --keep test/test1.txt
for z in gz bz2 xz
do
    runtest color_dot_$z   --color .   test/test1.txt.$z
    runtest color_foo1_$z  --color foo test/test1.txt.$z
    runtest color_FOO1_$z  --color FOO test/test1.txt.$z
done

rm -f test/test1.txt.gz test/test1.txt.bz2 test/test1.txt.xz

if [ $nr_errors -gt 0 ]
then
    echo "$nr_errors ERRORS"
    exit 1
else
    TERSE $(green done)
    INFO "$(green ALL TESTS FINISHED SUCCESFULLY)"
    exit 0
fi

