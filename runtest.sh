#!/bin/bash

set -ue

t="test-result"
mkdir -p "$t"

strip_ctrl () {
    sed -e $'s/\033[^m]*m//g' -e $'s/\017//g'
}


runtest () {
    name="$1"
    shift
    echo "[$name] Testing: $*"
    for cmd in "${cmds[@]}"
    do
        (
            set +e
            ./$cmd "$@" | strip_ctrl > "$t/$name.$cmd"
            echo $? > "$t/$name.$cmd.ret"
        )
    done
    reference_cmd="${cmds[1]}"
    for cmd in "${cmds[@]}"
    do
        if [ $cmd = $reference_cmd ]
        then
            continue
        fi
        if ! diff -u "$t/$name.$reference_cmd" "$t/$name.$cmd"
        then
            echo "====================================="
            echo "Inconsistent results: ${reference_cmd} and ${cmd}"
            echo "Arguments were: $*"
            return 1
        fi
        if ! diff -u "$t/$name.$reference_cmd" "$t/$name.$cmd"
        then
            echo "====================================="
            echo "Inconsistentfor exit codes: ${reference_cmd} and ${cmd}"
            echo "Arguments were: $*"
            return 1
        fi
    done
    return 0
}

cmds=(hmlgrep amlgrep pymlgrep)

runtest dot       . test/test1.txt
runtest foo1      foo test/test1.txt
runtest FOO1      FOO test/test1.txt
runtest foo1      --rs='^----' foo test/test1.txt
runtest foo2      foo test/test2.txt
runtest FOO2      FOO test/test2.txt
runtest notfound  zzzzzzzz test/test1.txt
runtest date      -t foo test/date.txt
runtest foo1_i    -i foo test/test1.txt
runtest foo2_i    -i foo test/test2.txt
runtest foo1_i    -c foo test/test1.txt
runtest foo2_i    -c foo test/test2.txt
runtest datetime1 -t foo test/date.txt
runtest datetime2 -t 'logentry 2' test/date.txt

runtest multifile foo test/test[23].txt
runtest multifile -c foo test/test[23].txt

cmds=(hmlgrep amlgrep)

runtest foo_multi1  FOO foo test/test1.txt
runtest foo_multi2  --and  FOO foo test/test1.txt
runtest foo_multi3  foo hoge test/test2.txt
runtest foo_multi3  --and foo hoge test/test2.txt
runtest foo_multi3  --and foo logentry test/date.txt

# FIXME: this test fails
# runtest color_dot         --color . test/test1.txt
runtest color_foo1        --color foo test/test1.txt
runtest color_FOO1        --color FOO test/test1.txt
runtest color_FOO1        -i --color FOO test/test1.txt
runtest color_foo_multi3  --and foo logentry test/date.txt
