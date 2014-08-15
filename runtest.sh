#!/bin/bash

set -ue
cmds=(hmlgrep amlgrep pymlgrep)

t="test-result"
mkdir -p "$t"

runtest () {
    name="$1"
    shift
    echo "[$name] Testing: $*"
    for cmd in "${cmds[@]}"
    do
        (
            ./$cmd "$@" > "$t/$name.$cmd"
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

runtest dot      . test/test1.txt
runtest foo1     foo test/test1.txt
runtest FOO1     FOO test/test1.txt
runtest foo1     --rs='^----' foo test/test1.txt
#runtest foo2     foo test/test2.txt
runtest FOO2     FOO test/test2.txt
runtest notfound zzzzzzzz test/test1.txt
runtest date     -t foo test/date.txt
runtest foo1_i   -i foo test/test1.txt
runtest foo2_i   -i foo test/test2.txt
runtest foo1_i   -c foo test/test1.txt
runtest foo2_i   -c foo test/test2.txt


