#mlr-grep - Multi-line Record grep

Have you ever used `grep`'s  `-A`, `-B`, or `-C` option to search something like multi-line log entries? Then probably this command is for you.

mlr-grep is like `grep`, but _record_-oriented rather than line-oriented.
So command finds a match in a line, it prints all lines in a record which has the match line in it.
And of course, you can set record-separator using `--rs=REGEX` option.
This is similar to `-d` (delimiter) option of [agrep](http://www.tgries.de/agrep/agrephlp.html).

Useful for multi-line logs, config files with indents or section headers, command output like `ifconfig` or `pbsnodes`


## Basic syntax

`MLR_GREP [OPTIONS] PATTERN[...] [FILES...]`

* -v, --invert-match    Select non-matching lines (same as grep -v).
* -i, --ignore-case     Case-insensitive match (same as grep -i).
* -c, --count           Print number of matches (same as grep -c).
* -a, --and  Print entries match with all, instead of any, of PATTERNS.
* -r, --rs   Set input record separater. Default is "^$|(----*|====*)$"
* -t, --timestamp


## amlgrep
An `awk` implementation and is a 'full feature' version - equipped with most of basic grep options like `-i`,`-c`,`-v` options, and it can highlight matches. Also be able to handle compressed files like `*.gz`, `*.bz2`, and `*.xz` transparently. (But you need `gzip`, `bzip2`, and `xz` installation)

Being implemented by `awk`, it should ran on any Unix-like platform though I have only tested on Linux (Ubuntu 12.04, RHEL5.x) and MaxOSX. Note that it requires GNU awk, so won't run on MaxOSX out of the box with stock BSD awk.


## hmlgrep
A Haskell implementation. Only supports `--rs` and `--and` options so far.
Most actively maintained.


## pmlgrep
A Python implementation. Slow and doesn't support most of the options.
Not maintained.


## License
This projected is licensed under the terms of the MIT license.

