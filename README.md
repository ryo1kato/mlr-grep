Japanese presentation slide is [available here](http://www.slideshare.net/ryo1kato/mlrgrep-a-recordoriented-grep)

#mlr-grep - Multi-line Record grep, a Record-Oriented grep.

Have you ever used `grep`'s  `-A`, `-B`, or `-C` option or pcregrep, or maybe AWK, perl,oneliners, to search something like multi-line log entries?
Then probably this command is for you.

mlr-grep is like `grep`, but *_record_-oriented* rather than line-oriented; when it finds a match in a line, it prints all lines in the _record_ the match is found. In other words, all the lines around the match, surrounded by _record separator_.
And of course, you can specify record-separator using `--rs=REGEX` option, default of which is `^$|(-----\*|=====\*)$` (blank line or, four or more dashes). This is similar to `-d` (delimiter) option of [agrep](http://www.tgries.de/agrep/agrephlp.html), but our version accept arbitrary regex as record-separator

Useful for multi-line logs, config files with indents or section headers (like `*.ini` like format), command output like `ifconfig` or `pbsnodes`


## Basic syntax

`MLR_GREP [OPTIONS] [--] PATTERN[...] [--] [FILES...]`

* `-v`, `--invert-match` Select non-matching lines (same as `grep -v`).
* `-i`, `--ignore-case`  Case-insensitive match (same as `grep -i`).
* `-c`, `--count`        Print number of matches (same as `grep -c`).
* `--color`, `--mono`    (Do not) highlight matches (same as GNU `grep --color`).
                         Default is to highlight if stdout is a terminal.
* `-r`, `--rs`           Set input record separator. Default is `^$|(-----\*|=====\*)$`
* `-t`, `--timestamp`    Same as `--rs=`*TIMESTAMP_REGEX*, where the regex matches
                         timestamps often used in log files, e.g.,
                         `2014-12-31 12:34:56` or `Dec 31 12:34:56`.
* `-a`, `--and`          Print entries match with all (instead of any) of `PATTERN`s.
* `-h`, `--help`         Print help of a command.


## Examples

### Config file
For config file like this:
```
[name1]
attr1=abc
attr2=123

[name2]
attr1=xxx
attr2=456
attr3=zzz

[name3]
attr1=abc
attr2=789
```

You can use `hmlgrep attr2=456` to print whole entries of `[name3]` section, without guessing number of lines and using something like `grep -C 2` (In this case blank line is parsed as separators, but you can use `--rs` option like `hmlgrep --rs='^\['`

```
$ hmlgrep attr2=456

[name2]
attr1=xxx
attr2=456
attr3=zzz
```

### Log file with timestamps
When you have multi-line log entries begin with timestamp:

```
$ cat logfile.txt
2014-01-23 12:34:56 logentry 1
    foo
    bar
2014-01-24 12:34:57 logentry 2
    abc
    def
2014-01-23 12:34:58 logentry 3
    123
    456
```

You can use `--timestamp` option (or something like `--rs='^2014-[0-9][0-9]-[0-9][0-9]'` to split such lines into records. Then searching for entries with keyword is just like using ordinary `grep`.

```
$ hmlgrep --timestamp abc logfile
2014-01-24 12:34:57 logentry 2
    abc
    def
```


## Implementations
Currently we have AWK, Haskell, and Python implementation.
They're roughly equivalent, but has a few minor differences.
For example, AWK version accepts POSIX extended regular expressions and can match multiple lines, while Haskell version uses PCRE regex library and Python has its own variant.

### amlgrep
An `awk` implementation and is a 'full feature' version - equipped with most of basic grep options like `-i`,`-c`,`-v` options, and it can highlight matches. Also be able to handle compressed files like `*.gz`, `*.bz2`, and `*.xz` transparently. (But you need `gzip`, `bzip2`, and `xz` installation)

Being implemented by `awk`, it should ran on any Unix-like platform though I have only tested on Linux (Ubuntu 12.04, RHEL 5.x) and MaxOSX. Note that it requires *GNU awk*, so won't run on MaxOSX out of the box with stock BSD awk.

*KNOWN BUG* Subtle newline (`\n`) handling, it will output slightly wrong output when there's an empty record (two continuous separator lines). For example, when RS=`\n\n`, it consumes both of newlines. So when `\n\n\n` (two empty records with blank line as separator) appears in input, first iteration consume two characters and leave single '\n' behind and next iteration cannot find `\n\n`

### hmlgrep
A Haskell implementation. Most actively maintained.
Known issues:
* `--count` shows total line numbers only for multiple files
* `--` is stripped from argument (means you cannot search pattern `--` or search in a file named `--`) This is a bug of `optparse-applicate` and fix is proposed [here](https://github.com/pcapriotti/optparse-applicative/pull/99)

This is currently the fastest implementation in many use cases, often x5 to 10x faster than awk especially for sparse input.

### pymlgrep
A Python implementation. Doesn't support `--and`, and only accept single patterns.
Slowest, sometimes it's about twice slow than `amlgrep` or `hmlgrep`


## Install
* `amlgrep` - Just copy into any directory listed in `$PATH`. On non-Linux systems you may also need to install GNU awk.
* `hmlgrep` - Install the latest Haskell Platform, and run `cabal install`. You can also use `make` to build it.
* `pymlgrep` - Just copy into any directory listed in `$PATH`.


## Regex Libraries for `hmlgrep`
Latest version of `hmlgrep` uses `regex-pcre`(Text.Regex.PCRE) as underlaying regex engine and `stringsearch` (Data.ByteString.Search) for non-regex patterns.
In source code, there's experimental version using `regex-tdfa` and `haskell-re2` (jmillikin's version of Haskell wrapper for Google's regex implementation) disabled and swichable using `#ifdef`.

PCRE is the best with current implementation so far, slightly better than `re2` and much much better than `regex-tdfa`.
You can use these regex engines just by changing `#define PCRE` to `#define RE2` or `#define TDFA` (and comment/comment-out corresponding `import` statements)

Note that it's using patched version of `haskell-re2` which is available at https://github.com/ryo1kato/haskell-re2/tree/matchPos


## Tests

### `runtest.sh`
Run a commandline-level test. It's simply compare outputs from the three commands.

### `perftest.sh`
Run a simple performance test for several regex/non-regex and sparse/dense patterns on dummy multi-line log data.
You have to generate test data using `test/gentestlog.sh` first.

```
$ ./test/gentestlog.sh
$ ./performance.sh
```
You can change test data size with `./test/gentestlog.sh 512` (generate 512MB test data)

### doctest for `hmlgrep`
Limited number of unit tests are implemented using doctest.
Just run it with `cabal test`


## License
This software is licensed under the terms of the MIT license.
* Project page: https://github.com/ryo1kato/mlr-grep
* Author: Ryoichi Kato
