#!/usr/bin/env julia --project=@.
#
# TODO:
#   [x] basic features: --rs, -v, -i, -c
#   [x] additional features: --color, --mono with terminal detection
#   [ ] additional features multiple patterns, --and
#   [ ] multi-line record-separator
#   [ ] performance: use Boyre-moore or KMR for non-regex match
#   [ ] performance : mmap
#   [ ] performance: find-pattern-first algorithm for sparse matches
#
module JmlGrep

using ArgParse

const RS_DEFAULT = "^\$|^(=====*|-----*)\$"

re_dow="((Mon|Tue|Wed|Thu|Fri|Sat),?[ \t]+)?"
re_month="(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Dec),?[ \t]*"
re_date="[0-9]{1,2},?"
re_time="[0-2][0-9]:[0-5][0-9](:[0-5][0-9])?"
re_year="(,?[ \t]20[0-9][0-9])?"
re_dty="$re_date[ \t]$re_time$re_year"
re_isodate="20[0-9][0-9]-(0[0-9]|11|12)-(0[1-9]|[12][0-9]|3[01])"
const REGEX_DATETIME = "^($re_dow$re_month$re_dty|$re_isodate)"

function parse_commandline()
    s = ArgParseSettings()
    @add_arg_table s begin
        "--invert-match", "-v"
            help =  "Select non-matching lines (same as grep -v)."
            action = :store_true
        "--ignore-case",  "-i"
            help =  "Case-insensitive match (same as grep -i)."
            action = :store_true
        "--count", "-c"
            help =  "Print number of matches."
            action = :store_true
        "--rs", "-r"
            help = "Set input record separater. Default is: \"$RS_DEFAULT\""
            arg_type = String
            default = RS_DEFAULT
        # See https://github.com/JuliaLang/julia/pull/36671
        #"--color"
        #    help = "Highlight matches even when the output is not a terminal."
        #    action = :store_const
        #    constant = "always"
        #    default = "auto"
        #    dest_name = "highlight"
        "--mono"
            help = "Do not highlight matches even when the output is a terminal"
            arg_type = String
            action = :store_const
            constant = "never"
            default = "auto"
            dest_name = "highlight"
#        "--and", "-a"
#            help = "Print entries match with all, instead of any, of PATTERNS."
#            action = :store_true
        "--timestamp", "-t"
            help = "Synomym for --rs=REGEX_DATETIME, where the regex matches strings like '\n2014-12-31 12:34:56' or '\nDec 31 12:34:56'"
            action = :store_true
          "pattern"
              help = "An extended regex pattern."
              required = true
              nargs = '+'
              metavar = "PATTERN"
          "file"
              help = "Filename to search the pattern. Uses standard input if ommited."
              required = false
              nargs = '*'
              metavar = "FILE"
    end
    return parse_args(s)
end

struct RecLine
    line::String
    match::Union{RegexMatch,Nothing}
end

function print_record(rec, highlight::Bool)
    if highlight
        for recline in rec
            if isnothing(recline.match)
                println(recline.line)
            else
                pos = recline.match.offset
                sz = length(recline.match.match)
                str = recline.line
                print(str[1:pos-1])
                # See https://github.com/JuliaLang/julia/pull/36671
                printstyled(str[pos:(pos+sz)-1], bold=true, color=:red)
                println(str[pos+sz:end])
            end
        end
    else
        for recline in rec
            println(recline.line)
        end
    end
end

function mlgrep(input, rs::Regex, pat::Regex, highlight::Bool; invert=false::Bool, silent=false) :Int
    count = 0
    found = false
    rec = []
    for line in eachline(input)
        if !isnothing(match(rs, line))
            if found
                count += 1
                if !silent
                    print_record(rec, highlight)
                end
            end
            rec = []
            found = false
        end
        m = match(pat, line)
        push!(rec, RecLine(line, m))
        if xor(!isnothing(m), invert)
            found = true
        end
    end
    if found
        if !silent
            print_record(rec, highlight)
        end
        count += 1
    end

    return count
end


function pattern_filename_split(strings)
    patterns = [strings[1]]
    filenames = []
    is_pattern = true
    for s in strings[2:end]
        st = stat(s)
        if is_pattern && ( s == "-" || (st.mode != 0x0 && st.size > 0))
            is_pattern=false
        end
        if is_pattern
            push!(patterns, s)
        else
            push!(filenames, s)
        end
    end
    return (patterns, filenames)
end


function process_args(args)
    if length(args["pattern"]) == 0
        println(stderr, "At least one argument (a pattern) is required")
        exit(2)
    end

    patterns, filenames = pattern_filename_split(args["pattern"])

    if length(patterns) > 1
        println(stderr, "ERROR: only one pattern is supported at the moment.")
        exit(2)
    end

    if length(filenames) == 0
        filenames = ["-"]
    end

    if args["ignore-case"]
        reFlags = "i"
    else
        reFlags = ""
    end

    pat = Regex(patterns[1], reFlags)
    if args["timestamp"]
        rs = Regex(REGEX_DATETIME, reFlags)
    else
        rs = Regex(args["rs"], reFlags)
    end

    if args["highlight"] == "always"
        highlight = true
    elseif args["highlight"] == "never"
        highlight = false
    else
       highlight = isa(stdout, Base.TTY)
    end

    return (patterns, filenames, reFlags, rs, pat, highlight)
end


function main()
    args = parse_commandline()

    patterns, filenames, reFlags, rs, pat, highlight = process_args(args)

    count = 0
    for f in filenames
        if f == "-"
            input = stdin
        else
            input = open(f)
        end
        if args["count"]
            this_count = mlgrep(input, rs, pat, highlight, invert=args["invert-match"], silent=true)
            if length(filenames) > 1
                print("$f:")
            end
            println(this_count)
            count += this_count
        else
            count += mlgrep(input, rs, pat, highlight, invert=args["invert-match"])
        end
    end

    if count == 0
        exit(1)
    else
        exit(0)
    end
end


function julia_main()::Cint
    try
        ret = main()
    catch
        Base.invokelatest(Base.display_error, Base.catch_stack())
        return 1
    end
    return ret
end


if abspath(PROGRAM_FILE) == @__FILE__
    main()
end


end #module
