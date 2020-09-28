#!/usr/bin/env julia --project=@.
module JmlGrep

using ArgParse

const RS_DEFAULT = "\\n\\n|\\n(=====*|-----*)\\n"

re_dow="((Mon|Tue|Wed|Thu|Fri|Sat),?[ \t]+)?"
re_month="(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Dec),?[ \t]*"
re_date="[0-9]{1,2},?"
re_time="[0-2][0-9]:[0-5][0-9](:[0-5][0-9])?"
re_year="(,?[ \t]20[0-9][0-9])?"
re_dty="$re_date[ \t]$re_time$re_year"
re_isodate="20[0-9][0-9]-(0[0-9]|11|12)-(0[1-9]|[12][0-9]|3[01])"
const REGEX_DATETIME = "\n($re_dow$re_month$re_dty|$re_isodate)"

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
        "--color"
            help = "Highlight matches even when the output is a file."
            action = :store_true
            dest_name = "highlight"
        "--mono"
            help = "Do not highlight matches even when the output is a terminal"
            action = :store_true
            dest_name = "highlight"
        "--and", "-a"
            help = "Print entries match with all, instead of any, of PATTERNS."
            action = :store_true
        "--rs", "-r"
            help = "Set input record separater. Default is: \"$RS_DEFAULT\""
            arg_type = String
        "--timestamp", "-t"
            help = "Synomym for --rs=REGEX_DATETIME, where the regex matches strings like '\n2014-12-31 12:34:56' or '\nDec 31 12:34:56'"
            action = :store_true
          "pattern"
              help = "An extended regex pattern."
              required = true
              #nargs = '+'
              metavar = "PATTERN"
          "file"
              help = "Filename to search the pattern. Uses standard input if ommited."
              required = false
              metavar = "FILE"
    end
    return parse_args(s)
end

function is_regex(pat ::String) :Bool
    return true # FIXME - if not regex, 
end


function mlgrep(input ::IOStream, pat ::String; invert=false::Bool, ignore_case=false::Bool) :Bool
    found = false
    for line in eachline(input)
        if xor(!occursin(pat, line), invert)
            println(line)
            found = true
        end
    end
    return found
end

function mlgrep(input ::IOStream, re::Regex; invert=false::Bool, ignore_case=false::Bool) :Bool
    found = false
    for line in eachline(input)
        m = match(re, line)
        if xor(!isnothing(m), invert)
            println(line)
            found = true
        end
    end
    return found
end

function main()
    args = parse_commandline()
    #println("Parsed args:")
    #for (arg,val) in args
    #    println("  $arg  =>  $val")
    #end

    if isnothing(args["file"])
        input = stdin
    else
        input = open(args["file"])
    end

    pat = Regex(args["pattern"])
    #pat = args["pattern"]

    found = mlgrep(input, pat, invert=args["invert-match"])

    if found
        return 0
    else
        return 1
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
