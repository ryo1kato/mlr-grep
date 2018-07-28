package main

import (
    "fmt"
    "os"
    "io"
    "bytes"
    "errors"
    "bufio"
    "sync"
    "strings"
    "unsafe"
    "reflect"
    goopt "github.com/droundy/goopt"
    //"regexp"
    //sre2  "github.com/samthor/sre2"
    pcre "github.com/gijsbers/go-pcre"
    //rubex "github.com/moovweb/rubex"
)
var Usage = "gmlgrep [OPTIONS...] PATTERN[...] [--] [FILES...]"
var Summary = `
  grep(1) like tool, but "record-oriented", instead of line-oriented.
  Useful to search/print multi-line log entries separated by e.g., empty
  lines, '----' or timestamps, etc. If an argument in argument list is a
  name of existing file or '-' (means stdin), such argument and all arguments
  after that will be treated as filenames to read from. Otherwise arguments
  are considered to be regex to search. (could be confusing if you specify
  nonexistent filename!)`

// The Flag function creates a boolean flag, possibly with a negating
// alternative.  Note that you can specify either long or short flags
// naturally in the same list.
var optCount = goopt.Flag([]string{"-c", "--count"}, nil,
    "Print number of matches. (same as grep -c)", "")
var optIgnoreCase = goopt.Flag([]string{"-i", "--ignore-case"}, nil,
    "Case insensitive matching. Default is case sensitive.", "")
var optInvert = goopt.Flag([]string{"-v", "--invert"}, nil,
    "Select non-matching records (same as grep -v).", "")
var optAnd   = goopt.Flag([]string{"-a", "--and"}, nil,
    "Extract records with all of patterns. (default: any)", "")
var optTimestamp  = goopt.Flag([]string{"-t", "--timestamp"}, nil,
    "Same as --rs=TIMESTAMP_REGEX, where the regex matches timestamps often used in log files, e.g., '2014-12-31 12:34:56' or 'Dec 31 12:34:56'.", "")
var optColor = goopt.Flag([]string{"--color", "--hl"}, nil,
    "Highlight matches. Default is enabled iff stdout is a TTY.", "")

const RS_REGEX = "^$|^(=====*|-----*)$"
var rs = goopt.StringWithLabel([]string{"-r", "--rs"}, RS_REGEX, "RS_REGEX",
    fmt.Sprintf("Input record separator. default: /%s/", RS_REGEX))

//const DATETIME_REGEX = ""


//////////////////////////////////////////////////////////////////////////////
//maxBufferSize = 1 * 1024 * 1024


//////////////////////////////////////////////////////////////////////////////
// regex wrapper

type FindIndex func (d []byte) []int
type Regexp struct {
    //r *rubex.Regexp
    r pcre.Regexp
    FindIndex
}


func reComp(restr string) Regexp {
    //nlchars := regexp.MustCompile("\\^|\\$")
    //restr = nlchars.ReplaceAllString(restr, "\n")
    //return regexp.MustCompile( restr )
    //return regexp.MustCompile( "(?m)" + restr )
    //r := rubex.MustCompile(restr)
    //return Regexp{r, r.FindIndex}
    r := pcre.MustCompile( restr, pcre.MULTILINE )
    f := func (d []byte) []int { return r.FindIndex(d, 0) }
    return Regexp{r, f}
}



//////////////////////////////////////////////////////////////////////////////

func checkError(e error) {
    if e != nil {
        fmt.Fprintf(os.Stdout, "ERROR: %d\n", e)
        os.Exit(1)
    }
}

func debug(format string, args ...interface{}) {
    //fmt.Fprintf(os.Stderr, ">> DEBUG: " + format, args...)
}

//escape newline for debug output.
//func esc(s string) {
//    strings.Replace(s, "\n", "\\n", -1)
//}

func esc(b []byte) string {
    return strings.Replace(string(b), "\n", "\\n", -1)
}

//////////////////////////////////////////////////////////////////////////////
// Find-pattern-first algorithm

type PatternFirstFinder struct {
    found bool;
    patFinder   func(data []byte) (int, int)
    rsFinder    func(data []byte) (int, int)
    rsRevFinder func(data []byte) (int, int)
}

func NewPatternFirstFinder(pat, rs string) *PatternFirstFinder{
    //compile regex and set MLRFinder fields
    s := new(PatternFirstFinder)
    s.found = false
    s.patFinder   = func(d []byte) (int, int) { return bytes.Index(d, []byte(pat)), len(pat) }
    s.rsFinder    = func(d []byte) (int, int) { return bytes.Index(d, []byte(rs)), len(rs) }
    s.rsRevFinder = func(d []byte) (int, int) {
        if len(d) < len (rs) {
            return -1, 0
        }
        for pos := 0; pos < len(d); pos++ {
            if bytes.HasSuffix(d[0:len(d)-pos], []byte(rs)) {
                return pos, len(rs)
            }
        }
        return -1, 0
    }
    return s
}

func (s *PatternFirstFinder) Split(data []byte, atEOF bool, tooLong bool) (advance int, token []byte, err error) {
    s.found = false
    //debug("split(\"%s\", %v, %v)\n", esc(data[:60]), atEOF, tooLong)

    if atEOF && len(data) == 0 {
        return 0, nil, nil
    }

    if (tooLong) {
        // so this is retry with tooLong flag enabled; we cannot request more data
        // and there's no match of the pattern in data. So we return
        rsPos, rsSize := s.rsRevFinder(data)
        if rsPos < 0 {
            return 0, nil, errors.New("record is too long and didn't fit into a buffer")
        } else {
            //return non-match records with s.found set to false
            return len(data) - rsPos + rsSize, data[len(data):len(data)], nil
        }
    }

    loc, size := s.patFinder(data)
    if loc < 0 {
        return 0, nil, nil //request more data.
    }
    s.found = true
    debug("patFinder() loc=%d, size=%d, '%s'\n", loc, size, esc(data[loc:loc+size]))
    preLoc := 0
    preSize := 0
    if loc != 0 {
        var lastRsOffset int
        lastRsOffset, preSize = s.rsRevFinder(data[:loc])
        if lastRsOffset > 0 {
            preLoc = loc - lastRsOffset - preSize
        }
    }
    debug("rs='%s'\n", data[preLoc:preLoc+preSize])

    postLoc, postSize := s.rsFinder(data[loc+size:])
    if (postLoc < 0) {
        if (atEOF) {
            return len(data), data[preLoc:], nil
        } else {
            return 0, nil, nil //not enough data
        }
    }
    debug("postLoc, postSize = %d, %d\n", postLoc, postSize)
    debug("post string: %s\n", data[loc+size+postLoc:loc+size+postLoc+postSize])

    recBegin := preLoc+preSize
    recEnd   := loc+size+postLoc+postSize
    rec      := data[recBegin:recEnd]
    debug("RETURN: %d, %s\n", recEnd, esc(rec))
    return recEnd, rec, nil
}

//////////////////////////////////////////////////////////////////////////////
// Find-pattern-first algorithm

type SplitRecordFirstFinder struct {
    found bool
    rsSize int
    rsPos int
    rsFinder func(data []byte) (int, int)
}



func regexFinder(restr string) (func (d []byte) (int, int)) {
    re := reComp(restr)
    return func(d []byte) (int, int) {
        m := re.FindIndex(d)
        if m != nil {
            return m[0], (m[1] - m[0])
        } else {
            return -1, 0
        }
    }
}

func NewSplitRecordFirstFinder(pat, rs string) *SplitRecordFirstFinder{
    s := new(SplitRecordFirstFinder)
    s.rsFinder = regexFinder(rs)
    //s.rsFinder = func(d []byte) (int, int) { return bytes.Index(d, []byte(rs)), len(rs) }
    return s
}

func (s *SplitRecordFirstFinder) Split(data []byte, atEOF bool) (advance int, token []byte, err error) {
    s.rsPos = 0
    if atEOF && len(data) == 0 {
        return 0, nil, nil
    }
    pos, sz := s.rsFinder(data)
    if (pos < 0) {
        if (atEOF) {
            s.rsPos = len(data)
            return len(data), data, nil
        } else {
            return 0, nil, nil //not enough data
        }
    }
    if (pos+sz == 0) {
        //FIXME: is this the best way to handle empty match?
        // The only known case so far is when using /^$/ with (?m) flag
        s.rsPos = 1
        return 1, data[0:1], nil
    } else {
        s.rsPos = pos
        return pos+sz, data[0:pos+sz], nil
    }
}



//////////////////////////////////////////////////////////////////////////////
//Split Record First

// records returned from the splitter is terminted with RS
// for speed reason, but we want to have RS at the begining of
// records (it makes sense if RS is a time stamp or other time
// header info.
type Record struct {
    chunk string
    rsPos int
}

func unsafeStrToByte(s string) []byte {
    strHeader := (*reflect.StringHeader)(unsafe.Pointer(&s))

    var b []byte
    byteHeader := (*reflect.SliceHeader)(unsafe.Pointer(&b))
    byteHeader.Data = strHeader.Data

    // need to take the length of s here to ensure s is live until after we update b's Data
    // field since the garbage collector can collect a variable once it is no longer used
    // not when it goes out of scope, for more details see https://github.com/golang/go/issues/9046
    //l := len(s)
    //byteHeader.Len = l
    //byteHeader.Cap = l
    return b
}

func grep_record(pat string, pipe chan Record, wg* sync.WaitGroup) {
    defer wg.Done()
    var prevRS string
    /*
    // plain text
    for rec := range pipe {
        if strings.Index(rec.chunk, pat) > 0 {
            fmt.Print(prevRS)
            fmt.Print(rec.chunk[:rec.rsPos])
        }
        prevRS = rec.chunk[rec.rsPos:]
    }
    */

    //regex
    re := reComp(pat)
    for rec := range pipe {
        //if ( re.FindIndex([]byte(rec.chunk)) != nil ) {
        if ( re.FindIndex( unsafeStrToByte(rec.chunk) ) != nil ) {
            fmt.Print(prevRS)
            fmt.Print(rec.chunk[:rec.rsPos])
        }
        prevRS = rec.chunk[rec.rsPos:]
        //fmt.Println(">>'" + prevRS + "'")
    }
}


func mlrgrep_srf(pat string, rs string, r io.Reader) {
    var wg sync.WaitGroup
    w := bufio.NewWriter(os.Stdout)
    pipe := make(chan Record, 128)
    scanner := bufio.NewScanner(r)
    splitter := NewSplitRecordFirstFinder(pat, rs)

    scanner.Split(splitter.Split)
    wg.Add(1)
    go grep_record(pat, pipe, &wg)

    for scanner.Scan() {
        rec := scanner.Text()
        pipe <- Record{chunk: rec, rsPos: splitter.rsPos}
    }
    close(pipe)
    wg.Wait()
    w.Flush()
}

//Find Pattern First
func mlrgrep_fpf(pat string, rs string, r io.Reader) {
    w := bufio.NewWriter(os.Stdout)
    scanner := NewScanner(r)
    splitter := NewPatternFirstFinder(pat, rs)

    scanner.Split(splitter.Split)

    for scanner.Scan() {
        line := scanner.Bytes()
        if splitter.found {
            w.Write(line)
        }
    }
    w.Flush()
}



func main() {
    goopt.Description = func() string {
        return "Example program for using the goopt flag library."
    }
    goopt.Version = "0.1"
    goopt.Usage = func() string {
        usage := "Usage: " + Usage
        usage += fmt.Sprintf("%s", Summary)
        usage += fmt.Sprintf("\n%s", goopt.Help())
        return usage
    }
    goopt.Parse(nil)

    var regex []string
    var files []string
    //defer fmt.Print("\033[0m") // defer resetting the terminal to default colors

    debug("os.Args: %s\n", os.Args)
    debug("rs=%s\n", *rs)

    i := 0;
    for _, a := range goopt.Args[i:] {
        if (a == "--") {
            i++
            break;
        }
        // if an argument is a filename for existing one,
        // assume that (and everything follows) as filename.
        f, err := os.Stat(a)
        if (err == nil && !f.IsDir() ) {
            break;
        }
        regex = append(regex, a)
        i++
    }

    for _, a := range goopt.Args[i:] {
        if (a == "--") {
            regex = append(regex, files...)
            files = nil
        }
        files = append(files, a)
    }
    debug("regex: %s\n", regex)
    debug("files: %s\n", files)

    for _, f := range files {
        file, e := os.Open(f)
        checkError(e)
        defer file.Close()
        mlrgrep_srf(regex[0], *rs, file)
        //mlrgrep_fpf(regex[0], *rs, file)
    }
}
