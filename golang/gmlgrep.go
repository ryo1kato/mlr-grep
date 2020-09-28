package main

import (
	"bufio"
	"fmt"
	goopt "github.com/droundy/goopt"
	"io"
	"os"
	"reflect"
	//"regexp"
	"strings"
	"sync"
	"unsafe"
	//pcre "github.com/gijsbers/go-pcre"
	rubex "github.com/go-enry/go-oniguruma"
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
var optAnd = goopt.Flag([]string{"-a", "--and"}, nil,
	"Extract records with all of patterns. (default: any)", "")

//var optTimestamp = goopt.Flag([]string{"-t", "--timestamp"}, nil,
//	"Same as --rs=TIMESTAMP_REGEX, where the regex matches timestamps often used in log files, e.g., '2014-12-31 12:34:56' or 'Dec 31 12:34:56'.", "")
//var optColor = goopt.Flag([]string{"--color", "--hl"}, nil,
//	"Highlight matches. Default is enabled iff stdout is a TTY.", "")

const RS_REGEX = "^$|^(=====*|-----*)$"

var rs = goopt.StringWithLabel([]string{"-r", "--rs"}, RS_REGEX, "RS_REGEX",
	fmt.Sprintf("Input record separator. default: /%s/", RS_REGEX))

//////////////////////////////////////////////////////////////////////////////
//
// regex wrapper (fix import as well)
//
type FindIndex func(d []byte) []int

type Regexp struct {
	//r *regexp.Regexp
	//r pcre.Regexp
	r *rubex.Regexp
	FindIndex
}

func reComp(restr string) Regexp {
	// golang's standard regexp - broken for last newline
	/*
		nlchars := regexp.MustCompile("\\^|\\$")
		restr = nlchars.ReplaceAllString(restr, "\n")
		r := regexp.MustCompile(restr)
		return Regexp{r, r.FindIndex}
	*/

	/* go-oniguruma(rubex) */
	r := rubex.MustCompile(restr)
	f := func(d []byte) []int { return r.FindIndex(d) }

	/* PCRE */
	//r := pcre.MustCompile(restr, pcre.MULTILINE)
	//f := func(d []byte) []int { return r.FindIndex(d, 0) }

	return Regexp{r, f} // common for go-oniguruma & PCRE
}

//////////////////////////////////////////////////////////////////////////////

func checkError(e error) {
	if e != nil {
		fmt.Fprintf(os.Stdout, "ERROR: %d\n", e)
		os.Exit(1)
	}
}

func debug(format string, args ...interface{}) {
	//fmt.Fprintf(os.Stderr, ">> DEBUG: "+format, args...)
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
	found       bool
	patFinder   func(data []byte) (int, int)
	rsFinder    func(data []byte) (int, int)
	rsRevFinder func(data []byte) (int, int)
}

//////////////////////////////////////////////////////////////////////////////
// Find-pattern-first algorithm

type SplitRecordFirstFinder struct {
	found    bool
	rsSize   int
	rsPos    int
	rsFinder func(data []byte) (int, int)
}

func regexFinder(restr string) func(d []byte) (pos int, size int) {
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

func NewSplitRecordFirstFinder(pat, rs string) *SplitRecordFirstFinder {
	s := new(SplitRecordFirstFinder)
	s.rsFinder = regexFinder(rs)
	return s
}

func (s *SplitRecordFirstFinder) Split(data []byte, atEOF bool) (advance int, token []byte, err error) {
	s.rsPos = 0
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}
	pos, sz := s.rsFinder(data)
	if pos < 0 {
		if atEOF {
			s.rsPos = len(data)
			return len(data), data, nil
		} else {
			return 0, nil, nil //not enough data
		}
	}
	if pos+sz == 0 {
		//FIXME: is this the best way to handle empty match?
		// The only known case so far is when using /^$/ with (?m) flag
		s.rsPos = 1
		return 1, data[0:1], nil
	} else {
		debug("Not empty\n")
		s.rsPos = pos
		return pos + sz, data[0 : pos+sz], nil
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
	l := len(s)
	byteHeader.Len = l
	byteHeader.Cap = l
	return b
}

func grep_record(pat string, pipe chan Record, wg *sync.WaitGroup) {
	defer wg.Done()
	var prevRS string

	re := reComp(pat)
	for rec := range pipe {
		debug("------------------------------------\n")
		if re.FindIndex(unsafeStrToByte(rec.chunk)) != nil {
			fmt.Print(prevRS)
			fmt.Print(rec.chunk[:rec.rsPos])
		}
		prevRS = rec.chunk[rec.rsPos:]
		debug("prevRS='%s'\n", prevRS)
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

func main() {
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

	debug("os.Args: %s\n", os.Args)
	debug("rs=%s\n", *rs)

	i := 0
	for _, a := range goopt.Args[i:] {
		if a == "--" {
			i++
			break
		}
		// if an argument is a filename for existing one,
		// assume that (and everything follows) as filename.
		f, err := os.Stat(a)
		if err == nil && !f.IsDir() {
			break
		}
		regex = append(regex, a)
		i++
	}

	for _, a := range goopt.Args[i:] {
		if a == "--" {
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
	}
}
