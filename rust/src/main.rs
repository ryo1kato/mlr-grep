extern crate getopts;
extern crate libc;
extern crate regex;
extern crate itertools;

use getopts::Options;
use regex::Regex;
use std::boxed::Box;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::fs::File;
use std::io::{Read,Write,BufReader,BufRead,BufWriter,Error};
use std::process::exit;
use itertools::Itertools;

//use std::panic;

static RS_REGEX : &'static str = "^$|^(=====*|-----*)$";
static RS_TIMESTAMP : &'static str = concat!(
    "^(",
    "((Mon|Tue|Wed|Thu|Fri|Sat),?[ \t]+)?",  // Day of Week
    "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Dec),?[ \t]", // Months
    "[0-9]{1,2},?", // Date
    "[ \t]",
    "[0-2][0-9]:[0-5][0-9](:[0-5][0-9])?", // Time
    "(,?[ \t]20[0-9][0-9])?", //Year
    "|", "20[0-9][0-9]-(0[0-9]|11|12)-(0[1-9]|[12][0-9]|3[01])", // ISO date
    ")"
);

fn define_opts() -> getopts::Options {
    let mut opts = Options::new();
    opts.optflag("a", "and",    "Extract records with all of patterns.");
    opts.optflag("h", "help",   "print this help.");
    opts.optopt( "r", "rs", format!("Input record separator. default: /{}/", RS_REGEX).as_str(), "RS_REGEX");
    opts.optflag("t","timestamp",   "Same as --rs=TIMESTAMP_REGEX, where the regex matches\n\
                                     timestamps often used in log files, e.g., '2014-12-31\n\
                                     12:34:56' or 'Dec 31 12:34:56'.");
    opts.optflag("i","ignore-case", "Case insensitive matching.");
    opts.optflag("c","count",   "Print number of matches. (same as grep -c)");
    //opts.optflag("v","invert",  "Select non-matching records. (same as grep -v)");
    //opts.optflag("", "color",   "Highlight matches. Auto-enabled iff stdout is a TTY.");
    //opts.optflag("", "hl",      "Same as --color.");
    //opts.optflag("m","mono",    "Do not Highlight matches.");
    opts.optflag("d","debug",   "Enable debug.");

    return opts;
}


fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} [OPTIONS...] REGEX[...] [/] [FILE]", program);
    print!("{}", opts.usage(&brief));
}


/* FIXME: should use */
fn log(header: &str, msg: &str) {
    match writeln!(&mut std::io::stderr(), "{}: {}", header, msg) {
        Ok(_) => (),
        Err(e) => { panic!("Failed writing to stderr: {}", e) }
    };
}
fn warn(msg: &str)  { log("WARNING", msg); }
//fn debug(msg: &str) { log("DEBUG", msg); }
//fn error(msg: &str) { log("ERROR", msg); } /* FIXME: */

/***********************************************************************/

fn fputs(bw:&mut BufWriter<Box<Write>>, s:&str) -> Result<(), Error>{
     bw.write_all(s.as_bytes());
     bw.write_all("\n".as_bytes())
}


fn re_to_plain_string(s:&str) -> Result<String, ()> {
    let regex_chars      = r"^$(|)[]{}.*\";
    let regex_ctrl_chars = r"nt";
    let mut to_ctrl_char = HashMap::new();
    to_ctrl_char.insert('n', '\n');
    to_ctrl_char.insert('t', '\t');

    let mut escaped = String::new();

    let mut i = s.chars().peekable();
    loop {
        if let Some(c) = i.next() {
            if c == '\\' {
                match i.next() {
                    None => { escaped.push(c) }
                    Some(nc) => {
                        if regex_ctrl_chars.contains(nc) {
                            escaped.push(*(to_ctrl_char.get(&nc).unwrap()));
                        } else if regex_chars.contains(nc) {
                            escaped.push(nc);
                        } else {
                            return Err(());
                        }
                    }
                }
            } else if regex_chars.contains(c) {
                return Err(());
            } else {
                escaped.push(c);
            }
        } else {
            break;
        }
    }
    return Ok(escaped);
}


fn mlrgrep<M,G,R>(rs_matcher:M,
                  grep:G,
                  in_reader:Box<BufRead>,
                  mut reporter:R,
                  counter: &mut i32)
        -> Result<bool, Error>
    where M: Fn(&str) -> bool,
          G: Fn(&str) -> bool,
          R: FnMut(&str) -> Result<(),Error>
{
    let mut iter = in_reader.lines();
    let mut found = false;
    let mut matching = false;

    let mut rec : Vec<String> = Vec::new();
    while let Some(line) = iter.next() {
        if rs_matcher(line.as_ref().unwrap().as_str()) {
            matching = false;
            rec = Vec::new();
            if grep(line.as_ref().unwrap().as_str()) {
                matching = true;
                found = true;
                *counter += 1;
                reporter(&line.unwrap());
            } else {
                rec.push( line.unwrap() );
            }
        } else if matching {
            reporter(&line.unwrap());
        } else if grep(line.as_ref().unwrap().as_str()) {
            matching = true;
            found = true;
            *counter += 1;
            for l in rec {
                reporter(&l);
            }
            rec = Vec::new();
            reporter(&line.unwrap());
        } else {
            rec.push(line.unwrap());
        }
    }

    return Ok(found);
}



fn do_grep(flags: getopts::Matches,
           patterns: Vec<String>,
           inputs: Vec<(String, Box<Read>)>,
           output: Box<Write>)
    -> Result<bool, Error>
{
    let rs = if flags.opt_present("timestamp") {
                 String::from(RS_TIMESTAMP)
             } else if let Some(rs) = flags.opt_str("rs") {
                 rs
             } else {
                 String::from(RS_REGEX)
             };

    let pat = &patterns[0].as_str();

    let ( rs_regex, pat ) =
        if flags.opt_present("ignore-case") {
            ( Regex::new( & (String::from("(?i)") + &rs)  ).unwrap(),
              Regex::new( & (String::from("(?i)") + &pat) ).unwrap() )
        } else {
            ( Regex::new(&rs).unwrap(),
              Regex::new(&pat).expect("Failed to compile pattern to regex") )
        };


    let mut buf_output = BufWriter::new(output);
    let mut rs_matcher = |line:&str| rs_regex.is_match(line);
    let mut grep       = |line:&str| pat.is_match(line);

    let fcount = inputs.len();
    let mut found = false;
    for (fname,reader) in inputs {
        let mut ireader = Box::new(BufReader::new(reader));
        let mut counter = 0i32;
        {
            let mut printer = |found:&str| {
                if flags.opt_present("count") {
                     Ok(())
                } else {
                     fputs(&mut buf_output, found)
                }};
            match mlrgrep(&rs_matcher, &grep, ireader, &mut printer, &mut counter)
            {
                Err(e) => { return Err(e) }
                Ok(ret) => { found = true }
            }
        }
        if flags.opt_present("count") {
            let count_str = if fcount > 1 {
                format!("{}:{}", fname, counter)
            } else {
                format!("{}", counter)
            };
            fputs(&mut buf_output, &count_str);
        }
    }

    return Ok(found);
}


/* Parse args into vec of patterns and filenames
 * A '/' or any valid (existing) filename will stop reading args as patterns
 */
fn parse_pattern_or_filename(args: Vec<String>)
    -> (Vec<String>, Vec<String>)
{
    let mut patterns = Vec::new();
    let mut filenames = Vec::new();

    let mut i = args.iter();
    for a in &mut i {
        if a == "/" {
            break;
        } else {
            match fs::metadata(a) {
                Ok(stat)  => {
                    if stat.is_dir() {
                        // ignore directory names.
                        patterns.push(a.clone());
                    } else {
                        filenames.push(a.clone()); break;
                    }
                }
                Err(_) => { patterns.push(a.clone()); }
            }
        }
    }
    for a in &mut i {
       filenames.push(a.clone());
    }
    return (patterns, filenames);
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let progname = args[0].clone();
    let istty = unsafe { libc::isatty(libc::STDIN_FILENO as i32) } != 0;

    /* Parse command-line options */
    let optdef  = define_opts();
    let opts = match optdef.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    if opts.opt_present("h") {
        print_usage(&progname , optdef);
        exit(0);
    }

    if opts.free.is_empty() {
        panic!("Too few arguments");
    }

    let (patterns, filenames) = parse_pattern_or_filename(opts.free.clone());
    if opts.opt_present("debug") {
        println!("DEBUG: patterns: {}", patterns.join(", "));
        println!("DEBUG: files:    {}", filenames.join(", "));
    }


    let inputs: Vec<(String, Box<Read>)>;
    if filenames.len() == 0 {
        inputs = vec![(String::from("-"), Box::new(std::io::stdin()))];
        if istty {
            warn("reading from terminal");
        }
    } else {
        inputs = filenames.iter().map( |fname| {
                    let reader = Box::new(File::open(fname).expect(fname));
                    (fname.clone(), reader as Box<Read>) }
                 ).collect();
    };


    match do_grep(opts, patterns, inputs, Box::new(std::io::stdout()) as Box<Write>) {
        Ok(ret) => {
            if ret {
                exit(0);
            } else {
                exit(1);
            }
        }
        Err(_) => exit(2)
    }
}

