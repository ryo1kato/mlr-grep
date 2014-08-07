--
-- hmlgrep - Haskell Multi-Line Grep
--
import Text.Regex
import System.IO
import System.Environment

progname = "hmlgrep"

help = "Usage: " ++ progname ++
  " [OPTIONS...] PATTERN[...] [--] [FILES...]\n" ++
  "Find multi-line record with PATTERN(s) in FILE(s) or stdin\n" ++
  "\n" ++
  "OPTIONS\n" ++
  "  -h,--help          Print this help.\n" ++
  "  -v,--invert-match  Select non-matching lines (same as grep -v).\n" ++
  "  -i,--ignore-case   Case-insensitive match (same as grep -i).\n" ++
  "  -c,--count         Print number of matches.\n" ++
  "  -r,--rs=REGEX      Set record separator to REGEX.\n" ++
  "  -o,--ors=STRING    Set output record separator to STRING.\n"

printHelp:: IO ()
printHelp = do System.IO.hPutStr stdout (help)

----------------------------------------------------------------------------
default_rs = "^$|----*"
default_ors = Nothing



----------------------------------------------------------------------------
-- data Log      = [String] deriving Show
type LogEntry = (String, [String])
type Log      = [LogEntry]
type Pattern  = String -- Regex String (not compiled) deriving Show

----------------------------------------------------------------------------
containPattern :: Pattern -> String -> Bool
containPattern re str = matchRegex (mkRegex re) str /= Nothing


linesContainRegex :: [String] -> Pattern -> Bool
linesContainRegex lines re = or $ map (containPattern re) lines


matchRecord :: Bool -> [Pattern] -> LogEntry -> Bool
matchRecord andor patterns (header, lines)
    | andor     = and $ map (linesContainRegex lines) patterns
    | otherwise = or  $ map (linesContainRegex lines) patterns


toLogEntry :: Pattern -> [String] -> LogEntry
toLogEntry _ [] = ([],[])
toLogEntry sep (l:ls) = if containPattern sep l
                then (l, ls)
                else ([], (l:ls))


lines2log :: Pattern -> [String] -> Log
lines2log sep [] = []
lines2log sep (l:ls) = head : tail
    where head = toLogEntry sep $ l:(takeWhile notsep ls)
          tail = lines2log sep (dropWhile notsep ls)
          notsep line = not (containPattern sep line)

hmlgrep :: Bool -> [Pattern] -> Log -> Log
hmlgrep _ [] log = log
hmlgrep _ _ [] = []
hmlgrep andor pattern log = filter (matchRecord andor pattern) log

hmlgrep_lines :: Bool -> [Pattern] -> Pattern -> [String] -> Log
hmlgrep_lines andor patterns rs lines = hmlgrep andor patterns (lines2log rs lines)


log2lines :: Log -> [String]
log2lines [] = []
log2lines ((h, l):[])   = h : l
log2lines (([], l):logs) = l ++ (log2lines logs)
log2lines ((h, l):logs) = h : l ++ (log2lines logs)


mainProc pat lines = log2lines $ hmlgrep_lines True [pat] default_rs lines

----------------------------------------------------------------------------
interactWith function inputStream outputStream = do
    input <- hGetContents inputStream
    hPutStr outputStream $ unlines . function . lines $ input
    hFlush outputStream

--
-- FIXME: Use optparse-applicative to parse commandline options
--
main :: IO ()
main = do args <- getArgs
          case args of
              ["-h"]     -> printHelp
              ["--help"] -> printHelp
              []         -> printHelp
              [pat, input]   -> do inputStream  <- openFile input ReadMode
                                   interactWith (mainProc pat) inputStream stdout
              [pat]          -> interactWith (mainProc pat) stdin stdout

-- vim: set makeprg=ghc
