--
-- hmlgrep - Haskell Multi-Line Grep
--
import Text.Regex
import System.IO
import System.Environment
import Options.Applicative


progname = "hmlgrep"

help_string = "Usage: " ++ progname ++
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
printHelp = do System.IO.hPutStr stdout (help_string)

----------------------------------------------------------------------------
default_rs = "^$|----*"
default_ors = Nothing

data HmlGrepOpts = HmlGrepOpts {
                     andor  :: Bool
                   , rs :: Maybe String
                   , patterns :: [String]
             --    , count  :: Bool
             --    , invert :: Bool
             --    , ignoreCase :: Bool
                 }


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


hmlgrep' :: Bool -> [Pattern] -> Log -> Log
hmlgrep' _ [] log = log
hmlgrep' _ _ [] = []
hmlgrep' andor pattern log = filter (matchRecord andor pattern) log

hmlgrep andor patterns rs lines =
    log2lines $ hmlgrep' andor patterns (lines2log rs lines)


log2lines :: Log -> [String]
log2lines [] = []
log2lines ((h, l):[])   = h : l
log2lines (([], l):logs) = l ++ (log2lines logs)
log2lines ((h, l):logs) = h : l ++ (log2lines logs)


----------------------------------------------------------------------------
interactWith function inputStream outputStream = do
    input <- hGetContents inputStream
    hPutStr outputStream $ unlines . function . lines $ input
    hFlush outputStream

runWithOptions :: HmlGrepOpts -> IO ()
runWithOptions opts = interactWith mainProc stdin stdout
    where
        mainProc = hmlgrep a p r
        p = patterns opts
        a = andor opts
        r = get_rs $ rs opts

get_rs (Just rs) = rs
get_rs Nothing   = default_rs

--
-- FIXME: Use optparse-applicative to parse commandline options
--
main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info parser ( fullDesc
             <> progDesc "grep for multi-line text data files like logs")
    parser = HmlGrepOpts
      <$> switch (short 'a'  <>
                  long "and" <>
                  help "Extract records with all of patterns (default any)")
      <*> (optional $ strOption (
             short 'r' <>
             long "rs" <>
             metavar "RECORD_SEPARATOR" <>
             help ("Input record separator. default: " ++ default_rs) ) )
      <*> some (argument str (metavar "PATTERN"))




-- vim: set makeprg=ghc
