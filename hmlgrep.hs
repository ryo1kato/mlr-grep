--
-- hmlgrep - Haskell Multi-Line Grep
--
import Text.Regex
import System.IO
import System.Environment

progname = "hmlgrep"

help = "Usage: " ++ progname ++
  " [OPTIONS...] PATTERN[...] [--] [FILES...]\n" ++
  "Find multi-line record with PATTERN in FILES or stdin\n" ++
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
containRegex :: String -> String -> Bool
containRegex re str = matchRegex (mkRegex re) str /= Nothing

linesContainRegex :: [String] -> String -> Bool
linesContainRegex lines re = or $ map (containRegex re) lines


matchRecord :: Bool -> [String]-> [String] -> Bool
matchRecord logiAnd res lines
    | logiAnd   = and $ map (linesContainRegex lines) res
    | otherwise = or  $ map (linesContainRegex lines) res

hmlgrep :: [String] -> [String]
hmlgrep lines
    | matchRecord True ["foo"] lines = lines
    | otherwise = ["<no match>"]

-- hmlgrep :: Bool [String] String [String] -> [String]
-- hmlgrep logiAnd delim patterns lines =
--     filter matchRecord
--     takeWhile containRegex delim
--     dropWhile
--     splitRecords delim

-- splitRecords :: String [String]
-- splitRecords delim lines =


----------------------------------------------------------------------------
interactWith function inputStream outputStream = do
    -- datestr <- getDateStringToday
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
              [input]        -> do inputStream  <- openFile input ReadMode
                                   interactWith hmlgrep inputStream stdout
              []             -> interactWith hmlgrep stdin stdout
              _              -> printHelp

-- vim: set makeprg=ghc\ --make\ %
