--
-- hmlgrep - Haskell Multi-Line Grep
--
{-
TODOs:
    * implement --count
    * implement --invert-match
    * implement --timestamp
    * implement highlight

-}
import Text.Regex
import Data.List
import System.IO
import System.Environment
import System.Posix.Files
import qualified System.IO.Streams as S
import Options.Applicative
import Control.Monad

helpdoc = concat $ intersperse " "
    [
      "grep(1) like tool, but \"record-oriented\", instead of line-oriented,",
      "to search and print multi-line log entries separated by empty lines,",
      "'----' or timestamps, etc.",
      "If an argument in argument list is a name of",
      "existing file or '-', that argument and",
      "everything after that will be treated as filenames to read from.",
      "Otherwise arguments are considered to be patterns. ('-' means stdin)",
      "(could be confusing if you specify nonexistent filename!)",
      "If a file name ends with .gz, .bz2 or .xz, uncompress it on-the-fly before",
      "reading from it."
    ]


----------------------------------------------------------------------------
default_rs = "^$|^----*$"

data HmlGrepOpts = HmlGrepOpts {
                     andor  :: Bool
                   , rs :: Maybe String
             --    , timestamp :: Bool
             --    , count  :: Bool
             --    , invert :: Bool
             --    , ignoreCase :: Bool
                   , args :: [String]
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

hmlgrep andor rs patterns lines =
    log2lines $ hmlgrep' andor patterns (lines2log rs lines)


log2lines :: Log -> [String]
log2lines [] = []
log2lines ((h, l):[])   = h : l
log2lines (([], l):logs) = l ++ (log2lines logs)
log2lines ((h, l):logs) = h : l ++ (log2lines logs)


----------------------------------------------------------------------------
runPipe cmd inHandles = do
    streams <- forM inHandles hGetContents
    hPutStr stdout $ unlines . cmd . lines $ concat streams
    hFlush stdout

get_rs (Just rs) = rs
get_rs Nothing   = default_rs

runWithOptions :: HmlGrepOpts -> IO ()
runWithOptions opts = do
    (ps, fs) <- splitArg (args opts)
    if fs == []
        then runPipe (mainProc ps) [stdin]
        else forM fs openRO >>= runPipe (mainProc ps)
    where
        mainProc = hmlgrep (andor opts) (get_rs $ rs opts)
        openRO fname
            | fname == "-"  = return stdin
            | otherwise     = openFile fname ReadMode

----------------------------------------------------------------------------
-- Parse ARG1 ARG2 [--] ARG3 ARG4 to ([ARG1, ARG2], [ARG3, ARG4])
splitArg' :: [String] -> [String] -> IO ([String], [String])
splitArg' ps [] = return (ps, [])
splitArg' ps (a:as)
    | a == "-"  = return (ps, as)
    | otherwise = do
        isFile <- fileExist a
        if isFile
        then return (ps, a:as)
        else (splitArg' (ps++[a]) as)

splitArg = splitArg' []

----------------------------------------------------------------------------
main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> parser) ( fullDesc
             -- <> progDesc "grep for multi-line text data files like logs"
             <> progDesc helpdoc
            )
    parser = HmlGrepOpts
      <$> switch (short 'a'  <>
                  long "and" <>
                  help "Extract records with all of patterns (default: any)")
      <*> (optional $ strOption (
             short 'r' <>
             long "rs" <>
             metavar "RS_REGEX" <>
             help ("Input record separator. default: /" ++ default_rs ++ "/") ) )
      <*> some (argument str (metavar "PATTERN[...] [--] [FILES...]"))


-- vim: set makeprg=ghc
