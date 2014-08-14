--
-- hmlgrep - Haskell Multi-Line Grep
--
{-
TODOs:
    * FIX: Text.Regex.Posix.String died: (ReturnCode 17,"illegal byte sequence")
    * FIX: '-' and '--' handling in optparse-applicative
    * String to ByteString ? http://www.haskell.org/haskellwiki/Wc
    * Automated tests
    * Implement match highlight
    * Show filenames if multiple file input
    * Use Boyer-Moore for non-regex patterns using stringsearch library:
      http://hackage.haskell.org/package/stringsearch-0.3.3/docs/Data-ByteString-Search.html

INSTALL
    $ cabal install directory
    $ cabal install optparse-appricative
    $ ghc --make hmlgrep.hs
-}

import Control.Monad
import Data.List
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Regex

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

default_rs = "^$|^(====*|----)*$"
re_month= "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Dec)"
re_isodate = "20[0-9][0-9]-(0[0-9]|11|12)-(0[1-9]|[12][0-9]|3[01])"
re_time = "[0-2][0-9]:[0-5][0-9]:[0-5][0-9]"

timestamp_rs = "^(" ++ re_month ++ "[ \t]*[0-9][0-9]?,?|" ++ re_isodate
                ++ ")[ \t]*" ++ re_time


----------------------------------------------------------------------------

data HmlGrepOpts = HmlGrepOpts {
                     opt_andor  :: Bool
                   , opt_rs :: Maybe String
                   , opt_timestamp :: Bool
                   , opt_count  :: Bool
                   , opt_invert :: Bool
                   , ignoreCase :: Bool
                   , opt_args :: [String]
                 }

type LogEntry = (String, [String])
type Log      = [LogEntry]
type Pattern  = Regex


----------------------------------------------------------------------------
--
-- main logic
--
containPattern :: Pattern -> String -> Bool
containPattern re str = matchRegex re str /= Nothing


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


log2lines :: Log -> [String]
log2lines [] = []
log2lines ((h, l):[])   = h : l
log2lines (([], l):logs) = l ++ (log2lines logs)
log2lines ((h, l):logs) = h : l ++ (log2lines logs)



hmlgrep' :: HmlGrepOpts -> [Pattern] -> Log -> Log
hmlgrep' _ [] log = log
hmlgrep' _ _ [] = []
hmlgrep' opts pattern log
    | opt_invert opts = filter (not.matcher) log
    | otherwise       = filter (matcher) log
    where matcher = matchRecord (opt_andor opts) pattern

hmlgrep :: HmlGrepOpts -> [String] -> String -> (String, Bool)
hmlgrep opts patterns indata =
    if do_command == []
    then (toString do_command, False)
    else (toString do_command, True)
    where recsep = if opt_timestamp opts
                   then timestamp_rs
                   else withDefault default_rs $ opt_rs opts
          logs   = lines2log (toRegex recsep) $ lines indata
          toString = if opt_count opts
                     then show.length
                     else unlines.log2lines
          do_command = hmlgrep' opts (map toRegex patterns) logs
          toRegex str = mkRegexWithOpts str True (not $ ignoreCase opts)


----------------------------------------------------------------------------
--
-- Run as a Unix command-line filter (pipe)
--
runPipe :: (String -> (String, Bool)) -> Handle -> [Handle] -> IO Bool
runPipe cmd outHandle inHandles = do
    streams <- forM inHandles hGetContents
    case (cmd $ concat streams) of
        (result, ret) -> do
            hPutStr outHandle result
            return ret


withDefault :: a -> (Maybe a) -> a
withDefault def (Just val) = val
withDefault def Nothing = def


runWithOptions :: HmlGrepOpts -> IO ()
runWithOptions opts = do
    (ps, fs) <- splitArg (opt_args opts)
    ret <- if fs == []
           then runPipe (mainProc ps) stdout [stdin]
           else forM fs openRO >>= runPipe (mainProc ps) stdout
    if ret
    then exitSuccess
    else exitFailure
    where
        mainProc = hmlgrep opts
        openRO fname
            -- FIXME. optparse-applicative seems to strips off
            -- all '-' and '--' occurrence in arguments
            | fname == "-"  = return stdin
            | otherwise     = openFile fname ReadMode


----------------------------------------------------------------------------
-- Parse ARG1 ARG2 [--] ARG3 ARG4 to ([ARG1, ARG2], [ARG3, ARG4])
splitArg' :: [String] -> [String] -> IO ([String], [String])
splitArg' ps [] = return (ps, [])
splitArg' ps (a:as)
    | a == "-"  = return (ps, as)
    | otherwise = do
        isFile <- doesFileExist a
        if isFile
        then return (ps, a:as)
        else (splitArg' (ps++[a]) as)

splitArg = splitArg' []


main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> parser) ( fullDesc
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
      <*> switch (short 't' <> long "timestamp" <>
                  help ("Same as --rs=TIMESTAMP_REGEX, where the regex matches " ++
                       "timestamps often used in log files, e.g., " ++
                       "'2014-12-31 12:34:56' or 'Dec 31 12:34:56'."))
      <*> switch (short 'c' <> long "count" <>
                  help "Print number of matches. (same as grep -c)")
      <*> switch (short 'v' <> long "invert" <>
                  help "Select non-matching records (same as grep -v).")
      <*> switch (short 'i' <> long "ignore-case" <>
                  help "Case insensitive matching. Default is case sensitive")
      <*> some (argument str (metavar "PATTERN[...] [--] [FILES...]"))


-- vim: set makeprg=ghc
