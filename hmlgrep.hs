--
-- hmlgrep - Haskell Multi-Line Grep
--

{-# LANGUAGE CPP #-}

{-
TODOs:
    * FIX: '-' and '--' handling in optparse-applicative
        * https://github.com/pcapriotti/optparse-applicative/pull/99
    * Use Cabal for build?
    * Use Boyer-Moore for non-regex patterns using stringsearch library:
      http://hackage.haskell.org/package/stringsearch-0.3.3/docs/Data-ByteString-Search.html

INSTALL
    $ cabal install directory
    $ cabal install optparse-appricative
    $ cabal install regex-pcre
    $ cabal install ansi-terminal
    $ ghc --make hmlgrep.hs
-}


import Control.Monad
import Data.Int
import Data.Maybe
import Options.Applicative
import System.Console.ANSI
import System.Directory
import System.Exit
import System.Posix.IO ( stdInput, stdOutput )
import System.Posix.Terminal ( queryTerminal )
import Text.Regex.PCRE
import qualified Data.List as DL

#if defined(USE_STRING)
---- Using String is x30 slower than ByteString.Lazy ----
import Prelude as BS
import System.IO as BS
type ByteStr = String
type BSInt = Int
pack = id
#else
---- ByteString.Lazy ----
import System.IO
import Text.Regex.PCRE.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS
type ByteStr = BS.ByteString
type BSInt = Int64
pack = BS.pack
#endif


helpdoc = concat $ DL.intersperse " "
    [ "grep(1) like tool, but \"record-oriented\", instead of line-oriented."
    , "Useful to search/print multi-line log entries separated by e.g., empty lines,"
    , "'----' or timestamps, etc."
    , "If an argument in argument list is a name of"
    , "existing file or '-' (means stdin), such argument and"
    , "all arguments after that will be treated as filenames to read from."
    , "Otherwise arguments are considered to be regex to search."
    , "(could be confusing if you specify nonexistent filename!)"
--   ,"If a file name ends with .gz, .bz2 or .xz, uncompress it on-the-fly before"
--   ,"reading from it."
    ]

default_rs   = "^$|^(=====*|-----*)$"

re_dow     = "((Mon|Tue|Wed|Thu|Fri|Sat),?[ \t]+)?"
re_month   = "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Dec),?[ \t]"
re_date    = "[0-9]{1,2},?"
re_time    = "[0-2][0-9]:[0-5][0-9](:[0-5][0-9])?"
re_year    = "(,?[ \t]20[0-9][0-9])?"
re_dty     = re_date ++ "[ \t]" ++ re_time ++ re_year
re_isodate = "20[0-9][0-9]-(0[0-9]|11|12)-(0[1-9]|[12][0-9]|3[01])"

timestamp_rs = "^(" ++ re_dow ++ re_month ++ re_dty ++ "|"
                    ++ re_isodate ++ ")"


----------------------------------------------------------------------------

data HmlGrepOpts = HmlGrepOpts {
                     opt_and  :: Bool
                   , opt_rs :: Maybe String
                   , opt_timestamp :: Bool
                   , opt_count  :: Bool
                   , opt_invert :: Bool
                   , ignoreCase :: Bool
                   , opt_highlight :: Bool
                   , opt_mono :: Bool
                   , opt_args :: [String]
                 }

-- type LogEntry = (Maybe String, [String])
type LogEntry = (Maybe ByteStr, [ByteStr])
type Log      = [LogEntry]
type Pattern  = Regex


----------------------------------------------------------------------------
--
-- Match Highlights
--
-- hlCode  = setSGRCode [SetColor Foreground Vivid Red]
hlCode  = setSGRCode [SetSwapForegroundBackground True]
hlReset = setSGRCode [Reset]

highlightRange :: ByteStr -> (Int, Int) -> ByteStr
highlightRange str mch = BS.concat [ (BS.take start str), (pack hlCode) ,
                       (BS.take len $ BS.drop start str), (pack hlReset) ,
                       (BS.drop (start+len) str)]
    -- ByteString versions of 'take', 'drop', etc. take Int64, not Int
    where start = fromIntegral (fst mch) :: BSInt
          len   = fromIntegral (snd mch) :: BSInt

-- lighlight matches in ByteString with _reverse sorted_ list of matches
-- It has to be reversed so that we don't have to re-calculate offset everytime
-- control codes are inserted.
highlightRangesRSorted :: ByteStr -> [(Int, Int)] -> ByteStr
highlightRangesRSorted str [] = str
highlightRangesRSorted str (r:rs) = highlightRangesRSorted (highlightRange str r) rs


allMatchAsList re str = getAllMatches $
    (match re str :: AllMatches [] (MatchOffset, MatchLength))


highlightAllMatches re str =
    if m == []
    then Nothing
    else Just (highlightRangesRSorted str (reverse m))
    where m = allMatchAsList re str

highlightAllMatchesLines re ls =
    if concat ms == []
    then Nothing
    else Just (zipWith highlight ls ms)
    where ms = map (allMatchAsList re) ls
          highlight str m = highlightRangesRSorted str (reverse m)

----------------------------------------------------------------------------
--
-- line parsing and regex matching
--

-- Similar to =~ but RHS is Regex
(==~) :: ByteStr -> Regex -> Bool
(==~) source re = match re source


toLogEntry _ [] = (Nothing, [])
toLogEntry sep (l:ls) = if (l ==~ sep)
                then (Just l, ls)
                else (Nothing, (l:ls)) -- First record without header(separator)


lines2log sep [] = []
lines2log sep (l:ls) = head : tail
    where head = toLogEntry sep $ l:(takeWhile notsep ls)
          tail = lines2log sep (dropWhile notsep ls)
          notsep line = not (line ==~ sep)


log2lines [] = []
log2lines ((Nothing, l):[])  = l
log2lines ((Just h, l):[])   = h : l
log2lines ((Nothing, l):logs) = l ++ (log2lines logs)
log2lines ((Just h, l):logs) = h : l ++ (log2lines logs)

logEntry2lines (hdr,ls) = case hdr of
                               Nothing -> ls
                               Just x  -> (x:ls)


matchAny lines re = or $ map (match re) lines

matchRecord p le = matchAny (logEntry2lines le) p

matchRecordAll ps le = and $ map (matchAny $ logEntry2lines le) ps

matchRecordHighlight p (maybehdr,ls)
    | isNothing maybehdr =
        if isNothing hl_ls
        then Nothing
        else Just (Nothing, fromMaybe ls hl_ls)
    | otherwise =
        if isNothing hl_hdr && isNothing hl_ls
        then Nothing
        else Just (Just (fromMaybe hdr hl_hdr), fromMaybe ls hl_ls)
        where
            hl_hdr = highlightAllMatches p hdr
            hl_ls  = highlightAllMatchesLines p ls
            hdr    = fromJust maybehdr


----------------------------------------------------------------------------
--
-- main logic
--

toRE opts str = makeRegexOpts (ic) execBlank str
    where ic = if (ignoreCase opts) then compCaseless else compBlank

-- all RE strings combined with '|'
-- used for OR search and highlights
composeRE opts str = toRE opts $ DL.intercalate "|" str

hmlgrep_hl re log = catMaybes $ map (matchRecordHighlight re) log

hmlgrep' :: HmlGrepOpts -> Bool -> [String] -> Log -> Log
hmlgrep' _ _ [] log = []
hmlgrep' _ _ _ [] = []
hmlgrep' opts hl patterns log
    | o_invert        = filter (not.matcher) log -- never highlights
    | hl              = if o_and
                        then hmlgrep_hl regexOR $ filter (matcher) log
                        else hmlgrep_hl regexOR log
    | otherwise       = filter (matcher) log
    where
          -- when there's only one pattern, opt_and is meaningless
          o_and    = (opt_and opts) && (length patterns) /= 1
          o_invert = opt_invert opts
          regexs   = map (toRE opts) patterns
          regexOR  = composeRE opts patterns
          matcher  = if o_and
                     then matchRecordAll regexs
                     else matchRecord regexOR


hmlgrep :: HmlGrepOpts -> Bool -> [String] -> ByteStr -> (ByteStr, Bool)
hmlgrep opts hl patterns indata =
    if do_command == []
    then (toString do_command, False)
    else (toString do_command, True)
    where recsep = if opt_timestamp opts
                   then timestamp_rs
                   else fromMaybe default_rs (opt_rs opts)
          logs   = lines2log (toRE opts recsep) $ BS.lines indata
          toString = if opt_count opts
                     then (\x -> pack (((show.length) x) ++ "\n"))
                     else BS.unlines.log2lines
          do_command = hmlgrep' opts hl patterns logs


----------------------------------------------------------------------------
--
-- Run as a Unix command-line filter (pipe)
--
runPipe' cmd outHandle inHandles = do
    streams <- forM inHandles BS.hGetContents
    case (cmd $ BS.concat streams) of
        (result, ret) -> do
            BS.hPutStr outHandle result
            return ret

runPipe :: (ByteStr -> (ByteStr, Bool)) -> Handle -> Handle -> IO Bool
runPipe cmd outHandle inHandle = do
    stream <- BS.hGetContents inHandle
    case cmd stream of
        (result, ret) -> do
            BS.hPutStr outHandle result
            return ret

warning str = hPutStr stderr ("WARNING: " ++ str ++ "\n")
warnIfTerminal = do
    stdIsTty <- queryTerminal stdInput
    if stdIsTty then warning "Reading from terminal"
                else return ()

runWithOptions :: HmlGrepOpts -> IO ()
runWithOptions opts = do
    (ps, fs) <- splitArg (opt_args opts)
    istty <- queryTerminal stdOutput
    -- If input is stdin of a terminal, probably it's not the user wants.
    let runPipeCmd = runPipe (hmlgrep opts (useColor opts istty) ps) stdout
    let runPipeCmdPrint fname =
            hPutStr stdout (fname ++ ":") >> openRO fname >>= runPipeCmd
    ret <- if fs == []
           then warnIfTerminal >> runPipeCmd stdin
           else if opt_count opts && length fs > 1
                then (liftM or) (return fs >>= mapM runPipeCmdPrint)
                else (liftM or) (forM fs openRO >>= mapM runPipeCmd)
    if ret
    then exitSuccess
    else exitFailure
    where
        useColor opts istty =
            if opt_invert opts
            then False
            else
                if istty
                then not $ opt_mono opts
                else opt_highlight opts
        openRO fname
            | fname == "-"  = warnIfTerminal >> return stdin
            | otherwise     = openFile fname ReadMode


----------------------------------------------------------------------------
-- Parse ARG1 ARG2 [--] ARG3 ARG4 to ([ARG1, ARG2], [ARG3, ARG4])
splitArg' :: [String] -> [String] -> IO ([String], [String])
splitArg' ps [] = return (ps, [])
splitArg' ps (a:as)
    | a == "--"  = return (ps, as)
    | a == "-" = return (ps, (a:as))
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
      <*> switch (long "color" <> long "hl" <>
                  help "Highlight matches. Default is enabled iff stdout is a TTY")
      <*> switch (short 'm' <> long "mono" <>
                  help "Do not Highlight matches.")
      <*> some (argument str (metavar "PATTERN[...] [--] [FILES...]"))


-- vim: set makeprg=ghc
