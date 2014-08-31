--
-- hmlgrep - Haskell Multi-Line Grep
--

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
import System.Posix.IO ( stdOutput )
import System.IO.MMap
import System.Posix.Terminal ( queryTerminal )
import Text.Regex.PCRE
import Data.ByteString.Search
import qualified Data.List as DL

import System.IO
import Text.Regex.PCRE.ByteString
import qualified Data.ByteString.Char8 as BS
type ByteStr = BS.ByteString
type BSInt = Int
pack = BS.pack


helpdoc = concat $ DL.intersperse " "
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
                   , opt_ignorecase :: Bool
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
-- plain text (non-regex) search
--

regexChars = "^$(|)[]{}.*"
regexCharsLast = ")]}.*"

toPlainString' :: String -> String -> Maybe String
toPlainString' s []         = Just s
toPlainString' s ('$':[])   = Just ('\n':s)
toPlainString' s (r:[])     = if r `elem` regexCharsLast
                             then Nothing
                             else Just (r:s)
toPlainString' s (r1:r2:re) = if r1 `elem` regexChars
                              then Nothing
                              else if r1 == '\\' && r2 `elem` ('\\':regexChars)
                                   then toPlainString' (r2:s) re
                                   else toPlainString' (r1:s) (r2:re)

-- Return Just String where string is un-escaped plain text string
-- '^' and '$' at very beginning / end will be converted to '\n'
-- e.g. /^\(/ -> '^('
toPlainString :: String -> Maybe String
toPlainString ('^':re) = liftM reverse $ toPlainString' "\n" re
toPlainString re       = liftM reverse $ toPlainString' [] re


fgrep' :: ByteStr -> ByteStr -> ByteStr -> ByteStr
fgrep' rs pat bstr  =
    if BS.null bstr
    then BS.empty
    else if hasPattern pat h
         then if BS.null t
              then BS.concat [(BS.tail rs), h]
              else BS.concat [(BS.tail rs), h, (pack "\n"), search_remaining]
         else search_remaining
    where (h, t)           = BS.breakSubstring rs bstr
          remaining        = BS.drop (BS.length rs) t
          search_remaining = fgrep' rs pat remaining

-- Take care of the beginning of a file (first record)
-- that possibly without a record header.
-- Note that there's no leading '\n' in the very first record header.
fgrep rs pat bstr =
    if (not $ beginWith (tail rs') bstr) && hasPattern (pack pat) h
    then BS.concat [h, (pack "\n"), search_remaining]
    else search_remaining
    where (h, t) = BS.breakSubstring (pack rs') bstr
          remaining = BS.drop (length $ tail rs') t
          search_remaining = fgrep' (pack rs') (pack pat) remaining
          rs' = if head rs == '\n'
                then rs
                else ('\n':rs)


hasPattern :: ByteStr -> ByteStr -> Bool
hasPattern pat bstr =
    if BS.null t then False else True
    where (h, t) = BS.breakSubstring pat bstr

beginWith :: String -> ByteStr -> Bool
beginWith pat str =
    if length pat > BS.length str
    then False
    else ((pack pat) == BS.take (length pat) str)


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
    where ic = if (opt_ignorecase opts) then compCaseless else compBlank

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
    if isFgrep
    then
        if BS.null do_fgrep
        then (BS.empty, False)
        else (do_fgrep, True)
    else
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
          ---- fgrep ----
          strRS    = toPlainString recsep
          strPat   = toPlainString (head patterns)
          isFgrep  = isJust strRS && isJust strPat &&
                     (length patterns) == 1 &&
                     not (opt_ignorecase opts || opt_count opts)
                     -- FIXME: use fgrep for --count too.
          do_fgrep = fgrep (fromJust strRS) (fromJust strPat) indata


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

runPipeMmap cmd outHandle fname = do
    stream <- mmapFileByteString fname Nothing
    case cmd stream of
        (result, ret) -> do
            BS.hPutStr outHandle result
            return ret


runWithOptions :: HmlGrepOpts -> IO ()
runWithOptions opts = do
    (ps, fs) <- splitArg (opt_args opts)
    istty <- queryTerminal stdOutput
    let runPipeCmd     = runPipe     (hmlgrep opts (useColor opts istty) ps) stdout
    let runPipeCmdMmap = runPipeMmap (hmlgrep opts (useColor opts istty) ps) stdout
    let runPipeCmdPrint fname =
            hPutStr stdout (fname ++ ":") >> openRO fname >>= runPipeCmd
    let runPipeCmdMmapPrint fname =
            hPutStr stdout (fname ++ ":") >> runPipeCmdMmap fname
    ret <- if fs == []
           then runPipeCmd stdin
           else if opt_count opts && length fs > 1
                then (liftM or) (mapM runPipeCmdMmapPrint fs)
                else (liftM or) (mapM runPipeCmdMmap fs)
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
            | fname == "-"  = return stdin
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
