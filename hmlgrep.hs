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

import Debug.Trace

import Control.Monad
import Data.Int
import Data.Maybe
import Options.Applicative
import System.Console.ANSI
import System.Directory
import System.Exit
import System.Posix.IO ( stdInput, stdOutput )
import System.IO.MMap
import System.Posix.Terminal ( queryTerminal )
import Text.Regex.PCRE
import Data.ByteString.Search
import qualified Data.List as DL

import System.IO
import Text.Regex.PCRE.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BSUS
type ByteStr = BS.ByteString
type BSInt = Int
pack = BS.pack
cat = BS.concat


p = pack
u = BS.unpack

escapeNewline [] = []
escapeNewline (c:cs) = case c of
      '\n' -> '\\' : 'n' : escapeNewline cs
      _    -> c : escapeNewline cs

traceB :: ByteStr -> ByteStr
traceB b = BS.pack $ traceId $
    ">>>" ++ hlCode ++ (escapeNewline (BS.unpack b)) ++ hlReset ++ "<<<"
debug3b triplet =
    ((u a), (u b), (u c))
    where
    (a,b,c) = triplet


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
-- Regex Handling Helper Functions
--
reCompile pat = makeRegexOpts compMultiline execBlank pat

regexChars = "^$(|)[]{}.*"
regexCharsLast = ")]}.*"
ctrlChars = "nt" -- \n, \t
toCtrlChars c = case c of
    'n' -> '\n'
    't' -> '\t'
    _   -> c

toPlainString' :: String -> String -> Maybe String
toPlainString' s []         = Just s
toPlainString' s ('$':[])   = Just ('\n':s)
toPlainString' s (r:[])     = if r `elem` regexCharsLast
                             then Nothing
                             else Just (r:s)
toPlainString' s (r1:r2:re)
  | r1 `elem` regexChars   = Nothing
  | r1 == '\\' && r2 `elem` ('\\':regexChars)  = toPlainString' (r2:s) re
  | r1 == '\\' && r2 `elem` ctrlChars  = toPlainString' ((toCtrlChars r2):s) re
  | otherwise   = toPlainString' (r1:s) (r2:re)

-- Return Just String where string is un-escaped plain text string
-- '^' and '$' at very beginning / end will be converted to '\n'
-- e.g. /^\(/ -> '^('
toPlainString :: String -> Maybe String
toPlainString ('^':re) = liftM reverse $ toPlainString' "\n" re
toPlainString re       = liftM reverse $ toPlainString' [] re


--
-- convert ^ and $ to '\n' in regex string because
-- with PCRE multiline mode /^$/ matches at very end of "foobar\n"
-- which we don't want.
--
sanitizeRe' [] = []
sanitizeRe' ('$':[])      =  "\n"
sanitizeRe' (r:[])        =  r:[]
sanitizeRe' ('\\':r:rs)   =  '\\' : r : sanitizeRe' rs
sanitizeRe' ('(':'^':rs)  =  "(\n" ++ sanitizeRe' rs
sanitizeRe' ('|':'^':rs)  =  "|\n" ++ sanitizeRe' rs
sanitizeRe' ('$':')':rs)  =  "\n)" ++ sanitizeRe' rs
sanitizeRe' ('$':'|':rs)  =  "\n|" ++ sanitizeRe' rs
sanitizeRe' (r1:r2:rs)    =  r1 : sanitizeRe' (r2:rs)

sanitizeRe rs | head rs == '^'  =  '\n' : sanitizeRe' rs
              | otherwise       =  sanitizeRe' rs

----------------------------------------------------------------------------
{- TODOs
 - convert ^$ in regex to \n for breakNextRegex
 - \n handling
-}

dropLast n bstr = BS.take (BS.length bstr - n) bstr
takeLast n bstr = BS.drop (BS.length bstr - n) bstr
chomp bstr  | BS.null bstr         = bstr
            | BS.last bstr == '\n' = BS.init bstr
            | otherwise            = bstr

chompl bstr | BS.null bstr         = bstr
            | BS.head bstr == '\n' = BS.tail bstr
            | otherwise            = bstr

chomp2 bstr | BS.null bstr         = (BS.empty, BS.empty)
            | BS.last bstr == '\n' = (BS.init bstr, (pack "\n"))
            | otherwise            = (bstr, BS.empty)



----------------------------------------------------------------------------


breakNextRegex re bstr =
    if pos >= 0
    then BS.splitAt pos bstr
    else (bstr, BS.empty)
    where (pos, len) = bstr =~ re :: (MatchOffset,MatchLength)

fromLast :: ByteStr -> ByteStr -> ByteStr
fromLast pat bstr = revSearch 0 bstr
  where
  revSearch n s
        | BS.null s             = bstr -- not found
        | pat `BS.isSuffixOf` s = BS.drop totalDrop bstr
        | otherwise             = revSearch (n+1) (BSUS.unsafeInit s)
        where
        totalDrop = BS.length bstr - n - BS.length pat

afterLast pat bstr
    | BS.null pat  = BS.empty
    | BS.null bstr = bstr
    | otherwise    = if pat `BS.isPrefixOf` fromlast
                     then BS.drop (BS.length pat) fromlast
                     else fromlast
        where fromlast = fromLast pat bstr


fromLastRegex :: String -> ByteStr -> ByteStr
fromLastRegex re bstr = cat [revSearchRE 0 body, newline]
    where
    -- chomp the trailing '\n' to avoid /^$/ to match very end of
    -- something like "foo\nbar\nbaz\n"
    (body, newline) = chomp2 bstr
    revSearchRE n s
        | BS.null s   = body
        | offset >= 0 = takeLast (consumed - offset) body
        | otherwise   = revSearchRE consumed remaining
        where
        lastline    = fromLast (pack "\n") s
        (offset, l) = (BS.tail lastline) =~ re :: (MatchOffset,MatchLength)
        consumed    = n + BS.length lastline
        remaining   = dropLast consumed body


--
-- split bstr into 3 parts: before, on, and after first line with match
--
fgrep_line pat bstr
    | BS.null bstr  = (BS.empty, BS.empty, BS.empty)
    | BS.null pat   = (BS.empty, BS.empty, bstr)
    | BS.null t     = (bstr,     BS.empty, BS.empty)
    | BS.null rest  = (h, t,     BS.empty)
    | otherwise     = (head, cat [left, right, (p "\n")], BS.tail rest)
        where
        (h, t)        = breakOn pat bstr
        left          = afterLast (pack "\n") h -- FIXME
        (right, rest) = breakOn (pack "\n") t
        head          = dropLast (BS.length left) h


fgrep' highlight fromLastRS beforeNextRS pat bstr
    | BS.null bstr  = BS.empty
    | BS.null l     = BS.empty
    | otherwise     = cat [match_rec, newline, fgrep_rest]
    where
        (h, l, t)    = fgrep_line pat bstr
        rec1         = chompl $ fromLastRS $ cat [h, l]
        (rec2, rest) = beforeNextRS t
        match_rec    = highlight $ cat [rec1, rec2]
        fgrep_rest   = fgrep' highlight fromLastRS beforeNextRS pat rest
        newline | BS.null rest         = BS.empty
                | BS.head rest == '\n' = (pack "\n") --the one removed by chompl
                | otherwise            = BS.empty



fgrep isHl rsStr pat bstr =
    if isNothing rsPlain
    then -- Regex
        fgrep' highlight (fromLastRegex rsStr) (breakNextRegex $ sanitizeRe rsStr) (pack pat) bstr
    else -- plain text pattern
        fgrep' highlight (fromLast rs) (breakOn rs) (pack pat) bstr
    where
        rsPlain   = toPlainString rsStr
        rs        = (pack $ fromJust rsPlain)
        rsRE      = reCompile rsStr
        highlight = if isHl
                    then highlightAllMatches $ reCompile pat
                    else id




----------------------------------------------------------------------------
--
-- Match Highlights
--
-- hlCode  = setSGRCode [SetColor Foreground Vivid Red]
hlCode  = setSGRCode [SetSwapForegroundBackground True]
hlReset = setSGRCode [Reset]

highlightRange :: ByteStr -> (Int, Int) -> ByteStr
highlightRange str mch = cat [ (BS.take start str), (pack hlCode) ,
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

highlightAllMatches re str = highlightRangesRSorted str (reverse m)
    where m = allMatchAsList re str

tryHighlightAllMatches re str =
    if m == []
    then Nothing
    else Just (highlightRangesRSorted str (reverse m))
    where m = allMatchAsList re str

tryHighlightAllMatchesLines re ls =
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
            hl_hdr = tryHighlightAllMatches p hdr
            hl_ls  = tryHighlightAllMatchesLines p ls
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
          strPat   = toPlainString (head patterns)
          isFgrep  = isJust strPat && (length patterns) == 1 &&
                     not (opt_ignorecase opts || opt_count opts || opt_invert opts)
                     -- FIXME: use fgrep for --count too.
          do_fgrep = fgrep hl recsep (fromJust strPat) indata


----------------------------------------------------------------------------
--
-- Run as a Unix command-line filter (pipe)
--
runPipe' cmd outHandle inHandles = do
    streams <- forM inHandles BS.hGetContents
    case (cmd $ cat streams) of
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


warning str = hPutStr stderr ("WARNING: " ++ str ++ "\n")
warnIfTerminal = do
    stdIsTty <- queryTerminal stdInput
    if stdIsTty then warning "Reading from terminal"
                else return ()


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
           then warnIfTerminal >> runPipeCmd stdin
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
