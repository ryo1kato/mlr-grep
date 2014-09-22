--
-- hmlgrep - Haskell Multi-Line-Record Grep
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Control.Monad
import qualified Data.ByteString.Search as StrSearch
import Data.Maybe
import Options.Applicative hiding (str)
import qualified Options.Applicative.Builder as AP
import System.Console.ANSI
import System.Directory
import System.Exit
import System.IO
import System.IO.Posix.MMap (unsafeMMapFile)
--import System.IO.MMap (mmapFileByteStringLazy)
import System.Posix.IO ( stdInput, stdOutput )
import System.Posix.Terminal ( queryTerminal )
-- import Text.Regex.PCRE
-- import Text.Regex.PCRE.Light
import Text.Regex.TDFA
import qualified Data.List as DL
-- import Debug.Trace

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
type ByteStr = BS.ByteString
type BSInt = Int
pack = BS.pack
cat  = BS.concat :: [BS.ByteString] -> BS.ByteString
size = BS.length


-- breakOn require pattern to be strict ByteString
-- breakBefore pattern bstr = StrSearch.breakOn (BS.toStrict pattern) bstr
breakBefore pattern bstr = StrSearch.breakOn pattern bstr

-- ByteStr utility functions.
dropLast n bstr = BS.take (size bstr - n) bstr
takeLast n bstr = BS.drop (size bstr - n) bstr
chomp2 bstr | BS.null bstr         = (BS.empty, BS.empty)
            | BS.last bstr == '\n' = (BS.init bstr, "\n")
            | otherwise            = (bstr, BS.empty)


-----------------------------------------------------------------------------
-- $setup
-- The doctest in this module require GHC's `OverloadedStrings`
-- extension:
--
-- >>> :set -XOverloadedStrings

-- for doctest
u = BS.unpack
us bs = map BS.unpack bs
u2 (one, two) = (u one, u two)
u3 (one, two, three) = (u one, u two, u three)
_ignore = (u,us,u2,u3) -- suppress compiler warnings


-----------------------------------------------------------------------------

helpdoc = concat $ DL.intersperse " "
  [ "grep(1) like tool, but \"record-oriented\", instead of line-oriented."
  , "Useful to search/print multi-line log entries separated by e.g., empty"
  , "lines, '----' or timestamps, etc."
  , "If an argument in argument list is a name of"
  , "existing file or '-' (means stdin), such argument and"
  , "all arguments after that will be treated as filenames to read from."
  , "Otherwise arguments are considered to be regex to search."
  , "(could be confusing if you specify nonexistent filename!)"
  ,"If a file name ends with .gz, .bz2 or .xz, uncompress it on-the-fly before"
  ,"reading from it."
  ]


-----------------------------------------------------------------------------
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

----------------------------------------------------------------------------
--
-- Regex Conversions
--
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

{- |
    convert ^ and $ to '\n' in regex string because we don't want to
    use PCRE multiline mode in which /^$/ matches at very end of "foobar\n"
    which we don't want.

    >>> _sanitizeRe "^$|^(----*|====*)$"
    "\n\n|\n(----*|====*)\n"
    >>> _sanitizeRe "[^]^|$piyo"
    "[^]^|$piyo"
-}
sanitizeRe' [] = []
sanitizeRe' ('$':[])      =  "\n"
sanitizeRe' (r:[])        =  r:[]
sanitizeRe' ('\\':r:rs)   =  '\\' : r : sanitizeRe' rs
sanitizeRe' ('(':'^':rs)  =  "(\n" ++ sanitizeRe' rs
sanitizeRe' ('|':'^':rs)  =  "|\n" ++ sanitizeRe' rs
sanitizeRe' ('$':')':rs)  =  "\n)" ++ sanitizeRe' rs
sanitizeRe' ('$':'|':rs)  =  "\n" ++ sanitizeRe' ('|':rs)
sanitizeRe' (r1:r2:rs)    =  r1 : sanitizeRe' (r2:rs)

_sanitizeRe rs | head rs == '^'  =  '\n' : (sanitizeRe' $ tail rs)
              | otherwise       =  sanitizeRe' rs

--
-- Regex Wrappers to be able to swap underlaying libraries
--
-- _toRegex     :: Bool -> String -> Regex
matchAllOf  :: [Regex] -> ByteStr -> Bool
firstMatch  :: Regex -> ByteStr -> (Int,Int)

toRegex :: Bool     -- Ignore case
        -> Bool     -- Multiline mode (convert ^$ to \n)
        -> String   -- Regex string to compile
        -> Regex
#define TDFA
#if defined(PCRE)
toRegex caseless multi_line str = makeRegexOpts (ic+ml) execBlank str
    where ic = if caseless   then compCaseless else compBlank
          ml = if multi_line then compMultiline else compBlank
#elif defined(TDFA)
toRegex caseless multi_line str = makeRegexOpts c e str
    where
      c = defaultCompOpt { caseSensitive = (not caseless),
                           multiline = multi_line }
      e = defaultExecOpt { captureGroups = False }
#endif


-- all RE strings combined with '|', used for OR search and highlights
toRegexDisjunction caseless multi_line strs =
    toRegex caseless multi_line $ DL.intercalate "|" strs

allMatchAsList re str = getAllMatches $
    (match re str :: AllMatches [] (MatchOffset, MatchLength))
matchAllOf res bstr = and $ map (\p -> match p bstr) res
firstMatch re bstr = match re bstr :: (MatchOffset,MatchLength)


----------------------------------------------------------------------------
--
-- Regex and Match Highlights
--
-- hlCode  = setSGRCode [SetColor Foreground Vivid Red]
hlCode  = setSGRCode [SetSwapForegroundBackground True]
hlReset = setSGRCode [Reset]

highlightRange :: ByteStr -> (Int, Int) -> ByteStr
highlightRange str mch =
    cat [ (BS.take start str)
        , (pack hlCode)
        , (BS.take len $ BS.drop start str)
        , (pack hlReset)
        , (BS.drop (start+len) str)  ]
    where start = fromIntegral (fst mch) :: BSInt
          len   = fromIntegral (snd mch) :: BSInt

-- lighlight matches in ByteString with _reverse sorted_ list of matches
-- It has to be reversed so that we don't have to re-calculate offset everytime
-- control codes are inserted.
highlightRangesRSorted :: ByteStr -> [(Int, Int)] -> ByteStr
highlightRangesRSorted str [] = str
highlightRangesRSorted str (r:rs) = highlightRangesRSorted (highlightRange str r) rs

highlightAllMatches re str = highlightRangesRSorted str (reverse m)
    where m = allMatchAsList re str

tryHighlightAllMatches re bstr =
    if m == []
    then Nothing
    else Just (highlightRangesRSorted bstr (reverse m))
    where m = allMatchAsList re bstr

----------------------------------------------------------------------------
--
-- Utility functions for find-pattern-first algorithm.
--

-- If pat is '^foo', is converted to '\nfoo' which can't match
-- an occurrence of 'foo' at very beginning of bstr
match_head :: Maybe String -> ByteStr -> Bool
match_head pat bstr =
    if isNothing pat
    then False
    else (head (fromJust pat) == '\n') &&
         (pack $ tail (fromJust pat)) `BS.isPrefixOf` bstr

-- Like '^foo', 'foo$' is converted to 'foo\n'.
match_tail :: Maybe String -> ByteStr -> Bool
match_tail pat bstr =
    if isNothing pat
    then False
    else (BS.last bpat == '\n') && (BS.init bpat) `BS.isSuffixOf` bstr
    where
        bpat = pack $ fromJust pat


{- |
    >>> let re = toRegex False True "^----$|^$"
    >>> let bstr = "one\ntwo\nthree\n----\nfoo\n\nbar\nbaz"
    >>> breakNextRegex re bstr
    ("one\ntwo\nthree\n","----\nfoo\n\nbar\nbaz")
    >>> let re = toRegex False True "^$"
    >>> breakNextRegex re bstr
    ("one\ntwo\nthree\n----\nfoo\n","\nbar\nbaz")
-}
breakNextRegex :: Regex -> ByteStr -> (ByteStr, ByteStr)
breakNextRegex re bstr
    | BS.null bstr  = (BS.empty, BS.empty)
    | pos < 0       = (bstr, BS.empty)
    | pos == 0      = (BS.empty, bstr)
    | otherwise     = BS.splitAt (fromIntegral pos) bstr
    where
        (pos, _) = firstMatch re bstr

fromLast :: ByteStr -> ByteStr -> ByteStr
fromLast pat bstr = revSearch 0 bstr
  where
  revSearch n s
        | BS.null s             = bstr -- not found
        | pat `BS.isSuffixOf` s = BS.drop totalDrop bstr
        | otherwise             = revSearch (n+1) (BS.init s)
        where
        totalDrop = (size bstr) - n - (size pat)

afterLast pat bstr
    | BS.null pat  = BS.empty
    | BS.null bstr = bstr
    | otherwise    = if pat `BS.isPrefixOf` fromlast
                     then BS.drop (size pat) fromlast
                     else fromlast
        where fromlast = fromLast pat bstr

{-|
    >>> let bstr = "one\ntwo\nthree\n----\nfoo\nbar\nbaz"
    >>> let reStr = "^----$"
    >>> fromLastRegex reStr bstr
    "----\nfoo\nbar\nbaz"
-}
fromLastRegex :: String -> ByteStr -> ByteStr
fromLastRegex reStr bstr = cat [revSearchRE 0 body, newline]
    where
    re              = toRegex False False reStr
    -- chomp the trailing '\n' to avoid /^$/ to match very end of
    -- something like "foo\nbar\nbaz\n"
    (body, newline) = chomp2 bstr
    revSearchRE n s
        | BS.null s   = body
        | offset >= 0 = takeLast (consumed - offset) body
        | otherwise   = revSearchRE consumed remaining
        where
        lastline     = fromLast "\n" s
        (offset_, _) = if BS.head lastline == '\n'
                       then firstMatch re (BS.tail lastline)
                       else firstMatch re lastline
        offset       = if offset_ < 0 then fromIntegral offset_
                       else if BS.head lastline == '\n'
                           then fromIntegral (offset_ + 1)
                           else fromIntegral offset_
        consumed     = n + size lastline
        remaining    = dropLast consumed body

getMatchFuncs :: Bool -> String -> [String]
                 -> (Maybe String,
                     ByteStr -> ByteStr,
                     ByteStr -> (ByteStr, ByteStr),
                     ByteStr -> (ByteStr, ByteStr))
getMatchFuncs caseless rsStr patStrs =
    if caseless || isNothing rsPlain
    then -- RS is Regex
        (patPlain,
         fromLastRegex rsStr,
         breakOnPattern,
         breakNextRegex $ toRegex caseless True rsStr)
    else -- RS is plain text pattern
        (patPlain,
         fromLast rs,
         breakOnPattern,
         breakBefore rs)
    where
        rsPlain         = toPlainString rsStr
        rs              = (pack $ fromJust rsPlain)
        patPlain        = toPlainString $ head patStrs
        breakOnPattern  = if caseless || length patStrs > 1 || isNothing patPlain
             then breakNextRegex $ toRegexDisjunction caseless True patStrs
             else breakBefore (pack $ fromJust patPlain)

----------------------------------------------------------------------------
--
-- grep - find-pattern-first algorithm.
--

{-
-- split bstr into 3 parts: before, on, and after first line with match
-}
grep_line :: (ByteStr -> (ByteStr, ByteStr))
           -> ByteStr
           -> (ByteStr, ByteStr, ByteStr)
grep_line breakOnPattern bstr
    | BS.null bstr  = (BS.empty, BS.empty, BS.empty)
    | BS.null t     = (bstr,     BS.empty, BS.empty)
    | BS.null rest  = (h, t,     BS.empty)
    | otherwise     = (h', cat [left, right, "\n"], BS.tail rest)
        where
        (h, t)         = breakOnPattern bstr
        left           = afterLast "\n" h
        (right, rest)  = breakBefore "\n" t
        h' = dropLast (BS.length left) h


grep' highlight pat fromLastRS breakOnPattern beforeNextRS bstr
    | BS.null bstr  = BL.empty
    | BS.null l     = if match_tail pat h
                      then BL.fromStrict $ fromLastRS h
                      else BL.empty
    | otherwise     = BL.concat [match_rec, newline, grep_rest]
    where
        (h, l, t)    = grep_line breakOnPattern bstr
        rec1         = fromLastRS $ cat [h, l]
        (rec2, rest) = beforeNextRS t
        match_rec    = BL.fromStrict $ highlight $ cat [rec1, rec2]
        grep_rest   = grep' highlight pat fromLastRS breakOnPattern beforeNextRS rest
        newline      = if (not $ BS.null rest) && BL.last match_rec /= '\n'
                       then "\n"
                       else BL.empty


grep :: Bool -> Bool -> String -> [String] -> ByteStr -> BL.ByteString
grep isHl o_ic rsStr patStrs bstr =
        BL.concat [BL.fromStrict $ hilite first, do_grep rest]
    where
        (pat, fromLastRS, breakOnPattern, beforeNextRS)
                = getMatchFuncs o_ic rsStr patStrs
        hilite = if isHl
                 then highlightAllMatches $ toRegexDisjunction False True patStrs
                 else id
        do_grep = grep' hilite pat fromLastRS breakOnPattern beforeNextRS
        (first, rest) = if match_head pat bstr
                        then beforeNextRS bstr
                        else (BS.empty, bstr)

grep_count' pat beforeNextRS breakOnPattern bstr
    | BS.null bstr  = 0
    | BS.null t     = if match_tail pat bstr
                      then 1
                      else 0
    | otherwise     = 1 + count_rest
    where
        (_, t)       = breakOnPattern bstr
        (_, rest)    = beforeNextRS t
        count_rest   = grep_count' pat beforeNextRS breakOnPattern rest


grep_count :: Bool -> String -> [String] -> ByteStr -> Int
grep_count o_ic rsStr patStrs bstr =
    if BS.null first
    then     grep_count' pat beforeNextRS breakOnPattern bstr
    else 1 + grep_count' pat beforeNextRS breakOnPattern rest
    where
        (pat, _, breakOnPattern, beforeNextRS) = getMatchFuncs o_ic rsStr patStrs
        (first, rest)        = if match_head pat bstr
                               then beforeNextRS bstr
                               else (BS.empty, bstr)


----------------------------------------------------------------------------
--
-- Utility functions for split-to-record-first algorithm
--

{-| Find position and length of next header(record separator) line,
    including trailing newline.
    >>> let re = toRegex True True "^$|^----"
    >>> findNextHeaderLine re "123456\n----123456"
    (7,10)
    >>> findNextHeaderLine re "123456\n----123456\nfoobar"
    (7,11)
    >>> findNextHeaderLine re "123456----1234\n\n456"
    (15,1)
-}
findNextHeaderLine sep bstr
    | pos < 0    = (pos, 0)
    | otherwise  = (pos, matchLineLen)
    where
        (pos, len)      = firstMatch sep bstr
        endOfMatch      = fromIntegral (pos + len)
        nextNewLine     = (BS.elemIndex '\n' $ BS.drop endOfMatch bstr)
                            >>= (\x -> Just (endOfMatch + x + 1))
        endOfMatchLine  = fromIntegral $ if isJust nextNewLine
                          then fromJust nextNewLine
                          else (size bstr)
        matchLineLen    = endOfMatchLine - fromIntegral pos

{-|
    >>> let records = ["one\ntwo\nthree\n","-------------\nfoo\nbar\nbaz\n","-----asdf\nhoge\n","\npiyo\nhuga\n"]
    >>> let bstr = cat records
    >>> let re = toRegex True True "^$|^----"
    >>> toRecords re bstr == records
    True
-}
toRecords :: Regex -> ByteStr -> [ByteStr]
toRecords sep bstr0 = splitRecords 0 bstr0
  where
  splitRecords dropLen bstr
    | BS.null bstr   =  []
    | pos < 0        =  bstr : []
    | matchPos == 0  =  splitRecords len rest
    | otherwise      =  first : splitRecords len rest
        where
        body           = BS.drop (fromIntegral dropLen) bstr
        (pos, len)     = findNextHeaderLine sep body
        matchPos       = fromIntegral (dropLen + pos)
        (first, rest)  = BS.splitAt matchPos bstr


----------------------------------------------------------------------------
--
-- grep - split-to-record-first algorithm
--
record_grep_hl :: Regex -> [ByteStr] -> [ByteStr]
record_grep_hl re records = catMaybes $ map (tryHighlightAllMatches re) records

record_grep' :: HmlGrepOpts -> Bool -> [String] -> [ByteStr] -> [ByteStr]
record_grep' _ _ [] _ = []
record_grep' _ _ _ [] = []
record_grep' opts hl patterns records
    | o_invert    = filter (not.matcher) records -- never highlights
    | hl          = if o_and
                    then record_grep_hl regexOR $ filter (matcher) records
                    else record_grep_hl regexOR records
    | otherwise   = filter (matcher) records
    where
        -- when there's only one pattern, opt_and is meaningless
        o_and    = (opt_and opts) && (length patterns) /= 1
        o_invert = opt_invert opts
        o_ic     = opt_ignorecase opts
        regexs   = map (toRegex o_ic True) patterns
        regexOR  = toRegexDisjunction o_ic True patterns
        matcher  = if o_and
                   then matchAllOf regexs
                   else match regexOR

record_grep :: HmlGrepOpts -> Bool -> Regex -> [String] -> ByteStr -> (BL.ByteString, Bool)
record_grep opts hl rs patterns bstr =
    if null do_record_grep
    then (toString do_record_grep, False)
    else (toString do_record_grep, True)
    where
        do_record_grep = record_grep' opts hl patterns $ toRecords rs bstr
        toString = if opt_count opts
                   then (\x -> BL.pack (((show.length) x) ++ "\n"))
                   else BL.concat . (map BL.fromStrict)


----------------------------------------------------------------------------
--
-- Top most interface to decide to decide which algorithm to use:
-- find-pattern-first or split-to-records
--

hmlgrep :: HmlGrepOpts -> Bool -> [String] -> ByteStr -> (BL.ByteString, Bool)
hmlgrep opts hl patterns bstr =
    if patternFirst
    then
        if BL.null do_grep
        then (BL.empty, False)
        else (do_grep, True)
    else
        record_grep opts hl (toRegex o_ic True rsStr) patterns bstr
    where
        o_and    = (opt_and opts) && (length patterns) /= 1
        o_invert = opt_invert opts
        o_ic     = opt_ignorecase opts
        rsStr    = if opt_timestamp opts
                   then timestamp_rs
                   else fromMaybe default_rs (opt_rs opts)
        patternFirst  = not o_and && not o_invert
        do_grep       = if opt_count opts
                        then BL.pack $ (show do_grep_count ++ "\n")
                        else grep hl o_ic rsStr patterns bstr
        do_grep_count = grep_count o_ic rsStr patterns bstr


----------------------------------------------------------------------------
--
-- Run as a Unix command-line filter (pipe)
--

-- open separate streams per file and feed into command separately
runPipe :: (ByteStr -> (BL.ByteString, Bool)) -> Handle -> Handle -> IO Bool
runPipe cmd outHandle inHandle = do
    stream <- BS.hGetContents inHandle
    case cmd stream of
        (result, ret) -> do
            BL.hPutStr outHandle result
            return ret

runPipeMmap cmd outHandle fname = do
    -- stream <- mmapFileByteStringLazy fname Nothing -- Uses mmap
    stream <- unsafeMMapFile fname -- Uses bytestring-mmap
    case cmd stream of
        (result, ret) -> do
            BL.hPutStr outHandle result
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
    let _runPipeCmdPrint fname =
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
        useColor options istty =
            if opt_invert opts
            then False
            else
                if istty
                then not $ opt_mono options
                else opt_highlight options
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
      <*> some (argument AP.str (metavar "PATTERN[...] [--] [FILES...]"))

-- vim: set makeprg=ghc
