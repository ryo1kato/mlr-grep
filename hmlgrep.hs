--
-- hmlgrep - Haskell Multi-Line-Record Grep
--
{-# LANGUAGE OverloadedStrings #-}

-- import Debug.Trace
import Control.Monad
import qualified Data.ByteString.Lazy.Search as StrSearch
import Data.Maybe
import Options.Applicative hiding (str)
import qualified Options.Applicative.Builder as AP
import System.Console.ANSI
import System.Directory
import System.Exit
import System.IO
import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
--import System.IO.MMap (mmapFileByteStringLazy)
import System.Posix.IO ( stdInput, stdOutput )
import System.Posix.Terminal ( queryTerminal )
import Text.Regex.PCRE
import qualified Data.List as DL


-- import qualified Data.ByteString.Lazy.Char8 as BS
{-
    Using bytestr.hs, instead of importing the above,
    which is dumb copy of it but with isSuffixOf support
    because isSuffixOf is somehow not exported there.
-}
import qualified ByteStr as BS
import Data.Int
type ByteStr = BS.ByteString
type BSInt = Int64
pack = BS.pack
cat = BS.concat :: [BS.ByteString] -> BS.ByteString


-- breakOn require pattern to be strict ByteString
breakBefore pattern bstr = StrSearch.breakOn (BS.toStrict pattern) bstr


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

-- type LogEntry = (Maybe String, [String])

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
--
-- ByteStr utility functions.
--

dropLast n bstr = BS.take (BS.length bstr - n) bstr
takeLast n bstr = BS.drop (BS.length bstr - n) bstr
{-
chomp bstr  | BS.null bstr         = bstr
            | BS.last bstr == '\n' = BS.init bstr
            | otherwise            = bstr
-}

chompl bstr | BS.null bstr         = bstr
            | BS.head bstr == '\n' = BS.tail bstr
            | otherwise            = bstr

chomp2 bstr | BS.null bstr         = (BS.empty, BS.empty)
            | BS.last bstr == '\n' = (BS.init bstr, "\n")
            | otherwise            = (bstr, BS.empty)



----------------------------------------------------------------------------

-- If pat is '^foo', is converted to '\nfoo' which can't match
-- an occurrence of 'foo' at very beginning of bstr
match_head pat bstr =
    (head pat == '\n') && (pack $ tail pat) `BS.isPrefixOf` bstr

-- Like '^foo', 'foo$' is converted to 'foo\n'.
match_tail bpat bstr =
    (BS.last bpat == '\n') && (BS.init bpat) `BS.isSuffixOf` bstr


breakNextRegex :: Regex -> ByteStr -> (ByteStr, ByteStr)
breakNextRegex re bstr =
    if pos >= 0
    then BS.splitAt (fromIntegral pos) bstr
    else (bstr, BS.empty)
    where (pos, _) = match re bstr :: (MatchOffset,MatchLength)

fromLast :: ByteStr -> ByteStr -> ByteStr
fromLast pat bstr = revSearch 0 bstr
  where
  revSearch n s
        | BS.null s             = bstr -- not found
        | pat `BS.isSuffixOf` s = BS.drop totalDrop bstr
        | otherwise             = revSearch (n+1) (BS.init s)
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
        lastline    = fromLast "\n" s
        (offset_, _) = (BS.tail lastline) =~ re :: (MatchOffset,MatchLength)
        offset      = fromIntegral offset_
        consumed    = n + BS.length lastline
        remaining   = dropLast consumed body


getRsMatchFuncs rsStr =
    if isNothing rsPlain
    then -- RS is Regex
        (fromLastRegex rsStr, breakNextRegex $ reCompile $ sanitizeRe rsStr)
    else -- RS is plain text pattern
        (fromLast rs, breakBefore rs)
    where
        rsPlain   = toPlainString rsStr
        rs        = (pack $ fromJust rsPlain)


--
-- split bstr into 3 parts: before, on, and after first line with match
--
fgrep_line :: ByteStr -> ByteStr -> (ByteStr, ByteStr, ByteStr)
fgrep_line pat bstr
    | BS.null bstr  = (BS.empty, BS.empty, BS.empty)
    | BS.null pat   = (BS.empty, BS.empty, bstr)
    | BS.null t     = (bstr,     BS.empty, BS.empty)
    | BS.null rest  = (h, t,     BS.empty)
    | otherwise     = (h', cat [left, right, "\n"], BS.tail rest)
        where
        (h, t)        = breakBefore pat bstr
        left          = afterLast "\n" h -- FIXME
        (right, rest) = breakBefore "\n" t
        h' = dropLast (BS.length left) h


fgrep' highlight fromLastRS beforeNextRS pat bstr
    | BS.null bstr  = BS.empty
    | BS.null l     = if match_tail pat h
                      then fromLastRS h
                      else BS.empty
    | otherwise     = cat [match_rec, newline, fgrep_rest]
    where
        (h, l, t)    = fgrep_line pat bstr
        rec1         = chompl $ fromLastRS $ cat [h, l]
        (rec2, rest) = beforeNextRS t
        match_rec    = highlight $ cat [rec1, rec2]
        fgrep_rest   = fgrep' highlight fromLastRS beforeNextRS pat rest
        newline | BS.null rest         = BS.empty
                | BS.head rest == '\n' = "\n" --the one removed by chompl
                | otherwise            = BS.empty


fgrep :: Bool -> String -> String -> ByteStr -> ByteStr
fgrep isHl rsStr pat bstr = BS.concat [hilite first, do_fgrep rest]
    where
        (fromLastRS, beforeNextRS) = getRsMatchFuncs rsStr
        hilite = if isHl
                 then highlightAllMatches $ reCompile pat
                 else id
        do_fgrep = fgrep' hilite fromLastRS beforeNextRS (pack pat)
        (first, rest) = if match_head pat bstr
                        then beforeNextRS bstr
                        else (BS.empty, bstr)


fgrep_count' beforeNextRS pat bstr
    | BS.null bstr  = 0
    | BS.null t     = if match_tail pat bstr
                      then 1
                      else 0
    | otherwise     = 1 + count_rest
    where
        (_, t)       = breakBefore pat bstr
        (_, rest)    = beforeNextRS t
        count_rest   = fgrep_count' beforeNextRS pat rest


fgrep_count :: String -> String -> ByteStr -> Int
fgrep_count rsStr pat bstr =
    if BS.null first
    then     fgrep_count' beforeNextRS (pack pat) bstr
    else 1 + fgrep_count' beforeNextRS (pack pat) rest
    where
        (_, beforeNextRS) = getRsMatchFuncs rsStr
        (first, rest)     = if match_head pat bstr
                            then beforeNextRS bstr
                            else (BS.empty, bstr)

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

matchAllOf :: [Regex] -> ByteStr -> Bool
matchAllOf ps logentry = and $ map (\p -> match p logentry) ps

allMatchAsList re str = getAllMatches $
    (match re str :: AllMatches [] (MatchOffset, MatchLength))

highlightAllMatches re str = highlightRangesRSorted str (reverse m)
    where m = allMatchAsList re str

tryHighlightAllMatches re bstr =
    if m == []
    then Nothing
    else Just (highlightRangesRSorted bstr (reverse m))
    where m = allMatchAsList re bstr


----------------------------------------------------------------------------
--
-- Split input ByteStr with RecordSeparator
--

toLogs :: Regex -> ByteStr -> [ByteStr]
toLogs sep bstr0 = splitLogs 0 bstr0
  where
      splitLogs dropLen bstr
        | BS.null bstr      =  []
        | pos < 0           =  bstr : []
        | matchPos == 0     =  splitLogs matchLen rest
        | otherwise         =  first : splitLogs matchLen rest
          where
            (pos, len)    = match sep (BS.drop (fromIntegral dropLen) bstr)
                            ::(MatchOffset,MatchLength)
            (first, rest) = BS.splitAt matchPos bstr
            matchPos      = fromIntegral (dropLen + pos)
            matchTailPos  = matchPos + (fromIntegral len)
            -- FIXME, to be precise, we have to check if we have
            -- '^' or '$' in the separator pattern
            matchLen      = if BS.length bstr > matchTailPos &&
                               '\n' == BS.index bstr matchTailPos
                            then len + 1
                            else len

----------------------------------------------------------------------------
--
-- main logic
--
toRE :: HmlGrepOpts -> String -> Regex
toRE opts str = makeRegexOpts (ic+compMultiline) execBlank str
    where ic = if (opt_ignorecase opts) then compCaseless else compBlank

-- all RE strings combined with '|', used for OR search and highlights
composeRE opts str = toRE opts $ DL.intercalate "|" str

hmlgrep_hl :: Regex -> [ByteStr] -> [ByteStr]
hmlgrep_hl re logs = catMaybes $ map (tryHighlightAllMatches re) logs

hmlgrep' :: HmlGrepOpts -> Bool -> [String] -> [ByteStr] -> [ByteStr]
hmlgrep' _ _ [] _ = []
hmlgrep' _ _ _ [] = []
hmlgrep' opts hl patterns logs
    | o_invert    = filter (not.matcher) logs -- never highlights
    | hl          = if o_and
                    then hmlgrep_hl regexOR $ filter (matcher) logs
                    else hmlgrep_hl regexOR logs
    | otherwise   = filter (matcher) logs
    where
        -- when there's only one pattern, opt_and is meaningless
        o_and    = (opt_and opts) && (length patterns) /= 1
        o_invert = opt_invert opts
        regexs   = map (toRE opts) patterns
        regexOR  = composeRE opts patterns
        matcher  = if o_and
                   then matchAllOf regexs
                   else match regexOR



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
    where
        recsep = if opt_timestamp opts
                 then timestamp_rs
                 else fromMaybe default_rs (opt_rs opts)
        logs   = toLogs (toRE opts recsep) indata
        toString = if opt_count opts
                   then (\x -> pack (((show.length) x) ++ "\n"))
                   else BS.concat
        do_command = hmlgrep' opts hl patterns logs
        ---- fgrep ----
        strPat   = toPlainString (head patterns)
        isFgrep  = isJust strPat && (length patterns) == 1 &&
                   not (opt_ignorecase opts ||
                        opt_invert opts)
                   -- FIXME: use fgrep for --count too.
        do_fgrep = if opt_count opts
                   then pack $ (show do_fgrep_count ++ "\n")
                   else fgrep hl recsep (fromJust strPat) indata
        do_fgrep_count = fgrep_count recsep (fromJust strPat) indata


----------------------------------------------------------------------------
--
-- Run as a Unix command-line filter (pipe)
--

-- concat input from all files and feed into a command
_runPipe' cmd outHandle inHandles = do
    streams <- forM inHandles BS.hGetContents
    case (cmd $ cat streams) of
        (result, ret) -> do
            BS.hPutStr outHandle result
            return ret

-- open separate streams per file and feed into command separately
runPipe :: (ByteStr -> (ByteStr, Bool)) -> Handle -> Handle -> IO Bool
runPipe cmd outHandle inHandle = do
    stream <- BS.hGetContents inHandle
    case cmd stream of
        (result, ret) -> do
            BS.hPutStr outHandle result
            return ret

runPipeMmap cmd outHandle fname = do
    -- stream <- mmapFileByteStringLazy fname Nothing -- Uses mmap
    stream <- unsafeMMapFile fname -- Uses bytestring-mmap
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
