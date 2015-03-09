{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, PatternGuards #-}
import Text.CSL
import Text.CSL.Style
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types (get_field)
--import Text.Pandoc.Definition
--import Text.Pandoc.Generic
import qualified Data.Map as M
import Control.Monad (unless)
import System.Exit
import System.IO
import Data.Maybe (fromMaybe)
import Data.List (intersperse)

-- represents the citeproc-js citation data object 
data CitationData = CitationData { citationItems :: Citations
                                   -- TODO: data structure for properties
                                 , properties :: [String]
                                 }
  
instance JSON CitationData where
  -- showJSON = toJSON -- TODO: re-encode using citationItems, etc.
  readJSON (JSObject o) = case get_field o "citationItems" of
    Just (JSArray cs) ->
      Ok $ CitationData { citationItems = [cites]
                        , properties = [] -- TODO
                        }
        where cites = reverse $ foldl getCites [] cs
              getCites acc obj = case readJSON obj of
                Ok c@(Cite _ _ _ _ _ _ _ _ _ _ _) -> c:acc 
                _ -> acc -- TODO: error if non-citation in citationItems?
    _ -> Error "Not a citations data cluster"

instance JSON Cite where
  showJSON = toJSON
  readJSON (JSObject o) = case get_field o "id" of
    Just (JSString x) ->
        Ok $ emptyCite{ citeId = fromJSString x
                      , citePrefix = case get_field o "prefix" of
                                       Just (JSString x) -> PlainText $ fromJSString x
                                       _ -> PandocText []
                      , citeSuffix = case get_field o "suffix" of
                                       Just (JSString x) -> PlainText $ fromJSString x
                                       _ -> PandocText []
                      -- , citeLabel = case get_field o "label" of
                      --                  Just (JSString x) -> fromJSString x
                      --                  _ -> ""
                      -- , citeLocator = case get_field o "locator" of
                      --                  Just (JSString x) -> fromJSString x
                      --                  _ -> ""
                      -- , citeNoteNumber = case get_field o "note_number" of
                      --                  Just (JSString x) -> fromJSString x
                      --                  _ -> ""
                      -- , citePosition = case get_field o "position" of
                      --                  Just (JSString x) -> fromJSString x
                      --                  _ -> ""
                      -- , nearNote = case get_field o "near_note" of
                      --                  Just (JSBool True) -> True
                      --                  _ -> False
                      , suppressAuthor = case get_field o "suppress-author" of
                                       Just (JSBool True) -> True
                                       _ -> False
                      -- , authorInText = case get_field o "author_in_text" of
                      --                  Just (JSBool True) -> True
                      --                  _ -> False
                      }
    _ -> Error "Not a citation item"
  readJSON x = fromJSON x

jsString :: String -> JSValue
jsString = JSString . toJSString

data CiteprocResult = CiteprocResult { cites  :: [String]
                                     , bib    :: [String]
                                     } deriving (Typeable, Data)

-- instance JSON CiteprocResult where
--   showJSON res = JSObject $
--                  toJSObject [("citations", showJSON $ cites res)
--                             ,("bibliography", showJSON $ bib res)
--                             ]
--   readJSON = fromJSON
                                                
instance Show CiteprocResult where                                                
  show cr = concat $ intersperse "\n" (cites cr) ++
            ["\n=====\n"] ++
            intersperse "\n" (bib cr) ++
            ["\n"]


-- rendering functions:
-- common helpers:
wrap :: String -> String -> String -> String
wrap openTag closeTag s = openTag ++ s ++ closeTag 

-- plain text: use citeproc-hs' renderPlain

-- HTML: 
renderHTML :: [FormattedOutput] -> String
renderHTML ((FO fmt embedded):fos) = wrapHTMLStyle s fmt ++ renderHTML fos
  where s = renderHTML embedded 
renderHTML ((FN n fmt):fos) = wrapHTMLStyle n fmt ++ renderHTML fos	
renderHTML ((FS s fmt):fos) = wrapHTMLStyle s fmt ++ renderHTML fos	
renderHTML ((FDel s):fos) = s ++ renderHTML fos
renderHTML ((FUrl target@(url, title) fmt):fos) = wrapHTMLStyle link fmt ++ renderHTML fos
  where link = wrap ("<a href=\"" ++ url ++ "\">") "</a>" title
renderHTML ((FPan inlines):fos) = "(renderHTML undefined for FPan)" --undefined 
renderHTML ((FNull):fos) = renderHTML fos
renderHTML [] = ""

wrapHTMLStyle :: String -> Formatting -> String
wrapHTMLStyle s fmt = s'
  where pfx = prefix fmt
        sfx = suffix fmt
        ffam = cssProp "font-family" (fontFamily fmt)
        fsty = cssProp "font-style" (fontStyle fmt)
        fvt = cssProp "font-variant" (fontVariant fmt)
        fwt = cssProp "font-weight" (fontWeight fmt)
        tdec = if (noDecor fmt) then ""
               else cssProp "text-decoration" (textDecoration fmt)
        valn = cssProp "vertical-align" (verticalAlign fmt)
        tcs = if (noCase fmt) then ""
              else cssProp "text-transform" (textCase fmt)
        dsp = cssProp "display" (display fmt)
        props = concat $ filter (not . null) [ffam, fsty, fvt, fwt, tdec,
                                              valn, tcs, dsp]
        spanOpen = if null props then ""
                   else "<span style=\"" ++ props ++ "\">"
        spanClose = if null props then ""
                    else "</span>"
        -- TODO: quotes? stripPeriods?
        -- quotes :: Quote
        -- stripPeriods :: Bool
        -- TODO: check that all values are suitable for CSS...
        s' = pfx ++ " " ++ wrap spanOpen spanClose s ++ sfx
                
cssProp :: String -> String -> String
cssProp name val = if null val then ""
                   else name ++ ": " ++ val ++ "; "


-- ODT: TODO                


-- TODO:
-- 1. formatting functions for Plain, HTML, and ODT
--    fix spacing issues!
-- 4. use citeproc-js-compatible JSON: input should be an array of objects with
--    citationItems properties (each such object represents *one* citation,
--    possibly containing multiple references)

data OutputFormat = Ascii | Html   -- | Odt ...

chooseRenderer :: OutputFormat -> [FormattedOutput] -> String
chooseRenderer Ascii = renderPlain
chooseRenderer Html = renderHTML

chooseOutputFormat :: String -> OutputFormat
chooseOutputFormat s
  | s == "ascii" = Ascii
  | s == "html" = Html
  | otherwise = error $ "Unknown output format: " ++ s
  
main :: IO ()
main = do
  args <- getArgs
  progname <- getProgName
  unless (length args >= 3) $ do
    hPutStrLn stderr $ "Usage:  " ++ progname ++ " OUTPUT-FORMAT CSLFILE BIBFILE.."
    exitWith (ExitFailure 1)
  let (backend : cslfile : bibfiles) = args
  sty <- readCSLFile cslfile
  refs <- concat `fmap` mapM readBiblioFile bibfiles
  res <- decode `fmap` getContents
  -- hPutStrLn stderr $ show res
  let Ok citesData = res
  -- for debugging:
  -- hPutStrLn stderr $ show cites'
  let bibdata = citeproc procOpts sty refs (citationItems citesData)
  let renderer = chooseRenderer $ chooseOutputFormat backend
  -- hPutStrLn stderr $ show bibdata
  let citeprocres = CiteprocResult {
                          cites = map renderer (citations bibdata)
                        , bib   = map renderer (bibliography bibdata)
                        }
  putStrLn $ show citeprocres
