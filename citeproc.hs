{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, PatternGuards #-}
import Text.CSL
import Text.CSL.Style
import Text.CSL.Output.Plain ((<>))
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

--
-- INPUT PROCESSING
-- 

-- represents arrays of citeproc-js citation data JSON objects
data CitationsData = CitationsData [CitationData]

instance JSON CitationsData where
  -- showJSON = toJSON -- TODO: re-encode as array
  readJSON (JSArray cds) = Ok $ CitationsData citations
    where citations = reverse $ foldl getCitations [] cds
          getCitations acc obj = case readJSON obj of
                Ok cd@(CitationData _ _) -> cd:acc 
                _ -> acc -- TODO: error if non-citation in the array?
 
-- represents the citeproc-js citation data JSON object 
data CitationData = CitationData { citationItems :: [Cite]
                                   -- TODO: data structure for properties
                                 , properties :: [String]
                                 }
  
instance JSON CitationData where
  -- showJSON = toJSON -- TODO: re-encode using citationItems, etc.
  readJSON (JSObject o) = case get_field o "citationItems" of
    Just (JSArray cs) ->
      Ok $ CitationData { citationItems = cites
                        , properties = [] -- TODO
                        }
        where cites = reverse $ foldl getCites [] cs
              getCites acc obj = case readJSON obj of
                Ok c@(Cite _ _ _ _ _ _ _ _ _ _ _) -> c:acc 
                _ -> acc -- TODO: error if non-citation in citationItems?
    _ -> Error "Not a citations data cluster"

-- JSON reader for citeproc-hs' Cite type
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
                      , citeLabel = case get_field o "label" of
                                       Just (JSString x) -> fromJSString x
                                       _ -> ""
                      , citeLocator = case get_field o "locator" of
                                       Just (JSString x) -> fromJSString x
                                       _ -> ""
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
                      , authorInText = case get_field o "author-in-text" of
                                       Just (JSBool True) -> True
                                       _ -> False
                      }
    _ -> Error "Not a citation item"
  readJSON x = fromJSON x

jsString :: String -> JSValue
jsString = JSString . toJSString

--
-- OUTPUT PROCESSING
-- 

-- output format selection
data OutputFormat = Ascii | Html   -- | Odt ...

type Renderer = [FormattedOutput] -> String

chooseRenderers :: OutputFormat -> (Renderer, Renderer)
chooseRenderers Ascii = (renderPlain, renderPlain)
chooseRenderers Html = (renderCiteHTML, renderBibEntryHTML) 

chooseOutputFormat :: String -> OutputFormat
chooseOutputFormat s
  | s == "ascii" = Ascii
  | s == "html" = Html
  | otherwise = error $ "Unknown output format: " ++ s
 
-- represents result of citeproc-hs processing
data CiteprocResult = CiteprocResult { cites  :: [String]
                                     , bib    :: [String]
                                     } deriving (Typeable, Data)

                                               
instance Show CiteprocResult where                                                
  show cr = concat $ intersperse "\n" (cites cr) ++
            ["\n=====\n"] ++
            intersperse "\n" (bib cr) ++
            ["\n"]


-- rendering functions:
-- common helpers:
wrap :: String -> String -> String -> String
wrap openTag closeTag s = openTag ++ s ++ closeTag 

trim :: String -> String 
trim = unwords . words

-- plain text: use citeproc-hs' renderPlain

-- HTML: 
renderCiteHTML :: [FormattedOutput] -> String
renderCiteHTML = (wrap spanOpen spanClose) . renderHTML
  where spanOpen = "<span class=\"citation\">"
        spanClose = "</span>"

renderBibEntryHTML :: [FormattedOutput] -> String
renderBibEntryHTML = (wrap pOpen pClose) . renderHTML
  where pOpen = "<p class=\"bibliography-entry\">"
        pClose = "</p>"
  
renderHTML :: [FormattedOutput] -> String
renderHTML = concatMap htmlify

htmlify :: FormattedOutput -> String
htmlify fo = case fo of
  (FO fmt xs) -> wrapHTMLStyle (trim $ renderHTML xs) fmt 
  (FN n fmt) -> wrapHTMLStyle n fmt 
  (FS s fmt) -> wrapHTMLStyle s fmt
  (FDel s) -> s 
  (FUrl target@(url, title) fmt) -> wrapHTMLStyle link fmt
    where link = wrap ("<a href=\"" ++ url ++ "\">") "</a>" title
  otherwise -> ""

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
                                   -- TODO: capitalize-first is not a valid
                                   -- CSS text-transform value
              else cssProp "text-transform" (textCase fmt)
        dsp = cssProp "display" (display fmt)
        props = concat $ filter (not . null) [ffam, fsty, fvt, fwt, tdec,
                                              valn, tcs, dsp]
        spanOpen = if null props then ""
                   else "<span style=\"" ++ (trim props) ++ "\">"
        spanClose = if null props then ""
                    else "</span>"
        cssProp name val = if null val then ""
                           else name ++ ": " ++ val ++ "; "
        quoted s = if null s || quotes fmt == NoQuote then s
                   else wrap "\"" "\"" s
        -- TODO: stripPeriods :: Bool
        s' = pfx <> wrap spanOpen spanClose (quoted s) <> sfx
                
-- ODT: TODO                

 
--
-- MAIN
-- 
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
  let Ok (CitationsData inputCitations) = res
  -- for debugging:
  -- hPutStrLn stderr $ show cites'
  let bibdata = citeproc procOpts sty refs $ map citationItems inputCitations
  let (crenderer, brenderer) = chooseRenderers $ chooseOutputFormat backend
  -- hPutStrLn stderr $ show bibdata
  let citeprocres = CiteprocResult {
                          cites = map crenderer (citations bibdata)
                        , bib   = map brenderer (bibliography bibdata)
                        }
  putStrLn $ show citeprocres
