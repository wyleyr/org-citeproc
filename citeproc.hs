{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, PatternGuards #-}
import Text.CSL
import Text.CSL.Style
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types (get_field)
import Text.Pandoc.Definition hiding (Cite)
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.OpenDocument
import Text.Pandoc.Options
--import Text.Pandoc.Generic
import qualified Data.Map as M
import Data.Set (empty)
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

-- JSON reader for pandoc-citeproc's Cite type
instance JSON Cite where
  showJSON = toJSON
  readJSON (JSObject o) = case get_field o "id" of
    Just (JSString x) ->
        Ok $ emptyCite{ citeId = fromJSString x
                      , citePrefix = case get_field o "prefix" of
                                       Just (JSString x) ->
                                         Formatted $ [Str (fromJSString x)]
                                       _ -> Formatted []
                      , citeSuffix = case get_field o "suffix" of
                                       Just (JSString x) ->
                                         Formatted $ [Str (fromJSString x)]
                                       _ -> Formatted []
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
data OutputFormat = Ascii | Html | OpenDocument -- ...

type Renderer = Formatted -> String

chooseRenderers :: Style -> OutputFormat -> (Renderer, Renderer)
chooseRenderers sty Ascii =
  (renderPandocPlain . renderCite . renderPandoc sty,
   renderPandocPlain . renderBibEntry . renderPandoc sty)
chooseRenderers sty Html =
  (renderPandocHTML . renderCite . renderPandoc sty,
   renderPandocHTML . renderBibEntry . renderPandoc sty) 
chooseRenderers sty OpenDocument =
  (renderPandocODT . renderCite . renderPandoc sty,
   renderPandocODT . renderBibEntry . renderPandoc sty)

chooseOutputFormat :: String -> OutputFormat
chooseOutputFormat s
  | s == "ascii" = Ascii
  | s == "html" = Html
  | s == "odt" = OpenDocument
  | otherwise = error $ "Unknown output format: " ++ s
 
-- represents result of pandoc-citeproc processing
data CiteprocResult = CiteprocResult { cites  :: [String]
                                     , bib    :: [String]
                                     } deriving (Typeable, Data)

                                               
instance Show CiteprocResult where                                                
  show cr = concat $ intersperse citeSep (cites cr) ++ 
            [citeSep, bibSecSep] ++
            intersperse bibEntrySep (bib cr) 
    where citeSep = "////\n"
          bibEntrySep = "\n" -- TODO: should bib entries be wrapped in paragraphs?
          bibSecSep = "====\n"


-- rendering functions:
-- common helpers:
wrap :: String -> String -> String -> String
wrap openTag closeTag s = openTag ++ s ++ closeTag 

trim :: String -> String 
trim = unwords . words

renderItem :: Attr -> [Inline] -> [Inline]
renderItem attr inlines = [Span attr inlines]

renderCite :: [Inline] -> [Inline]
renderCite = renderItem citeAttr
  where citeAttr = ("", ["citation"], []) -- no id, citation class, no other attrs

renderBibEntry :: [Inline] -> [Inline]
renderBibEntry = renderItem bibEntryAttr
  where bibEntryAttr = ("", ["bibliography-entry"], []) 

-- plain text:
renderPandocPlain :: [Inline] -> String
renderPandocPlain inlines = writePlain opts doc
  where opts = WriterOptions { writerStandalone = False
                             , writerTableOfContents = False
                             , writerCiteMethod = Citeproc
                             , writerWrapText = True -- TODO: don't break lines?
                             , writerColumns = 80 -- TODO: adjustable?
                             , writerExtensions = empty -- TODO: need any exts?
                             }
        doc = Pandoc nullMeta $ [Plain inlines]


-- HTML: 
renderPandocHTML :: [Inline] -> String
renderPandocHTML inlines = writeHtmlString opts doc 
  where opts = WriterOptions { writerStandalone = False
                             , writerTableOfContents = False
                             , writerCiteMethod = Citeproc
                             , writerSlideVariant = NoSlides
                             }
        doc = Pandoc nullMeta $ [Plain inlines]

-- ODT: 
renderPandocODT :: [Inline] -> String        
renderPandocODT inlines = writeOpenDocument opts doc
  where opts = WriterOptions { writerStandalone = False
                             , writerTableOfContents = False
                             , writerCiteMethod = Citeproc
                             , writerWrapText = False
                             -- TODO: , writerReferenceODT
                             }
        doc = Pandoc nullMeta $ [Plain inlines]
 
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
  sty <- readCSLFile Nothing cslfile
  refs <- concat `fmap` mapM readBiblioFile bibfiles
  res <- decode `fmap` getContents
  -- hPutStrLn stderr $ show res
  let Ok (CitationsData inputCitations) = res
  -- for debugging:
  -- hPutStrLn stderr $ show inputCitations
  let bibdata = citeproc procOpts sty refs $ map citationItems inputCitations
  let (crenderer, brenderer) = chooseRenderers sty $ chooseOutputFormat backend
  -- hPutStrLn stderr $ show bibdata
  let citeprocres = CiteprocResult {
                          cites = map crenderer (citations bibdata)
                        , bib   = map brenderer (bibliography bibdata)
                        }
  putStrLn $ show citeprocres
