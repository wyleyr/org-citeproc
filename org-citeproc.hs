{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
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
import Data.Set (empty)
import Control.Monad (unless)
import System.Exit
import System.IO
import Data.List (intersperse)

--
-- INPUT PROCESSING
-- 

-- represents arrays of citeproc-js citation data JSON objects
data CitationsData = CitationsData [CitationData] deriving (Typeable, Data)

instance JSON CitationsData where
  showJSON (CitationsData cds) = JSArray $ map showJSON cds 
  readJSON (JSArray cds) = Ok $ CitationsData cs
    where cs = reverse $ foldl getCitations [] cds
          getCitations acc obj = case readJSON obj of
                Ok cd@(CitationData _ _) -> cd:acc 
                _ -> acc -- TODO: error if non-citation in the array?
  readJSON x = fromJSON x
 
-- represents the citeproc-js citation data JSON object 
data CitationData = CitationData { citationItems :: [Cite]
                                   -- TODO: data structure for properties
                                 , properties :: [String]
                                 } deriving (Typeable, Data)
  
instance JSON CitationData where
  showJSON = toJSON  
  readJSON (JSObject o) = case get_field o "citationItems" of
    Just (JSArray cs) ->
      Ok CitationData { citationItems = cis
                      , properties = [] -- TODO
                      }
        where cis = reverse $ foldl getCites [] cs
              getCites acc obj = case readJSON obj of
                Ok c@(Cite {}) -> c:acc 
                _ -> acc -- TODO: error if non-citation in citationItems?
    _ -> Error "Not a citations data cluster"
  readJSON x = fromJSON x

-- JSON reader for pandoc-citeproc's Cite type
-- (this needs to be an orphan instance, since
-- neither Cite nor class JSON is defined here)
instance JSON Cite where
  showJSON = toJSON
  readJSON (JSObject o) = case get_field o "id" of
    Just (JSString citeid) ->
        Ok $ emptyCite{ citeId = fromJSString citeid
                      , citePrefix = case get_field o "prefix" of
                                       Just (JSString x) ->
                                         Formatted [Str (fromJSString x)]
                                       _ -> Formatted []
                      , citeSuffix = case get_field o "suffix" of
                                       Just (JSString x) ->
                                         Formatted [Str (fromJSString x)]
                                       _ -> Formatted []
                      -- TODO: can pandoc parse label, locator out of the suffix?
                      , citeLabel = case get_field o "label" of
                                       Just (JSString x) -> fromJSString x
                                       _ -> ""
                      , citeLocator = case get_field o "locator" of
                                       Just (JSString x) -> fromJSString x
                                       _ -> ""
                      , suppressAuthor = case get_field o "suppress-author" of
                                       Just (JSBool True) -> True
                                       _ -> False
                      , authorInText = case get_field o "author-in-text" of
                                       Just (JSBool True) -> True
                                       _ -> False
                      -- TODO: itemData, uris
                      -- See: https://raw.githubusercontent.com/citation-style-language/schema/master/csl-citation.json
                      -- https://raw.githubusercontent.com/citation-style-language/schema/master/csl-data.json
                      }
    _ -> Error "Not a citation item"
  readJSON x = fromJSON x

--
-- OUTPUT PROCESSING
-- 

-- output format selection
data OutputFormat = Ascii | Html | OpenDocument -- ...

chooseOutputFormat :: String -> OutputFormat
chooseOutputFormat s
  | s == "ascii" = Ascii
  | s == "html" = Html
  | s == "odt" = OpenDocument
  | otherwise = error $ "Unknown output format: " ++ s
 
type ItemRenderer = Formatted -> String
type BlockRenderer = [Formatted] -> String

chooseRenderers :: Style -> OutputFormat -> (ItemRenderer, BlockRenderer)
chooseRenderers sty fmt = (topRenderer . renderCite . toPandoc,
                           topRenderer . renderBibAndEntries)
  where toPandoc = renderPandoc sty
        renderBibAndEntries = renderBib . map (renderBibEntry . toPandoc)
        topRenderer = case fmt of
          Ascii -> renderPandocPlain
          Html -> renderPandocHTML
          OpenDocument -> renderPandocODT
        
-- represents result of pandoc-citeproc processing
data CiteprocResult = CiteprocResult { cites  :: [String]
                                     , bib    :: String
                                     } deriving (Typeable, Data)

                                               
instance Show CiteprocResult where 
  show cr = concat $ intersperse citeSep (cites cr) ++ 
            [citeSep, bibSecSep] ++
            [bib cr] 
    where citeSep = "////\n"
          bibSecSep = "====\n"


-- rendering functions:
-- common helpers:
renderCite :: [Inline] -> Block
renderCite inlines = Plain [Span citeAttr inlines]
  where citeAttr = ("", ["citation"], []) -- no id, citation class, no other attrs

renderBibEntry :: [Inline] -> Block
renderBibEntry = Para -- TODO: any other attrs? unique ID for linking's sake?

renderBib :: [Block] -> Block
renderBib = Div bibAttrs 
  where bibAttrs = ("", ["bibliography"], [])
        
withBlockAsDoc ::  (Pandoc -> String) -> Block -> String        
withBlockAsDoc writer block = writer $ Pandoc nullMeta [block]
              
-- plain text:
renderPandocPlain :: Block -> String
renderPandocPlain = withBlockAsDoc $ writePlain opts 
  where opts = def { writerStandalone = False
                   , writerTableOfContents = False
                   , writerCiteMethod = Citeproc
                   , writerWrapText = True -- TODO: don't break lines?
                   , writerColumns = 80 -- TODO: adjustable?
                   , writerExtensions = empty -- TODO: need any exts?
                   }

-- HTML: 
renderPandocHTML :: Block -> String
renderPandocHTML = withBlockAsDoc $ writeHtmlString opts 
  where opts = def { writerStandalone = False
                   , writerTableOfContents = False
                   , writerCiteMethod = Citeproc
                   , writerWrapText = False
                   , writerSlideVariant = NoSlides
                   }

-- ODT: 
renderPandocODT :: Block -> String        
renderPandocODT = withBlockAsDoc $ writeOpenDocument opts
  where opts = def { writerStandalone = False
                   , writerTableOfContents = False
                   , writerCiteMethod = Citeproc
                   , writerWrapText = False
                   -- TODO: , writerReferenceODT                    
                   }
 
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
                        , bib   = brenderer (bibliography bibdata)
                        }
  print citeprocres

