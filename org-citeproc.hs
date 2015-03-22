{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
import Text.CSL hiding (Citation, Cite(..))
import Text.CSL.Pandoc
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types (get_field)
import Text.Pandoc.Definition hiding (Cite)
import qualified Text.Pandoc.Definition as PDD (Inline(Cite))
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
-- (this needs to be an orphan instance, since neither Cite nor class JSON is
-- defined here)
-- represents an individual work in a citation data JSON object
data Cite = Cite { citeId :: String
                 , citePrefix :: [Inline]
                 , citeSuffix :: [Inline]
                 , citeLabel :: String
                 , citeLocator :: String
                 , suppressAuthor :: Bool
                 , authorInText :: Bool
                 -- , itemData :: [ItemData] -- TODO
                 , uris :: [String]
                 } deriving (Typeable, Data, Show)

instance JSON Cite where
  showJSON = toJSON
  readJSON (JSObject o) = case get_field o "id" of
    Just (JSString citeid) ->
        Ok $ Cite { citeId = fromJSString citeid
                  , citePrefix = case get_field o "prefix" of
                      Just (JSString s) -> [Str $ fromJSString s]
                      _ -> [] 
                  , citeSuffix = case get_field o "suffix" of
                      Just (JSString s) -> [Str $ fromJSString s]
                      _ -> []
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
                  , uris = case get_field o "uris" of
                      Just (JSArray ss) -> us
                        where us = reverse $ foldl getUris [] ss
                              getUris acc obj = case readJSON obj of
                                -- TODO: convert to Links
                                Ok (JSString s) -> (fromJSString s):acc 
                                _ -> acc -- TODO: error if non-strings in field?
                      _ -> []
                      -- TODO: itemData
                      -- See: https://raw.githubusercontent.com/citation-style-language/schema/master/csl-citation.json
                      -- https://raw.githubusercontent.com/citation-style-language/schema/master/csl-data.json
                      }
    _ -> Error "Not a citation item"
  readJSON x = fromJSON x

-- functions to transform input into a Pandoc
itemAsCitation :: Cite -> Citation
itemAsCitation i = Citation { citationId = citeId i
                            , citationPrefix = citePrefix i
                            , citationSuffix = citeSuffix i
                            , citationMode = if authorInText i 
                                             then AuthorInText
                                             else if suppressAuthor i
                                                  then SuppressAuthor
                                                  else NormalCitation
                            , citationNoteNum = 0 
                            , citationHash = 0 
                            }
                   
toPandocCite :: CitationData -> Inline
toPandocCite cd = PDD.Cite citas []
  where citas = map itemAsCitation $ citationItems cd

toMultiCiteGroup :: CitationData -> [Inline]
toMultiCiteGroup cd = group
  where items = citationItems cd
        props = properties cd 
        asCd i = CitationData { citationItems = [i],
                                properties = props} 
        citas = map (toPandocCite . asCd) items
        sep = Str ", " -- TODO: should grab separator from CSL
        group = intersperse sep citas 

citationsAsPandoc :: [CitationData] -> Pandoc
citationsAsPandoc cds = Pandoc nullMeta [citationBlock]
  where citeSep = Str "////\n" -- TODO: something like "<!--endCite-->"?
        citeBibSep = Str "====\n"
        -- behave like LaTeX by splitting up in-text citations with 2+
        -- references:
        atLeastTwo xs = not (null xs) && not (null $ tail xs)
        multiInText cd = atLeastTwo (citationItems cd) &&
                         all authorInText (citationItems cd)
        getInlines acc cd = if multiInText cd
                            then acc ++ toMultiCiteGroup cd ++ [citeSep]
                            else acc ++ [toPandocCite cd, citeSep]
        inlines = foldl getInlines [] cds ++ [citeBibSep]
        citationBlock = Plain inlines
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
 
       
chooseRenderer :: OutputFormat -> Pandoc -> String
chooseRenderer fmt = case fmt of
  Ascii -> renderPandocPlain
  Html -> renderPandocHTML
  OpenDocument -> renderPandocODT
        
-- rendering functions:
-- plain text:
renderPandocPlain :: Pandoc -> String
renderPandocPlain = writePlain opts 
  where opts = def { writerStandalone = False
                   , writerTableOfContents = False
                   , writerCiteMethod = Citeproc
                   , writerWrapText = True -- TODO: don't break lines?
                   , writerColumns = 80 -- TODO: adjustable?
                   , writerExtensions = empty -- TODO: need any exts?
                   }

-- HTML: 
renderPandocHTML :: Pandoc -> String
renderPandocHTML = writeHtmlString opts 
  where opts = def { writerStandalone = False
                   , writerTableOfContents = False
                   , writerCiteMethod = Citeproc
                   , writerWrapText = False
                   , writerSlideVariant = NoSlides
                   }

-- ODT: 
renderPandocODT :: Pandoc -> String        
renderPandocODT = writeOpenDocument opts
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
  --hPutStrLn stderr $ show inputCitations
  let doc = processCites sty refs $ citationsAsPandoc inputCitations
  putStrLn $ (chooseRenderer . chooseOutputFormat) backend $ doc

