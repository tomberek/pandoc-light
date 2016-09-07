{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-
Copyright (C) 2006-2016 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the main writers, readers, and data
structure definitions from the Pandoc libraries.

A typical application will chain together a reader and a writer
to convert strings from one format to another.  For example, the
following simple program will act as a filter converting markdown
fragments to reStructuredText, using reference-style links instead of
inline links:

> module Main where
> import Text.Pandoc
> import Text.Pandoc.Error (handleError)
>
> markdownToRST :: String -> String
> markdownToRST = handleError .
>   writeRST def {writerReferenceLinks = True} .
>   readMarkdown def
>
> main = getContents >>= putStrLn . markdownToRST

Note:  all of the readers assume that the input text has @'\n'@
line endings.  So if you get your input text from a web form,
you should remove @'\r'@ characters using @filter (/='\r')@.

-}

module Text.Pandoc
               (
               -- * Definitions
               module Text.Pandoc.Definition
               -- * Generics
               , module Text.Pandoc.Generic
               -- * Options
               , module Text.Pandoc.Options
               -- * Lists of readers and writers
               , readers
               , writers
               -- * Readers: converting /to/ Pandoc format
               , Reader (..)
               , mkStringReader
               , readMarkdown
               , readLaTeX
               , readHtml
               , readJSON
               -- * Writers: converting /from/ Pandoc format
              , Writer (..)
               , writeJSON
               , writeMarkdown
               , writeLaTeX
               , writeHtml
               , writeHtmlString
               , module Text.Pandoc.Templates
               -- * Miscellaneous
               , getReader
               , getWriter
               , ToJsonFilter(..)
               , pandocVersion
             ) where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.JSON
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Templates
import Text.Pandoc.Options
import Text.Pandoc.Shared (safeRead, warn, mapLeft, pandocVersion)
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.Error
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Error
import qualified Text.Pandoc.UTF8 as UTF8

parseFormatSpec :: String
                -> Either ParseError (String, Set Extension -> Set Extension)
parseFormatSpec = parse formatSpec ""
  where formatSpec = do
          name <- formatName
          extMods <- many extMod
          return (name, \x -> foldl (flip ($)) x extMods)
        formatName = many1 $ noneOf "-+"
        extMod = do
          polarity <- oneOf "-+"
          name <- many $ noneOf "-+"
          ext <- case safeRead ("Ext_" ++ name) of
                       Just n  -> return n
                       Nothing
                         | name == "lhs" -> return Ext_literate_haskell
                         | otherwise -> fail $ "Unknown extension: " ++ name
          return $ case polarity of
                        '-'  -> Set.delete ext
                        _    -> Set.insert ext


data Reader = StringReader (ReaderOptions -> String -> IO (Either PandocError Pandoc))
              | ByteStringReader (ReaderOptions -> BL.ByteString -> IO (Either PandocError (Pandoc,MediaBag)))

mkStringReader :: (ReaderOptions -> String -> Either PandocError Pandoc) -> Reader
mkStringReader r = StringReader (\o s -> return $ r o s)

mkStringReaderWithWarnings :: (ReaderOptions -> String -> Either PandocError (Pandoc, [String])) -> Reader
mkStringReaderWithWarnings r  = StringReader $ \o s ->
  case r o s of
    Left err -> return $ Left err
    Right (doc, warnings) -> do
      mapM_ warn warnings
      return (Right doc)

-- | Association list of formats and readers.
readers :: [(String, Reader)]
readers = [
           ("json"         , mkStringReader readJSON )
           ,("markdown"     , mkStringReaderWithWarnings readMarkdownWithWarnings)
           ,("markdown_strict" , mkStringReaderWithWarnings readMarkdownWithWarnings)
           ,("markdown_phpextra" , mkStringReaderWithWarnings readMarkdownWithWarnings)
           ,("markdown_github" , mkStringReaderWithWarnings readMarkdownWithWarnings)
           ,("markdown_mmd",  mkStringReaderWithWarnings readMarkdownWithWarnings)
           ,("html"         , mkStringReader readHtml)
           ,("latex"        , mkStringReader readLaTeX)
           ]

data Writer = PureStringWriter   (WriterOptions -> Pandoc -> String)
            | IOStringWriter     (WriterOptions -> Pandoc -> IO String)
            | IOByteStringWriter (WriterOptions -> Pandoc -> IO BL.ByteString)

-- | Association list of formats and writers.
writers :: [ ( String, Writer ) ]
writers = [
  ("json"         , PureStringWriter writeJSON)
  ,("html"         , PureStringWriter writeHtmlString)
  ,("html5"        , PureStringWriter $ \o ->
     writeHtmlString o{ writerHtml5 = True })
  ,("s5"           , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = S5Slides
                      , writerTableOfContents = False })
  ,("slidy"        , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = SlidySlides })
  ,("slideous"     , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = SlideousSlides })
  ,("dzslides"     , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = DZSlides
                      , writerHtml5 = True })
  ,("revealjs"      , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = RevealJsSlides
                      , writerHtml5 = True })
  ,("latex"        , PureStringWriter writeLaTeX)
  ,("markdown"     , PureStringWriter writeMarkdown)
  ,("markdown_strict" , PureStringWriter writeMarkdown)
  ,("markdown_phpextra" , PureStringWriter writeMarkdown)
  ,("markdown_github" , PureStringWriter writeMarkdown)
  ,("markdown_mmd" , PureStringWriter writeMarkdown)
  ]

getDefaultExtensions :: String -> Set Extension
getDefaultExtensions "markdown_strict" = strictExtensions
getDefaultExtensions "markdown_phpextra" = phpMarkdownExtraExtensions
getDefaultExtensions "markdown_mmd" = multimarkdownExtensions
getDefaultExtensions "markdown_github" = githubMarkdownExtensions
getDefaultExtensions "markdown"        = pandocExtensions
getDefaultExtensions "plain"           = plainExtensions
getDefaultExtensions "org"             = Set.fromList [Ext_citations,
                                                       Ext_auto_identifiers]
getDefaultExtensions "textile"         = Set.fromList [Ext_auto_identifiers]
getDefaultExtensions "html"            = Set.fromList [Ext_auto_identifiers,
                                                       Ext_native_divs,
                                                       Ext_native_spans]
getDefaultExtensions "html5"           = getDefaultExtensions "html"
getDefaultExtensions "epub"            = Set.fromList [Ext_raw_html,
                                                       Ext_native_divs,
                                                       Ext_native_spans,
                                                       Ext_epub_html_exts]
getDefaultExtensions _                 = Set.fromList [Ext_auto_identifiers]

-- | Retrieve reader based on formatSpec (format+extensions).
getReader :: String -> Either String Reader
getReader s =
  case parseFormatSpec s of
       Left e  -> Left $ intercalate "\n" [m | Message m <- errorMessages e]
       Right (readerName, setExts) ->
           case lookup readerName readers of
                   Nothing  -> Left $ "Unknown reader: " ++ readerName
                   Just  (StringReader r)  -> Right $ StringReader $ \o ->
                                  r o{ readerExtensions = setExts $
                                            getDefaultExtensions readerName }
                   Just (ByteStringReader r) -> Right $ ByteStringReader $ \o ->
                                  r o{ readerExtensions = setExts $
                                            getDefaultExtensions readerName }

-- | Retrieve writer based on formatSpec (format+extensions).
getWriter :: String -> Either String Writer
getWriter s
  = case parseFormatSpec s of
         Left e  -> Left $ intercalate "\n" [m | Message m <- errorMessages e]
         Right (writerName, setExts) ->
             case lookup writerName writers of
                     Nothing -> Left $ "Unknown writer: " ++ writerName
                     Just (PureStringWriter r) -> Right $ PureStringWriter $
                             \o -> r o{ writerExtensions = setExts $
                                              getDefaultExtensions writerName }
                     Just (IOStringWriter r) -> Right $ IOStringWriter $
                             \o -> r o{ writerExtensions = setExts $
                                              getDefaultExtensions writerName }
                     Just (IOByteStringWriter r) -> Right $ IOByteStringWriter $
                             \o -> r o{ writerExtensions = setExts $
                                              getDefaultExtensions writerName }

{-# DEPRECATED toJsonFilter "Use 'toJSONFilter' from 'Text.Pandoc.JSON' instead" #-}
-- | Deprecated.  Use @toJSONFilter@ from @Text.Pandoc.JSON@ instead.
class ToJSONFilter a => ToJsonFilter a
  where toJsonFilter :: a -> IO ()
        toJsonFilter = toJSONFilter

readJSON :: ReaderOptions -> String -> Either PandocError Pandoc
readJSON _ = mapLeft ParseFailure . eitherDecode' . UTF8.fromStringLazy

writeJSON :: WriterOptions -> Pandoc -> String
writeJSON _ = UTF8.toStringLazy . encode
