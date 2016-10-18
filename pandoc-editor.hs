{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures      #-}

module Main where
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Options (def,ReaderOptions(..),WriterOptions(..))
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
-- import Control.Monad (void)
import Data.Text
import GHCJS.Foreign.Callback
import GHCJS.DOM.Document (load,getElementById)
import GHCJS.DOM.Element (change,keyUp,setInnerHTML)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM (currentDocument,currentWindow)
import GHCJS.Types (JSString,JSVal)
import GHCJS.Prim
import Control.AutoUpdate
import Control.Exception
import Control.Exception.Base
import Text.HTML.SanitizeXSS(sanitizeBalance)

main :: IO ()
main = do
    callback <- syncCallback1' pandocToHtml
    set_callback_html callback

    callback <- syncCallback1' htmlToPandoc
    set_callback_markdown callback
    return ()

pandocToHtml :: JSVal -> IO JSVal
pandocToHtml content = do
    let content' = GHCJS.Prim.fromJSString content
    case converter content' of
      Just res -> do
          return $ GHCJS.Prim.toJSString res
      Nothing -> return $ GHCJS.Prim.toJSString "err"
htmlToPandoc :: JSVal -> IO JSVal
htmlToPandoc content = do
    let content' = GHCJS.Prim.fromJSString content
    case converter' content' of
      Just res -> do
          return $ GHCJS.Prim.toJSString res
      Nothing -> return $ GHCJS.Prim.toJSString "err"

foreign import javascript unsafe "pandocToHtml = $1;"
    set_callback_html :: (Callback (JSVal -> IO JSVal)) -> IO ()
foreign import javascript unsafe "htmlToPandoc = $1;"
    set_callback_markdown :: (Callback (JSVal -> IO JSVal)) -> IO ()

converter :: String -> Maybe String
converter input = do
    p <- case readMarkdown def input of
        Left _ -> Nothing
        Right parsed -> Just parsed

    Just $ (unpack . sanitizeBalance . pack ) $ writeHtmlString (def{writerHtml5=True}) p

converter' :: String -> Maybe String
converter' input = do
    p <- case readHtml def input of
        Left _ -> Nothing
        Right parsed -> Just parsed
    Just $ writeMarkdown def p

