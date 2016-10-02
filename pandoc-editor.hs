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
import Text.Pandoc.Writers.HTML (writeHtml)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
-- import Control.Monad (void)
import Data.Text
import Text.Blaze.Html.Renderer.String(renderHtml)
import GHCJS.Foreign.Callback
import GHCJS.DOM.Document (load,getElementById)
import GHCJS.DOM.Element (change,keyUp,setInnerHTML)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM (currentDocument,currentWindow)
import GHCJS.Types (JSString,JSVal)
import Data.JSString (pack)
import GHCJS.Prim
import Control.AutoUpdate
import Control.Exception
import Control.Exception.Base

main :: IO ()
main = do
    pandocToHtml' <- pandocToHtml
    callback <- asyncCallback (catch pandocToHtml' (\e -> return $ const () (e :: BlockedIndefinitelyOnMVar)))
    --callback <- asyncCallback1 ContinueAsync pandocToHtml
    set_callback_html callback
    callback' <- asyncCallback1 htmlToPandoc
    --callback' <- asyncCallback1 ContinueAsync htmlToPandoc
    set_callback_md callback'
        {-
    Just d <- currentDocument
    Just edit <- getElementById d ("edit" :: String)
    content <- getEditVal
    converter_ content
        edit `on` keyUp $ liftIO $ do
        content <- getEditVal
        converter_ content
        -}
    return ()

foreign import javascript unsafe "$('#edit').val()"
  getEditVal :: IO JSVal
foreign import javascript unsafe "$('#preview').html($1)"
  setPreviewVal :: JSString -> IO ()

foreign import javascript unsafe "window.editor.getValue()"
  getCodeMirrorContent :: IO JSVal

pandocToHtml :: IO (IO ())
pandocToHtml = mkAutoUpdate defaultUpdateSettings {
    updateFreq = 200000,
    updateAction = do
    new <- getEditVal
    case converter (GHCJS.Prim.fromJSString new) of
      Just res -> do
          setPreviewVal $ Data.JSString.pack res
      Nothing -> return ()}

htmlToPandoc :: JSVal -> IO ()
htmlToPandoc new = do
    case converter' (GHCJS.Prim.fromJSString new) of
      Just res -> do
          set_return_md $ Data.JSString.pack res
      Nothing -> return ()

foreign import javascript unsafe "js_toHTML = $1"
    set_callback_html :: Callback a -> IO ()
foreign import javascript unsafe "js_ret_html = $1"
    set_return_html :: JSString -> IO ()
foreign import javascript unsafe "js_toMarkdown = $1"
    set_callback_md :: Callback a -> IO ()
foreign import javascript unsafe "js_ret_md = $1"
    set_return_md :: JSString -> IO ()

converter :: String -> Maybe String
converter input = do
    p <- case readMarkdown def input of
        Left _ -> Nothing
        Right parsed -> Just parsed
    Just $ renderHtml $ writeHtml (def{writerHtml5=True}) p

converter' :: String -> Maybe String
converter' input = do
    p <- case readHtml def input of
        Left _ -> Nothing
        Right parsed -> Just parsed
    Just $ writeMarkdown def p

