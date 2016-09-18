{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures      #-}

module Main where
import Text.Pandoc
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
-- import Control.Monad (void)
import Data.Text
import Text.Blaze.Html.Renderer.String
import GHCJS.Foreign.Callback
import GHCJS.DOM.Document (load,getElementById)
import GHCJS.DOM.Element (keyUp,setInnerHTML)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM (currentDocument,currentWindow)
import GHCJS.Types (JSString,JSVal)
import Data.JSString (pack)
import GHCJS.Prim
import Control.Concurrent

main :: IO ()
main = do
    callback <- syncCallback1 ContinueAsync converter_
    set_callback callback
    threadDelay $ 200 * 1000
    Just w <- currentWindow
    Just d <- currentDocument
    Just edit <- getElementById d ("edit" :: String)
    content <- getCodeMirrorContent
    converter_ content
    edit `on` keyUp $ liftIO $ do
        content <- getCodeMirrorContent
        converter_ content
    return ()


foreign import javascript unsafe "window.editor.getValue()"
  getCodeMirrorContent :: IO JSVal

converter_ :: JSVal -> IO ()
converter_ new = do
    case converter (Data.Text.pack $ GHCJS.Prim.fromJSString new) "test" of
      Just res -> do
          Just d <- currentDocument
          Just preview <- getElementById d ("preview" :: String)
          setInnerHTML preview (Just res :: Maybe String)
          set_return $ Data.JSString.pack res

      Nothing -> return ()

foreign import javascript unsafe "js_callback_ = $1"
    set_callback :: Callback a -> IO ()
foreign import javascript unsafe "js_ret_ = $1"
    set_return :: JSString -> IO ()

converter :: Text -> String -> Maybe String
converter input _ = do
    p <- case readMarkdown def (Data.Text.unpack input) of
        Left _ -> Nothing
        Right parsed -> Just parsed
    Just $ renderHtml $ writeHtml def p
--}

