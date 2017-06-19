{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Exception      as E
import           Data.Bool              (bool)
import           Data.Either            (isLeft)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import           Data.Monoid            ((<>))
import qualified Data.Text.Encoding     as T
import           Reflex.Dom
import           Data.FileEmbed
import qualified Data.Text              as T
import           GHCJS.DOM.Types        hiding (Event(..), Text(..))
import           Data.String.QQ

import           AbsSyn
import           Interpreter
import           Parser
import           TypeChecker


main :: IO ()
main = mainWidgetWithHead header $ divClass "content" $ do
    menu
    rec divClass "widget" $ do
            c <- codeBox ex
            fvgView c
        ex <- examplesPicker
        blank
    footer

codeBox
    :: MonadWidget t m
    => Event t T.Text
    -> m (Dynamic t (Either E.SomeException Expr))
codeBox setText = divClass "code-box" $ do
    pb <- getPostBuild
    t <- fmap value $
         textArea $ def
                  & textAreaConfig_attributes .~
                    constDyn ("style" =: "width:500px;height:500px;")
                  & textAreaConfig_setValue .~ setText
                  & textAreaConfig_initialValue .~ code0

    let evalTime = leftmost [tag (current t) pb, updated t]
    evals  <- performEvent $ ffor evalTime $ \code ->
        liftJSM . liftIO . E.try . E.evaluate . runScript $ T.unpack code
    res <- holdDyn (Left $ E.SomeException (error "e" :: E.IOException)) evals
    divClass "error-message" $
        dynText $ bool "" "There is a problem" . isLeft <$> res
    return res


fvgView
    :: MonadWidget t m
    => Dynamic t (Either E.SomeException Expr)
    -> m ()
fvgView e = do
    srcdoc <- holdDyn "<h1>Waiting</h1>"
              (T.pack . show <$> fmapMaybe hush (updated e))
    elDynAttr "iframe" (iframeAttrs <$> srcdoc) blank
    where iframeAttrs sd = "width"  =: "500"
                        <> "height" =: "500"
                        <> "srcdoc" =: sd

examplesPicker :: MonadWidget t m => m (Event t T.Text)
examplesPicker = do
    dd <- dropdown "" (constDyn $ ("" =: "Examples") <> Map.fromList examples) def
    return $ _dropdown_change dd

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a


--Toggle the bool to work from an external stylesheet
-- (much faster, if all you want to do is tweak the
--  page style)
header :: MonadWidget t m => m ()
header = if True
         then el "style" $ text (T.pack style)
         else elAttr "link" ("rel"  =: "stylesheet" <>
                             "href" =: "style.css"  <>
                             "type" =: "text/css"
                            ) blank


menu :: MonadWidget t m => m ()
menu = divClass "menu" $ do
    text "fvg"
    elAttr "a" ("href" =: "https://github.com/lemmih/fvg")
        -- (elAttr "img" ("src" =: "where's a link to octocat?") blank)
        (text "Github")

footer :: MonadWidget t m => m ()
footer = blank

examples :: [(T.Text, T.Text)]
examples = filt $ $(embedDir "tests")
    where
      filt xs = do
          (fn, f) <- xs
          let ft = T.pack fn
          guard $ ".fvg" `T.isSuffixOf` ft
          return (T.decodeUtf8 f, ft)

code0 :: T.Text
code0 = T.unlines [
   "data Bool = True | False"
  , ""
  ,"main : Int"
  ,"main ="
  ,"  case True of"
  ,"    True  -> 1"
  ,"    False -> 0"
  ]

style :: String
style = [s|
.menu {
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    font-size: 16pt;
    padding-bottom: 20px;
}

.content {
    display: flex;
    flex-direction: column;
    padding: 10px;
}

.widget {
    display: flex;
    flex-direction: row;
    padding-bottom: 20px;
}

.code-box {
    position: relative;
}

.error-message {
    position: absolute;
    top: 20px;
    right: 20px;
    color: hsla(0, 50%, 50%, 1);
}

a {
    text-decoration: none;
    color: hsl(234, 35%, 53%);
}

html {
    width: 100%;
    height: 100%;
    padding: 0px;
    margin: 0px;
}

body {
    background-color: hsl(0,0%, 95%);
    font-family: Helvetica;
    display: flex;
    flex-direction: column;
    width: 100%;
    height: 100%;
    padding: 0px;
    margin: 0px;
}

textarea {
    /* background-color: rgba(0,0,0,0); */
    background-color: hsl(0, 0%, 75%);
    color: hsl(240, 53%, 28%);
    border: none;
    resize: none;
    outline: none;
    font-family: "Lucida Console", Monaco, monospace;
    padding: 0px;
    padding-left: 10px;
}

iframe {
    border-style: solid;
    border: 0px;
    border-left: thick double rgba(0,0,0,0.25);
}

select {
    width: 400px;
}
|]
