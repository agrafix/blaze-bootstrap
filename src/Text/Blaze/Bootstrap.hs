{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Bootstrap where

import Control.Monad
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

stylesheet :: AttributeValue -> Html
stylesheet path =
    link ! href path ! rel "stylesheet" ! type_ "text/css"

javascript :: AttributeValue -> Html
javascript path =
    script ! src path $ emptyEl

emptyEl :: Html
emptyEl = toHtml T.empty

container :: Html -> Html
container x =
    H.div ! class_ "container" $ x

row :: Html -> Html
row x =
    H.div ! class_ "row" $ x

dataToggle :: AttributeValue -> Attribute
dataToggle = dataAttribute "toggle"

dataTarget :: AttributeValue -> Attribute
dataTarget = dataAttribute "target"

mainNavigation :: AttributeValue
               -> Html -> [(AttributeValue, Html)] -> Html
mainNavigation indexPath pageTitle navPoints =
    nav ! class_ "navbar navbar-default navbar-fixed-top" $
     container $
      do H.div ! class_ "navbar-header page-scroll" $
          do button ! type_ "button" ! class_ "navbar-toggle" ! dataToggle "collapse" ! dataTarget "#main-nav" $
              do H.span ! class_ "sr-only" $ "Toggle navigation"
                 H.span ! class_ "icon-bar" $ emptyEl
                 H.span ! class_ "icon-bar" $ emptyEl
                 H.span ! class_ "icon-bar" $ emptyEl
             a ! class_ "navbar-brand" ! href indexPath $ pageTitle
         H.div ! class_ "collapse navbar-collapse" ! A.id "main-nav" $
           ul ! class_ "nav navbar-nav navbar-right" $
              forM_ navPoints $ \(url, val) ->
                li (a ! href url $ val)
