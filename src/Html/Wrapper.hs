module Html.Wrapper where

import Lucid (Html)
import Lucid.Html5
import Lucid.Base

wrapper  :: Text -> Html () -> Html ()
wrapper title body =
  doctypehtml_ $ do
    head_ $ do
      title_ $ toHtml title
      link_ [rel_ "stylesheet", type_ "text/css", href_ "style.css"]
    body_ [] $ do
      body
