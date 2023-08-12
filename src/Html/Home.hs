module Html.Home where
import Lucid
import Html.Wrapper (wrapper)

homePage :: Html ()
homePage =  wrapper "hnefatafl"  $ do
  h1_ "Hnefatafl"
  br_ []
  h2_ "choose game type:"
