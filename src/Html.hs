{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Html where

------------------------------------------------------------------------------
--import           Types
------------------------------------------------------------------------------
--import           Control.Concurrent.STM
import           Protolude                   hiding (div)
import qualified Text.Blaze.Html4.FrameSet   as BFS
import           Text.Blaze.Html5            as BH
import           Text.Blaze.Html5.Attributes as BA hiding (form, label, title)
------------------------------------------------------------------------------

hxGet,hxPost,hxPut,hxSwap,hxTarget :: AttributeValue -> Attribute
hxGet    = customAttribute "hx-get"
hxPost   = customAttribute "hx-post"
hxPut    = customAttribute "hx-put"
hxSwap   = customAttribute "hx-swap"
hxTarget = customAttribute "hx-target"

startPage :: Int -> BFS.Html
startPage id0 = do
  title "start page"
  body $ do
    script ! src "htmx.min.js" $ "WHY?"
    div $ do
      p (b "create credential")
      button ! hxGet (textValue ("/required/" <> show id0)) ! hxSwap "outerHTML" $ "start"

requiredInfoInitial :: Int -> BFS.Html
requiredInfoInitial id0 = html $ do
  body $ do
    div ! hxTarget "this" ! hxSwap "outerHTML" $ do
      p (b "required fields")
      div $ do label "First Name" ; ":"
      div $ do label "Last  Name" ; ":"
      div $ do label "email"      ; ":"
      button ! hxGet (textValue ("/required/" <> show id0 <> "/edit"))
             ! class_ "btn btn-primary" $ "click to edit"

requiredInfoEdit :: Int -> BFS.Html
requiredInfoEdit id0 = html $ do
  let loc = textValue ("/required/" <> show id0)
  form ! hxPut loc ! hxTarget "this" ! hxSwap "outerHTML" $ do
    p (b "required fields")
    div $ do
      label "First Name"
      input ! type_ "text"  ! name "firstName" ! value ""
    div ! class_ "form-group" $ do
      label "Last  Name"
      input ! type_ "text"  ! name "lastName"  ! value ""
    div ! class_ "form-group" $ do
      label "email"
      input ! type_ "email" ! name "email"     ! value ""
    button ! class_ "btn" $ "Submit"
    button ! class_ "btn" ! hxGet loc $ "Cancel"

optionalInfo :: Int -> [(Text, Text)] -> BFS.Html
optionalInfo id0 ats = html $ do
  div ! id "table-and-form" $ do
    h2 "attributes"
    table ! class_ "table" $ do
      thead $ tr $ do th (text "name"); th (text "value")
      tbody ! id "attributes-table" $
        forM_ ats $ \(n,v) -> tr $ do td (text n); td (text v)
    h2 "add an attribute"
    form ! hxPost (textValue ("/new-attribute/" <> show id0)) ! hxTarget "#table-and-form" $ do
      label "name"
      input ! type_ "text" ! name "name"
      label "value"
      input ! type_ "text" ! name "value"
      button ! class_ "btn" $ "add"
      button ! class_ "btn"
             ! hxGet (textValue ("/done/" <> show id0))
             $ "DONE"

{-# HLINT ignore optionalInfo "Redundant id" #-}
