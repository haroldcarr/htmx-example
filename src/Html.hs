{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Html where

------------------------------------------------------------------------------
import           Protolude                   hiding (div)
import qualified Text.Blaze.Html4.FrameSet   as BFS
import           Text.Blaze.Html5            as BH
import           Text.Blaze.Html5.Attributes as BA hiding (form, label, title)
------------------------------------------------------------------------------

{-# HLINT ignore "Redundant id" #-}

hxGet,hxPost,hxPut,hxSwap,hxTarget :: AttributeValue -> Attribute
hxGet    = customAttribute "hx-get"
hxPost   = customAttribute "hx-post"
hxPut    = customAttribute "hx-put"
hxSwap   = customAttribute "hx-swap"
hxTarget = customAttribute "hx-target"

startPage :: Int -> BFS.Html
startPage id0 = html $ do
  title "start page"
  script ! src "/assets/htmx.min.js" $ "WHY?"
  body $ do
    div ! id "acme-create-credential" $ do
      img ! src "/assets/acme.png"
      p (b "create credential")
      button ! hxGet (textValue ("/required/" <> show id0)) ! hxSwap "outerHTML" $ "start"
    div "copyright 02023 - all rights reserved"

requiredInfoInitial :: Int -> BFS.Html
requiredInfoInitial id0 = html $ do
  div ! hxTarget "this" ! hxSwap "outerHTML" $ do
    p (b "required attributes")
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

requiredInfoComplete :: Int -> [(Text, Text)] -> BFS.Html
requiredInfoComplete id0 ats = html $ do
  div ! id "table-and-form" $ do
    h2 "attributes"
    mkTable ats
    div $
      button ! class_ "btn"
             ! hxGet (textValue ("/required/" <> show id0 <> "/edit"))
             ! hxTarget "#table-and-form"
             $ "edit required"
    div $
      button ! class_ "btn"
             ! hxGet (textValue ("/add-attribute/" <> show id0))
             ! hxTarget "#table-and-form"
             $ "add an attribute"
    div $
      button ! class_ "btn"
             ! hxGet (textValue ("/done/" <> show id0))
             ! hxTarget "#acme-create-credential"
             $ "DONE"

optionalInfo :: Int -> [(Text, Text)] -> BFS.Html
optionalInfo id0 ats = html $ do
  div ! id "table-and-form" $ do
    h2 "attributes"
    mkTable ats
    h2 "add an attribute"
    form ! hxPost (textValue ("/new-attribute/" <> show id0)) ! hxTarget "#table-and-form" $ do
      label "name"
      input ! type_ "text" ! name "name"
      label "value"
      input ! type_ "text" ! name "value"
      button ! class_ "btn" $ "submit"

done :: Text -> BFS.Html
done sig = html $ do
  div $ do
    img ! src "/assets/acme.png"
    img ! src "/assets/certified.png" ! width "125" ! height "100"
    b (text sig)
  div $
    button ! class_ "btn"
           ! hxGet "/"
           ! hxTarget "#acme-create-credential"
           $ "spin again"

error :: Text -> Int -> BFS.Html
error  err id0 =
  div ! customAttribute "hx-swap-oob" "true"
      ! id "acme-create-credential" $ do
    div "server error : sorry we lost all your work because we are not really web developers"
    div (text err)
    button ! class_ "btn"
           ! hxGet (textValue ("/required/" <> show id0))
           ! hxTarget "#acme-create-credential"
           $ "start over"

------------------------------------------------------------------------------

mkTable :: [(Text, Text)] -> BFS.Html
mkTable ats =
  table ! class_ "table" $ do
    thead $ tr $ do th (text "name"); th (text "value")
    tbody ! id "attributes-table" $
      forM_ ats $ \(n,v) -> tr $ do td (text n); td (text v)
