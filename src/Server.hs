{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server where

------------------------------------------------------------------------------
import           Html
import           Types
------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Exception.Safe
import qualified Data.Map.Strict                      as M
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.Lazy                       as TL
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Middleware.Cors          as Cors
import           Network.Wai.Middleware.RequestLogger
import           Protolude                            hiding (get, put, throwIO)
import qualified Text.Blaze.Html.Renderer.Text        as BT
import qualified Text.Blaze.Html4.FrameSet            as BFS
import           Web.Scotty                           as Scty
------------------------------------------------------------------------------

data HxException = HxException TL.Text deriving Show
instance Exception HxException

main :: IO ()
main  = do
  db <- newTVarIO M.empty
  scott db `catchAny` (\e -> do print e; scott db)

scott :: DB -> IO ()
scott db = scotty 3002 $ do
  -- WAI middleware is run top-down.
  middleware $
    Cors.cors (const $ Just (Cors.simpleCorsResourcePolicy
                              { Cors.corsRequestHeaders =
                                  [ "Accept"
                                  , "Accept-Language"
                                  , "Content-Language"
                                  , "Content-Type"
                                  , "hx-current-url"
                                  , "hx-request"
                                  ] } ))
  middleware logStdoutDev

  get "/" $ do
    id    <- getNextId db
    toHtml (Html.startPage id)

  get "/assets/:asset" $ do
    a     <- param' "asset"
    file ("assets/" <> a)

  get "/required/:id" $ do
    id    <- param' "id"
    toHtml (Html.requiredInfoInitial id)

  get "/required/:id/edit" $ do
    id    <- param' "id"
    toHtml (Html.requiredInfoEdit id)

  put "/required/:id" $ do
    id    <- param' "id"
    ps    <- params
    ats   <- updateDB db id ps
    toHtml (Html.requiredInfoComplete id ats)

  get "/add-attribute/:id" $ do
    id    <- param' "id"
    ats   <- updateDB db id [] -- TODO
    toHtml (Html.optionalInfo id ats)

  -- Use this to force an exception to see how they are handled
  --post "/new-attribute/:id:name:value" $ do
  post "/new-attribute/:id" $ do
    id    <- param' "id"
    name  <- param' "name"
    value <- param' "value"
    ats   <- updateDB db id [(name, value)]
    toHtml (Html.requiredInfoComplete id ats)

  get "/done/:id" $ do
    id    <- param' "id"
    dbc   <- getDBContents db
    let all'  = map (\(k, v) -> k <> v) (M.toList (lookupPanic id dbc))
    toHtml (Html.done ("signature on " <> T.concat all'))

  get "/error/" $ do
    ps    <- params
    id    <- getNextId db
    toHtml (Html.error (show ps) id)

 where
  thr :: TL.Text -> ActionM a
  thr t = do
    met  <- Wai.requestMethod <$> request
    path <- Wai.rawPathInfo   <$> request
    let msg = show met <> " " <> TL.fromStrict (TE.decodeLatin1 path) <> " '" <> t <>"'"
    print msg
    redirect ("/error/?err=" <> msg)

  param' :: Parsable a => TL.Text -> ActionM a
  param' p = param p `rescue` thr

toHtml :: BFS.Html -> ActionM ()
toHtml  = Scty.html . BT.renderHtml

getNextId :: DB -> ActionM Int
getNextId db = liftIO $ atomically $ do
  v     <- readTVar db
  let id = case M.keys v of [] -> 1; ks -> maximum ks + 1
  writeTVar db (M.insert id M.empty v)
  pure id

updateDB :: DB -> Int -> [Param] -> ActionM [(Text,Text)]
updateDB db id ps = liftIO $ atomically $ do
  v     <- readTVar db
  let v' = M.union (lookupPanic id v) (M.fromList (toSt ps))
  writeTVar db (M.insert id v' v)
  pure (M.toList v')
 where
  toSt :: [Param] -> [(Text, Text)]
  toSt  = map (\(l,r) -> (TL.toStrict l, TL.toStrict r))
{-# HLINT toSt ignore "Use bimap" #-}

getDBContents :: DB -> ActionM DBContents
getDBContents db = liftIO (atomically (readTVar db))

lookupPanic :: Ord k => k -> Map k a -> a
lookupPanic k m = case M.lookup k m of
  Nothing -> panic "impossible"
  Just a  -> a

