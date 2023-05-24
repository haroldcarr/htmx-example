{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server where

------------------------------------------------------------------------------
import           Html
import           Types
------------------------------------------------------------------------------
import           Control.Concurrent.STM
import qualified Data.Map.Strict                      as M
import           Data.Monoid
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.IO                    as TLIO
import qualified Network.Wai.Middleware.Cors          as Cors
import           Network.Wai.Middleware.RequestLogger
import           Protolude                            hiding (get, put)
import qualified Text.Blaze.Html.Renderer.Text        as BT
import qualified Text.Blaze.Html4.FrameSet            as BFS
import           Web.Scotty                           as Scty
------------------------------------------------------------------------------

main :: IO ()
main  = do
  db <- newTVarIO M.empty
  scott db

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

  let baseDir = "assets"

  get "/" $ do
    id <- liftIO (getNextId db)
    toHtml (Html.startPage id)

  get "/htmx.min.js" $ do
    liftIO (TLIO.readFile (baseDir <> "/htmx.min.js")) >>= html

  get "/required/:id" $ do
    id <- param "id"
    toHtml (Html.requiredInfoInitial id)

  get "/required/:id/edit" $ do
    id <- param "id"
    toHtml (Html.requiredInfoEdit id)

  put "/required/:id" $ do
    id  <- param "id"
    ps  <- params
    ats <- liftIO (updateDB db id ps)
    toHtml (Html.optionalInfo id ats)

  post "/new-attribute/:id:name:value" $ do
    id    <- param "id"
    name  <- param "name"
    value <- param "value"
    ats   <- liftIO (updateDB db id [(name, value)])
    toHtml (Html.optionalInfo id ats)

  get "/done/:id" $ do
    id  <- param "id"
    db' <- liftIO (atomically (readTVar db))
    let all' = map (\(k, v) -> k <> v) (M.toList (lookupPanic id db'))
    html ("signature on " <> TL.concat (map TL.fromStrict all'))

toHtml :: BFS.Html -> ActionM ()
toHtml  = Scty.html . BT.renderHtml

getNextId :: DB -> IO Int
getNextId db = atomically $ do
  v     <- readTVar db
  let id = case M.keys v of [] -> 1; ks -> maximum ks + 1
  writeTVar db (M.insert id M.empty v)
  pure id

updateDB :: DB -> Int -> [Param] -> IO [(Text,Text)]
updateDB db id ps = atomically $ do
  v     <- readTVar db
  let v' = M.union (lookupPanic id v) (M.fromList (toSt ps))
  writeTVar db (M.insert id v' v)
  pure (M.toList v')
 where
  toSt :: [Param] -> [(Text, Text)]
  toSt  = map (\(l,r) -> (TL.toStrict l, TL.toStrict r))
{-# HLINT toSt ignore "Use bimap" #-}

lookupPanic :: Ord k => k -> Map k a -> a
lookupPanic k m = case M.lookup k m of
  Nothing -> panic "impossible"
  Just a  -> a

