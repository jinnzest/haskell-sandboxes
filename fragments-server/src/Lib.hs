{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    (
    runService
    ) where

import Control.Monad.Logger (logInfoN, MonadLogger,LoggingT)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify, lift, liftM)
import System.Cron.Schedule
import Network.HTTP.Client (newManager, defaultManagerSettings, httpLbs, parseRequest, responseStatus, responseBody)
import Network.HTTP.Types.Status (statusCode)
import Control.Monad    (msum)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path, seeOther, ServerPartT)
import Happstack.Server.SURI (toSURI)
import Data.Acid
import Data.Acid.Abstract
import Data.Function
import Data.IntMap (IntMap, size)
import Data.List
import Data.Ord
import Data.SafeCopy
import Data.Typeable
import Data.Time
import qualified Data.IntMap as IntMap
import Happstack.Server (port)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Read
import Data.List as L
import Data.ByteString.Lazy as BSL
import Data.Configurator
import Data.Configurator.Types
import Data.Time.Clock
import Data.Time.ISO8601
import Control.Monad.IO.Class
import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Data.Data            ( Data, Typeable )
import Happstack.Server     ( Response, ServerPart, dir
                            , nullDir, nullConf, ok
                            , simpleHTTP, toResponse )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Acid.Local      ( createCheckpointAndClose )
import Control.Applicative
import Control.Monad.Trans.Control
import Data.Time.Clock ( UTCTime, getCurrentTime )


data Cassettes = Cassettes{cassettes :: [CassetteWrapper]}deriving (Show, Generic, ToJSON, FromJSON)

data CassetteWrapper = CassetteWrapper{cassette :: Cassette}deriving (Show, Generic, ToJSON, FromJSON)

data Cassette = Cassette { cassette_id :: !Text, template_wrapper :: TemplateWrapper}deriving (Show, Generic)

data TemplateWrapper = TemplateWrapper{templates :: [Template]}deriving (Show, Generic, ToJSON, FromJSON)

data Template = Template { id :: !Int, template :: !Text }deriving (Show, Generic, ToJSON, FromJSON, Typeable)

data TemplatesDb = TemplatesDb { allTemplates :: IntMap Template }
  deriving (Typeable)

cassette_noprefix "cassette_id" = "id"
cassette_noprefix s = s

instance FromJSON Cassette where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = cassette_noprefix })

instance ToJSON Cassette where
  toJSON (Cassette cassette_id template_wrapper) = object ["id" .= cassette_id, "template_wrapper" .= template_wrapper]

templatesQuery :: Query TemplatesDb (IntMap Template)
templatesQuery = allTemplates <$> ask

addTemplates :: [Template] -> Update TemplatesDb ()
addTemplates templatesQuery = modify dbOp
 where
 dbOp (TemplatesDb db) = TemplatesDb $
  case IntMap.maxViewWithKey db of
   Just _ -> addToMap templatesQuery db
   Nothing -> templatesToMap templatesQuery

addToMap q db = IntMap.union (templatesToMap q) db

templatesToMap :: [Template] -> IntMap Template
templatesToMap [] = IntMap.empty
templatesToMap (x:xs) = IntMap.insert (Lib.id x) x (templatesToMap xs)

deriveSafeCopy 0 'base ''Template
deriveSafeCopy 0 'base ''TemplatesDb
makeAcidic ''TemplatesDb [ 'addTemplates, 'templatesQuery]

extractTemplates:: BSL.ByteString -> [Template]
extractTemplates resp =
  case (Data.Aeson.decode resp) of
    Nothing -> []
    Just r -> Data.List.foldl fromCassettes [] (cassettes r)
 where
  fromCassettes  tmpls cw  = tmpls ++ extrTmpls cw
  extrTmpls  = L.foldr (:) [] . templates . template_wrapper . cassette

downloadJsonJob   :: Data.Acid.Abstract.AcidState Lib.TemplatesDb -> LoggingT IO ()
downloadJsonJob state = do
  response <- liftIO downloadJson
  logStatusCode response
  let templates = extractTemplates $ responseBody response
  added <- liftIO $ update state $ AddTemplates templates
  logAddedAmount templates
  allTemplates <- liftIO $ query state TemplatesQuery
  logDownloadingIsDone

logInfo:: String -> LoggingT IO ()
logInfo str = do
 time <- liftIO getCurrentTime
 let timeStr = show time
 logInfoN $ T.pack $ timeStr++" "++str

logStatusCode response = logInfo $ " The status code was: " ++ statusCodeStr response
logAddedAmount t = logInfo $ " bodies len in Just(_): " ++ amountStr t
 where amountStr = show . size . templatesToMap
logDownloadingIsDone = logInfo " templates are downloaded"
statusCodeStr = show . statusCode . responseStatus

downloadJson = do
 manager <- newManager defaultManagerSettings
 request <- parseRequest "http://localhost:9000/db.json"
 httpLbs request manager

initialCounterState = TemplatesDb IntMap.empty

runService :: Control.Monad.Logger.LoggingT IO ()
runService = do
 logInfo "starting service"
 liftBaseWith $ \runInIO ->
   bracket (openLocalState initialCounterState) (createCheckpointAndClose) (runAll $ runInIO . downloadJsonJob)

runAll jsonJob acid = do
 execSchedule $ addJob (jsonJob acid)  "* * * * *"
 simpleHTTP nullConf $ handlers acid

handlers :: AcidState TemplatesDb -> ServerPart Response
handlers state = msum [
 dir "template" $ do
  templatesMap <- query' state TemplatesQuery
  path $ \id -> ok $ toResponse $ show (IntMap.lookup (id::Int) templatesMap)
 ,dir "templates" $ do
  templatesMap <- query' state TemplatesQuery
  ok $ toResponse $ show(templatesMap)
 ]