{-# LANGUAGE FlexibleContexts #-}

module Carl2.Bot where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text as T
import Data.Time
import Database.Persist.Sqlite hiding (Update)
import Telegram.Bot.API
import Telegram.Bot.Simple as TGS
import Telegram.Bot.Simple.Debug

type DbBotM m a = ReaderT m BotM a

-- A bot whose model is stored in a database.
data BotCfg a = BotCfg
  { botCfgMigration    :: Migration
  , botCfgHandleUpdate :: Update -> Maybe a
  , botCfgHandleAction :: a -> DbBotM SqlBackend ()
  }

instance Show SqlBackend where
  show _ = "<sqlbackend>"

-- The function to initialize and run it in IO.
runBot :: Show a => Token -> Text -> BotCfg a -> IO ()
runBot token connString c = runStderrLoggingT $ withSqliteConn connString $ liftIO . \conn -> do
  env <- defaultTelegramClientEnv token
  bot <- initBot conn c
  startBot_ (traceBotDefault bot) env

initBot :: SqlBackend -> BotCfg a -> IO (BotApp SqlBackend (Maybe a))
initBot conn c = do
  runSqlConn (runMigration $ botCfgMigration c) conn
  pure BotApp
    { botInitialModel = conn
    , botAction  = wrapUpdate $ botCfgHandleUpdate c
    , botHandler = wrapAction $ botCfgHandleAction c
    , botJobs         = []
    }
  where
    wrapUpdate f update _ = Just $ f update

    wrapAction f Nothing       conn = pure conn
    wrapAction f (Just action) conn = runDbBotM conn $ f action

-- helpers

runDbBotM :: SqlBackend -> DbBotM SqlBackend () -> Eff (Maybe a) SqlBackend
runDbBotM conn f = conn <# do
  runReaderT f conn
  pure Nothing

currentChatId :: DbBotM SqlBackend (Maybe ChatId)
currentChatId = lift $ TGS.currentChatId

replyText :: Text -> DbBotM SqlBackend ()
replyText = lift . TGS.replyText

runSql query = do
  conn <- ask
  lift $ liftIO $ runSqlConn query conn

getCurrentTime :: DbBotM SqlBackend UTCTime
getCurrentTime = lift $ liftIO $ Data.Time.getCurrentTime
