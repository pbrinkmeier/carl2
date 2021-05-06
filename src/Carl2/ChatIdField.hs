module Carl2.ChatIdField where

import Control.Arrow
import Data.Text as T
import Database.Persist.Sql
import Telegram.Bot.API
import Text.Read

instance PersistField ChatId where
  toPersistValue   (ChatId id) = PersistText $ T.pack $ show id
  fromPersistValue (PersistText text) = do
    id <- left T.pack $ readEither $ T.unpack text
    pure $ ChatId id

instance PersistFieldSql ChatId where
  sqlType _ = SqlString
