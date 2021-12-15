-- |
-- Module      : Tablebot.Plugin.Discord
-- Description : Discord helpers for building plugins.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains helpful Discord functionality for building plugins
-- without having to lift Discord operations constantly.
module Tablebot.Plugin.Discord
  ( sendMessage,
    sendChannelMessage,
    sendReplyMessage,
    sendCustomReplyMessage,
    sendEmbedMessage,
    reactToMessage,
    findGuild,
    getMessage,
    getMessageMember,
    getReplyMessage,
    getPrecedingMessage,
    toMention,
    toMention',
    toMentionStr,
    toMentionStr',
    toTimestamp,
    toTimestamp',
    toRelativeTime,
    getMessageLink,
    Message,
    Format (..),
    formatText,
    formatInput,
    TimeFormat,
  )
where

import Control.Monad.Exception
import Data.Maybe (listToMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Discord (RestCallErrorCode, restCall)
import qualified Discord.Requests as R
import Discord.Types
import Tablebot.Handler.Embed
import Tablebot.Plugin (DatabaseDiscord, EnvDatabaseDiscord, liftDiscord)
import Tablebot.Plugin.Exception (BotException (..))

-- | @sendMessage@ sends the input message @t@ in the same channel as message
-- @m@. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
sendMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s ()
sendMessage m t = do
  res <- liftDiscord . restCall $ R.CreateMessage (messageChannel m) t
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendChannelMessage@ sends the input message @t@ into the provided channel
-- @m@. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
sendChannelMessage ::
  ChannelId ->
  Text ->
  EnvDatabaseDiscord s ()
sendChannelMessage c t = do
  res <- liftDiscord . restCall $ R.CreateMessage c t
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendReplyMessage@ sends the input message @t@ as a reply to the triggering message
-- @m@. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
sendReplyMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s ()
sendReplyMessage m t = do
  let ref = MessageReference (Just (messageId m)) Nothing Nothing False
  res <- liftDiscord . restCall $ R.CreateMessageDetailed (messageChannel m) (R.MessageDetailedOpts t False Nothing Nothing Nothing (Just ref))
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendCustomReplyMessage@ sends the input message @t@ as a reply to a provided message id
-- @m@. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
-- @fail'@ indicates whether the message should still send if the provided message id is invalid
sendCustomReplyMessage ::
  Message ->
  MessageId ->
  Bool ->
  Text ->
  EnvDatabaseDiscord s ()
sendCustomReplyMessage m mid fail' t = do
  let ref = MessageReference (Just mid) Nothing Nothing fail'
  res <- liftDiscord . restCall $ R.CreateMessageDetailed (messageChannel m) (R.MessageDetailedOpts t False Nothing Nothing Nothing (Just ref))
  case res of
    Left _ -> throw $ MessageSendException "Failed to send message."
    Right _ -> return ()

-- | @sendEmbedMessage@ sends the input message @t@ in the same channel as message
-- @m@ with an additional full Embed. This returns an @Either RestCallErrorCode Message@ to denote failure or
-- return the 'Message' that was just sent.
-- This is *really* janky. The library exposes *no way* to create a coloured embed through its main api,
-- so I'm having to manually reimplement the sending logic just to add this in.
-- If you suffer from nightmares, don't look in 'Tablebot.Handler.Embed'. Nothing good lives there.
-- In the future, I may actually submit a PR to discord-haskell with a fix to allow colours properly.
sendEmbedMessage ::
  Embeddable e =>
  Message ->
  Text ->
  e ->
  EnvDatabaseDiscord s ()
sendEmbedMessage m t e = do
  res <- liftDiscord . restCall $ TablebotEmbedRequest (messageChannel m) t (asEmbed e)
  case res of
    Left _ -> throw $ MessageSendException "Failed to send embed message."
    Right _ -> return ()

-- | @getChannel@ gets the relevant Channel object for a given 'ChannelId'
-- and 'MessageId', or returns an error ('RestCallErrorCode').
getChannel ::
  ChannelId ->
  EnvDatabaseDiscord s (Either RestCallErrorCode Channel)
getChannel cid = liftDiscord . restCall $ R.GetChannel cid

-- | @getMessage@ gets the relevant 'Message' object for a given 'ChannelId'
-- and 'MessageId', or returns an error ('RestCallErrorCode').
getMessage ::
  ChannelId ->
  MessageId ->
  EnvDatabaseDiscord s (Either RestCallErrorCode Message)
getMessage cid mid = liftDiscord . restCall $ R.GetChannelMessage (cid, mid)

-- | @reactToMessage@ reacts to the given message with the emoji specified
-- by the text input (see README.md from discord-haskell). Returns @()@ if
-- successful, else the error ('RestCallErrorCode').
reactToMessage ::
  Message ->
  Text ->
  EnvDatabaseDiscord s (Either RestCallErrorCode ())
reactToMessage m e =
  liftDiscord . restCall $
    R.CreateReaction (messageChannel m, messageId m) e

-- | @getReplyMessage@ returns the message being replied to (if applicable)
getReplyMessage :: Message -> EnvDatabaseDiscord s (Maybe Message)
getReplyMessage m = do
  let m' = referencedMessage m
  let mRef = messageReference m
  case m' of
    Just msg -> return $ Just msg
    Nothing -> case mRef of
      Nothing -> return Nothing
      Just mRef' -> maybeGetMessage (referenceChannelId mRef') (referenceMessageId mRef')
  where
    maybeGetMessage :: Maybe ChannelId -> Maybe MessageId -> EnvDatabaseDiscord s (Maybe Message)
    maybeGetMessage (Just cId) (Just mId) = do
      m' <- getMessage cId mId
      case m' of
        Left _ -> return Nothing
        Right msg -> return $ Just msg
    maybeGetMessage _ _ = return Nothing

-- | @getPrecedingMessage@ returns the message immediately above the provided message
getPrecedingMessage :: Message -> EnvDatabaseDiscord s (Maybe Message)
getPrecedingMessage m = do
  mlst <- liftDiscord . restCall $ R.GetChannelMessages (messageChannel m) (1, R.BeforeMessage (messageId m))
  case mlst of
    Right mlst' ->
      return $ listToMaybe mlst'
    Left _ -> return Nothing

-- | @getMessageMember@ returns the message member object if it was sent from a Discord server,
-- or @Nothing@ if it was sent from a DM (or the API fails)
getMessageMember :: Message -> EnvDatabaseDiscord s (Maybe GuildMember)
getMessageMember m = gMM (messageGuild m) m
  where
    maybeRight :: Either a b -> Maybe b
    maybeRight (Left _) = Nothing
    maybeRight (Right a) = Just a
    gMM :: Maybe GuildId -> Message -> EnvDatabaseDiscord s (Maybe GuildMember)
    gMM Nothing _ = return Nothing
    gMM (Just g') m' = do
      a <- liftDiscord $ restCall $ R.GetGuildMember g' (userId $ messageAuthor m')
      return $ maybeRight a

findGuild :: Message -> DatabaseDiscord (Maybe GuildId)
findGuild m = case messageGuild m of
  Just a -> pure $ Just a
  Nothing -> do
    let chanId = messageChannel m
    channel <- getChannel chanId
    case fmap channelGuild channel of
      Right a -> pure $ Just a
      Left _ -> pure Nothing

-- | @toMention@ converts a user to its corresponding mention
toMention :: User -> Text
toMention = pack . toMentionStr

-- | @toMention'@ converts a user ID to its corresponding mention
toMention' :: UserId -> Text
toMention' = pack . toMentionStr'

-- | @toMentionStr@ converts a user to its corresponding mention, returning a string to prevent packing and unpacking
toMentionStr :: User -> String
toMentionStr = toMentionStr' . userId

toMentionStr' :: UserId -> String
toMentionStr' u = "<@!" ++ show u ++ ">"

data TimeFormat = Default | ShortTime | LongTime | ShortDate | LongDate | ShortDateTime | LongDateTime | Relative deriving (Show, Enum, Eq)

toTimestamp' :: TimeFormat -> UTCTime -> Text
toTimestamp' format t = "<t:" <> pack (show $ toUtcSeconds t) <> toSuffix format <> ">"
  where
    toUtcSeconds :: UTCTime -> Integer
    toUtcSeconds = truncate . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    toSuffix :: TimeFormat -> Text
    toSuffix Default = ""
    toSuffix ShortTime = ":t"
    toSuffix LongTime = ":T"
    toSuffix ShortDate = ":d"
    toSuffix LongDate = ":D"
    toSuffix ShortDateTime = ":f"
    toSuffix LongDateTime = ":F"
    toSuffix Relative = ":R"

toTimestamp :: UTCTime -> Text
toTimestamp = toTimestamp' Default

toRelativeTime :: UTCTime -> Text
toRelativeTime = toTimestamp' Relative

getMessageLink :: GuildId -> ChannelId -> MessageId -> Text
getMessageLink g c m = pack $ "https://discord.com/channels/" ++ show g ++ "/" ++ show c ++ "/" ++ show m

data Format = Bold | Underline | Strikethrough | Italics | Code | CodeBlock
  deriving (Show, Eq)

formatInput :: (IsString a, Show b, Semigroup a) => Format -> b -> a
formatInput f b = formatText f (fromString $ show b)

formatText :: (IsString a, Semigroup a) => Format -> a -> a
formatText Bold s = "**" <> s <> "**"
formatText Underline s = "__" <> s <> "__"
formatText Strikethrough s = "~~" <> s <> "~~"
formatText Italics s = "*" <> s <> "*"
formatText Code s = "`" <> s <> "`"
formatText CodeBlock s = "```" <> s <> "```"
