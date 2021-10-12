-- |
-- Module      : Tablebot.Handler.Permission
-- Description : Some internal code for handling permissions
-- Copyright   : (c) Anna Bruce 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This contains some functions to extract and handle privileged commands
module Tablebot.Handler.Permission where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust)
import Discord (RestCallErrorCode)
import Discord.Types (GuildMember, Message, RoleId, memberRoles)
import System.Environment (lookupEnv)
import Tablebot.Plugin.Discord (getMessageMember, sendMessageVoid)
import Tablebot.Plugin.Types

data KnownRoles = KnownRoles
  { krExec :: Maybe RoleId,
    krModerator :: Maybe RoleId,
    krSuperuser :: Maybe RoleId
  }

userHasPermission :: RequiredPermission -> UserPermission -> Bool
userHasPermission _ (UserPerm _ _ True) = True -- Superuser always has perm
userHasPermission None _ = True
userHasPermission Any (UserPerm exec moderator _) = exec || moderator
userHasPermission Exec (UserPerm exec _ _) = exec
userHasPermission Moderator (UserPerm _ moderator _) = moderator
userHasPermission Both (UserPerm exec moderator _) = exec && moderator
userHasPermission _ _ = False

getKnownRoles :: IO KnownRoles
getKnownRoles = do
  exec <- lookupEnv "EXEC_GROUP"
  moderator <- lookupEnv "MODERATOR_GROUP"
  superuser <- lookupEnv "SUPERUSER_GROUP"
  return $ KnownRoles (maybeRead exec) (maybeRead moderator) (maybeRead superuser)
  where
    maybeRead (Just a) = read a
    maybeRead Nothing = Nothing

getMemberGroups :: Maybe GuildMember -> [RoleId]
getMemberGroups (Just gm) = memberRoles gm
getMemberGroups Nothing = []

permsFromGroups :: KnownRoles -> [RoleId] -> UserPermission
permsFromGroups krls gps =
  UserPerm
    (krExec krls `elemish` gps)
    (krModerator krls `elemish` gps)
    (krSuperuser krls `elemish` gps)
  where
    elemish (Just a) b = a `elem` b
    elemish Nothing _ = True

getSenderPermission :: Message -> DatabaseDiscord UserPermission
getSenderPermission m = do
  member <- getMessageMember m
  knownroles <- liftIO getKnownRoles
  return $ permsFromGroups knownroles $ getMemberGroups member