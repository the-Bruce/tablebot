{-# LANGUAGE OverloadedStrings #-}
module Tablebot.Plugins.Test (testPlugin) where

import Tablebot.Plugin
import Discord.Internal.Rest (Message)
import Tablebot.Plugin.Discord (sendMessage)
import Control.Monad (void)
import Data.Text.Internal (Text)
import qualified Text.Show as T
import Data.Semigroup ((<>))

-- | @pong@ is a command that takes no arguments (using 'noArguments') and
-- replies with "ping". It is the younger sibling of @ping@.

testSpec :: ArgSpec
testSpec = IntArg :/: IntArg :/: Multiple StringArg

test :: SmartCommand
test = SmartCommand "test" testEffect [] testSpec
  where
    testEffect :: [SmartValue] -> Message -> DatabaseDiscord ()
    testEffect [SInt a, SInt b, lst] m = void $ sendMessage m ("testing: "<>(T.show a)<>" "<>(T.show b)<>(T.show lst))
      
-- | @pingPlugin@ assembles these commands into a plugin containing both ping
-- and pong.
testPlugin :: Plugin
testPlugin = plug { commands = [] }