{-# LANGUAGE BangPatterns #-}

{-|
Module      : Tablebot
Description : The main runner for the Tablebot Discord bot.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the main runner for Tablebot. If you're just looking to
run the bot with existing plugins, importing this and your favourite plugins
from "Tablebot.Plugins".
-}
module Tablebot (
    runTablebot
) where

import Tablebot.Handler (eventHandler, killCron, runCron)
import Tablebot.Plugin (Plugin, combinePlugins, migrations, cronJobs)
import Tablebot.Plugin.Help

import Data.Text (Text, pack)
import Discord
import qualified Data.Text.IO as TIO (putStrLn)
import Database.Persist.Sqlite
    (runMigration, runSqlPool, createSqlitePool) 
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent
    (ThreadId, MVar, newEmptyMVar, putMVar, takeMVar)

-- | runTablebot @dToken@ @prefix@ @dbpath@ @plugins@ runs the bot using the
-- given Discord API token @dToken@ and SQLite connection string @dbpath@. Only
-- the plugins provided by @plugins@ are run, and all commands are prefixed
-- with @prefix@.
-- The plugins given are combined into a single plugin with their combined
-- functionality. Each migration present in the combined plugin is run, and
-- each cron job and handler is set up.
-- This creates a small pool of database connections used by the event handler,
-- builds an event handler and starts cron jobs. It also kills the cron jobs on
-- bot close.
runTablebot :: Text -> Text -> FilePath -> [Plugin] -> IO ()
runTablebot dToken prefix dbpath plugins =
    let !plugin = generateHelp $ combinePlugins plugins
    in do
    -- Create multiple database threads.
    pool <- runNoLoggingT $ createSqlitePool (pack dbpath) 8
    -- TODO: this might have issues with duplicates?
    -- TODO: in production, this should probably run once and then never again.
    mapM_ (\migration -> runSqlPool (runMigration migration) pool) $ migrations plugin
    -- Create a var to kill any ongoing tasks.
    mvar <- newEmptyMVar :: IO (MVar [ThreadId])
    userFacingError <- runDiscord $ def {
        discordToken = dToken,
        discordOnEvent =
            flip runSqlPool pool . eventHandler plugin prefix,
        discordOnStart =
            -- Build list of cron jobs, saving them to the mvar.
            runSq-lPool (mapM runCron (cronJobs plugin) >>= liftIO . putMVar mvar) pool,
        -- Kill every cron job in the mvar.
        discordOnEnd = takeMVar mvar >>= killCron
    }
    TIO.putStrLn userFacingError
