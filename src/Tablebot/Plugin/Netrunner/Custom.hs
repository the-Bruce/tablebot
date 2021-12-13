-- |
-- Module      : Tablebot.Plugin.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The functionality of the custom card features of the Netrunner command.
module Tablebot.Plugin.Netrunner.Custom (customCard) where

import Prelude hiding (lookup)
import Data.Char (toLower)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack, unpack)
import Tablebot.Plugin.Fuzzy (FuzzyCosts (..), closestMatch, closestPairWithCosts, closestValue)
import Tablebot.Plugin.Netrunner.NrApi (NrApi (..))
import Tablebot.Plugin.Netrunner.Card as Card
import Tablebot.Plugin.Netrunner.Faction as Faction

-- | @NrData@ allows us to parse parameters into a type of our choosing.
data NrData = NrText Text | NrInt Int | NrBool Bool

-- | @nrToText@ attempts to get Text from a NrData.
nrToText :: NrData -> Maybe Text
nrToText (NrText t) = Just t
nrToText _ = Nothing

-- | @nrToText@ attempts to get an Int from a NrData.
nrToInt :: NrData -> Maybe Int
nrToInt (NrInt x) = Just x
nrToInt _ = Nothing

-- | @nrToText@ attempts to get a Bool from a NrData.
nrToBool :: NrData -> Maybe Bool
nrToBool (NrBool b) = Just b
nrToBool _ = Nothing

-- | @keys@ maps accepted card record keys to their parser functions.
keys :: [(String, Maybe String -> Maybe NrData)]
keys =
  [ ("advancement", fmap $ NrInt . (read :: String -> Int)),
    ("points"     , fmap $ NrInt . (read :: String -> Int)),
    ("link"       , fmap $ NrInt . (read :: String -> Int)),
    ("cost"       , fmap $ NrInt . (read :: String -> Int)),
    ("faction"    , fmap $ NrText . pack),
    ("influence"  , fmap $ NrInt . (read :: String -> Int)),
    ("flavour"    , fmap $ NrText . pack),
    ("maxInf"     , fmap $ NrInt . (read :: String -> Int)),
    ("keywords"   , fmap $ NrText . pack),
    ("minSize"    , fmap $ NrInt . (read :: String -> Int)),
    ("strength"   , fmap $ NrInt . (read :: String -> Int)),
    ("text"       , fmap $ NrText . pack),
    ("title"      , fmap $ NrText . pack),
    ("trash"      , fmap $ NrInt . (read :: String -> Int)),
    ("type"       , fmap $ NrText . pack),
    ("unique"     , fmap $ NrBool . fuzzyBool)
  ]

-- | @fuzzyBool@ maps a string to true or false (whichever is closest).
fuzzyBool :: String -> Bool
fuzzyBool = closestValue [("true", True), ("false", False)]

-- | @fuzzyType@ maps a string to the closest card type.
fuzzyType :: String -> String
fuzzyType = closestMatch ["agenda", "asset", "event", "hardware", "ice", "identity", "operation", "program", "resource", "upgrade"]

-- | @fuzzyFaction@ maps a string to the closest card faction by name.
fuzzyFaction :: NrApi -> String -> String
fuzzyFaction api query =
  let names = (unpack . Faction.name) <$> factions api
      codes = (unpack . Faction.code) <$> factions api
  in closestValue (zip names codes) query

-- | @customCard@ takes a set of parameters and does its best to turn that data
-- into a card.
customCard :: NrApi -> [(String, String)] -> Card
customCard api cardData =
  let params = fromList $ catMaybes $ readParam <$> cardData
  in Card
  { advancement_cost  = nrToInt =<< lookup "advancement" params,
    agenda_points     = nrToInt =<< lookup "points" params,
    base_link         = nrToInt =<< lookup "link" params,
    Card.code         = Nothing,
    cost              = nrToInt =<< lookup "cost" params,
    deck_limit        = Nothing,
    faction_code      = nrToText =<< lookup "faction" params,
    faction_cost      = nrToInt =<< lookup "influence" params,
    flavor            = nrToText =<< lookup "flavour" params,
    illustrator       = Nothing,
    influence_limit   = nrToInt =<< lookup "maxInf" params,
    keywords          = nrToText =<< lookup "keywords" params,
    minimum_deck_size = nrToInt =<< lookup "minSize" params,
    pack_code         = Nothing,
    position          = Nothing,
    quantity          = Nothing,
    Card.side_code    = Nothing,
    strength          = nrToInt =<< lookup "strength" params,
    stripped_text     = nrToText =<< lookup "text" params, -- TODO - strip this
    stripped_title    = nrToText =<< lookup "title" params, -- TODO - ditto
    text              = nrToText =<< lookup "text" params,
    title             = nrToText =<< lookup "title" params,
    trash_cost        = nrToInt =<< lookup "trash" params,
    type_code         = nrToText =<< lookup "type" params,
    uniqueness        = nrToBool =<< lookup "unique" params
  }
    where
      readParam :: (String, String) -> Maybe (String, NrData)
      readParam (k,v) =
        let (field, parser) = closestPairWithCosts costs keys k
        in case parser (Just v) of
          Just value -> Just (field, value)
          Nothing -> Nothing
      costs :: FuzzyCosts
      costs = FuzzyCosts
        { deletion = 10,
          insertion = 1,
          substitution = 10,
          transposition = 10
        }