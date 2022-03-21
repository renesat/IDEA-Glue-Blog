{-# LANGUAGE OverloadedStrings #-}

-- | Configs functions

module Configs where

import           Data.HashMap.Strict            ( (!)
                                                , HashMap
                                                , fromList
                                                )
import           Data.Ini                       ( Ini(..)
                                                , readIniFile
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Hakyll                         ( Configuration(..)
                                                , FeedConfiguration(..)
                                                , defaultConfiguration
                                                )

-- Blog config
type BlogConfig = (HashMap Text Text)

getConfig :: String -> IO BlogConfig
getConfig file = do
  configEither <- readIniFile file
  case configEither of
    Left  errs       -> error errs
    Right blogConfig -> return $ fromList $ iniGlobals blogConfig


-- Generator Config
-- Base on https://github.com/ysndr/blog/blob/release/generator/Main.hs
generatorConfig :: Configuration
generatorConfig = defaultConfiguration { destinationDirectory = "build/site"
                                       , storeDirectory       = "build/_store"
                                       , tmpDirectory         = "build/_tmp"
                                       , providerDirectory    = "site"
                                       }


-- Feed Config
feedConfig :: BlogConfig -> FeedConfiguration
feedConfig blogConfig = FeedConfiguration
  { feedTitle       = unpack $ blogConfig ! "name"
  , feedDescription = unpack $ blogConfig ! "description"
  , feedAuthorName  = unpack $ blogConfig ! "author"
  , feedAuthorEmail = unpack $ blogConfig ! "email"
  , feedRoot        = unpack $ blogConfig ! "link"
  }
