{-# LANGUAGE OverloadedStrings #-}
module Pygments.Pygments (pygmentizeIO) where

import           Hakyll (unixFilter, Compiler(..))
import           Text.Pandoc.Definition (Block(..), Inline(..), Pandoc, Format(..))
import           Text.Pandoc.Walk (walkM)
import           Data.Char(toLower)
import qualified Data.Text as T

pygmentizeIO :: Pandoc -> Compiler Pandoc
pygmentizeIO = walkM pygmentize

pygmentize :: Block -> Compiler Block
pygmentize (CodeBlock (_, options , _ ) code) = do
  htmlCode <- pygmentizeToHTML code options
  return $ RawBlock (Format "html") htmlCode
pygmentize x = return x

pygmentizeToHTML :: T.Text -> [T.Text] -> Compiler T.Text
pygmentizeToHTML code options
         | (length options) >= 1 = return . T.pack =<< unixFilter "pygmentize" ["-l", (map toLower (T.unpack (head options))),  "-f", "html"] (T.unpack code)
         | otherwise = return $ T.pack $ "<div class =\"highlight\"><pre>" ++ T.unpack code ++ "</pre></div>"
