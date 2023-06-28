{-# LANGUAGE OverloadedStrings #-}

module Pygments.Pygments
    ( pygmentizeIO
    , pygmentizeStyleCompiler
    )
where

import Data.Char (toLower)
import qualified Data.Text as T
import Hakyll
    ( Compiler
    , unixFilter
    )
import Text.Pandoc.Definition
    ( Block (..)
    , Format (..)
    , Pandoc
    )
import Text.Pandoc.Walk (walkM)

pygmentizeIO :: Pandoc -> Compiler Pandoc
pygmentizeIO = walkM pygmentize

pygmentize :: Block -> Compiler Block
pygmentize (CodeBlock (_, options, _) code) = do
    htmlCode <- pygmentizeToHTML code options
    return $ RawBlock (Format "html") htmlCode
pygmentize x = return x

pygmentizeToHTML :: T.Text -> [T.Text] -> Compiler T.Text
pygmentizeToHTML code options
    | not (null options) =
        T.pack
            <$> unixFilter
                "pygmentize"
                ["-l", map toLower (T.unpack (head options)), "-f", "html"]
                (T.unpack code)
    | otherwise =
        return $
            T.pack $
                "<div class =\"highlight\"><pre>"
                    ++ T.unpack code
                    ++ "</pre></div>"

pygmentizeStyleCompiler :: String -> Compiler String
pygmentizeStyleCompiler style =
    unixFilter
        "pygmentize"
        ["-S", style, "-f", "html", "-a", ".highlight"]
        ""
