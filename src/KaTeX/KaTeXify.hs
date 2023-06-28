-- Source: https://ifazk.com/blog/2018-11-20-JavaScript-free-Hakyll-site.html
module KaTeX.KaTeXify
    ( kaTeXifyIO
    )
where

import qualified Data.Text as T
import Data.Text.Conversions (convertText)
import System.Process
    ( readCreateProcess
    , shell
    )
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
    ( Format (..)
    , Inline (..)
    , MathType (..)
    , Pandoc
    )
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Walk (walkM)

--------------------------------------------------------------------------------
kaTeXCmd :: MathType -> String
kaTeXCmd DisplayMath = "katex --display-mode"
kaTeXCmd _ = "katex"

rawKaTeX :: MathType -> T.Text -> IO String
rawKaTeX mt inner = readCreateProcess (shell $ kaTeXCmd mt) (T.unpack inner)

parseKaTeX :: String -> Maybe Inline
parseKaTeX str =
    -- Ensure str is parsable HTML
    case runPure $ readHtml def (convertText str :: T.Text) of
        Right _ -> Just (RawInline (Format (T.pack "html")) (T.pack str))
        _ -> Nothing

kaTeXify :: Inline -> IO Inline
kaTeXify orig@(Math mt str) = do
    s <- parseKaTeX <$> rawKaTeX mt str
    case s of
        Just inl -> return inl
        Nothing -> return orig
kaTeXify x = return x

--------------------------------------------------------------------------------
kaTeXifyIO :: Pandoc -> IO Pandoc
kaTeXifyIO = walkM kaTeXify
