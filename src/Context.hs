-- | Contecst functions

module Context where

import           Hakyll
import           Data.Text (unpack, Text)
import           Configs ( BlogConfig)
import           Data.HashMap.Strict (toList)

------------------------------------------
-- Common contexts

-- Blog context
contextFromBlogConfig :: BlogConfig -> IO (Context String)
contextFromBlogConfig blogConfig = return $
  (foldl1 mappend $ map addField (toList blogConfig)) <>
  activeClassField <>
  defaultContext
  where
    addField :: (Text, Text) -> Context String
    addField (name, value) = constField ("blog-" ++ (unpack name))
                                        (unpack value)

-- Base on https://groups.google.com/forum/#!searchin/hakyll/if$20class/hakyll/WGDYRa3Xg-w/nMJZ4KT8OZUJ
activeClassField :: Context a
activeClassField = functionField "activeClass" $ \[p] item -> do
  page <- getMetadataField (itemIdentifier item) "page-id" >>= pageExists
  return $ if page == p then "active" else page
  where
    pageExists (Just v) = return v
    pageExists Nothing  = return "none"



------------------------------------------
-- Pages contexts

-- Post page
postCtx :: Context String -> Context String
postCtx context =
    dateField "date" "%B %e, %Y" <>
    mathCtx <>
    context

-- Archive page
archiveCtx :: [Item (String, [Item String])] -> Context String -> Context String
archiveCtx years context =
    listField "years" (  field "year" (return . fst . itemBody)
                      <> listFieldWith "posts" (postCtx context) (return . snd . itemBody)
                      )
                      (return years) <>
    context

-- Index page
indexCtx :: [Item String] -> Context String -> Context String
indexCtx posts context =
    mathCtx <>
    listField "posts" (teaserField "teaser" "_content" <> postCtx context) (return posts) <>
    context

------------------------------------------
-- Support contexts

-- Math
-- https://axiomatic.neophilus.net/using-katex-with-hakyll/
mathCtx :: Context a
mathCtx = field "katex-header" $ \item -> do
  katex <- getMetadataField (itemIdentifier item) "katex"
  return $ case katex of
    Just "false" -> ""
    Just "off" -> ""
    _ -> unlines [ "<link rel='stylesheet' href='/css/katex.min.css'>"
                 , "<script type='text/javascript' src='/js/katex.min.js'></script>"
                 , "<script src='/js/auto-render.min.js'></script>"
                 ]

------------------------------------------
-- Feed functions

-- Feed context
feedCtx :: Context String -> Context String
feedCtx context =
  (postCtx context) <>
  bodyField "description"
