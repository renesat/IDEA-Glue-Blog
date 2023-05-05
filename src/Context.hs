{-# LANGUAGE OverloadedStrings #-}

-- | Contecst functions
module Context where

import Configs (BlogConfig)
import Data.HashMap.Strict (toList)
import Data.Text
    ( Text
    , unpack
    )
import Hakyll
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

------------------------------------------
-- Common contexts

-- Blog context
contextFromBlogConfig :: BlogConfig -> IO (Context String)
contextFromBlogConfig blogConfig =
    return $
        foldl1 mappend (map addField (toList blogConfig))
            <> activeClassField
            <> defaultContext
  where
    addField :: (Text, Text) -> Context String
    addField (name, value) = constField ("blog-" ++ unpack name) (unpack value)

-- Base on https://groups.google.com/forum/#!searchin/hakyll/if$20class/hakyll/WGDYRa3Xg-w/nMJZ4KT8OZUJ
activeClassField :: Context a
activeClassField = functionField "activeClass" $ checkActiveClass
  where
    checkActiveClass [p] item = do
        page <- getMetadataField (itemIdentifier item) "page-id" >>= pageExists
        return $ if page == p then "active" else page
    checkActiveClass _ _ = fail "activeClass need only one parameter of page class!"

    pageExists (Just v) = return v
    pageExists Nothing = return "none"

------------------------------------------
-- Pages contexts

-- Post page
postCtx :: Tags -> Context String
postCtx tags = tagsField "tags" tags <> dateField "date" "%B %e, %Y" <> mathCtx

-- Archive page
archiveCtx ::
    [Item (String, [Item String])] -> Context String -> Context String
archiveCtx years postctx =
    listField
        "years"
        ( field "year" (return . fst . itemBody)
            <> listFieldWith "posts" postctx (return . snd . itemBody)
        )
        (return years)

-- Index page
indexCtx :: [Item String] -> Context String -> Context String
indexCtx posts postContext =
    listField
        "posts"
        (teaserField "teaser" "_content" <> postContext)
        (return posts)

-- Tag page
tagCtx :: String -> [Item (String, [Item String])] -> Context String -> Context String
tagCtx tag years postContext =
    constField "title" tag
        <> listField
            "years"
            ( field "year" (return . fst . itemBody)
                <> listFieldWith "posts" (teaserField "teaser" "_content" <> postContext) (return . snd . itemBody)
            )
            (return years)

-- Tags page
tagsCtx :: Tags -> Context String
tagsCtx tags =
    field
        "tags"
        ( \_ ->
            renderTags
                (\tag url count _ _ -> renderHtml $ H.li $ H.a ! A.href (toValue url) $ toHtml $ tag ++ " (" ++ show count ++ ")")
                concat
                tags
        )

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
        _ ->
            unlines
                [ "<link rel='stylesheet' href='/css/katex.min.css'>"
                , "<script type='text/javascript' src='/js/katex.min.js'></script>"
                , "<script src='/js/auto-render.min.js'></script>"
                ]

------------------------------------------
-- Feed functions

-- Feed context
feedCtx :: Tags -> Context String
feedCtx tags = postCtx tags <> bodyField "description"
