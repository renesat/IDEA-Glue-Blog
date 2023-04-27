--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (liftA2)
import Control.Monad (filterM)
import qualified Data.ByteString.Lazy as LBS
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import KaTeX.KaTeXify (kaTeXifyIO)
import Language.JavaScript.Parser (readJs)
import Language.JavaScript.Pretty.Printer
    ( renderToString
    )
import Language.JavaScript.Process.Minify
    ( minifyJS
    )
import Pygments.Pygments (pygmentizeIO)
import System.Process
    ( StdStream (..)
    , createProcess
    , proc
    , std_err
    , std_out
    , use_process_jobs
    , waitForProcess
    )
import Text.Pandoc.Highlighting (pygments)
import Text.Pandoc.Options
    ( Extension (..)
    , HTMLMathMethod (KaTeX)
    , enableExtension
    , writerExtensions
    , writerHTMLMathMethod
    , writerHighlightStyle
    , writerTableOfContents
    )

import Configs
import Context

--------------------------------------------------------------------------------
-- Contexts

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith generatorConfig $ do
    configDep <- makePatternDependency "config.ini"
    blogConfig <- rulesExtraDependencies [configDep] $ do
        preprocess $ getConfig "site/config.ini"
    blogContext <- preprocess $ contextFromBlogConfig blogConfig

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag p -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots p "_content"
            years <- postsByYears posts
            let postContext = postCtx tags <> blogContext
                ctx = tagCtx tag years postContext <> blogContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    iconDep <- makePatternDependency "images/icon.svg"
    rulesExtraDependencies [iconDep] $ create ["favicon.ico"] $ do
        route idRoute
        compile $ faviconCompiler "site/images/icon.svg"

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match "css/*.sass" $ do
        route $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    match "css/fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route idRoute
        compile compressJsCompiler

    match (fromList ["about.org"]) $ do
        route $ setExtension "html"
        compile $
            blogPandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" blogContext
                >>= relativizeUrls

    match "archive.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "_content"
            years <- postsByYears posts
            let postContext = postCtx tags <> blogContext
                ctx = archiveCtx years postContext <> blogContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "_content"
            let postContext = postCtx tags <> blogContext
                ctx = indexCtx posts postContext <> blogContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "tags.html" $ do
        route idRoute
        compile $ do
            let ctx = tagsCtx tags <> blogContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $
            feedCompiler
                renderAtom
                (feedConfig blogConfig)
                (feedCtx tags <> blogContext)

    matchMetadata "posts/*" (\m -> lookupString "status" m == Just "published") $
        do
            route $ setExtension "html"
            let ctx = postCtx tags <> blogContext
            compile $
                blogPandocCompiler
                    >>= saveSnapshot "_content"
                    >>= loadAndApplyTemplate "templates/post.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

-- Get only post with status pablished
getPublished :: MonadMetadata m => [Item a] -> m [Item a]
getPublished = filterM publishedFilter

-- Filter for published posts
publishedFilter :: MonadMetadata m => Item a -> m Bool
publishedFilter item = do
    status <- getMetadataField (itemIdentifier item) "status" >>= pageExists
    return $ status == "published"
  where
    pageExists (Just v) = return v
    pageExists Nothing = return "none"

-- | Create a JavaScript compiler that minify content
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
    let minifier = renderToString . minifyJS . readJs . itemBody
    mJS <- minifier <$> getResourceString
    makeItem mJS

-- https://nickcharlton.net/posts/custom-pandoc-options-hakyll-4.html
blogPandocCompiler :: Compiler (Item String)
blogPandocCompiler =
    pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        writerOptions
        (\p -> pygmentizeIO =<< unsafeCompiler (kaTeXifyIO p))
  where
    customExtensions =
        [Ext_latex_macros, Ext_tex_math_dollars, Ext_tex_math_double_backslash]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    newExtensions = foldr enableExtension defaultExtensions customExtensions
    writerOptions =
        defaultHakyllWriterOptions
            { writerHighlightStyle = Just pygments
            , writerTableOfContents = True
            , writerExtensions = newExtensions
            , writerHTMLMathMethod = KaTeX ""
            }

-- Feed
-- https://robertwpearce.com/hakyll-pt-3-generating-rss-and-atom-xml-feeds.html
type FeedRenderer =
    FeedConfiguration ->
    Context String ->
    [Item String] ->
    Compiler (Item String)
feedCompiler ::
    FeedRenderer ->
    FeedConfiguration ->
    Context String ->
    Compiler (Item String)
feedCompiler renderer config ctx =
    renderer config ctx
        =<< fmap (take 10) . recentFirst
        =<< loadAllSnapshots
            "posts/*"
            "_content"

-- Favicon
createFaviconExportLine :: Int -> TmpFile -> String
createFaviconExportLine size (TmpFile path) =
    "export-width:"
        ++ show size
        ++ "px; export-filename:"
        ++ path
        ++ "; export-do;"

getFaviconPngs :: FilePath -> [Int] -> Compiler [TmpFile]
getFaviconPngs svgPath sizes = do
    tmpFiles <-
        sequence $
            map
                (\size -> newTmpFile $ "hakyll-blog-favicon-" ++ show size ++ ".png")
                sizes
    actions <-
        return $
            foldr1 (++) $
                map (uncurry createFaviconExportLine) $
                    zip
                        sizes
                        tmpFiles
    (_, _, _, handle) <-
        unsafeCompiler $
            createProcess
                (proc "inkscape" ["--without-gui", "--actions", actions, svgPath])
                    { std_err = NoStream
                    , std_out = NoStream
                    , use_process_jobs = True
                    }
    _ <- unsafeCompiler $ waitForProcess handle
    return tmpFiles

faviconCompiler :: FilePath -> Compiler (Item LBS.ByteString)
faviconCompiler svgPath =
    let sizes = [32, 16] :: [Int]
     in do
            _ <- unsafeCompiler $ putStrLn "  Compile favicon..."
            tmpFiles <- getFaviconPngs svgPath sizes
            let paths = map (\(TmpFile tmpPath) -> tmpPath) tmpFiles
            ico <- unixFilterLBS "convert" (paths ++ ["ico:-"]) ""
            makeItem ico

-- Get post grouped by years
postsByYears :: [Item a] -> Compiler [Item (String, [Item a])]
postsByYears posts = do
    groups <- groupByM isEqYear posts
    mapM createGroup groups
  where
    isEqYear a b = liftA2 (==) (getYear a) (getYear b)
    createGroup lst = do
        year <- getYear $ head lst
        makeItem (year, lst)

getYear :: MonadMetadata m => Item a -> m String
getYear item = do
    date <- getMetadataField (itemIdentifier item) "published"
    return $ maybe "Unknown" (take 4) date

-- Source: https://doisinkidney.com/posts/2018-02-11-monadic-list.functions.html
groupByM :: Applicative m => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM p xs =
    fmap
        snd
        (foldr f (const (pure ([], []))) xs (const (pure False)))
  where
    f x a q = liftA2 st (q x) (a (p x))
      where
        st b (ys, zs)
            | b = (x : ys, zs)
            | otherwise = ([], (x : ys) : zs)
