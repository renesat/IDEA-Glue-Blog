--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Hakyll
import           Language.JavaScript.Process.Minify (minifyJS)
import           Language.JavaScript.Parser (readJs)
import           Language.JavaScript.Pretty.Printer (renderToString)
import           Text.Pandoc.Options ( writerExtensions
                                     , writerHighlightStyle
                                     , writerHTMLMathMethod
                                     , writerTableOfContents
                                     , Extension(..)
                                     , enableExtension
                                     , HTMLMathMethod(KaTeX))
import           Text.Pandoc.Highlighting (pygments)
import           KaTeX.KaTeXify (kaTeXifyIO)
import           Pygments.Pygments (pygmentizeIO)
import           Hakyll.Web.Sass (sassCompiler)
import           Control.Monad (filterM)
import           System.Process (createProcess, proc, std_err, std_out, use_process_jobs, StdStream(..), waitForProcess)
import qualified Data.ByteString.Lazy as LBS
import           Control.Applicative (liftA2)

import           Configs
import           Context

--------------------------------------------------------------------------------
-- Contexts

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith generatorConfig $ do

  blogConfig <- preprocess $ getConfig "config.ini"
  blogContext <- preprocess $ contextFromBlogConfig blogConfig

  iconDep <- makePatternDependency "images/icon.svg"
  rulesExtraDependencies [iconDep] $ create ["favicon.ico"] $ do
      route   idRoute
      compile $ faviconCompiler "site/images/icon.svg"

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  match "css/*.sass" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "css/fonts/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "js/*" $ do
    route   idRoute
    compile compressJsCompiler

  match (fromList ["about.org"]) $ do
    route   $ setExtension "html"
    compile $ blogPandocCompiler
      >>= loadAndApplyTemplate "templates/default.html"
                               blogContext
      >>= relativizeUrls

  match "archive.html" $ do
    route   idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "_content"
      years <- postsByYears posts
      getResourceBody
        >>= applyAsTemplate (archiveCtx years blogContext)
        >>= loadAndApplyTemplate "templates/default.html"
                                 (archiveCtx years blogContext)
        >>= relativizeUrls

  match "index.html" $ do
    route   idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "_content"
      getResourceBody
        >>= applyAsTemplate (indexCtx posts blogContext)
        >>= loadAndApplyTemplate "templates/default.html"
                                 (indexCtx posts blogContext)
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ feedCompiler renderAtom
                           (feedConfig blogConfig)
                           (feedCtx blogContext)

  matchMetadata "posts/*" (\m -> lookupString "status" m == Just "published") $ do
    route $ setExtension "html"
    compile $ blogPandocCompiler
      >>= saveSnapshot "_content"
      >>= loadAndApplyTemplate "templates/post.html"
                               (postCtx blogContext)
      >>= loadAndApplyTemplate "templates/default.html"
                               (postCtx blogContext)
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
    pageExists Nothing  = return "none"

-- | Create a JavaScript compiler that minify content
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifier = renderToString . minifyJS . readJs . itemBody
  mJS <- return . minifier =<< getResourceString
  makeItem mJS

-- https://nickcharlton.net/posts/custom-pandoc-options-hakyll-4.html
blogPandocCompiler :: Compiler (Item String)
blogPandocCompiler =
    pandocCompilerWithTransformM defaultHakyllReaderOptions writerOptions
      -- (unsafeCompiler . kaTeXifyIO)
      (\p -> (unsafeCompiler $ kaTeXifyIO p) >>= pygmentizeIO)
    where
        customExtensions = [ Ext_latex_macros
                           , Ext_tex_math_dollars
                           , Ext_tex_math_double_backslash
                           ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr enableExtension defaultExtensions customExtensions
        writerOptions = defaultHakyllWriterOptions {
            writerHighlightStyle = Just pygments,
            writerTableOfContents = True,
            writerExtensions = newExtensions,
            writerHTMLMathMethod = KaTeX ""
        }

-- Feed
-- https://robertwpearce.com/hakyll-pt-3-generating-rss-and-atom-xml-feeds.html
type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)
feedCompiler :: FeedRenderer -> FeedConfiguration -> Context String -> Compiler (Item String)
feedCompiler renderer config ctx =
    renderer config ctx
        =<< fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*" "_content"


-- Favicon
createFaviconExportLine :: Int -> TmpFile -> String
createFaviconExportLine size (TmpFile path) =
  "export-width:" ++ show size ++ "px; export-filename:" ++ path ++ "; export-do;"

getFaviconPngs :: FilePath -> [Int] -> Compiler [TmpFile]
getFaviconPngs svgPath sizes = do
  tmpFiles <- sequence $ map
      (\size -> newTmpFile $ "hakyll-blog-favicon-" ++ show size ++ ".png")
      sizes
  actions <- return
    $ foldr1 (++)
    $ map (uncurry createFaviconExportLine)
    $ zip sizes tmpFiles
  (_, _, _, handle) <- unsafeCompiler $ createProcess
        (proc "inkscape" ["--without-gui", "--actions",  actions, svgPath])
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
    paths <- return $ map (\(TmpFile tmpPath) -> tmpPath) tmpFiles
    ico <- (unixFilterLBS "convert" (paths ++ ["ico:-"]) "")
    makeItem ico

-- Get post grouped by years
postsByYears :: [Item a] -> Compiler [Item (String, [Item a])]
postsByYears posts = do
  groups <- groupByM isEqYear posts
  mapM createGroup groups
  where
    isEqYear a b = liftA2 (==)
                          (getYear a)
                          (getYear b)
    createGroup lst = do
      year <- getYear $ head lst
      makeItem (year, posts)

getYear :: MonadMetadata m => Item a -> m String
getYear item = do
  date <- getMetadataField (itemIdentifier item) "date"
  return $ maybe "Unknown"
                 (take 4)
                 date

-- Source: https://doisinkidney.com/posts/2018-02-11-monadic-list.functions.html
groupByM :: Applicative m => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM p xs =
  fmap snd (foldr f (const (pure ([], []))) xs (const (pure (False))))
  where
    f x a q = liftA2 st (q x) (a (p x)) where
      st b (ys,zs)
        | b = (x : ys, zs)
        | otherwise = ([], (x:ys):zs)
