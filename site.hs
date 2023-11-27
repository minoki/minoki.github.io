{-# LANGUAGE OverloadedStrings #-}
import           Hakyll

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "Mizuki's Blog"
    , feedDescription = "Posts about functional programming languages"
    , feedAuthorName = "Arata Mizuki"
    , feedAuthorEmail = "aratamizuki@miz-ar.info"
    , feedRoot = "https://minoki.github.io"
    }

main :: IO ()
main = do
  hakyllWith config $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["about.md", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts) <>
              constField "title" "Archives"            <>
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderRss feedConfiguration feedCtx posts

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) <>
              constField "title" "Home"                <>
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultContext
