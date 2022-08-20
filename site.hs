--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Hakyll
import Data.List
import System.FilePath
import Text.Parsec
import Text.Parsec.Token
import Data.Either
import Control.Applicative
import GHC.IO.Encoding (setLocaleEncoding, utf8)

{- TODO

- irish recordings
- links to projects on github
- better home page
- updated date for articles
- next, previous and related articles links
- add photo of me

-}
--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration {
    inMemoryCache = True,
    ignoreFile = ignoreFile
  } where
  ignoreFile path
    | "#" `isPrefixOf` name = True
    | ".#" `isPrefixOf` name = True
    | "~" `isSuffixOf` name = True
    | ".swp" `isSuffixOf` name = True
    | ".tmp" `isSuffixOf` name = True
    | otherwise = False
    where name = takeFileName path

noExtRoute = customRoute $ (++"/index.html") . dropExtension . toFilePath

main :: IO ()
main = do
  setLocaleEncoding utf8
  hakyllWith config $ do
    match "root/*" $ do
        route $ gsubRoute "root/" (const "")
        compile copyFileCompiler

    match ("files/**" .||. "images/**" .||. "font/*") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "about.markdown" $ do
        route $ noExtRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ composeRoutes noExtRoute $ customRoute $ \ident ->
            "p" ++ snd (break (=='/') $ toFilePath ident)
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "404.html" $ do
      route idRoute
      compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderAtom feedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> field "url" (\item -> do
                       Just route <- getRoute $ itemIdentifier item
                       return $ if "/index.html" `isSuffixOf` route
                                then "/" ++ takeDirectory route
                                else route)
    <> defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "AtnNn.com"
    , feedDescription = "Articles"
    , feedAuthorName  = "Etienne Laurin"
    , feedAuthorEmail = ""
    , feedRoot        = "https://atnnn.com"
    }

