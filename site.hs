--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid
import           Hakyll
import           Data.List
import           System.FilePath

{- TODO

- no .html links, no links to index.html
- tags aka categories (buildTags)
- better layout
- link to other blogs
- link to soundcloud?
- CV?
- links to projects on github
- no home page, just list posts + pages
- updated date for articles
- next, previous and related articles links
- add photo of me
- add 404 page

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
main = hakyllWith config $ do
    match "github/*" $ do
        route $ gsubRoute "github/" (const "")
        compile copyFileCompiler

    match ("images/**" .||. "font/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.markdown" $ do
        route   $ noExtRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ composeRoutes noExtRoute $ customRoute $ \ident ->
            "p" ++ snd (break (=='/') $ toFilePath ident)
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
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
