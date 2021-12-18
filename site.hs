--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.Monoid                   (mappend, mconcat)
import           Data.List                     (sortBy, intersperse, intercalate)
import           Data.Ord                      (comparing)
import           Hakyll
import           Control.Monad                 (liftM, forM_)
import           System.FilePath               (takeBaseName)
import           Text.Blaze.Html               (toHtml, toValue, (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Html.Renderer.String (renderHtml)


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "js/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "error/*" $ do
        route $ gsubRoute "error/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= applyAsTemplate siteCtx
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)

    match "pages/*" $ do
        route $ setExtension "html"
        compile $ do
            pageName <- takeBaseName . toFilePath <$> getUnderlying
            let pageCtx = constField pageName "" `mappend`
                          baseNodeCtx
            let evalCtx = functionField "get-meta" getMetadataKey `mappend`
                          functionField "eval" (evalCtxKey pageCtx)
            let activeSidebarCtx = sidebarCtx (evalCtx <> pageCtx)

            pandocCompiler
                >>= saveSnapshot "page-content"
                >>= loadAndApplyTemplate "templates/page.html"    siteCtx
                >>= loadAndApplyTemplate "templates/default.html" (activeSidebarCtx <> siteCtx)
                >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \ tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            let ctx = constField "title" title `mappend`
                      listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= relativizeUrls

    match "posts/*" $ version "meta" $ do
        route   $ setExtension "html"
        compile getResourceBody

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
            >>= relativizeUrls

    match "About.markdown" $ do
        route $ constRoute "index.html"
        compile $ do
            posts <- fmap (take 3) . recentFirst
                        =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    field "tags" (\_ -> renderAllTags tags)   `mappend`
                    constField "home" ""                     `mappend`
                    constField "title" "About"               `mappend`
                    siteCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/page.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"             `mappend`
                    constField "archive" ""                  `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> archiveCtx)
                >>= relativizeUrls

    paginate <- buildPaginateWith postsGrouper "posts/*" postsPageId

    paginateRules paginate $ \ page pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots (pat .&&. hasNoVersion) "content"
            let indexCtx =
                    constField "title" ("Blog posts, page " ++ show page) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "blog" "" `mappend`
                    paginateContext paginate page `mappend`
                    siteCtx

            makeItem ""
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/blog.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            renderAtom feedConfig feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            renderRss feedConfig feedCtx posts

--------------------------------------------------------------------------------

postsGrouper :: (MonadFail m, MonadMetadata m) => [Identifier] -> m [[Identifier]]
postsGrouper = fmap (paginateEvery 3) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ "blog/page" ++ show n ++ ".html"

--------------------------------------------------------------------------------

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "synthetic"
    , feedDescription = "A blog about higher category theory and life"
    , feedAuthorName  = "Dominic Verity"
    , feedAuthorEmail = "dominic.verity@mq.edu.au"
    , feedRoot        = "https://dom-verity.github.io"
    }

--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    baseCtx `mappend`
    constField "site-description" "Synthetic Perspectives" `mappend`
    constField "site-url" "https://dom-verity.github.io" `mappend`
    constField "tagline" "Life, the Universe, and Higher Categories" `mappend`
    constField "site-title" "Em/Prof. Dominic Verity" `mappend`
    constField "copy-year" "2021" `mappend`
    constField "site-author" "Dom Verity" `mappend`
    constField "site-email" "dominic.verity@mq.edu.au" `mappend`
    constField "github-repo" "https://github.com/dom-verity/dom-verity.github.io" `mappend`
    defaultContext

baseCtx =
    constField "baseurl" "https://dom-verity.github.io"
    -- constField "baseurl" "http://localhost:8000"

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = makeTagsField "tags" tags `mappend` postCtx

makeTagLink :: String -> FilePath -> H.Html
makeTagLink tag filePath =
    H.a ! A.title (H.stringValue ("All posts tagged '"++tag++"'."))
        ! A.href (toValue $ toUrl filePath)
        ! A.class_ "tag"
        $ toHtml tag

renderAllTags :: Tags -> Compiler String
renderAllTags = 
    return . mconcat . intersperse ", " . 
        fmap (\case (s, _) -> renderHtml $ makeTagLink s ("/tags/" ++ s ++ ".html")) .
        tagsMap

makeTagsField :: String -> Tags -> Context a
makeTagsField =
  tagsFieldWith getTags (fmap . makeTagLink) (mconcat . intersperse ", ")

--------------------------------------------------------------------------------
-- Function in this section generate a ranked list of "related" posts
-- This is currently deprecated in favour of tag based linking.

tagsRulesVersioned :: Tags -> (String -> [Identifier] -> Rules ()) -> Rules ()
tagsRulesVersioned tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            create [tagsMakeId tags tag] $
                rules tag identifiers

relatedPostsCtx :: [Item String]  -> Int  -> Context String
relatedPostsCtx posts n = listFieldWith "related_posts" postCtx selectPosts
  where
    rateItem ts i = length . filter (`elem` ts) <$> getTags (itemIdentifier i)
    selectPosts s = do
      postTags <- getTags $ itemIdentifier s
      let trimmedItems = filter (not . matchPath s) posts
      take n . reverse <$> sortOnM (rateItem postTags) trimmedItems

matchPath :: Item String -> Item String -> Bool
matchPath = eqOn (toFilePath . itemIdentifier)

eqOn :: Eq b => (a -> b) -> a -> a -> Bool
eqOn f x y = f x == f y

sortOnM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m [a]
sortOnM f xs = map fst . sortBy (comparing snd) . zip xs <$> mapM f xs

--------------------------------------------------------------------------------

sidebarCtx :: Context String -> Context String
sidebarCtx nodeCtx =
    listField "list_pages" nodeCtx (loadAllSnapshots ("pages/*" .&&. hasNoVersion) "page-content") `mappend`
    defaultContext

baseNodeCtx :: Context String
baseNodeCtx =
    urlField "node-url" `mappend`
    titleField "title" `mappend`
    baseCtx

baseSidebarCtx = sidebarCtx baseNodeCtx

evalCtxKey :: Context String -> [String] -> Item String -> Compiler String
evalCtxKey context [key] item =
    unContext context key [] item >>=
    \case
        StringField s -> return s
        _             -> error "Internal error: StringField expected"

getMetadataKey :: [String] -> Item String -> Compiler String
getMetadataKey [key] item = getMetadataField' (itemIdentifier item) key
