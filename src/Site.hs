--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Data.Monoid                        (mappend, mconcat)
import           Data.List                          (sortBy, intersperse, intercalate)
import           Data.Ord                           (comparing)
import           Hakyll
import           Control.Monad                      (liftM, forM_)
import           System.FilePath                    (takeBaseName, (<.>), takeFileName, replaceExtension)
import           Text.Blaze.Html                    (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.String    (renderHtml)

import           Compilers
--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "js/*" .||. "fonts/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "pdfs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "error/*" $ do
        route $ gsubRoute "error/" (const "") `composeRoutes` setExtension "html"
        compile $ compileToPandocAST 
            >>= renderPandocASTtoHTML
            >>= applyAsTemplate siteCtx
            >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
    
    match "pandoc/*.bib" $ 
        compile biblioCompiler

    match "pandoc/elsevier.csl" $
        compile cslCompiler

    match "pandoc/*.yaml" $
        compile metadataCompiler

    match "pages/*" $ version "ast" $
        compile compileToPandocAST

    match "pages/*" $ do
        route $ setExtension "html"
        compile $ do
            pageName <- takeBaseName . toFilePath <$> getUnderlying
            let pageCtx = constField pageName "" <>
                        baseNodeCtx
            let evalCtx = functionField "get-meta" getMetadataKey <>
                        functionField "eval" (evalCtxKey pageCtx)
            let activeSidebarCtx = sidebarCtx (evalCtx <> pageCtx)

            getUnderlying
                >>= loadBody . setVersion (Just "ast")
                >>= makeItem
                >>= renderPandocASTtoHTML
                >>= saveSnapshot "page-content"
                >>= loadAndApplyTemplate "templates/page.html"    siteCtx
                >>= loadAndApplyTemplate "templates/default.html" (activeSidebarCtx <> siteCtx)
                >>= relativizeUrls

    -- Temporarily disabled PDF generation for CV.md due to LaTeX compilation issues
    -- match "pages/CV.md" $ version "pdf" $ do
    --     route $ setExtension "pdf"
    --     compile $ getUnderlying 
    --         >>= loadBody . setVersion (Just "ast")
    --         >>= makeItem
    --         >>= renderPandocASTtoLaTeX
    --         >>= loadAndApplyTemplate "templates/CV.tex"
    --                                  (siteCtx <> modificationTimeField "modified" "%B %e, %Y")
    --         >>= buildLaTeX

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \ tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            let ctx = constField "title" title <>
                      listField "posts" (postCtxWithTags tags) (return posts) <>
                      defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= relativizeUrls

    match "posts/*" $ version "ast" $
        compile compileToPandocAST

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            pdfFileName <- flip replaceExtension "pdf" . takeFileName . toFilePath <$> getUnderlying
            getUnderlying
                >>= loadBody . setVersion (Just "ast")
                >>= makeItem
                >>= renderPandocASTtoHTML
                >>= \post -> do
                        teaser <- makeTeaser post
                        saveSnapshot "teaser" teaser
                        saveSnapshot "content" post
                >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags <>
                                                                    constField "pdf-filename" pdfFileName)
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= relativizeUrls

    -- Temporarily disabled PDF generation for posts due to LaTeX compilation issues
    -- match "posts/*" $ version "pdf" $ do
    --     route $ setExtension "pdf"
    --     compile $ getUnderlying 
    --         >>= loadBody . setVersion (Just "ast")
    --         >>= makeItem
    --         >>= renderPandocASTtoPDF

    -- Talks rules (similar to posts)
    match "talks/*" $ version "ast" $
        compile compileToPandocAST

    match "talks/*" $ do
        route $ setExtension "html"
        compile $ do
            getUnderlying
                >>= loadBody . setVersion (Just "ast")
                >>= makeItem
                >>= renderPandocASTtoHTML
                >>= loadAndApplyTemplate "templates/talk.html" (talkCtx <> siteCtx)
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> siteCtx)
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst
                        =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"

            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    field "tags" (\_ -> renderAllTags tags)   <>
                    constField "home" ""                     <>
                    constField "title" "About"               <>
                    siteCtx

            body <- loadSnapshotBody "pages/About.md" "page-content"

            makeItem body
                >>= makeTeaser
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/page.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> indexCtx)
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archive"             <>
                    constField "archive" ""                  <>
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (baseSidebarCtx <> archiveCtx)
                >>= relativizeUrls

    paginate <- buildPaginateWith postsGrouper ("posts/*" .&&. hasNoVersion) postsPageId

    paginateRules paginate $ \ page pat -> do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAllSnapshots (pat .&&. hasNoVersion) "teaser"
            let indexCtx =
                    constField "title" ("Blog posts, page " ++ show page) <>
                    listField "posts" postCtx posts                       <>
                    constField "blog" ""                                  <>
                    paginateContext paginate page                         <>
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
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("posts/*" .&&. hasNoVersion) "teaser"
            renderAtom feedConfig feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("posts/*" .&&. hasNoVersion) "teaser"
            renderRss feedConfig feedCtx posts

--------------------------------------------------------------------------------

postsGrouper :: (MonadFail m, MonadMetadata m) => [Identifier] -> m [[Identifier]]
postsGrouper = fmap (paginateEvery 5) . sortRecentFirst

postsPageId :: PageNumber -> Identifier
postsPageId n = fromFilePath $ "blog/page" ++ show n ++ ".html"

makeTeaserWithSeparator :: String -> Item String -> Compiler (Item String)
makeTeaserWithSeparator separator item =
    case needlePrefix separator (itemBody item) of
        Nothing -> fail $
            "Main: no teaser defined for " ++
                show (itemIdentifier item)
        Just t -> return (itemSetBody t item)

teaserSeparator :: String
teaserSeparator = "<!--more-->"

makeTeaser :: Item String -> Compiler (Item String)
makeTeaser = makeTeaserWithSeparator teaserSeparator

--------------------------------------------------------------------------------

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Paul Lessard"
    , feedDescription = "A blog about category theory, deep learning, and techno-feudalism"
    , feedAuthorName  = "Paul Lessard"
    , feedAuthorEmail = "paulrlessard@gmail.com"
    , feedRoot        = "https://paul-lessard.github.io"
    }

--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    constField "site-description" "Paul Lessard - Research Mathematician"         <>
    constField "site-url" "https://paul-lessard.github.io"                         <>
    constField "tagline" "Categories, Types, Spaces, and Knowledge" <>
    constField "site-title" "Paul Lessard"                                        <>
    constField "copy-year" "2025"                                                 <>
    constField "site-author" "Paul Lessard"                                       <>
    constField "site-email" "paulrlessard@gmail.com"                             <>
    constField "github-url" "https://github.com/paul-lessard"                      <>
    constField "github-repo" "https://github.com/paul-lessard/paul-lessard.github.io" <>
    constField "twitter-url" "https://x.com/PaulRoyLessard"                    <>
    defaultContext

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    modificationTimeField "modified" "%B %e, %Y" <>
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = makeTagsField "tags" tags <> postCtx

talkCtx :: Context String
talkCtx =
    dateField "date" "%B %e, %Y" <>
    modificationTimeField "modified" "%B %e, %Y" <>
    defaultContext

makeTagLink :: String -> FilePath -> H.Html
makeTagLink tag filePath =
    H.a ! A.title (H.stringValue ("All posts tagged '"++tag++"'."))
        ! A.href (toValue $ toUrl filePath)
        ! A.class_ "tag"
        $ toHtml tag

renderAllTags :: Tags -> Compiler String
renderAllTags =
    return . mconcat .
        fmap (\case (s, _) -> renderHtml $ makeTagLink s ("/tags/" ++ s ++ ".html")) .
        tagsMap

makeTagsField :: String -> Tags -> Context a
makeTagsField =
  tagsFieldWith getTags (fmap . makeTagLink) mconcat

--------------------------------------------------------------------------------

sidebarCtx :: Context String -> Context String
sidebarCtx nodeCtx =
    listField "list_pages" nodeCtx
              (loadAllSnapshots ("pages/*" .&&. hasNoVersion) "page-content") <>
    defaultContext

baseNodeCtx :: Context String
baseNodeCtx =
    urlField "node-url" <>
    titleField "title"

baseSidebarCtx = sidebarCtx baseNodeCtx

evalCtxKey :: Context String -> [String] -> Item String -> Compiler String
evalCtxKey context [key] item =
    unContext context key [] item >>=
    \case
        StringField s -> return s
        _             -> error "Internal error: StringField expected"

getMetadataKey :: [String] -> Item String -> Compiler String
getMetadataKey [key] item = getMetadataField' (itemIdentifier item) key
