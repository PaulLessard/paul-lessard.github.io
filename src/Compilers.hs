--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints -O2 #-}
--------------------------------------------------------------------------------

module Compilers
    ( pandocHTMLCompiler
    , pandocLaTeXCompiler
    , pandocPDFCompiler
    , buildLatex
    ) where

import           Hakyll

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.Char                     (isDigit, isSpace, isAlpha)
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.List                     (isPrefixOf, stripPrefix, sort)
import           Data.Fixed                    (Fixed)
import           Data.Functor                  ((<&>))
import           Data.Traversable
import qualified Data.Set as S
import qualified Data.Char as C

import           GHC.IO                        (unsafePerformIO)

import           System.Process                (system)
import           System.Directory              (createDirectory, setCurrentDirectory, listDirectory,
                                                renameFile, copyFile)
import           System.FilePath               (replaceExtension, takeDirectory,
                                                dropExtensions, takeFileName, takeBaseName,
                                                replaceBaseName, takeExtension, (</>), (<.>), (-<.>),
                                                replaceDirectory)

import           Control.Monad
import           Control.Applicative
import           Control.Monad.State.Lazy
import qualified Control.Monad.Reader as R


import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.Pandoc.Shared            (filteredFilesFromArchive)
import           Text.Pandoc.Builder
import           Text.Pandoc.Parsing           (runF, defaultParserState, extractIdClass)
import           Text.Pandoc.Readers.Markdown  (yamlToMeta)
import           Text.Blaze.Html5.Attributes   (xmlns, item)
import qualified Text.XML as X
import qualified GHC.TypeLits as T

buildLatex :: Item String -> Compiler (Item TmpFile)
buildLatex item = do
    latexFile@(TmpFile latexPath) <- newTmpFile "lualatex.tex"
    tmp <- unsafeCompiler $ do
        writeFile latexPath $ itemBody item
        latexToPDF latexPath
    makeItem (TmpFile tmp)

latexToPDF :: FilePath  -> IO FilePath
latexToPDF latexPath = do
    system $ unwords ["lualatex", "-halt-on-error",
        "-output-directory", takeDirectory latexPath, latexPath, ">/dev/null", "2>&1"]
    return $ latexPath -<.> "pdf"

pdfToSVGs :: FilePath -> IO (M.Map Int FilePath)
pdfToSVGs pdfPath = do
    system $ unwords ["pdf2svg", pdfPath, imgPath, "all"]
    M.fromList . mapMaybe imageFile <$> listDirectory pdfDir
    where
        pdfDir, imgBaseName, imgPath :: FilePath
        pdfDir = takeDirectory pdfPath
        imgBaseName = takeBaseName pdfPath ++ "-pdf2svg"
        imgPath = flip replaceBaseName (imgBaseName ++ "-%i") $ pdfPath -<.> "svg"

        imageFile :: FilePath -> Maybe (Int, FilePath)
        imageFile fp = do
            guard $ takeExtension fp == ".svg"
            str <- stripPrefix (imgBaseName ++ "-") (takeBaseName fp)
            return (read str, pdfDir </> fp)

--------------------------------------------------------------------------------

domsDefaultReaderOptions :: ReaderOptions
domsDefaultReaderOptions = defaultHakyllReaderOptions

domsDefaultHTMLWriterOptions :: WriterOptions
domsDefaultHTMLWriterOptions = defaultHakyllWriterOptions

{-# NOINLINE domsDefaultLaTeXWriterOptions #-}
domsDefaultLaTeXWriterOptions :: WriterOptions
domsDefaultLaTeXWriterOptions = unsafePerformIO $ do
    templ <- runIO (compileDefaultTemplate "latex") >>= handleError
    return $ def
        {   writerTemplate = Just templ
        }

domsDefaultXMLRenderSettings :: X.RenderSettings
domsDefaultXMLRenderSettings = X.def { X.rsXMLDeclaration = False }

-------------------------------------------------------------------------------
-- Load standard pandoc option sets

{-# NOINLINE latexOptions #-}
latexOptions :: Meta
latexOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/latexOptions.yaml"
    runIOorExplode $ yamlToMeta domsDefaultReaderOptions Nothing yaml

replaceMetaField :: T.Text -> MetaValue -> Meta -> Meta
replaceMetaField nm mv meta = Meta $ M.insert nm mv (unMeta meta)

addToStartMetaList :: T.Text -> MetaValue -> Meta -> Meta
addToStartMetaList nm mv meta =
    case lookupMeta nm meta of
        Just (MetaList ls)      -> replaceMetaField nm (MetaList (mv:ls)) meta
        Just x                  -> replaceMetaField nm (MetaList [mv,x]) meta
        Nothing                 -> replaceMetaField nm mv meta

imgGenOptions :: Meta
imgGenOptions =
    addToStartMetaList "header-includes" (MetaInlines [RawInline "latex" "\\PassOptionsToPackage{active}{preview}"]) $
        replaceMetaField "fontsize" (toMetaValue ("17pt" :: String)) latexOptions

-- Meta is a monoid and when applying <> options in its rhs override corresponding 
-- options in its lhs.

--------------------------------------------------------------------------------

writePandocTyped :: (Pandoc -> PandocPure T.Text) -> Pandoc -> String
writePandocTyped writer doc =
    case runPure $ writer doc of
        Left err    -> error $ "Compiler.writePandocTyped: " ++ show err
        Right doc' -> T.unpack doc'

writePandocLaTeX :: Pandoc -> String
writePandocLaTeX = writePandocTyped $ writeLaTeX domsDefaultLaTeXWriterOptions

writePandocHTML :: Pandoc -> String
writePandocHTML = writePandocTyped $ writeHtml5String domsDefaultHTMLWriterOptions

--------------------------------------------------------------------------------

renderPandocTypedTransformM :: ReaderOptions -> (Pandoc -> PandocPure T.Text)
                            -> (Pandoc -> Compiler Pandoc)
                            -> Item String -> Compiler (Item String)
renderPandocTypedTransformM ropt writer trans item =
    fmap (writePandocTyped writer) <$> (readPandocWith ropt item >>= traverse trans)

pandocCompilerTypedTransformM :: ReaderOptions -> (Pandoc -> PandocPure T.Text)
                              -> (Pandoc -> Compiler Pandoc)
                              -> Compiler (Item String)
pandocCompilerTypedTransformM ropt writer f =
    getResourceBody >>= renderPandocTypedTransformM ropt writer f

pandocCompilerTyped :: ReaderOptions -> (Pandoc -> PandocPure T.Text)
                    -> Compiler (Item String)
pandocCompilerTyped ropt writer = pandocCompilerTypedTransformM ropt writer return

--------------------------------------------------------------------------------

renderPandocLaTeX :: Item String -> Compiler (Item String)
renderPandocLaTeX = renderPandocTypedTransformM defaultHakyllReaderOptions (writeLaTeX def) return

pandocLaTeXCompiler :: Compiler (Item String)
pandocLaTeXCompiler = getResourceBody >>= renderPandocLaTeX

pandocHTMLCompiler :: Compiler (Item String)
pandocHTMLCompiler = getResourceBody >>= renderPandocHTML

renderPandocPDF :: Item String -> Compiler (Item TmpFile)
renderPandocPDF =
    renderPandocTypedTransformM defaultHakyllReaderOptions 
        (writeLaTeX domsDefaultLaTeXWriterOptions) addOpts >=> buildLatex
    where
        addOpts :: Pandoc -> Compiler Pandoc
        addOpts (Pandoc meta body) = return $ Pandoc (meta <> latexOptions) body

pandocPDFCompiler :: Compiler (Item TmpFile)
pandocPDFCompiler = getResourceBody >>= renderPandocPDF 

-------------------------------------------------------------------------------
-- Very simple backtracking parser monad.

type ParserMonad = StateT String Maybe

stripSpaces :: ParserMonad ()
stripSpaces = modify $ dropWhile isSpace

word :: (Char -> Bool) -> ParserMonad String
word p = do
    s0 <- get
    let (x, s1) = span p s0
    put s1
    return x

token :: String -> ParserMonad String
token t = do
    s0 <- get
    let (s,s1) = splitAt (length t) s0
    guard (s == t)
    put s1
    return s

number :: ParserMonad Int
number = read <$> word isDigit

-------------------------------------------------------------------------------
-- Parser to read the image dimensions recorded in a LaTeX log file.

type Points = Fixed 100000

ptConvFactor :: Points
ptConvFactor = 1.00375

data ImageInfo = ImageInfo
    { depth :: Points
    , height :: Points
    , width :: Points
    } deriving (Show)

getEqnDimens :: FilePath -> IO [(Int, Int, ImageInfo)]
getEqnDimens fp = mapMaybe (evalStateT parseImageDimens) . lines <$> readFile fp
    where
        parseDimen :: ParserMonad Points
        parseDimen = do
            n1 <- word isDigit
            n2 <- (token "." >> word isDigit) <|> return "0"
            token "pt"
            return $ read $ n1 ++ "." ++ n2

        parseImageDimens :: ParserMonad (Int, Int, ImageInfo)
        parseImageDimens = do
            token "Preview: image ("
            i <- number
            token ") equation ("
            e <- number
            token ") dimensions" >> stripSpaces
            d1 <- parseDimen
            token "," >> stripSpaces
            d2 <- parseDimen
            token "," >> stripSpaces
            d3 <- parseDimen
            return (i, e, ImageInfo (d1 / ptConvFactor)
                                    (d2 / ptConvFactor)
                                    (d3 / ptConvFactor))

-------------------------------------------------------------------------------
-- Build an HTML file with embedded SVG sections from an input containing 
-- LaTeX equations

renderPandocHTML :: Item String -> Compiler (Item String)
renderPandocHTML item = do
    doc <- readPandocWith domsDefaultReaderOptions item
    latexFile@(TmpFile latexPath) <- newTmpFile "eqnimages.tex"
    for doc $ \body -> do
        imgs <- makeEquationImages latexPath body
        let imgSubdir = takeBaseName $ toFilePath $ itemIdentifier item
        writePandocHTML <$> embedEquationImages imgs body
    where
        inputFile :: FilePath
        inputFile = toFilePath (itemIdentifier item)

embedEquationImages :: Monad m => M.Map Int (X.Document, ImageInfo) ->
    Pandoc -> m Pandoc
embedEquationImages imgs = flip evalStateT 0 . walkM transformEquation
    where
        transformEquation :: Monad m => Inline -> StateT Int m Inline
        transformEquation (Math typ body) = do
            modify (+1)
            num <- get
            let classes = case typ of
                    InlineMath -> ["inline-equation"]
                    DisplayMath -> ["displayed-equation"]
            case M.lookup num imgs of
                Nothing ->
                    return $ Span ("", classes, [("style", "color: red;")])
                                [Str "<missing image>"]
                Just img ->
                    return $ RawInline "html" (transformAndRenderImage num img typ)
        transformEquation x = return x

makeEquationImages :: FilePath -> Pandoc -> Compiler (M.Map Int (X.Document, ImageInfo))
makeEquationImages latexPath doc =
    if numEqns > 0
    then unsafeCompiler $ do
            writeFile latexPath latex
            pdfPath <- latexToPDF latexPath
            imgInfo <- getEqnDimens $ pdfPath -<.> "log"
            svgFiles <- pdfToSVGs pdfPath
            imgData <- traverse (X.readFile X.def) svgFiles
            return $ M.fromList $
                mapMaybe (\(i, e, d) -> (e,) . (,d) <$> M.lookup i imgData) imgInfo
    else return M.empty
    where
        transformEquation :: Inline -> State Int Inline
        transformEquation (Math typ text) = do
            modify (+1)
            num <- get
            return $ Math typ $ T.pack $ "\\begin{shipper}{" ++ (
                case typ of
                    InlineMath -> "\\textstyle"
                    DisplayMath -> "\\displaystyle"
                ) ++ "}{" ++ show num ++ "}" ++ T.unpack text ++ "\\end{shipper}"
        transformEquation x = return x

        addImgGenOptions :: Pandoc -> Pandoc
        addImgGenOptions (Pandoc meta body) =
            Pandoc (meta <> imgGenOptions) body

        addShippers :: Pandoc -> (Pandoc, Int)
        addShippers = flip runState 0 . walkM transformEquation

        transDoc :: Pandoc
        numEqns :: Int
        (transDoc, numEqns) = addShippers $ addImgGenOptions doc

        latex :: String
        latex = writePandocLaTeX transDoc

transformAndRenderImage :: Int -> (X.Document, ImageInfo) -> MathType -> T.Text
transformAndRenderImage num (svg, ImageInfo dp _ _) typ =
    LT.toStrict $ X.renderText domsDefaultXMLRenderSettings $
        R.runReader (walkM transformElement svg) allIDs
    where
        transformElement :: X.Element -> R.Reader (S.Set T.Text) X.Element
        transformElement = transformTags . transformID . addExtraAttrAtRoot

        allIDs :: S.Set T.Text
        allIDs = query queryID svg
            where
                queryID :: X.Element -> S.Set T.Text
                queryID (X.Element _ attr _) = maybe S.empty S.singleton (M.lookup "id" attr)

        addExtraAttrAtRoot :: X.Element -> X.Element
        addExtraAttrAtRoot e@(X.Element nm attr nodes) =
            if nm == "{http://www.w3.org/2000/svg}svg"
                then X.Element nm (attr <> extraRootAttr) nodes
                else e

        extraRootAttr :: M.Map X.Name T.Text
        extraRootAttr =
            case typ of
                DisplayMath -> M.fromList
                    [ ( "class", "displayed-equation" )
                    ]
                InlineMath -> M.fromList
                    [ ( "class", "inline-equation" )
                    , ( "style"
                      , T.pack $ "transform: translateY(" ++
                                 show (dp + 0.5) ++ "pt);" )
                    ]

        transformID :: X.Element -> X.Element
        transformID e@(X.Element nm attr nodes) =
            case M.lookup "id" attr of
                Just t ->
                    X.Element
                        nm
                        (M.insert "id" (T.concat ["equation-", T.pack (show num), "-", t]) attr)
                        nodes
                Nothing -> e

        -- return $ foldr (M.adjust transformID) attr ["id", "{http://www.w3.org/1999/xlink}href"]

        transformTags :: X.Element -> R.Reader (S.Set T.Text) X.Element
        transformTags (X.Element nm attr nodes) = do
            transAttr <- traverse transformAttrValue attr
            return $ X.Element nm transAttr nodes

        transformAttrValue :: T.Text -> R.Reader (S.Set T.Text) T.Text
        transformAttrValue s = do
            trans <- mapM transformTag (tail splitup)
            return $ T.concat $ head splitup:trans
            where
                splitup :: [T.Text]
                splitup = T.splitOn "#" s

                transformTag :: T.Text -> R.Reader (S.Set T.Text) T.Text
                transformTag s  = do
                    ids <- R.ask
                    if S.member (T.takeWhile (\c -> C.isAlphaNum c || c == '-') s) ids
                        then return $ T.concat ["#", "equation-", T.pack (show num), "-", s]
                        else return $ T.concat ["#", s]

-- Walker for XML documents
-- Incomplete: only walks Document, Node and Element nodes

-- Walkable instance declarations
-- Walk Document
instance Walkable X.Document X.Document where
    walkM f = f
    query f = f

instance Walkable X.Element X.Document where
    walkM = walkDocumentM
    query = queryDocument

instance Walkable [X.Node] X.Document where
    walkM = walkDocumentM
    query = queryDocument

instance Walkable X.Node X.Document where
    walkM = walkDocumentM
    query = queryDocument

-- Walk Element
instance Walkable X.Element X.Element where
    walkM f e = walkElementM f e >>= f
    query f e = f e <> queryElement f e

instance Walkable [X.Node] X.Element where
    walkM = walkElementM
    query = queryElement

instance Walkable X.Node X.Element where
    walkM = walkElementM
    query = queryElement

-- Walk node list
instance {-# OVERLAPPING #-}
         Walkable [X.Node] [X.Node] where
    walkM f = traverse (walkNodeM f) >=> f
    query f nodes = f nodes <> mconcat (map (queryNode f) nodes)

-- Walk node
instance Walkable X.Node X.Node where
    walkM f n = walkNodeM f n >>= f
    query f n = f n <> queryNode f n

instance Walkable X.Element X.Node where
    walkM = walkNodeM
    query = queryNode

-- Concrete walkers

-- Helper methods to walk and query the components of a Document
walkDocumentM :: (Walkable a X.Element, Applicative m, Monad m) =>
    (a -> m a) -> X.Document -> m X.Document
walkDocumentM f (X.Document p e ms) = do
    e' <- walkM f e
    return $ X.Document p e' ms

queryDocument :: (Walkable a X.Element, Monoid c) =>
    (a -> c) -> X.Document -> c
queryDocument f (X.Document p e ms) = query f e

-- Helper methods to walk and query the components of an Elemement
walkElementM :: (Walkable a [X.Node], Applicative m, Monad m) =>
    (a -> m a) -> X.Element -> m X.Element
walkElementM f (X.Element nm attr nodes) = do
    nodes' <- walkM f nodes
    return $ X.Element nm attr nodes'

queryElement :: (Walkable a [X.Node], Monoid c) =>
    (a -> c) -> X.Element -> c
queryElement f (X.Element _ _ nodes) = query f nodes

-- Helper methods to walk and query the components of an Node
walkNodeM :: (Walkable a X.Element, Applicative m, Monad m) =>
    (a -> m a) -> X.Node -> m X.Node
walkNodeM f (X.NodeElement e) = do
    e' <- walkM f e
    return $ X.NodeElement e'
walkNodeM _ e = return e

queryNode :: (Walkable a X.Element, Monoid c) =>
    (a -> c) -> X.Node -> c
queryNode f (X.NodeElement e) = query f e
queryNode _ e = mempty




