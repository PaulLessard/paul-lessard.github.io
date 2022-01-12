--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------------------

module Compilers
    ( pandocHTMLCompiler
    , pandocLaTeXCompiler
    , buildLatex
    ) where

import           Hakyll

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.Char                     (isDigit, isSpace)
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.List                     (isPrefixOf, stripPrefix, sort)
import           Data.Fixed                    (Fixed)
import           Data.Functor                  ((<&>))

import           GHC.IO                        (unsafePerformIO)

import           System.Process                (system)
import           System.Directory              (createDirectory, setCurrentDirectory, listDirectory, 
                                                renameFile, copyFile)
import           System.FilePath               (replaceExtension, takeDirectory,
                                                dropExtensions, takeFileName, takeBaseName,
                                                replaceBaseName, takeExtension, (</>), (<.>), (-<.>), 
                                                replaceDirectory)

import           Control.Monad
import           Control.Monad.State.Lazy
import           Control.Applicative

import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.Pandoc.Shared            (filteredFilesFromArchive)
import           Text.Pandoc.Builder
import           Text.Pandoc.Parsing           (runF, defaultParserState, extractIdClass)
import           Text.Pandoc.Readers.Markdown  (yamlToMeta)
import           Text.Blaze.Html5.Attributes   (xmlns, item)
import qualified Text.XML as X
import Data.Traversable
import qualified GHC.TypeLits as T

buildLatex :: Item String -> Compiler (Item TmpFile)
buildLatex item = do
    latexFile <- newTmpFile "lualatex.tex"
    traverse (unsafeCompiler . buildLatex' latexFile) item
    where
        buildLatex' :: TmpFile -> String -> IO TmpFile
        buildLatex' (TmpFile latexPath) body = do
            writeFile latexPath $ itemBody item
            pdfPath <- latexToPDF latexPath
            return (TmpFile pdfPath)

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
pandocLaTeXCompiler = getResourceBody  >>= renderPandocLaTeX

pandocHTMLCompiler :: Compiler (Item String)
-- pandocHTMLCompiler = pandocCompilerWith domsDefaultReaderOptions domsDefaultHTMLWriterOptions
pandocHTMLCompiler = getResourceBody  >>= renderPandocHTML

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
    for doc $ \body -> unsafeCompiler $ do
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

makeEquationImages :: FilePath -> Pandoc -> IO (M.Map Int (X.Document, ImageInfo))
makeEquationImages latexPath doc =
    if numEqns > 0
    then do
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
    LT.toStrict $ X.renderText domsDefaultXMLRenderSettings $ traverseDocument svg
    where
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

        updateRoot :: X.Element -> X.Element
        updateRoot (X.Element nm attr nodes) =
            X.Element nm (M.union attr extraRootAttr) (map traverseNode nodes)

        traverseDocument :: X.Document -> X.Document
        traverseDocument (X.Document p e ms) = X.Document p (updateRoot e) ms

        traverseElement :: X.Element -> X.Element
        traverseElement (X.Element nm attr nodes) =
            X.Element nm (updateAttr attr) (map traverseNode nodes)

        updateAttr :: M.Map X.Name T.Text -> M.Map X.Name T.Text
        updateAttr attr =
            foldr (M.adjust transformID) attr ["id", "{http://www.w3.org/1999/xlink}href"]

        transformID :: T.Text -> T.Text
        transformID = flip T.append (T.pack $ "-equation-" ++ show num)

        traverseNode :: X.Node -> X.Node
        traverseNode (X.NodeElement elem) = X.NodeElement $ traverseElement elem
        traverseNode n = n
