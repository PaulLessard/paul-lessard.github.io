--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}

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
import qualified Data.Map as M                 (Map, fromList, empty, intersectionWith, toList, lookup, adjust, insert, union)
import           Data.Char                     (isDigit, isSpace)
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.List                     (isPrefixOf, stripPrefix, sort)
import           Data.Fixed

import           GHC.IO                        (unsafePerformIO)

import           System.Process                (system)
import           System.Directory              (createDirectory, setCurrentDirectory, listDirectory, renameFile)
import           System.FilePath               (replaceExtension, takeDirectory,
                                                dropExtensions, takeFileName, takeBaseName,
                                                replaceBaseName, takeExtension, (</>), (<.>), (-<.>))

import           Control.Monad                 (forM_)
import           Control.Monad.State.Lazy
import           Control.Applicative           ((<|>))

import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.Pandoc.Shared            (filteredFilesFromArchive)
import           Text.Pandoc.Builder
import           Text.Pandoc.Parsing           (runF, defaultParserState)
import           Text.Pandoc.Readers.Markdown  (yamlToMeta)
import           Text.Blaze.Html5.Attributes   (xmlns, item)
import qualified Text.XML as X

buildLatex :: Item String -> Compiler (Item TmpFile)
buildLatex item = do
    latexFile@(TmpFile latexPath) <- newTmpFile "lualatex.tex"
    pdfFile <- unsafeCompiler $ do
        writeFile latexPath $ itemBody item
        latexToPDF latexFile
    makeItem pdfFile

latexToPDF :: TmpFile -> IO TmpFile
latexToPDF latexFile = do
    system $ unwords ["lualatex", "-halt-on-error",
        "-output-directory", latexDir, latexPath, ">/dev/null", "2>&1"]
    return $ TmpFile pdfPath
    where
        latexPath, latexDir, pdfPath :: FilePath
        TmpFile latexPath = latexFile
        latexDir = takeDirectory latexPath
        pdfPath = latexPath -<.> "pdf"

pdfToSVGs :: TmpFile -> IO (M.Map Int TmpFile)
pdfToSVGs pdfFile = do
    system $ unwords ["pdf2svg", pdfPath, imgPath, "all"]
    M.fromList . mapMaybe imageFile <$> listDirectory pdfDir
    where
        pdfPath, pdfDir, imgBaseName, imgPath :: FilePath
        TmpFile pdfPath = pdfFile
        pdfDir = takeDirectory pdfPath
        imgBaseName = takeBaseName pdfPath ++ "-pdf2svg"
        imgPath = flip replaceBaseName (imgBaseName ++ "-%i") $ pdfPath -<.> "svg"

        imageFile :: FilePath -> Maybe (Int, TmpFile)
        imageFile fp = do
            guard $ takeExtension fp == ".svg"
            str <- stripPrefix (imgBaseName ++ "-") (takeBaseName fp)
            return (read str, TmpFile fp)

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

-------------------------------------------------------------------------------
-- Load standard pandoc option sets

{-# NOINLINE latexOptions #-}
latexOptions :: Meta
latexOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/latexOptions.yaml"
    runIOorExplode $ yamlToMeta domsDefaultReaderOptions Nothing yaml

{-# NOINLINE imgGenOptions #-}
imgGenOptions :: Meta
imgGenOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/imgGenOptions.yaml"
    runIOorExplode $ yamlToMeta domsDefaultReaderOptions Nothing yaml

-- Meta is a monoid and when applying <> options in its rhs override corresponding 
-- options in its lhs.

--------------------------------------------------------------------------------

writePandocTyped :: (Pandoc -> PandocPure T.Text)
                 -> Item Pandoc -> Item String
writePandocTyped writer (Item itemi doc) =
    case runPure $ writer doc of
        Left err    -> error $ "Compiler.writePandocTyped: " ++ show err
        Right item' -> Item itemi $ T.unpack item'

renderPandocTypedTransformM :: ReaderOptions -> (Pandoc -> PandocPure T.Text)
                            -> (Pandoc -> Compiler Pandoc)
                            -> Item String -> Compiler (Item String)
renderPandocTypedTransformM ropt writer trans item =
    writePandocTyped writer <$> (readPandocWith ropt item >>= traverse trans)

pandocCompilerTypedTransformM :: ReaderOptions -> (Pandoc -> PandocPure T.Text)
                              -> (Pandoc -> Compiler Pandoc)
                              -> Compiler (Item String)
pandocCompilerTypedTransformM ropt writer f =
    getResourceBody >>= renderPandocTypedTransformM ropt writer f

pandocCompilerTyped :: ReaderOptions -> (Pandoc -> PandocPure T.Text)
                    -> Compiler (Item String)
pandocCompilerTyped ropt writer = pandocCompilerTypedTransformM ropt writer return

--------------------------------------------------------------------------------

writePandocLaTeX :: Item Pandoc -> Item String
writePandocLaTeX = writePandocTyped (writeLaTeX domsDefaultLaTeXWriterOptions)

writePandocHTML :: Item Pandoc -> Item String
writePandocHTML = writePandocTyped (writeLaTeX domsDefaultHTMLWriterOptions)

renderPandocLaTeX :: Item String -> Compiler (Item String)
renderPandocLaTeX = renderPandocTypedTransformM defaultHakyllReaderOptions (writeLaTeX def) return

pandocLaTeXCompiler :: Compiler (Item String)
pandocLaTeXCompiler = getResourceBody  >>= renderPandocLaTeX

-- pandocHTMLCompiler :: Compiler (Item String)
-- pandocHTMLCompiler = pandocCompilerWith domsDefaultReaderOptions domsDefaultHTMLWriterOptions


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
            return (i, e, ImageInfo d1 d2 d3)

-------------------------------------------------------------------------------
-- Build an HTML file with embedded SVG sections from an input containing 
-- LaTeX equations

ptConvFactor :: Points
ptConvFactor = 1.00375

pandocHTMLCompiler :: Compiler (Item String)
pandocHTMLCompiler = getResourceBody  >>= renderPandocHTML

renderPandocHTML :: Item String -> Compiler (Item String)
renderPandocHTML item = do
    doc <- readPandocWith domsDefaultReaderOptions item
    imgs <- makeEquationImages doc
    let imgSubdir = takeBaseName $ toFilePath $ itemIdentifier item
    writePandocHTML <$> transformHTMLDocument imgSubdir imgs doc

transformHTMLDocument :: String -> M.Map Int (TmpFile, ImageInfo) ->
    Item Pandoc -> Compiler (Item Pandoc)
transformHTMLDocument imgSubdir imgs =
    traverse $ unsafeCompiler . flip evalStateT 0 . walkM transformEquation
    where
        transformEquation :: Inline -> StateT Int IO Inline
        transformEquation (Math typ body) = do
            num <- get
            let num = num + 1
            put num
            let classes = case typ of
                    InlineMath -> ["inline-equation"]
                    DisplayMath -> ["displayed-equation"]
            case M.lookup num imgs of
                Nothing ->
                    return $ Span ("", classes, [("style", "color: red;")])
                                [Str "<missing image>"]
                Just img ->
                    RawInline "xml" <$> liftIO (loadAndTransformSVG num img typ)
        transformEquation x = return x

makeEquationImages :: Item Pandoc -> Compiler (M.Map Int (TmpFile, ImageInfo))
makeEquationImages item =
    if numEqns > 0
    then do
        latexFile@(TmpFile latexPath) <- newTmpFile "lualatex.tex"
        unsafeCompiler $ do
            writeFile latexPath latex
            pdfFile <- latexToPDF latexFile
            imgInfo <- getEqnDimens $ latexPath -<.> "log"
            svgFiles <- pdfToSVGs pdfFile
            return $ M.fromList $ do
                (i, e, d) <- imgInfo
                let Just f = M.lookup i svgFiles
                return (e, (f,d))
    else return M.empty
    where
        transformEquation :: Inline -> State Int Inline
        transformEquation (Math typ text) = do
            num <- get
            let num = num + 1
            put num
            return $ Math typ $ T.pack $ "\\begin{shipper}{" ++ (
                case typ of
                    InlineMath -> "\\textstyle"
                    DisplayMath -> "\\displaystyle"
                ) ++ "}{" ++ show num ++ "}" ++ T.unpack text ++ "\\end{shipper}"
        transformEquation x = return x

        addImgGenOptions :: Pandoc -> Pandoc
        addImgGenOptions (Pandoc meta body) =
            Pandoc (meta <> latexOptions <> imgGenOptions) body

        addShippers :: Pandoc -> (Pandoc, Int)
        addShippers = flip runState 0 . walkM transformEquation

        numEqns :: Int
        latex :: String
        (latex, numEqns) =
            let item' = addShippers . addImgGenOptions <$> item
            in (itemBody $ writePandocLaTeX $ fst <$> item', snd $ itemBody item')

loadAndTransformSVG :: Int -> (TmpFile, ImageInfo) -> MathType -> IO T.Text
loadAndTransformSVG num (TmpFile fp, ImageInfo dp _ _) typ =
    LT.toStrict . X.renderText X.def . traverseDocument <$> X.readFile X.def fp
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
                                 show (dp / ptConvFactor + 0.5) ++ "pt);" )
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