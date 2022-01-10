--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------

module Compilers
    ( pandocHTMLCompiler
    , pandocLaTeXCompiler
    , buildLatex
    , HTMLBundle
    , compileToHTMLBundle
    , pdfToSVGs
    ) where

import           Hakyll

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LB
import           Data.Typeable                 (Typeable)
import qualified Data.Map as M                 (Map, fromList, empty, intersectionWith, toList, lookup)
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
import qualified Text.Pandoc.UTF8 as T

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
        pdfPath = replaceExtension "pdf" latexPath

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

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions

pandocHTMLWriterOptions :: WriterOptions
pandocHTMLWriterOptions = defaultHakyllWriterOptions
    {   writerHTMLMathMethod =
            MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    }

{-# NOINLINE pandocLaTeXWriterOptions #-}
pandocLaTeXWriterOptions :: WriterOptions
pandocLaTeXWriterOptions = unsafePerformIO $ do
    templ <- runIO (compileDefaultTemplate "latex") >>= handleError
    return $ def
        {   writerTemplate = Just templ
        }

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
writePandocLaTeX = writePandocTyped (writeLaTeX pandocLaTeXWriterOptions)

writePandocHTML :: Item Pandoc -> Item String
writePandocHTML = writePandocTyped (writeLaTeX pandocHTMLWriterOptions)

renderPandocLaTeX :: Item String -> Compiler (Item String)
renderPandocLaTeX = renderPandocTypedTransformM defaultHakyllReaderOptions (writeLaTeX def) return

pandocLaTeXCompiler :: Compiler (Item String)
pandocLaTeXCompiler = getResourceBody  >>= renderPandocLaTeX

pandocHTMLCompiler :: Compiler (Item String)
pandocHTMLCompiler = pandocCompilerWith pandocReaderOptions pandocHTMLWriterOptions


-------------------------------------------------------------------------------
-- Load standard pandoc option sets

{-# NOINLINE latexOptions #-}
latexOptions :: Meta
latexOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/latexOptions.yaml"
    runIOorExplode $ yamlToMeta pandocReaderOptions Nothing yaml

{-# NOINLINE imgGenOptions #-}
imgGenOptions :: Meta
imgGenOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/imgGenOptions.yaml"
    runIOorExplode $ yamlToMeta pandocReaderOptions Nothing yaml

-- Meta is a monoid and when applying <> options in its rhs override corresponding 
-- options in its lhs.

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

--------------------------------------------------------------------------------
-- A Writeable instance to represent an HTML output accompanied by SVG images
-- generated from displayed tikz code.

data HTMLBundle = HTMLBundle String [(Int, TmpFile)]
    deriving (Typeable)

instance Writable HTMLBundle where
    write fp item = do
        writeFile fp html
        removeDirectory imgsDir
        if null imgs
            then return ()
            else do
                createDirectory imgsDir
                forM_ imgs $ \(n, TmpFile ifp) ->
                    renameFile ifp (imgsDir </> "equation-" ++ show n ++ ".svg")
        where
            HTMLBundle html imgs = itemBody item
            imgsDir = dropExtensions fp

-------------------------------------------------------------------------------
-- Build an HTML bundle from an input containing LaTeX equations

ptConvFactor :: Points
ptConvFactor = 1.00375

compileToHTMLBundle :: Item String -> Compiler (Item HTMLBundle)
compileToHTMLBundle item = do
    doc <- readPandoc item
    imgs <- makeImages doc
    let imgSubdir = takeBaseName $ toFilePath $ itemIdentifier item
        html = writePandocHTML $
                flip evalState 0 . walkM (transformEquation imgSubdir imgs) <$> doc
        eqns = [ (e, f) | (e, (f, _)) <- M.toList imgs ]
    return $ flip HTMLBundle eqns <$> html
    where
        transformEquation :: String -> M.Map Int (TmpFile, ImageInfo) -> Inline -> State Int Inline
        transformEquation imgSubdir imgs (Math typ body) = do
            num <- get
            let num = num + 1
            put num
            let img = M.lookup num imgs
                cssClasses = case typ of
                    InlineMath -> ["inline-equation"]
                    DisplayMath -> ["displayed-equation"]
            return $ case img of
                Nothing ->
                    Span ("", cssClasses, [("style", "color: red;")]) $
                        [Str "<missing image>"]
                Just (imgFile, imgInfo) ->
                    Image ("", cssClasses, attributes) (toList $ text body)
                        ( T.pack $ imgSubdir </> "equation-" ++ show num <.> "svg"
                        , T.pack $ "equation: " ++ show num)
                    where
                        attributes :: [(T.Text, T.Text)]
                        attributes = case typ of
                            InlineMath ->
                                [ ( "style"
                                  , T.pack $ "transform: translateY(" ++
                                            show (depth imgInfo / ptConvFactor + 0.5) ++ "pt);"
                                  )
                                ]
                            DisplayMath -> []
        transformEquation _ _ x = return x

makeImages :: Item Pandoc -> Compiler (M.Map Int (TmpFile, ImageInfo))
makeImages item =
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

{-
testFilter :: FilePath -> IO ()
testFilter fp = do
    text <- TIO.readFile fp
    result <- runIO $ do
        doc <- readMarkdown pandocReaderOptions text
        let Pandoc meta body = evalState (walkM transformEquation doc) 1
        let doc' = Pandoc (meta <> latexOptions <> imgGenOptions) body
        writeLaTeX pandocLaTeXWriterOptions doc'
    latex <- handleError result
    TIO.putStrLn latex
    where
        transformEquation :: Inline -> State Int Inline
        transformEquation (Math typ text) = do
            num <- get
            put (num + 1)
            return $ Math typ $ T.pack $ "\\begin{shipper}{" ++ (
                case typ of
                    InlineMath -> "\\textstyle"
                    DisplayMath -> "\\displaystyle"
                ) ++ "}{" ++ show num ++ "}" ++ T.unpack text ++ "\\end{shipper}"
        transformEquation x = return x
-}


        