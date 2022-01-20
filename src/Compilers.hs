--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints -O2 #-}
--------------------------------------------------------------------------------

module Compilers
    ( pandocHTMLCompiler
    , renderPandocHTML
    , pandocLaTeXCompiler
    , renderPandocLaTeX
    , pandocPDFCompiler
    , renderPandocPDF
    , buildLatex
    , pdfToSVGs
    ) where

import           Hakyll
import           XMLWalker

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.Char                     (isDigit, isSpace, isAlpha)
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.List                     (isPrefixOf, stripPrefix, sort, intersperse)
import           Data.Fixed                    (Fixed)
import           Data.Functor                  ((<&>))
import           Data.Traversable
import           Data.Foldable
import qualified Data.Set as S
import qualified Data.Char as C

import           GHC.IO                        (unsafePerformIO)

import           System.Process                (system)
import           System.Exit                   (ExitCode(..))
import           System.Directory              (createDirectory, setCurrentDirectory, listDirectory,
                                                renameFile, copyFile, doesFileExist)
import           System.FilePath               (replaceExtension, takeDirectory,
                                                dropExtensions, takeFileName, takeBaseName,
                                                replaceBaseName, takeExtension, (</>), (<.>), (-<.>),
                                                replaceDirectory)

import           Control.Monad
import           Control.Applicative
import           Control.Monad.State.Lazy
import qualified Control.Monad.Reader as R
import           Control.Monad.Except


import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.Pandoc.Shared            (filteredFilesFromArchive)
import           Text.Pandoc.Builder as B
import           Text.Pandoc.Parsing           (runF, defaultParserState, extractIdClass)
import           Text.Pandoc.Readers.Markdown  (yamlToMeta)
import           Text.Blaze.Html5.Attributes   (xmlns, item)
import qualified Text.XML as X
import qualified GHC.TypeLits as T

buildLatex :: Item String -> Compiler (Item TmpFile)
buildLatex item = do
    latexFile@(TmpFile latexPath) <- newTmpFile "lualatex.tex"
    unsafeCompiler $ writeFile latexPath $ itemBody item
    runLuaLaTeX [] latexPath >>= makeItem . TmpFile

runLuaLaTeX :: [String] -> FilePath  -> Compiler FilePath
runLuaLaTeX extraOpts latexPath = do
    exitCode <- unsafeCompiler $ system $ unwords $
        ["lualatex", "-halt-on-error"] ++ extraOpts ++
        ["-output-directory", takeDirectory latexPath, latexPath, ">/dev/null", "2>&1"]
    id <- getUnderlying
    let idPath = toFilePath id
        logDir = takeDirectory idPath </> "_texlog"
        logDestinationPath = case identifierVersion id of
            Nothing -> logDir </> takeBaseName idPath <.> "log"
            Just v -> logDir </> (takeBaseName idPath ++ "#" ++ v) <.> "log"
        logSourcePath = latexPath -<.> "log"
    unsafeCompiler $ do
        exists <- doesFileExist logSourcePath
        when exists $ do
                makeDirectories logDestinationPath
                copyFile logSourcePath logDestinationPath
    case exitCode of
        ExitSuccess ->
            return $ latexPath -<.> "pdf"
        ExitFailure err ->
            throwError [ "LaTeX compiler: failed while processing item " ++
                show id ++ " exit code " ++ show err ++ "." ]

pdfToSVGs :: FilePath -> Compiler (M.Map Int FilePath)
pdfToSVGs pdfPath = do
    exitCode <- unsafeCompiler $ system $ unwords ["pdf2svg", pdfPath, imgPath, "all"]
    case exitCode of
        ExitSuccess ->
            M.fromList . mapMaybe imageFile <$> unsafeCompiler (listDirectory pdfDir)
        ExitFailure err -> do
            id <- getUnderlying
            throwError [ "PDFtoSVG compiler: failed while processing item " ++
                show id ++ " exit code " ++ show err ++ "."]
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

{-# NOINLINE commonOptions #-}
commonOptions :: Meta
commonOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/commonOptions.yaml"
    runIOorExplode $ yamlToMeta domsDefaultReaderOptions Nothing yaml

{-# NOINLINE pdfGenOptions #-}
pdfGenOptions :: Meta
pdfGenOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/pdfGenOptions.yaml"
    meta <- runIOorExplode $ yamlToMeta domsDefaultReaderOptions Nothing yaml
    return (meta <> commonOptions)

{-# NOINLINE imgGenOptions #-}
imgGenOptions :: Meta
imgGenOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/imgGenOptions.yaml"
    meta <- runIOorExplode $ yamlToMeta domsDefaultReaderOptions Nothing yaml
    return (meta <> commonOptions)

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
        addOpts (Pandoc meta body) = return $ Pandoc (meta <> pdfGenOptions) body

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

getEqnDimens :: FilePath -> Compiler [ImageInfo]
getEqnDimens fp = unsafeCompiler $
        mapMaybe (evalStateT parseImageDimens) . lines <$> readFile fp
    where
        parseDimen :: ParserMonad Points
        parseDimen = do
            n1 <- word isDigit
            n2 <- (token "." >> word isDigit) <|> return "0"
            token "pt"
            return $ read $ n1 ++ "." ++ n2

        parseImageDimens :: ParserMonad ImageInfo
        parseImageDimens = do
            token "Preview: eqn#"
            e <- number
            stripSpaces
            token "dims"
            stripSpaces
            d1 <- parseDimen
            token ","
            d2 <- parseDimen
            token ","
            d3 <- parseDimen
            return $ ImageInfo
                (d1 / ptConvFactor)
                (d2 / ptConvFactor)
                (d3 / ptConvFactor)

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

prependMeta :: Meta -> Pandoc -> Pandoc
prependMeta pmeta (Pandoc meta body) = Pandoc (pmeta <> meta) body

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _) = meta

makeEquationSVGs :: Pandoc -> Compiler [T.Text]
makeEquationSVGs inputDoc =
    if null eqnBlocks
    then do
        latexFile@(TmpFile latexPath) <- newTmpFile "eqnimages.tex"
        unsafeCompiler $ writeFile latexPath imgGenLaTeX
        pdfPath <- runLuaLaTeX ["--shell-escape"] latexPath
        imgInfo <- getEqnDimens $ latexPath -<.> "log"
        svg <- unsafeCompiler $ X.readFile X.def $ pdfPath -<.> "svg"
        return $ splitSVGAndRender imgInfo svg
    else
        return []
    where
        queryEquations :: Inline -> Blocks
        queryEquations (Math typ text) =
            rawBlock "latex" $ T.concat
                [ "\\begin{shipper}{"
                , case typ of
                    InlineMath -> "\\textstyle"
                    DisplayMath -> "\\displaystyle"
                , "}\n"
                , text
                , "\n\\end{shipper}\n"
                ]
        transformEquation x = mempty

        eqnBlocks :: Blocks
        eqnBlocks = query queryEquations inputDoc

        imgGenDoc :: Pandoc
        imgGenDoc =
            prependMeta imgGenOptions $
                prependMeta (getMeta inputDoc) $
                doc eqnBlocks

        imgGenLaTeX :: String
        imgGenLaTeX = writePandocLaTeX imgGenDoc

splitSVGAndRender :: [ImageInfo] -> X.Document -> [T.Text]
splitSVGAndRender imgInfo svg = 
    map (LT.toStrict . X.renderText domsDefaultXMLRenderSettings) (defsDoc:allEqnImages)
    where
        namedElementNodes :: X.Name -> X.Element -> [[X.Node]]
        namedElementNodes nm e@(X.Element enm _ nodes) | nm == enm = [nodes]
        namedElementNodes _ _ = []

        allEqnImages :: [X.Document]
        allEqnImages =
            zipWith3 mkImageSVG [1..] imgInfo $
                query (namedElementNodes "{http://www.w3.org/2000/svg}page") svg

        mkImageSVG :: Integer -> ImageInfo -> [X.Node] -> X.Document
        mkImageSVG num (ImageInfo dp ht wd) nodes =
            X.Document
                (X.Prologue [] Nothing [])
                (X.Element
                    "{http://www.w3.org/2000/svg}svg"
                    (M.fromList
                        [ ("version", "1.2")
                        , ("height", T.pack (show (dp + ht) ++ "pt"))
                        , ("width", T.pack (show wd ++ "pt"))
                        , ("viewBox",
                            T.concat ["0 0 ", T.pack (show wd), " ", T.pack (show (dp + ht))])
                        , ("style",
                            T.concat ["transform: translateY(", T.pack (show dp), "pt);"])
                        ]) transformedNodes) []
            where
                transformedNodes :: [X.Node]
                transformedNodes = walk (transformTags . transformID) nodes

                queryID :: X.Element -> S.Set T.Text
                queryID (X.Element _ attr _) = maybe S.empty S.singleton (M.lookup "id" attr)

                allIDs :: S.Set T.Text
                allIDs = query queryID nodes

                transformID :: X.Element -> X.Element
                transformID e@(X.Element nm attr nodes) =
                    case M.lookup "id" attr of
                        Just t ->
                            X.Element
                                nm
                                (M.insert "id" (T.concat ["equation-", T.pack (show num), "-", t]) attr)
                                nodes
                        Nothing -> e

                transformTags :: X.Element -> X.Element
                transformTags (X.Element nm attr nodes) = 
                    X.Element nm (fmap transformAttrValue attr) nodes

                transformAttrValue :: T.Text -> T.Text
                transformAttrValue s =
                    T.concat $ intersperse "#" $ head splitup:map transformTag (tail splitup)
                    where
                        splitup :: [T.Text]
                        splitup = T.splitOn "#" s

                        transformTag :: T.Text -> T.Text
                        transformTag s  =
                            if S.member (T.takeWhile (\c -> C.isAlphaNum c || c == '-') s) allIDs
                                then T.concat ["equation-", T.pack (show num), "-", s] 
                                else s

        defsDoc :: X.Document
        defsDoc = 
            X.Document
                (X.Prologue [] Nothing [])
                (X.Element
                    "{http://www.w3.org/2000/svg}svg"
                    (M.fromList 
                        [ ("version", "1.2")
                        , ("height", "0pt")
                        , ("width",  "0pt")
                        , ("viewBox", "0 0 0 0")
                        , ("style", "display: none;")
                        ]) 
                    [X.NodeElement defsElement]) []
            where
                defsElement :: X.Element
                defsElement = 
                    X.Element
                        "{http://www.w3.org/2000/svg}defs"
                        M.empty
                        (concat $ query (namedElementNodes "{http://www.w3.org/2000/svg}defs") svg)


