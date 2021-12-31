--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--------------------------------------------------------------------------------

module Compilers 
    ( pandocHTMLCompiler
    , pandocLaTeXCompiler
    , buildLatex
    ) where

import           Hakyll
import           Text.Pandoc
import           Text.Pandoc.Walk
import qualified Data.Text as T                (Text, unpack)
import qualified Data.ByteString.Lazy as LB    (ByteString, readFile)
import           Data.Map                      (union)
import           GHC.IO                        (unsafePerformIO)
import           System.Process                (system)
import           System.Directory              (createDirectory, setCurrentDirectory)
import           System.FilePath               (replaceExtension, takeDirectory, 
                                                dropExtensions, (</>))
import           Control.Monad                 (forM_)
import           Text.Pandoc.Shared            (filteredFilesFromArchive)
import           Text.Pandoc.Builder           (toMetaValue)
import           Text.Pandoc.Parsing           (runF, defaultParserState)
import           Text.Pandoc.Readers.Markdown  (yamlToMeta)

buildLatex :: Item String -> Compiler (Item TmpFile)
buildLatex item = do
    TmpFile texPath <- newTmpFile "lualatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- system $ unwords ["lualatex", "-halt-on-error",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        return ()

    makeItem $ TmpFile pdfPath

--------------------------------------------------------------------------------

pandocReaderOptions :: ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions

pandocHTMLWriterOptions :: WriterOptions
pandocHTMLWriterOptions = defaultHakyllWriterOptions
    {   writerHTMLMathMethod = 
            MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    }

--------------------------------------------------------------------------------

writePandocTyped :: (Pandoc -> PandocPure T.Text)
                 -> Item Pandoc -> Item String
writePandocTyped writer (Item itemi doc) =
    case runPure $ writer doc of
        Left err    -> error $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
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
writePandocLaTeX = writePandocTyped (writeLaTeX def)

renderPandocLaTeX :: Item String -> Compiler (Item String)
renderPandocLaTeX = renderPandocTypedTransformM defaultHakyllReaderOptions (writeLaTeX def) return

pandocLaTeXCompiler :: Compiler (Item String)
pandocLaTeXCompiler = getResourceBody  >>= renderPandocLaTeX

--------------------------------------------------------------------------------
-- A Writeable instance to represent an HTML output accompanied by SVG images
-- generated from displayed tikz code.

data HtmlBundle = HtmlBundle String [String]

instance Writable HtmlBundle where
    write fn item = do
        writeFile fn html
        removeDirectory imgsDir
        if null imgs
            then return ()
            else do
                createDirectory imgsDir
                setCurrentDirectory imgsDir
                forM_ (zip [1..] imgs) $ \(n,img) ->
                    writeFile ("img" ++ show n ++ ".svg") img
        where
            HtmlBundle html imgs = itemBody item
            imgsDir = dropExtensions fn

-------------------------------------------------------------------------------
-- Pandoc filters to convert raw blocks marked with the keyword "diagram" and 
-- process the displayed tikz code they contain to create SVG images. 

filterDiagramsToLaTeX :: Pandoc -> Pandoc 
filterDiagramsToLaTeX = walk toRawLaTeX
    where
        toRawLaTeX :: Block -> Block
        toRawLaTeX (RawBlock (Format "definition") text) = 
            RawBlock (Format "latex") text


{-# NOINLINE latexOptions #-}
latexOptions :: Meta
latexOptions = unsafePerformIO $ do
    yaml <- LB.readFile "pandoc/latexOptions.yaml"
    runIOorExplode $ yamlToMeta pandocReaderOptions Nothing yaml

applyLatexOptions :: Pandoc -> Pandoc 
applyLatexOptions (Pandoc meta body) = 
    Pandoc (Meta (unMeta meta `union` unMeta latexOptions)) body

pandocHTMLCompiler :: Compiler (Item String)
pandocHTMLCompiler = pandocCompilerWith pandocReaderOptions pandocHTMLWriterOptions
