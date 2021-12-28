--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--------------------------------------------------------------------------------

module Compilers 
    ( pandocCompiler
    , pandocLaTeXCompiler
    , buildLatex
    ) where

import           Hakyll                        hiding (pandocCompiler)
import qualified Text.Pandoc                   as P
import           System.Process                (system)
import qualified Data.Text                     as T
import           GHC.IO                        (unsafePerformIO)
import           System.FilePath               (takeBaseName, replaceExtension, takeDirectory)


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

pandocReaderOptions :: P.ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions

pandocHTMLWriterOptions :: P.WriterOptions
pandocHTMLWriterOptions = defaultHakyllWriterOptions
    {   P.writerHTMLMathMethod = 
            P.MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    }

pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompilerWith pandocReaderOptions pandocHTMLWriterOptions

--------------------------------------------------------------------------------

writePandocTyped :: (P.Pandoc -> P.PandocPure T.Text)
                 -> Item P.Pandoc -> Item String
writePandocTyped writer (Item itemi doc) =
    case P.runPure $ writer doc of
        Left err    -> error $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
        Right item' -> Item itemi $ T.unpack item'

writePandocLaTeX :: Item P.Pandoc -> Item String
writePandocLaTeX = writePandocTyped (P.writeLaTeX P.def)

renderPandocTypedTransformM :: P.ReaderOptions -> (P.Pandoc -> P.PandocPure T.Text)
                            -> (P.Pandoc -> Compiler P.Pandoc)
                            -> Item String -> Compiler (Item String)
renderPandocTypedTransformM ropt writer trans item = 
    writePandocTyped writer <$> (readPandocWith ropt item >>= traverse trans)

pandocCompilerTypedTransformM :: P.ReaderOptions -> (P.Pandoc -> P.PandocPure T.Text)
                              -> (P.Pandoc -> Compiler P.Pandoc)
                              -> Compiler (Item String)
pandocCompilerTypedTransformM ropt writer f = 
    getResourceBody >>= renderPandocTypedTransformM ropt writer f

pandocCompilerTyped :: P.ReaderOptions -> (P.Pandoc -> P.PandocPure T.Text)
                    -> Compiler (Item String)
pandocCompilerTyped ropt writer = pandocCompilerTypedTransformM ropt writer return

pandocLaTeXCompiler :: Compiler (Item String)
pandocLaTeXCompiler = getResourceBody  >>= renderPandocLaTeX

renderPandocLaTeX :: Item String -> Compiler (Item String)
renderPandocLaTeX = renderPandocTypedTransformM defaultHakyllReaderOptions (P.writeLaTeX P.def) return

    
