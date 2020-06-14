{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
-- import SBF.Compiler (Program (..))

import GHC.Stack (HasCallStack)
import Options.Applicative ((<**>), Parser)
import qualified Options.Applicative as O
import SBF.Compiler as C
import SBF.Compiler.Pass (CompilerPass (..))
import SBF.Compiler.Pass.ClearZero (ClearZero (..))
import SBF.Compiler.Pass.RLE (RLE (..))
import SBF.Interpreter as I

parseOpts :: Parser C.CompileOpts
parseOpts =
  C.CompileOpts
    <$> O.strOption (O.long "input" <> O.short 'i' <> O.metavar "INPUT" <> O.help "input file" <> O.action "file")
    <*> O.strOption (O.long "output" <> O.short 'o' <> O.metavar "OUTPUT" <> O.help "output file" <> O.showDefault <> O.value "a.ll")
    <*> O.switch (O.long "run" <> O.short 'r' <> O.help "run file in interpreter" <> O.showDefault)

mainBody :: HasCallStack => CompileOpts -> IO ()
mainBody co = do
  input <- B.readFile (inFile co)
  C.compile co [CompilerPass RLE, CompilerPass ClearZero] input >>= \case
    Interpreted i -> I.run i
    Compiled -> return ()

main :: HasCallStack => IO ()
main =
  mainBody
    =<< O.execParser
      ( O.info
          (parseOpts <**> O.helper)
          (O.fullDesc <> O.progDesc "an optimizing BF compiler")
      )
