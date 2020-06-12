module Main where

import Options.Applicative ((<**>), Parser)
import qualified Options.Applicative as O
import SBF.Compiler as C

parseOpts :: Parser C.CompileOpts
parseOpts =
  C.CompileOpts
    <$> (O.strOption (O.long "input" <> O.short 'i' <> O.metavar "INPUT" <> O.help "input file" <> O.action "file"))
    <*> (O.strOption (O.long "output" <> O.short 'o' <> O.metavar "OUTPUT" <> O.help "output file" <> O.showDefault <> O.value "a.ll"))
    <*> O.switch (O.long "run" <> O.short 'r' <> O.help "run file in interpreter" <> O.showDefault)

mainBody :: C.CompileOpts -> IO ()
mainBody co = print co

main :: IO ()
main =
  mainBody
    =<< O.execParser
      ( O.info
          (parseOpts <**> O.helper)
          (O.fullDesc <> O.progDesc "an optimizing BF compiler")
      )
