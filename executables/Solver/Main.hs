module Main where

import HidatoLib
import qualified Data.Map as M
import qualified Data.IntSet as S
import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
    {  file :: !String
    }

main :: IO()
main = do
    opts <- execParser optsParser
    putStrLn (concat ["Using ", file opts, " as selected file"])
    
    b <- parseGame $ file opts
    let sols = solve b
    if isNothing $ sols then putStrLn "There is not solution" else putStrLn $ printBoard $ head $ fromJust $ sols
   
optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <> progDesc "Hidato haskell implementation" <>
         header
             "Hidato - Project of haskell")
versionOption :: Parser (a -> a)
versionOption = infoOption "Hidato Version 0.1" (long "version" <> help "Show version")
programOptions :: Parser Opts
programOptions =
    Opts <$> strOption
        (long "file" <> metavar "VALUE" <> value "elipse" <>
         help "file with generated hidato")
