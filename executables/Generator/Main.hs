module Main where

import HidatoLib
import qualified System.Random as R

import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
    {  	template   :: !String,
	board_name :: !String,
	rotate     :: Int
    }

main :: IO ()
main = do
    opts <- execParser optsParser
    putStrLn (concat ["Using ", template opts, " as selected file"])
    putStrLn (concat ["Rotating ", show $ rotate opts, " time(s)"])
    let fileName = template opts 
    let name = board_name opts 
    board <- parseBoard fileName
    let boardRotated = rotateBoardN board $ rotate opts
    numberedBoard <- gameGenerator boardRotated
    putStrLn $ printBoard numberedBoard
    writeFile name $ printBoard numberedBoard
    
optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> versionOption <*> programOptions)
        (fullDesc <> progDesc "Hidato Generator haskell implementation" <>
         header
             "Hidato - Project of haskell")
versionOption :: Parser (a -> a)
versionOption = infoOption "Hidato Generator Version 0.1" (long "version" <> help "Show version")
programOptions :: Parser Opts
programOptions =
    Opts <$> strOption
        (long "template" <> metavar "VALUE" <> value "templates/Elipse.txt" <>
         help "Selected template to generate Board")
	<*> strOption
        (long "board_name" <> metavar "VALUE" <> value "board" <>
         help "Name of generated value")
	<*> option auto
        (long "rotate" <> metavar "INT" <> value 0 <>
         help "Rotate entry 90 degrees n times")
