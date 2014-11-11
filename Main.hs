{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Data.Version
import           Options.Applicative

import           Paths_popvox_scrape
import           PopVox.Types


main :: IO ()
main = print =<< execParser opts


opts' :: Parser PopVoxOptions
opts' = pure PopVoxOptions

opts :: ParserInfo PopVoxOptions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Assemble a dataset of bill positions and\
                        \ political contributions."
            <> header ("popvox-scrape v" ++ showVersion version))
