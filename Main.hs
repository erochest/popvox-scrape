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
            <> progDesc "Scrapes data from popvox.com and \
                        \mashes it with opensecrets.org data."
            <> header ("popvox-scrape v" ++ showVersion version))
