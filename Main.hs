{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import           Control.Monad
import qualified Data.Text.Format as F
import           Prelude          hiding (FilePath)
import           System.CPUTime

import           Opts
import           PopVox



main :: IO ()
main = do
    pvOpts <- execParser opts
    start <- getCPUTime

    -- individualReport pvOpts
    committeeReport pvOpts

    end <- getCPUTime
    when (_popVoxVerbose pvOpts) $ do
        let elapsed = fromIntegral (end - start) / ((10^12) :: Double)
        F.print  "Elapsed: {} sec\n" $ F.Only elapsed
