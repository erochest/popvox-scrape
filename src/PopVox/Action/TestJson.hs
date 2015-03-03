{-# LANGUAGE OverloadedStrings #-}


module PopVox.Action.TestJson
    ( testJSON
    ) where


import           Control.Error
import           Control.Monad             (forM_)
import           Data.Text.Format          hiding (print)
import qualified Data.Text.Format          as F
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, mapM)

import           PopVox.MapLight
import           PopVox.Types              hiding (maplightAPIDir)


testJSON :: FilePath -> IO ()
testJSON maplightAPIDir = forM_ sessions $ \s -> do
    F.print "\nQuerying for session {}... " $ Only s
    out <- runEitherT $ billList maplightAPIDir s
    case out of
        Right _ -> putStrLn "ok"
        Left e  -> F.print "ERROR: {}\n" $ Only e
