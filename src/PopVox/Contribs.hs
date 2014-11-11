module PopVox.Contribs
    ( listContribs
    ) where


import           PopVox.Types


listContribs :: [Int] -> [ContribEntry]
listContribs years =
    [ Contrib p t y | y <- years, t <- [minBound..maxBound], p <- [minBound..maxBound] ]

