module PopVox.Contribs
    ( majorParty
    , indexContribs'
    ) where


import qualified Data.Vector     as V

import           PopVox.MapLight
import           PopVox.Types


majorParty :: Party -> Bool
majorParty Dem = True
majorParty GOP = True
majorParty _   = False

indexContribs' :: (Header, V.Vector OrgContrib) -> OrgContribIndex
indexContribs' = indexContribs
               . V.filter (majorParty . contribParty . orgContribEntry)
               . snd

