module PwPlayer.Stereo where

import Data.Vector.Storable as SV
import Foreign

newtype MonoSample = MonoSample Float
    deriving (Show)

data StereoSample = StereoSample {left, right :: {-# UNPACK #-} !Float}
    deriving (Show)

addStereoSample :: StereoSample -> StereoSample -> StereoSample
addStereoSample a b = StereoSample (a.left + b.left) (a.right + b.right)

instance Storable StereoSample where
    sizeOf _ = 8
    alignment _ = 8
    peek ptr = StereoSample <$> peek (castPtr ptr) <*> peek (castPtr ptr `plusPtr` 4)
    poke ptr ss = poke (castPtr ptr) ss.left >> poke (castPtr ptr `plusPtr` 4) ss.right

-- From https://dsp.stackexchange.com/a/21736
monoToStereo :: Float -> MonoSample -> StereoSample
monoToStereo pan (MonoSample s) = StereoSample left right
  where
    angle = pan * 45 * pi / 180
    left = s * (sqrt 2 / 2) * (cos angle - sin angle)
    right = s * (sqrt 2 / 2) * (cos angle + sin angle)

-- From https://forum.cockos.com/showthread.php?t=49809
applyPan :: Float -> StereoSample -> StereoSample
applyPan pan ss = StereoSample left right
  where
    dbConv = 8.6858896380650365530225783783335
    lawdb = 3
    centergain = exp (-lawdb / dbConv)

    panatt = abs pan
    baseADJ = centergain + (1.0 - centergain) * (2.0 / (2.0 - panatt) - 1.0)
    (adj0, adj1)
        | pan < 0 = (baseADJ, baseADJ * (1 + pan))
        | pan > 0 = (baseADJ * (1 - pan), baseADJ)
        | otherwise = (baseADJ, baseADJ)

    left = ss.left * adj0
    right = ss.right * adj1
