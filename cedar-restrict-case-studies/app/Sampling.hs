module Sampling where

import Control.Monad.State.Strict
import Data.Function
import Data.List
import System.Random

import Config

privilegeRepresentation :: (Int, Int) -> Config -> (Int, Int)
privilegeRepresentation (cardIdealPriv, cardOverPriv) conf =
  (reasonableIdealPrivRep, reasonableOverPrivRep)
  where
    targetActualPrivRep :: Int
    targetActualPrivRep = ceiling $
      fromIntegral (cardIdealPriv + cardOverPriv) * (conf & priv_rep_ratio)

    targetIdealPrivRep :: Int
    targetIdealPrivRep = ceiling $
      fromIntegral (targetActualPrivRep * (100 - (conf & over_priv_percent))) / 100.0

    -- It could be that the size of the over-privilege is large compared to the
    -- ideal privilege; no reason to sample the ideal privilege more times than
    -- its cardinality
    reasonableIdealPrivRep :: Int
    reasonableIdealPrivRep = min targetIdealPrivRep cardIdealPriv

    reasonableOverPrivRep :: Int
    reasonableOverPrivRep = floor $
      fromIntegral reasonableIdealPrivRep
      * (100.0 / fromIntegral (100 - (conf & over_priv_percent)) - 1)

privilegeRepresentationDelta :: (Int,Int) -> (Int,Int) -> (Int,Int)
privilegeRepresentationDelta (ni, no) (oi, oo) = ( ni - oi , no - oo )

samplePrincipals :: RandomGen g => (c -> [e]) -> (c,c) -> Config -> State g [e]
samplePrincipals sel (delta, pool) conf = do
  choice <- state $ randomR (0.0, 1.0)
  let (deltaPrincipals, poolPrincipals) = (delta & sel, pool & sel)
  if choice <= (conf & sample_bias_new_principal) && not (null deltaPrincipals)
    then return deltaPrincipals
    else return poolPrincipals
