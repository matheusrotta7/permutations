{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Genomes
-- Description : Representation of a genome
-- Copyright   : (c) Gabriel Siqueira, 2021
-- License     : BSD3
-- Maintainer  : gabriel.gabrielhs@gmail.com
--
-- A genome comprises of a gene list and a integer list (correspondent to the sizes of intergenic regions).
module Genomes
  ( Genome,
    Gene,
    IR (..),
    Idx (..),
    intToGene,
    rearrangeGenome,
    writeGenome,
    reversal,
    transposition,
    swap,
    insert,
    toGeneList,
    fromGeneList,
  )
where

import Control.Exception (assert)
import Control.Monad.Random (MonadRandom, getRandomRs, getRandoms)
import Data.ByteString.Builder (intDec, toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Coerce (coerce)
import Data.Foldable (foldl', toList)
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import LocalBase
import System.Random (Random)
import System.Random.Shuffle (shuffleM)

newtype IR = IR Int deriving newtype (Eq, Show, Read, Num, Ord, Random)

newtype Idx = Idx Int deriving newtype (Eq, Show, Read, Num, Ord, Enum, Random, Integral, Real)

newtype Gene = Gene Int deriving newtype (Eq, Enum, Show, Read, Hashable, Ord, Num, Random)

-- | Representation of a genome, must be a non empty sequence of genes
type Genome = Vector Gene

genomeSize :: Genome -> Size
genomeSize = Size . Vec.length

--------------------------------
--      Random Generation     --
--------------------------------

rearrangeGenome :: MonadRandom mon => Genome -> mon Genome
rearrangeGenome g = do
  let ls = toGeneList True g
  ls' <- shuffleM ls
  return $ fromGeneList True ls'

--------------------------
--      Conversions     --
--------------------------

writeGenome :: Bool -> Genome -> ByteString
writeGenome rext = BS.unwords . fmap (toLazyByteString . intDec . coerce) . toGeneList rext

intToGene :: Int -> Gene
intToGene = coerce

geneToBS :: Gene -> ByteString
geneToBS = toLazyByteString . (<>) "g" . intDec . coerce

irToBS :: IR -> ByteString
irToBS = toLazyByteString . (<>) "i" . intDec . coerce

fromGeneList :: Bool -> [Gene] -> Genome
fromGeneList extend ls_ = Vec.fromList ls
  where
    ls =
      if extend
        then 0 : (ls_ ++ [if null ls_ then 1 else maximum ls_ + 1])
        else ls_

toGeneList :: Bool -> Genome -> [Gene]
toGeneList rext g = Vec.toList . (if rext then Vec.slice 1 (coerce $ genomeSize g - 2) else id) $ g

------------------------------------------
--           Operations                 --
------------------------------------------

transposition :: Idx -> Idx -> Idx -> Genome -> Genome
transposition i j k g =
  assert (2 <= i)
    . assert (i < j)
    . assert (j < k)
    . assert (k <= coerce (genomeSize g))
    $ g'
  where
    g' = Vec.modify updateG g

    updateG v =
      do
        aux1 <- MVec.clone . MVec.slice (coerce i - 1) (coerce $ j - i) $ v
        aux2 <- MVec.clone . MVec.slice (coerce j - 1) (coerce $ k - j) $ v
        MVec.move (MVec.slice (coerce i - 1) (coerce $ k - j) v) aux2
        MVec.move (MVec.slice (coerce $ i + k - j - 1) (coerce $ j - i) v) aux1

reversal :: Idx -> Idx -> Genome -> Genome
reversal i j g =
  assert (2 <= i)
    . assert (i < j)
    . assert (j <= coerce (genomeSize g) - 1)
    $ g'
  where
    g' = Vec.modify updateG g

    updateG v = do
      mapM_ (\k -> MVec.swap v (coerce $ i + k - 1) (coerce $ j - k - 1)) [0 .. (j - i + 1) `div` 2 - 1]

swap :: Idx -> Idx -> Genome -> Genome
swap i j g =
  assert (2 <= i)
    . assert (i < j)
    . assert (j <= coerce (genomeSize g) - 1)
    $ g'
  where
    g' = Vec.modify updateG g

    updateG v = MVec.swap v (coerce i - 1) (coerce j - 1)

insert :: Idx -> Idx -> Genome -> Genome
insert i j g =
  assert (2 <= i)
    . assert (i <= coerce (genomeSize g) - 1)
    . assert (2 <= j)
    . assert (j <= coerce (genomeSize g) - 1)
    $ g'
  where
    g' = Vec.modify updateG g

    updateG v =
      if i < j
        then do
          aux1 <- MVec.clone . MVec.slice (coerce i - 1) (coerce $ j - i + 1) $ v
          MVec.swap v (coerce i - 1) (coerce j - 1)
          MVec.move (MVec.slice (coerce i - 1) (coerce $ j - i) v) (MVec.tail aux1)
        else do
          aux1 <- MVec.clone . MVec.slice (coerce j - 1) (coerce $ i - j + 1) $ v
          MVec.swap v (coerce i - 1) (coerce j - 1)
          MVec.move (MVec.slice (coerce j) (coerce $ i - j) v) (MVec.init aux1)
