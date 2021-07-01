{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module DB where

import Control.Monad (replicateM, (>=>))
import Control.Monad.Random (Rand, StdGen, evalRandIO, getRandomR, getRandoms)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Coerce (coerce)
import Data.List (unfoldr)
import Genomes
import LocalBase
import Options.Applicative

opts :: ParserInfo Args
opts =
  info
    (argParser <**> helper)
    ( fullDesc
        <> progDesc "Generate database with genomes. Used for tests of solutions for rearrangement problems."
    )

data OpType = Rev | Trans | Swap | Insertion | RevTrans | SwapInsertion deriving (Read, Show)
data OneOp = Rev_ | Trans_ | Swap_ | Insertion_

splitOpType :: OpType -> (OneOp, OneOp)
splitOpType Rev = (Rev_, Rev_)
splitOpType Trans = (Trans_, Trans_)
splitOpType Swap = (Swap_, Swap_)
splitOpType Insertion = (Insertion_, Insertion_)
splitOpType RevTrans = (Rev_, Trans_)
splitOpType SwapInsertion = (Swap_, Insertion_)

data Args = Args
  { db_op_type :: OpType,
    db_num_pairs :: Int,
    db_size :: Size,
    db_nop :: Int,
    db_porc :: Int,
    db_output :: String
  }

argParser :: Parser Args
argParser =
  Args
    <$> option
      auto
      ( long "operations_type"
          <> short 't'
          <> metavar "OP"
          <> help "Type of operation(s) to apply"
      )
    <*> option
      auto
      ( long "number_genomes"
          <> short 'k'
          <> metavar "K"
          <> help "Number genome pairs to generate."
      )
    <*> option
      auto
      ( long "size_genome"
          <> short 'n'
          <> metavar "N"
          <> help "Size of the genomes."
      )
    <*> option
      auto
      ( long "number_op"
          <> short 'r'
          <> metavar "R"
          <> help "Number of operations to apply (-1 to use a random list)."
      )
    <*> option
      auto
      ( long "porcentage_rev"
          <> short 'p'
          <> metavar "P"
          <> showDefault
          <> value 100
          <> help "Percentage of first type of operation."
      )
    <*> strOption
      ( long "outfile"
          <> short 'o'
          <> metavar "oFILE"
          <> help "Output file"
      )

main :: IO ()
main = do
  args <- execParser opts
  db <- evalRandIO . fmap BS.unlines $ replicateM (db_num_pairs args) (genGenome args)
  BS.writeFile (db_output args) db

genGenome :: Args -> Rand StdGen ByteString
genGenome Args {..} = do
  let g = fromGeneList True [1 .. (intToGene $ coerce db_size)]
  h <-
    if db_nop == -1
      then rearrangeGenome g
      else applyOperations g
  return $ writeGenome True h
  where
    (op1,op2) = splitOpType db_op_type
    r_r = (db_nop * db_porc) `div` 100
    r_t = db_nop - r_r

    applyOperations :: Genome -> Rand StdGen Genome
    applyOperations g = do
      coins <- getRandoms
      let ops = unfoldr operations_for_one (r_t, r_r, coins)
      foldr (=<<) (return g) ops
    operations_for_one :: (Int, Int, [Bool]) -> Maybe (Genome -> Rand StdGen Genome, (Int, Int, [Bool]))

    operations_for_one (_, _, []) = Nothing
    operations_for_one (r_t', r_r', coin : coins)
      | r_t' == 0 && r_r' == 0 = Nothing
      | r_t' == 0 || r_r' /= 0 && coin = Just . (,(r_t', r_r' - 1, coins)) $ \g -> do
        applyOperation op1 g
      | otherwise = Just . (,(r_t' - 1, r_r', coins)) $ \g -> do
        applyOperation op2 g

    applyOperation Rev_ g = do
        i <- getRandomR (2 :: Idx, coerce db_size - 2)
        j <- getRandomR (i + 1, coerce db_size - 1)
        return $ reversal i j g
    applyOperation Trans_ g = do
        i <- getRandomR (2 :: Idx, coerce db_size - 2)
        j <- getRandomR (i + 1, coerce db_size - 1)
        k <- getRandomR (j + 1, coerce db_size)
        return $ transposition i j k g
    applyOperation Swap_ g = do
        i <- getRandomR (2 :: Idx, coerce db_size - 2)
        j <- getRandomR (i + 1, coerce db_size - 1)
        return $ swap i j g
    applyOperation Insertion_ g = do
        i <- getRandomR (2 :: Idx, coerce db_size - 1)
        j <- getRandomR (2 :: Idx, coerce db_size - 1)
        return $ insert i j g
