{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    ) where

-- https://github.com/lhartikk/naivechain

import Block
import Transact
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar, MVar, readMVar)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever, forM_, replicateM_)
import System.Random (getStdGen, randomR)
import Data.Time (getCurrentTime)

type Miner = ((MVar [Block]), String)

createMiner :: [Miner] -> String -> Int -> IO Miner
createMiner chain_refs name speed = do
  putStrLn $ name ++ "さんが登場しました"
  my_chain_ref <- newMVar [initial]
  g <- getStdGen
  forkIO $ loop g my_chain_ref
  return (my_chain_ref, name)
  where
    initial :: Block
    initial = initialBlock time1

    update, accept :: MVar [Block] -> IO ()

    accept my_chain_ref = forM_ chain_refs $ \(ref, central_name) -> do
      central <- readMVar $ ref
      mine <- takeMVar my_chain_ref
      if (length central) > (length mine)
         then if (verifyChain initial central)
              then do
                putStrLn $ name ++ "さんが" ++ central_name ++ "さんのチェーンを受け入れました:" ++ (show $ length central) ++ ">" ++ (show $ length mine)
                putMVar my_chain_ref (central)
              else do
                putMVar my_chain_ref (mine)
                putStrLn $ central_name ++"は偽チェーン" ++ showBlock (last mine) ++ "vs." ++ showBlock (last central) ++ show (last mine == last central)
         else do
           putMVar my_chain_ref mine
           putStrLn $ name ++ "さんが" ++ central_name ++ "さんのチェーンを受け入れませんでした:" ++ (show $ length central) ++ "<" ++ (show $ length mine)

    update my_chain_ref = do
      (a:mine) <- takeMVar my_chain_ref
      now <- getCurrentTime
      let new1 = nextBlock now (name++"の!") a
      putMVar my_chain_ref (new1 : a : mine)
      putStrLn $ name ++ "さんがアップデートしました:\n" ++ (showBlock new1)

    loop g my_chain_ref = do
      let (n, g') = randomR (1, 4) g
      let (times, g'') = randomR (1, 4) g'
      threadDelay $ n * speed
      accept my_chain_ref
      replicateM_ times $ update my_chain_ref
      loop g'' my_chain_ref


someFunc :: IO ()
someFunc = do
  rec
    chain_refs <- mapM (uncurry $ createMiner chain_refs) [("a", 23*(10^5)), ("b", 19*(10^5)), ("c", 13*(10^5)), ("d", 17*(10^5))]
  threadDelay $ 30 * (10^6)

  products <- mapM takeMVar $ map fst chain_refs
  let names = map snd chain_refs
  forM_ (zip products names) $
    \(product, name) -> do
      putStrLn "\n\n\n\n\n"
      putStrLn $ name ++ "さん"
      putStrLn $ concat $ map showBlock product
      let len = length product
      putStrLn $ "平均ノンス長: " ++ (show $ (`div` len) $ foldl1 (+) $ map (b_nonce) product)
  return ()


  {- let chain1 = makeChain initial 10
  print $ verifyBlock (chain1 !! 1) (chain1 !! 0)
  print $ verifyChain initial chain1

  print $ (bsToHex $ b_hash initial)
  print $ (bsToHex $ blockHash initial)

  print $ (bsToHex $ b_hash (chain1 !! 1))
  print $ (bsToHex $ blockHash (chain1 !! 1)) -}


  where
    printLen :: (Show a) => [a] -> IO ()
    printLen a = do
      putStrLn $ (show a) ++ " (len:" ++ (show $ length $ a) ++ ")"

    initial :: Block
    initial = initialBlock time1
