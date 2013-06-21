{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad

import Data.Array.IO
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (pack)

import System.Environment
import System.Random

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs

main = do
    names <- getArgs
    let nodes = zip [1..length names] names
    shuffled_verts <- cycle <$> shuffle [1..length names]
    let edges = take (length names) $ zip3 shuffled_verts (tail shuffled_verts) (repeat ())
        g :: Gr String () = mkGraph nodes edges
    let params :: GraphvizParams Node String () () String
        params = defaultParams { isDirected = True, fmtNode = \(_,l) -> [(Label . StrLabel . pack) l] }
    print (graphToDot params g)
    runGraphviz (graphToDot params g) Svg "review-graph.svg"

