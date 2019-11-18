{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-|
Module      : Verilog.Align
Description : Align text into columns
Copyright   : (c) Jonathan Tanner, 2019
Licence     : GPL-3
Maintainer  : jonathan.tanner@sjc.ox.ac.uk
Stability   : experimental
-}
module Verilog.Align (combineLines, combineLines') where

import ClassyPrelude
import Data.List (transpose)

padRightTo :: Int -> Char -> Text -> Text
padRightTo n x ys = ys ++ replicate (n - length ys) x

align :: Int -> Char -> Text -> (Text, Text) -> Text
align n p sep (s1, s2)
  | null s2   = s1
  | otherwise = padRightTo n p s1 ++ sep ++ s2

align' :: [Int] -> Char -> Text -> [Text] -> Text
align'  _     _ _    []    = ""
align'  _     _ _    [s]   = s
align' (n:ns) p sep (s:ss) = padRightTo n p s ++ sep ++ align' ns p sep ss
align'  []    _ _   (_:_)  = error "ns shorter than ss"

alignLines :: Char -> Text -> [(Text, Text)] -> [Text]
alignLines p colSep ls = map (align indent p colSep) ls
  where indent :: Int
        indent = maximum . ncons 0 . map (length . fst) $ ls

alignLines' :: Char -> Text -> [[Text]] -> [Text]
alignLines' p colSep ls = map (align' indents p colSep) ls
  where indents :: [Int]
        indents = map (maximum . ncons 0 . map length) . transpose $ ls

combineLines :: Char -> Text -> Text -> [(Text, Text)] -> Text
combineLines p colSep lineSep = intercalate lineSep . alignLines p colSep

combineLines' :: Char -> Text -> Text -> [[Text]] -> Text
combineLines' p colSep lineSep = intercalate lineSep . alignLines' p colSep

