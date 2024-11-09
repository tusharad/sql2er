{-# LANGUAGE FlexibleContexts #-}

module Sql2er.ERDiagram where

import qualified Data.Text as T
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Sql2er.Common.Types

tableDiagram :: Table -> Diagram B
tableDiagram (Table name cols _) = do
  let w = decideTableWidth $ findLongestStringLength name cols
  let colWidth = fromIntegral w
  vsep 0 ((text (T.unpack name) <> rect colWidth 1.8 # fc green) : map (columnDiagram colWidth) cols)
    # named (T.unpack name) # fc black
  where
    findLongestStringLength n1 = helper (T.length n1) 
    helper maxCount [] = maxCount
    helper maxCount (c:cs) = helper (max maxCount (T.length $ columnName c)) cs
    columnDiagram colWidth (Column cname _ constraints) =
      let cText = T.unpack cname ++ concatMap showConstraint constraints
       in text cText <> rect colWidth 1.8 # fc yellow
    showConstraint PrimaryKey = " (PK)"
    showConstraint (ReferencesColumn _ _) = " (FKey)"
    showConstraint Unique = " unique"
    showConstraint NotNull = " not null"
    showConstraint (Default x) = " " <> T.unpack x
    showConstraint _ = ""

decideTableWidth :: Int -> Int
decideTableWidth n 
    | n <= 8 = 8
    | n >= 9 && n <= 12 = 10
    | n >= 13 && n <= 17 = 12
    | otherwise = 14

renderErDiagram :: FilePath -> Double -> Double -> [Table] -> IO ()
renderErDiagram outputFilePath width_ height_ tableList = do
  renderSVG outputFilePath (dims2D width_ height_) (erDiagram tableList)

erDiagram :: [Table] -> Diagram B
erDiagram ts = do
  let tablesList = divideList 3 ts
  vsep 2.5 (map (hsep 3.5 . map tableDiagram) tablesList)
    # mconcat (map (\(t1, t2) -> connectOutside (T.unpack t1) (T.unpack t2)) (getTableConnections ts))

getTableConnections :: [Table] -> [(TableName, TableName)]
getTableConnections [] = []
getTableConnections (t : ts) = getConnections t <> getTableConnections ts
  where
    getConnections :: Table -> [(TableName, TableName)]
    getConnections t' =
      map
        ( \col -> do
            let constraints = cConstraints col
            mconcat $
              map
                ( \constraint -> do
                    case constraint of
                      ReferencesColumn connectedT _ -> (tableName t', connectedT)
                      _ -> mempty
                )
                constraints
        )
        (columns t)

divideList :: Int -> [a] -> [[a]]
divideList n xs = helper xs []
  where
    helper :: [a] -> [[a]] -> [[a]]
    helper [] res = res
    helper lst res = let (nElems, rest) = splitAt n lst in helper rest (nElems : res)
