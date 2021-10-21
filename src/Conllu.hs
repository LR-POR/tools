{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Conllu where

import Conllu.IO ( readConlluFile, writeConlluFile )
import Conllu.Type
import qualified Conllu.UposTagset as U


import qualified Type as Tp
import qualified MorphoTools as MT
import Data.Either
import Data.Char
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Data.List (groupBy, intercalate, sort, nub, sortOn,filter)
import Data.List.Split (splitOn, splitPlaces, chunksOf)
import Data.Maybe ( fromJust, isNothing )
import qualified Text.Regex as R

getClPath :: FilePath -> IO [FilePath]
getClPath p = do
    ps <- listDirectory p
    return (aux ps)
 where
     aux (x:xs)
      |takeExtension x == ".conllu" = x : aux xs
      | otherwise = aux xs
     aux [] = []

getID :: [(String,String)] -> String
getID (x:xs)
 | fst x == "sent_id " = snd x
 | otherwise = getID xs
getID [] = ""

check :: M.Map T.Text [(T.Text, T.Text)] -> String -> [CW AW] -> [T.Text]
check morpho sentID (x:xs)
 | isNothing (_upos x) = check morpho sentID xs
 | (fromJust (_upos x) == U.VERB) || (fromJust (_upos x) == U.AUX) = auxcheck morpho x : check morpho sentID xs
 | otherwise = check morpho sentID xs
 where
     auxcheck morpho x
        | isNothing (M.lookup (T.pack $ fromJust $ _lemma x) morpho) =
            T.pack (sentID ++"\t" ++ fromJust (_lemma x)  ++ "\t" ++ fromJust (_form x) ++ "\t*") 
        |notElem (T.toLower $ T.pack $ fromJust ( _form x)) (map fst (fromJust (M.lookup (T.pack $ fromJust (_lemma x)) morpho))) =
            T.pack (sentID ++"\t"++ fromJust (_lemma x) ++ "\t" ++ fromJust (_form x))
        | otherwise = ""
check morpho sentID [] = []

checkVerbs :: FilePath -> FilePath -> FilePath -> IO ()
checkVerbs vpath cldirpath outpath = do
    paths <- listDirectory vpath
    dicts <- mapM (MT.morphoMap . combine vpath) paths
    clpaths <- getClPath cldirpath
    txt <- mapM (aux cldirpath (foldr (M.unionWith (++)) M.empty dicts)) clpaths
    TO.writeFile outpath (T.intercalate "\n" $ concatMap (filter (/= "") ) txt)
 where
     aux cldirpath dicts clpath = do
         cldoc <- readConlluFile (combine cldirpath clpath) 
         return (concatMap (\x -> check dicts (getID $ _meta x) (_words x)) cldoc)