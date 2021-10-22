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
import Data.List (groupBy, intercalate, sort, nub, sortOn,filter,sortBy)
import Data.List.Split (splitOn, splitPlaces, chunksOf)
import Data.Maybe ( fromJust, isNothing )
import qualified Text.Regex as R


tags2feats :: FilePath -> IO (M.Map T.Text [(T.Text,[T.Text])])
tags2feats path = do
    doc <- TO.readFile path
    return $ M.fromListWith (++) $ aux (T.lines doc)
 where
    aux xs = map (\s -> let p = (map (\x -> (head $ T.splitOn "=" x, [last $ T.splitOn "=" x]))
                                $ T.splitOn "," (last $ T.splitOn "\t" s))
        in ((head $ T.splitOn "\t" s) , p)) xs

uniFS :: M.Map T.Text [(T.Text,[T.Text])] -> [Feat] -> [T.Text] -> Bool
uniFS m fs (x:xs) = do
    let mfs = M.fromListWith (++)
            (concatMap ((\m x -> fromJust $ M.lookup x m) m) (tail $ tail $ T.splitOn "+" x))
    let bfs = M.fromListWith (++) (map (\y -> (T.pack $ _feat y, map T.pack $_featValues y)) fs)
    aux m fs xs bfs mfs
 where 
     aux m fs xs bfs mfs
        -- | M.isSubmapOf bfs mfs = True
        | M.isSubmapOf mfs bfs = True
        | otherwise = uniFS m fs xs
uniFS m fs [] = False

checkFS :: M.Map T.Text [(T.Text,[T.Text])] -> [Feat] -> T.Text -> [(T.Text,[T.Text])]
checkFS m fs tags = do
    let mfs = M.fromListWith (++)
            (concatMap ((\m x -> fromJust $ M.lookup x m) m) (tail $ tail $ T.splitOn "+" tags))
    let bfs = M.fromListWith (++) (map (\x -> (T.pack $ _feat x, map T.pack $_featValues x)) fs)
    (M.toList $ M.difference mfs bfs) -- ++ (M.toList $ M.difference bfs mfs)


mkLine :: String -> CW AW -> String -> [(T.Text,[T.Text])] -> T.Text
mkLine tp x sentID fs = do
    let cs = [tp,sentID,show (_id x),show $fromJust (_upos x),fromJust (_form x),fromJust (_lemma x)]
    let f = T.intercalate "|" (map (\y -> T.append (fst y) (T.append "=" (head $ snd y))) fs)
    T.intercalate "\t" ((map T.pack cs)++[f])
mkLine tp x sentID [] = 
    T.intercalate "\t" 
        (map T.pack [tp,sentID,show (_id x),show $fromJust(_upos x),fromJust (_form x),fromJust (_lemma x),"_"])


getTags :: CW AW -> M.Map T.Text [(T.Text, T.Text)] -> [T.Text]
getTags x m = 
    aux (T.pack $ fromJust $ _form x) (fromJust $ M.lookup (T.pack $ fromJust $ _lemma x) m)
 where 
     aux form (x:xs)
      | fst x == form = snd x : aux form xs
      | otherwise = aux form xs
     aux form [] = []

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


check :: M.Map T.Text [(T.Text, T.Text)] -> M.Map T.Text [(T.Text,[T.Text])]-> String -> [CW AW] -> [T.Text]
check morpho mfs sentID (x:xs)
 | isNothing (_upos x) = check morpho mfs sentID xs
 | (fromJust (_upos x) == U.VERB) || (fromJust (_upos x) == U.AUX) = auxcheck morpho x mfs : check morpho mfs sentID xs
 | otherwise = check morpho mfs sentID xs
 where
     auxcheck morpho x m 
        |isNothing (M.lookup (T.pack $ fromJust $ _lemma x) morpho) =
            mkLine "lemma" x sentID []
        | notElem (T.toLower $ T.pack $ fromJust ( _form x)) (map fst (fromJust (M.lookup (T.pack $ fromJust (_lemma x)) morpho))) =
            mkLine "forma" x sentID []
        | uniFS m (_feats x) (getTags x morpho) = ""
        | null (map (checkFS m (_feats x)) (getTags x morpho)) = ""
        | otherwise = mkLine "feats" x sentID (minimum (map (checkFS m (_feats x)) (getTags x morpho)))
check morpho mfs sentID [] = []

checkVerbs :: FilePath -> FilePath -> FilePath -> IO ()
checkVerbs vpath cldirpath outpath = do
    paths <- listDirectory vpath
    dicts <- mapM (MT.morphoMap . combine vpath) paths
    clpaths <- getClPath cldirpath
    fsmap <- tags2feats "etc/tags2feats.dict"
    txt <- mapM (aux fsmap cldirpath (foldr (M.unionWith (++)) M.empty dicts)) clpaths
    TO.writeFile outpath (T.intercalate "\n" $ concatMap (filter (/= "") ) txt)
 where
     aux fsmap cldirpath dicts clpath = do
         cldoc <- readConlluFile (combine cldirpath clpath)
         return (concatMap (\x -> check dicts fsmap (getID $ _meta x) (_words x)) cldoc)
