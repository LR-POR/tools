
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Clean where

import MorphoTools
import Data.Either
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Data.List (groupBy, intercalate, sort, nub, sortOn)
import Data.List.Split (splitPlaces, chunksOf)
import Data.Maybe ( fromJust, isNothing )
import qualified Text.Regex as R


semSauxRegra2 :: (T.Text,[T.Text]) -> (T.Text,[T.Text]) -> [T.Text]
semSauxRegra2 (forma1,tags1) (forma2,tags2)
 | ((T.last forma1) == 's') && (T.last forma2) == 's'
   = []
 | (T.last forma1) == 's' = []
 | (T.last forma2) == 's' = [] 
 | otherwise = [T.append "*" (T.append forma1 (T.append "\t" (T.intercalate "+" tags1))),
     T.append "*" (T.append forma2 (T.append "\t" (T.intercalate "+" tags2)))]

errosAuxRegra2 :: (T.Text,[T.Text]) -> (T.Text,[T.Text]) -> [T.Text]
errosAuxRegra2 (forma1,tags1) (forma2,tags2)
 | ((T.last forma1) == 's') && (T.last forma2) == 's'
   = []-- [(T.append forma1 (T.append "\t" (T.intercalate "+" tags1))),
      --(T.append forma2 (T.append "\t" (T.intercalate "+" tags2)))] 
 | (T.last forma1) == 's' = [T.append forma2 (T.append "\t" (T.intercalate "+" tags2))]
 | (T.last forma2) == 's' = [T.append forma1 (T.append "\t" (T.intercalate "+" tags1))] 
 | otherwise = []

-- Sejam (forma1,feats1) e (forma2,feats2) de um dado lema, onde feats1==feats2 
-- E feats1 termina em 2+SG E forma1 != forma2 E forma1 ou forma2 termina em s, 
-- eliminar a forma duplicada que não termina em s
regra2 :: (T.Text, [T.Text]) -> (T.Text, [T.Text]) -> Bool
regra2 (forma1,tags1) (forma2,tags2)
 | (not (member (T.pack "IMP") tags1)) && (tags1 == tags2) && (member (T.pack "2") tags1)
  && (member (T.pack "SG") tags1) && (forma1 /= forma2) = True
 | otherwise = False

-- Se +IMP+ não é parte das features de uma forma de um dado lema de verbo, 
-- eliminar a forma se a primeira ou a segunda pessoa do plural não terminar em s.
regra1 :: (T.Text, [T.Text]) -> Bool
regra1 (forma,tags)
 | (not (member (T.pack "IMP") tags)) && (member (T.pack "PL") tags)
      && ((member "1" tags)||(member "2" tags))
      && ((T.last forma) /= 's') = True
 | otherwise = False

-- Eliminar as formas do infinitivo terminadas em (á|ê|ô|i|í)
regra3 :: (T.Text, [T.Text]) -> Bool
regra3 (forma,tags)
 | (member (T.last forma) ['á','ê','i','í','ô']) && (member (T.pack "INF") tags) = True
 | otherwise = False

regra2FilterEntries :: T.Text -> [(T.Text, [T.Text])] -> [T.Text]
regra2FilterEntries lema (x:y:xs)
 | regra1 x = [] ++ regra2FilterEntries lema (y:xs)    
 | regra2 x y = semSauxRegra2 x y  ++ regra2FilterEntries lema xs
 | regra3 x = [] ++ regra2FilterEntries lema (y:xs)  
 | otherwise =  regra2FilterEntries lema (y:xs) 
regra2FilterEntries lema [] = []
regra2FilterEntries lema [x]
 | ((regra1 x)||(regra3 x)) = [] 
 | otherwise = []

regra2GetEntries :: [(T.Text,[(T.Text, T.Text)])] -> [T.Text]
regra2GetEntries (x:xs) =
   regra2FilterEntries (fst x) (map aux (sortOn snd (nub (snd x)))) ++ regra2GetEntries xs
 where
   aux = \x -> (fst x, T.splitOn "+" (snd x))
regra2GetEntries [] = []

errosFilterEntries :: T.Text -> [(T.Text, [T.Text])] -> [T.Text]
errosFilterEntries lema (x:y:xs)
 | regra1 x = [T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x)))] ++ errosFilterEntries lema (y:xs) -- [] ++ filterEntries lema (y:xs)    
 | regra2 x y = errosAuxRegra2 x y  ++ errosFilterEntries lema xs
 | regra3 x = [T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x)))] ++ errosFilterEntries lema (y:xs) -- [] ++ filterEntries lema (y:xs)  
 | otherwise =  errosFilterEntries lema (y:xs) 
errosFilterEntries lema [] = []
errosFilterEntries lema [x]
 | ((regra1 x)||(regra3 x)) = [T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x)))] 
 | otherwise = []--[T.append (fst x) (T.append "\t" (T.intercalate "+" (snd x)))]

errosGetEntries :: [(T.Text,[(T.Text, T.Text)])] -> [T.Text]
errosGetEntries (x:xs) =
   errosFilterEntries (fst x) (map aux (sortOn snd (nub (snd x)))) ++ errosGetEntries xs
 where
   aux = \x -> (fst x, T.splitOn "+" (snd x))
errosGetEntries [] = []

mkMap :: FilePath -> IO (M.Map T.Text [(T.Text, T.Text)])
mkMap path = do
  content <- TO.readFile path
  return $ M.fromListWith (++) $ aux (T.lines content)
 where
   aux xs = map (\s -> let p = (T.breakOn "+" (last $ T.splitOn "\t" s))
    in (fst p , [(head (T.splitOn "\t" s), last (T.splitOn "\t" s))])) xs

clMap :: FilePath -> IO (M.Map T.Text [T.Text])
clMap path = do
  content <- TO.readFile path
  return $ M.fromListWith (++) $ aux (T.lines content)
 where
   aux xs = map (\s -> let p = (T.breakOn "-" s) in (fst p,[])) xs

errosSep :: [T.Text]  -> [T.Text]
errosSep (x:xs) 
 | T.head x == '*' = errosSep xs
 | otherwise = [x] ++ errosSep xs
errosSep [] = []

regra2sep :: [T.Text]  -> [T.Text]
regra2sep (x:xs) 
 | T.head x == '*' = [T.tail x] ++ regra2sep xs
 | otherwise =  regra2sep xs
regra2sep [] = []

getRegra2semS :: FilePath -> FilePath -> IO ()
getRegra2semS vdir outpath = do
  vpaths <- listDirectory vdir
  vdicts <- mapM (mkMap . combine vdir) vpaths
  TO.writeFile outpath (T.intercalate "\n" $ regra2sep 
    (regra2GetEntries  (M.toList $ foldr (M.unionWith (++)) M.empty vdicts)))


getErros :: FilePath -> FilePath -> IO ()
getErros vdir outpath = do
  vpaths <- listDirectory vdir
  vdicts <- mapM (mkMap . combine vdir) vpaths
  TO.writeFile outpath (T.intercalate "\n" $ errosSep 
    (errosGetEntries  (M.toList $ foldr (M.unionWith (++)) M.empty vdicts)))


notClitic :: FilePath -> FilePath -> FilePath -> IO ()
notClitic cdir entries outpath = do
  cpaths <- listDirectory cdir
  cdicts <- mapM (clMap . combine cdir) cpaths
  erros <- TO.readFile entries
  TO.writeFile outpath (T.intercalate "\n"
      (aux  (foldr (M.unionWith (++)) M.empty cdicts) (map (T.breakOn "\t") (T.lines erros))))
 where
   aux cmap (x:xs)
    |M.lookup (fst x) cmap == Nothing = T.append (fst x) (T.append "\t" (snd x)) : aux cmap xs
    | otherwise = aux cmap xs
   aux cmap [] = []

