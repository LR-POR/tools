{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MorphoTools where

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

------ Split

checkLemma :: Int -> [T.Text] -> ([T.Text],[T.Text])
checkLemma n xs
 | length xs < n = (xs,[])
 | head(T.splitOn "+" (last (T.splitOn "\t" (xs!!n)))) ==
   head(T.splitOn "+" (last (T.splitOn "\t" (xs!!(n-1))))) = checkLemma (n+1) xs
 | otherwise = splitAt n xs

splitEvery :: Int -> [T.Text] -> [[T.Text]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = checkLemma n list

split :: [T.Text] -> String -> FilePath -> IO [()]
split entries pos outdir =
  mapM (aux outdir) (splitEvery 1900 (nub $ sort entries))
 where
   aux outdir (x:xs) =
     TO.writeFile (combine outdir (pos++"-"++(take 3 $ T.unpack x)++".dict"))
     (T.append (T.intercalate "\n" (x:xs)) "\n")

------
-- (lema,[(forma,+tags)])
morphoMap :: FilePath -> IO (M.Map T.Text [(T.Text, T.Text)])
morphoMap path = do
  content <- TO.readFile path
  return $ M.fromListWith (++) $ aux (T.lines content)
 where
   aux xs = map (\s -> let p = (T.breakOn "+" (last $ T.splitOn "\t" s))
    in (fst p , [(head (T.splitOn "\t" s), snd p)])) xs

toEntries :: [(T.Text,[(T.Text, T.Text)])] -> [T.Text]
toEntries xs =  concatMap (\(a,b) ->  map (aux a) (nub b)) xs
 where 
   aux lema (forma,tags) = 
     T.append forma (T.append "\t" (T.append lema tags))


member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

------ 
auxCheckDup :: [(T.Text, T.Text)] -> T.Text -> [T.Text]
auxCheckDup (x:xs) tags
 | snd x == tags = (snd x) : auxCheckDup xs tags
 | otherwise = auxCheckDup xs tags
auxCheckDup [] tags = []

checkDup :: M.Map T.Text [(T.Text, T.Text)] -> (T.Text,T.Text) -> Bool
checkDup map (lema, tags)
 | length (auxCheckDup  (fromJust $ M.lookup lema map) tags) > 1 = True
 | otherwise = False

