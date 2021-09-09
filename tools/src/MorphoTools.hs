{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MorphoTools where

import qualified Type as Tp
import qualified Irregs as I
import Data.Either
import System.IO
import qualified Data.Map as M
import System.Directory
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import Data.List (groupBy, intercalate, sort, nub, sortOn)
import Data.List.Split (splitOn, splitPlaces, chunksOf)
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

-- recebe lista de entradas, nome do diretório e path para salvar as entradas em arquivos divididos
-- >>>
split :: [T.Text] -> String -> FilePath -> IO [()]
split entries dir dirPath =
  mapM (aux dirPath) (splitEvery 1900 (nub $ sort entries))
 where
   aux dirPath (x:xs) =
     TO.writeFile (combine dirPath (dir++"-"++(take 3 $ T.unpack x)++".dict"))
     (T.append (T.intercalate "\n" (x:xs)) "\n")

------
-- cria um Map das entradas, a chave é o lema e os valores são uma lista de tuplas contento forma e tags
-- (lema,[(forma,+tags)])
-- >>>
morphoMap :: FilePath -> IO (M.Map T.Text [(T.Text, T.Text)])
morphoMap path = do
  content <- TO.readFile path
  return $ M.fromListWith (++) $ aux (T.lines content)
 where
   aux xs = map (\s -> let p = (T.breakOn "+" (last $ T.splitOn "\t" s))
    in (fst p , [(head (T.splitOn "\t" s), snd p)])) xs

-- serializa as entradas
-- >>>
toEntries :: [(T.Text,[(T.Text, T.Text)])] -> [T.Text]
toEntries xs =  concatMap (\(a,b) ->  map (aux a) (nub b)) xs
 where
   aux lema (forma,tags) =
     T.append forma (T.append "\t" (T.append lema tags))

------ apagar entradas 

auxCheckDup :: [(T.Text, T.Text)] -> T.Text -> [T.Text]
auxCheckDup (x:xs) tags
 | snd x == tags = (snd x) : auxCheckDup xs tags
 | otherwise = auxCheckDup xs tags
auxCheckDup [] tags = []

-- recebe Map do arquivos e uma tupla com lema e tags de uma entrada,
-- retorna True se existe uma entrada equivalente (mesmo lema e tags) no Map
-- >>>
checkDup :: M.Map T.Text [(T.Text, T.Text)] -> (T.Text,T.Text) -> Bool
checkDup map (lema, tags)
 | length (auxCheckDup  (fromJust $ M.lookup lema map) tags) > 1 = True
 | otherwise = False

delEntry :: T.Text -> T.Text -> T.Text -> M.Map T.Text [(T.Text, T.Text)] ->  M.Map T.Text [(T.Text, T.Text)]
delEntry forma lema tags m = M.insert lema (aux forma tags $ fromJust $ M.lookup lema m) m
 where
   aux forma tags (x:xs)
    | (forma == fst x) && (tags == snd x) = xs
    | otherwise = x : aux forma tags xs
   aux forma tags [] = []

auxDelete :: M.Map T.Text [(T.Text, T.Text)] -> [T.Text] ->  M.Map T.Text [(T.Text, T.Text)]
auxDelete m (x:xs)
    | checkDup m (T.breakOn "+" $ last $ T.splitOn "\t" x) =
      auxDelete (delEntry (head $ T.splitOn "\t" x) -- forma
       (head $ T.splitOn "+" $ last $ T.splitOn "\t" x) (snd $ T.breakOn "+" x) m) xs -- lema tags
    | otherwise = auxDelete m xs
auxDelete m [] = m

-- recebe o path do diretório, path do arquivo com as entradas que vão ser apagadas e path de saída,
-- atualiza todos os arquivos do diretório
-- >>>
delete :: FilePath -> FilePath -> FilePath -> IO [()]
delete dir path outpath = do
  paths <- listDirectory dir
  let cl = head $ splitOn "-" $ head paths
  dicts <- mapM (morphoMap . combine dir) paths
  entries <- TO.readFile path
  mapM (aux cl outpath)
    (splitEvery 19000 (toEntries (M.toList $ auxDelete (foldr (M.unionWith (++)) M.empty dicts) (T.lines entries))))
 where
    aux cl outpath (x:xs) =
     TO.writeFile (combine outpath (cl++"-"++(take 7 $ T.unpack $ last $ T.splitOn "\t" x)++".dict"))
     (T.append (T.intercalate "\n" (x:xs)) "\n")


---- corrigir lema

-- recebe o nome do diretório, a lista de arquivos do diretório e um lema,
-- retorna o path do arquivo correspondente ao lema
-- >>>
getPath :: Tp.Dir -> [FilePath] -> Tp.Lemma -> FilePath
getPath dir (x:y:xs) lema
 |(dir ++ lema) < takeBaseName x = x
 |(takeBaseName x <= (dir ++ lema)) && ((dir ++ lema) < takeBaseName y) = x
 | otherwise = getPath dir (y:xs) lema
getPath dir [y] lema = y

auxCorLemma :: (Tp.Lemma,Tp.Lemma) -> [M.Map T.Text [(T.Text, T.Text)]] -> [[T.Text]]
auxCorLemma (del,new) [m] = [toEntries $ M.toList $
    M.delete (T.pack del) (M.insertWith (++) (T.pack new) (fromJust $ M.lookup (T.pack del) m) m)]
auxCorLemma (del,new) [x,y]
 | M.member (T.pack del) x = map (toEntries . M.toList)
    [M.insertWith (++) (T.pack new) (fromJust $ M.lookup (T.pack del) x) y, M.delete (T.pack del) x]
 | otherwise = map (toEntries . M.toList)
    [M.insertWith (++) (T.pack new) (fromJust $ M.lookup (T.pack del) y) x, M.delete (T.pack del) y]

-- recebe o path do diretório, o lema atual e o lema novo
-- atualiza apenas os arquivos modificados
-- >>>
corLemma :: FilePath -> (Tp.Lemma,Tp.Lemma) -> IO [()]
corLemma dirPath (del,new) = do
  paths <- listDirectory dirPath
  let dir = head $ splitOn "-" $ head paths
  dicts <- mapM (morphoMap . combine dirPath) (map (getPath dir (sort paths)) [del, new])
  mapM (aux dirPath dir paths) (auxCorLemma (del,new) dicts)
 where
    aux dirPath dir paths (x:xs)
     | elem (dir++"-"++(take 7 $ T.unpack $ last $ T.splitOn "\t" x)++".dict") paths =
      TO.writeFile (combine dirPath (dir++"-"++(take 7 $ T.unpack $ last $ T.splitOn "\t" x)++".dict"))
      (T.append (T.intercalate "\n" (x:xs)) "\n")
     | otherwise = do
      TO.writeFile (combine dirPath (dir++"-"++(take 7 $ T.unpack $ last $ T.splitOn "\t" x)++".dict")) (T.append (T.intercalate "\n" (x:xs)) "\n")
      removeFile (combine dirPath (getPath dir (sort paths) (T.unpack x)))

------- inserir entradas

getTags :: String -> FilePath -> IO [(T.Text, T.Text)]
getTags dir tagsPath = do
  content <- readFile tagsPath
  return $ map (\s -> let p = T.splitOn "\t" s in (head p, last p)) (aux dir (lines content))
 where 
   aux dir (x:xs)
    |(dir == "verbs") && (take 2 x) == "+V" = T.pack x : aux dir xs
    |(dir == "nouns") && (take 2 x) == "+N" = T.pack x : aux dir xs
    |(dir == "adjectives") && (take 2 x) == "+A" = T.pack x : aux dir xs
    | otherwise = aux dir xs
   aux dir [] = []

flex :: Tp.Lemma -> [(T.Text, T.Text)] -> M.Map T.Text [(R.Regex,String)] -> [(T.Text,T.Text)]
flex lemma tags rules =
   map (aux lemma rules) tags
 where 
   aux lemma rules (t,r) = ((head $ I.getRegForm (lemma++"\b") (fromJust $ M.lookup r rules)), t)

auxNewLemma :: Tp.Lemma 
  -> [(T.Text,T.Text)] 
  -> M.Map T.Text [(R.Regex,String)]
  -> [M.Map T.Text [(T.Text,T.Text)]]
  -> [M.Map T.Text [(T.Text,T.Text)]]
auxNewLemma lemma tags rules (x:y:xs)
 | ((fst $ head $ M.toList x) < (T.pack lemma)) && ((fst $ head $ M.toList y) > (T.pack lemma)) = 
   M.insertWith (++) (T.pack lemma) (flex lemma tags rules) x : (y:xs)
 | ((fst $ head $ M.toList x) > (T.pack lemma)) =  
   M.insertWith (++) (T.pack lemma) (flex lemma tags rules) x : (y:xs)
 | otherwise = x : auxNewLemma lemma tags rules (y:xs)

newLemma :: FilePath -> Tp.Lemma -> IO [()]
newLemma dirPath lemma = do
  paths <- listDirectory dirPath
  let dir = head $ splitOn "-" $ head paths
  tags <- getTags dir "../tags.dict"
  rules <- I.readRules "../irules.json"
  dicts <- mapM (morphoMap . combine dirPath) paths
  mapM (aux dirPath dir paths) 
     (map (toEntries . M.toList) (auxNewLemma lemma tags rules dicts))
 where 
   aux dirPath dir paths (x:xs)
    | elem (dir++"-"++(take 7 $ T.unpack $ last $ T.splitOn "\t" x)++".dict") paths =
      TO.writeFile (combine dirPath (dir++(take 7 $ T.unpack $ last $ T.splitOn "\t" x)++".dict"))
      (T.append (T.intercalate "\n" (x:xs)) "\n")
    | otherwise = do
      TO.writeFile (combine dirPath (dir++"-"++(take 7 $ T.unpack $ last $ T.splitOn "\t" x)++".dict")) (T.append (T.intercalate "\n" (x:xs)) "\n")
      removeFile (combine dirPath (getPath dir (sort paths) (T.unpack x)))

alfaClean :: [[(T.Text,[(T.Text,T.Text)])]] -> [[(T.Text,[(T.Text,T.Text)])]]
alfaClean (x:xs)
 |("a" <= [T.head (fst $ head x)]) && ("z" >= [T.head (fst $ head x)]) = x : alfaClean xs
 | otherwise = alfaClean xs
alfaClean [] = []

alfaJoin :: [[(T.Text,[(T.Text,T.Text)])]] -> [[(T.Text,[(T.Text,T.Text)])]]
alfaJoin xs = map (aux xs) xs
 where
   aux (x:xs) y
    | elem ([T.head (fst $ head x)], [T.head (fst $ head y)]) 
      [("a","á"),("e","é"),("í","i"),("o","ó"),("u","ú"),
      ( "a","â"),("e","ê"),("o","ô"),("a","ã"),("o","õ")] =  aux xs (y++x)
    | otherwise = aux xs y
   aux [] y = y

alfaSplit :: [(T.Text,[(T.Text,T.Text)])] -> [[(T.Text,[(T.Text,T.Text)])]]
alfaSplit = groupBy (\a b -> (T.head (fst a)) == (T.head (fst b))) 

alfaOrder :: FilePath -> FilePath -> IO [()]
alfaOrder dirPath outPath = do 
  paths <- listDirectory dirPath
  let dir = head $ splitOn "-" $ head paths
  dicts <- mapM (morphoMap . combine dirPath) paths
  mapM (aux dirPath dir outPath)
    ( map toEntries  (alfaClean $ alfaJoin $ alfaSplit (M.toList $ M.map (sortOn fst) (foldr (M.unionWith (++)) M.empty dicts))))
 where 
  aux dirPath dir outPath (x:xs) =
    TO.writeFile (combine outPath (dir++"-"++[T.head $ last $ T.splitOn "\t" x]++".dict"))
     (T.append (T.intercalate "\n" (x:xs)) "\n")
    