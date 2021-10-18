{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MorphoTools where

import qualified Type as Tp
import qualified Irregs as I
import Data.Either
import Data.Char
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
delete :: FilePath -> FilePath -> IO [()]
delete dirpath epath = do
  entries <- TO.readFile epath
  dir <- getDir dirpath
  let paths = nub $ map (\x -> getPath dir (getLemma x)) (T.lines entries)
  dicts <- mapM (morphoMap . combine dirpath) paths
  mapM (aux dir dirpath)
    (map toEntries $ alfaSplit (M.toList $ auxDelete (foldr (M.unionWith (++)) M.empty dicts) (T.lines entries)))
 where
    aux dir dirpath (x:xs) =
     TO.writeFile (combine dirpath (getPath dir (getLemma x)))
     (T.append (T.intercalate "\n" (x:xs)) "\n")


---- corrigir lema

getDir :: FilePath -> IO String
getDir x = do
  paths <- listDirectory x
  return (head $ splitOn "-" $ head paths)
 

getLemma :: T.Text -> Tp.Lemma
getLemma entry = T.unpack $ head $ T.splitOn "+" $ last $ T.splitOn "\t" entry
-- recebe o nome do diretório, e um lema, 
-- encontra o arquivo correspondente a esse lema 
-- >>>
getPath :: Tp.Dir -> Tp.Lemma -> FilePath
getPath dir lema
 | ([head lema] >= "a") && ([head lema] <= "z") = dir++"-"++[head lema]++".dict"
 | elem [head lema] ["á","â","ã"] = dir++"-a.dict"
 | elem [head lema] ["é","ê"] = dir++"-e.dict"
 | elem [head lema] ["í"] = dir++"-i.dict"
 | elem [head lema] ["ó","ô","õ"] = dir++"-o.dict"
 | elem [head lema] ["ú"] = dir++"-u.dict"
 | otherwise = ""

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
  dir <- getDir dirPath
  dicts <- mapM (morphoMap . combine dirPath) (nub $ map (getPath dir) [del, new])
  mapM (aux dirPath dir) (auxCorLemma (del,new) dicts)
 where
    aux dirPath dir (x:xs) = 
      TO.writeFile (combine dirPath (getPath dir (getLemma x))) (T.append (T.intercalate "\n" (x:xs)) "\n")


------- inserir entradas

-- separa as regras que correspondem a cada pos
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

-- cria todas as flexões de um lema através das regras 
flex :: Tp.Lemma -> [(T.Text, T.Text)] -> M.Map T.Text [(R.Regex,String)] -> [(T.Text,T.Text)]
flex lemma tags rules =
   map (aux lemma rules) tags
 where
   aux lemma rules (t,r) = ((head $ I.getRegForm (lemma++"\b") (fromJust $ M.lookup r rules)), t)

auxNewLemma :: Tp.Lemma
  -> [(T.Text,T.Text)]
  -> M.Map T.Text [(R.Regex,String)]
  -> M.Map T.Text [(T.Text,T.Text)]
  -> M.Map T.Text [(T.Text,T.Text)]
auxNewLemma lemma tags rules = 
  M.insertWith (++) (T.pack lemma) (flex lemma tags rules)

newLemma :: FilePath -> Tp.Lemma -> IO ()
newLemma dirPath lemma = do
  dir <- getDir dirPath
  let path = getPath dir lemma
  tags <- getTags dir "../tags.dict"
  rules <- I.readRules "../irules.json"
  dict <- morphoMap (combine dirPath path)
  aux dirPath dir path
     (toEntries $ M.toList $ auxNewLemma lemma tags rules dict)
 where
   aux dirPath dir path xs =
      TO.writeFile (combine dirPath path) (T.append (T.intercalate "\n" xs) "\n")


------ split por letras do alfabeto

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
       ("a","â"),("e","ê"),("o","ô"),("a","ã"),("o","õ"),
       ("á","a"),("é","e"),("í","i"),("ó","o"),("ú","u"),
       ("â","a"),("ê","e"),("ô","o"),("ã","a"),("õ","o")] =  aux xs (y++x)
    | otherwise = aux xs y
   aux [] y = y

alfaSplit :: [(T.Text,[(T.Text,T.Text)])] -> [[(T.Text,[(T.Text,T.Text)])]]
alfaSplit xs = alfaClean $ alfaJoin $ aux xs
 where 
   aux = groupBy (\a b -> (toLower $ T.head (fst a)) == (toLower $ T.head (fst b)))

-- recebe path do diretório e path para escrever os novos arquivos
-- separa as entradas usando como critério a primeira letra do lema
-- >>>
alfaOrder :: FilePath -> FilePath -> IO [()]
alfaOrder dirPath outPath = do
  paths <- listDirectory dirPath
  let dir = head $ splitOn "-" $ head paths
  dicts <- mapM (morphoMap . combine dirPath) paths
  mapM (aux dirPath dir outPath)
    ( map toEntries (alfaSplit (M.toList $ M.map (sortOn fst) (foldr (M.unionWith (++)) M.empty dicts))))
 where
  aux dirPath dir outPath (x:xs) =
    TO.writeFile (combine outPath (getPath dir (getLemma x)))
     (T.append (T.intercalate "\n" (x:xs)) "\n")
    