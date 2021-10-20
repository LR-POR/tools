
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Irregs where


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
import Data.Aeson
  ( FromJSON(parseJSON)
  , Options(fieldLabelModifier)
  , ToJSON(toEncoding, toJSON)
  , defaultOptions
  , eitherDecode
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  )
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Generics ( Generic )

------ parser para o JSON que contém os dados do arquivo my-irules.tdl 

data LexicalRule = 
  LexicalRule
    { identifier :: String
    , affix_type :: String
    , patterns :: [Patterns]
    } deriving (Show, Generic)

instance FromJSON LexicalRule
instance ToJSON LexicalRule  

type Patterns = (String,String)


data LetterSet =
  LetterSet
    { var :: Var
    , characters :: Characters
    } deriving (Show, Generic)

type Var = String
type Characters = String

instance FromJSON LetterSet
instance ToJSON LetterSet

data Document =
  Document
    { letterSet :: [LetterSet]
    , rules :: [LexicalRule]
    } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document


readJSON :: FilePath -> IO (Either String Document)
readJSON path = (eitherDecode <$> B.readFile path) :: IO (Either String Document)

------ 

-- apaga listas vazias
del :: [[T.Text]] -> [[T.Text]]
del (x:xs) 
 | x == [] = del xs
 | otherwise = x : del xs
del [] = []

------ construindo map das regras para as expressões regulares

-- variável -> conjunto de letras -> par de sufixos
auxSubLS ::  Var -> Characters -> Patterns -> [Patterns]
auxSubLS y (x:xs) (a,b) =
  ((R.subRegex (R.mkRegex y) a [x] ),(R.subRegex (R.mkRegex y) b [x] )):auxSubLS y xs (a,b)
auxSubLS y [] (a,b) = []

-- constrói as tuplas de sufixos substituindo as variáveis pelas letras que elas representam
subLS :: [LetterSet] -> Patterns -> [Patterns]
subLS (x:xs) (a,b)
 | (R.matchRegex (R.mkRegex $ var x) a) == Nothing = subLS xs (a,b)
 | otherwise = concatMap (subLS xs) (auxSubLS (var x) (characters x) (a,b) ) 
subLS [] (a,b) 
 |a == "*" = [("",b)] 
 |otherwise = [(a,b)] 

path2Doc :: [FilePath] -> IO [Document]
path2Doc = mapM $ fmap (\(Right x) -> x) . readJSON

auxReadRules :: [Patterns] -> [(R.Regex,String)]
auxReadRules  = map (\(a,b) -> (R.mkRegex (a ++ "\b"),b))

-- a partir do json das regras cria um map associando cada regra a uma lista de tuplas de sufixos, 
-- sendo o primeiro uma Regex (expressão regular)
readRules :: FilePath -> IO (M.Map T.Text [(R.Regex,String)])
readRules path = do
  irules <- path2Doc [path]
  return $ M.fromList $ map (aux (letterSet (head irules))) (rules (head irules))
 where
   aux ls r = (T.pack (identifier r), auxReadRules $ concatMap (subLS ls) (patterns r))

------ construindo map das entradas do MorphoBr 

getRule :: T.Text -> M.Map T.Text [T.Text] -> T.Text
getRule tags m
 | isNothing (M.lookup tags m) = ""
 | otherwise = head $ fromJust $ M.lookup tags m

-- constrói um map do tipo lemma: [(form, regra)] ou seja, para cada lema estão associadas as 
-- entradas que possuem o mesmo
lemmaDict :: M.Map T.Text [T.Text] -> FilePath -> IO (M.Map T.Text [(T.Text, T.Text)])
lemmaDict mtags path = do
  content <- TO.readFile path
  return $ M.fromListWith (++) $ aux mtags (T.lines content)
 where
   aux m xs = map (\s -> let p = (T.breakOn "+" (last $ T.splitOn "\t" s))
    in (T.append (fst p) "\b" , [((head (T.splitOn "\t" s)), getRule (snd p) m)])) xs

------  

-- constrói um map do tipo tags: [regra] para relacionar as tags do MorphoBr às regras 
-- do arquivo my-irules.tdl
tag2rule :: FilePath -> IO (M.Map T.Text [T.Text])
tag2rule path = do
  content <- TO.readFile path
  return $ M.fromListWith (++) $ map aux (T.lines content)
 where
   aux = \s -> let p = T.splitOn "\t" s in (head p, tail p)

------ comparando formas

-- para cada par de sufixos (R.Regex, String) correspondente a uma regra, se existir o primeiro sufixo 
-- (Regex) no lema, ele é substituído pelo segundo sufixo (String)
getRegForm :: String -> [(R.Regex,String)] -> [T.Text]
getRegForm lema (x:xs)
 | (R.matchRegex (fst x) lema) == Nothing = getRegForm lema xs
 | otherwise = [T.pack $ R.subRegex (fst x) lema (snd x)] ++ getRegForm lema xs
getRegForm lema [] = [T.pack lema]

-- verifica se a forma é regular, se não for retorna a forma irregular e a regular que 
-- foi construída pela regra
-- rs :: lista com as formas regulares produzidas pela func getRegForm
isRegular :: T.Text -> T.Text -> T.Text -> [T.Text] -> M.Map T.Text [(T.Text, T.Text)] -> [[T.Text]]
isRegular forma lema regra rs morpho
 | elem forma rs = [[]]
 | elem (head rs,regra) (fromJust (M.lookup lema morpho)) 
    = [[head rs,T.toUpper regra, T.init lema],[forma, T.toUpper regra, T.init lema]]
 | otherwise = [[forma, T.toUpper regra, T.init lema]]

-- para cada lema do map, a função verifica se suas formas são regulares, chamando a função isRegular
-- e concatenando a saída
getIrregs :: [(T.Text,[(T.Text,T.Text)])] -> M.Map T.Text [(R.Regex,String)] -> [[T.Text]]
getIrregs xs m =
  concatMap 
  ((\k morpho (x,ys) -> nub $ concatMap (aux x k morpho) (sortOn snd $ sortOn fst ys)) 
   m (M.fromList xs)) xs
 where
   aux l m morpho (f,r) 
    | isNothing (M.lookup r m) = [[]] 
    | otherwise = isRegular f l r (getRegForm (T.unpack l) (fromJust (M.lookup r m))) morpho

-- recebe dois paths, um com o diretório dos arquivos a serem verificados e outro onde serão 
-- escritas as formas irregulares
mkIrregsTab :: FilePath -> FilePath -> IO ()
mkIrregsTab dir outpath = do
  mtags <- tag2rule "etc/tags.dict"
  paths <- listDirectory dir
  dicts <- mapM ((lemmaDict mtags) . combine dir) paths
  rules <- readRules "etc/irules.json"
  TO.writeFile outpath (aux $ getIrregs (M.toList $ foldr (M.unionWith (++)) M.empty dicts) rules)
 where
   aux x = T.intercalate "\n" $ map (T.intercalate "\t") $ del x
