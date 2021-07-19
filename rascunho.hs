{-# LANGUAGE OverloadedStrings #-}

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
 
data LexicalRule = 
  LexicalRule
    { identifier :: String
    , affix_type :: String
    , patterns :: [(String,String)]
    } deriving (Show, Generic)

instance FromJSON LexicalRule
instance ToJSON LexicalRule  

data LetterSet =
  LetterSet
    { var :: String
    , characters :: String
    } deriving (Show, Generic)

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


-- para cada par de sufixos (R.Regex, String) correspondente a uma regra, se existir o primeiro sufixo 
-- (Regex) no lema, ele é substituído pelo segundo sufixo (String)
getRegForm :: String -> [(R.Regex,String)] -> [T.Text]
getRegForm lema (x:xs)
 | (R.matchRegex (fst x) lema) == Nothing = []
 | otherwise = [T.pack $ R.subRegex (fst x) lema (snd x)] ++ getRegForm lema xs
getRegForm lema [] = []

-- verifica se a forma é regular, se não for retorna a forma irregular e a regular que 
-- foi contruída pela regra
-- rs :: lista com as formas regulares produzidas pela func getRegForm
isRegular :: T.Text -> T.Text -> T.Text -> [T.Text] -> [[T.Text]]
isRegular forma lema regra rs 
 | member forma rs = [[]]
 | otherwise = [[forma, regra, lema],[(head rs), regra, lema]]


-- para cada lema do map, a função verifica se suas formas são regulares, chamando a função isRegular
-- e concatenando a saída
getIrregs :: [(T.Text,[(T.Text,T.Text)])] -> M.Map T.Text [(R.Regex,String)] -> [[T.Text]]
getIrregs xs m =
  concatMap ((\k (x,ys) -> concatMap (aux x k) ys) m) xs
 where 
   aux l m (f,r) = isRegular l r f (getRegForm (T.unpack l) (fromJust (M.lookup r m))) 

-- constrói um map do tipo lemma: [(form, tags)] ou seja, para cada lema estão associadas as 
-- entradas que possuem o mesmo
lemmaDict :: FilePath -> IO (M.Map T.Text [(T.Text, T.Text)])
lemmaDict path = do
  content <- TO.readFile path
  return $ M.fromListWith (++) $ aux (T.lines content)
 where
   aux = map (\s -> let p = (T.breakOn "+" (last $ T.splitOn "\t" s)) 
    in (fst p , [(head (T.splitOn "\t" s), snd p )]))

-- a partir do json das regras cria um map associando cada regra a uma lista de tuplas de sufixos, 
-- sendo o primeiro uma Regex (expressão regular)
readRules :: FilePath -> M.Map T.Text [(R.Regex,String)]


-- recebe três paths: 
-- mpath   - diretório dos arquivos a serem verificados
-- rpath   - arquivo das regras
-- outpath - onde serão escritas as formas irregulares
mkIrregsTab :: FilePath -> FilePath -> FilePath -> IO ()
mkIrregsTab mpath rpath outpath = do
  paths <- listDirectory mpath
  dicts <- mapM (lemmaDict . combine dir) paths
  rules <- readRules rpath
  TO.writeFile outpath (aux $ getIrregs (M.toList $ foldr (M.unionWith (++)) M.empty dicts) rules)
 where  
   aux x = T.intercalate "\n" $ map T.intercalate "\t" x
