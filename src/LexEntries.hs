{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveDataTypeable  #-}

module LexEntries where

import qualified Type as Tp
import qualified MorphoTools as MT

import Data.Typeable
import Data.Data
import Text.StringTemplate
import Text.StringTemplate.GenericStandard


data LexicalEntries =
  LexicalEntries
    { lexType :: LexType
    , verbs :: [Verb]
    } deriving (Show, Generic)

type LexType = String
type Verb = String

instance FromJSON LexicalEntries
instance ToJSON LexicalEntries

data Document =
  Document
    { lexicalEntries :: [LexicalEntries]
    } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document

readJSON :: FilePath -> IO (Either String Document)
readJSON path = (eitherDecode <$> B.readFile path) :: IO (Either String Document)

readEntries :: [FilePath] -> IO [Document]
readEntries = mapM $ fmap (\(Right x) -> x) . readJSON


checkVerbs :: [Verb] -> MorphoMap -> [Verb]
checkVerbs (v:vs) m
  | isNothing (M.lookup v m) = v : checkVerbs vs m
  | otherwise = checkVerbs vs m


{-
joe =  Person { name = "Joe Bloggs", age = 23 }
names = ("Joe", "Bloggs")

t1 = newSTMP $ unlines [
  "Hello $names.0$",
  "Your full name is $person.name$, you are $person.age$ years old."
  ] :: StringTemplate String

main = putStrLn $ toString $ setAttribute "names" names $ setAttribute "person" joe t1
-}

{- 
acuar := trans-verb-lex &
  [ STEM < "acuar" >,
    SYNSEM.LKEYS.KEYREL.PRED "_acuar_v_rel" ].

-}

enTemp = newSTMP $ unlines [
  "$verb$ := $type$ &",
  "[ STEM < $lemma$ >,",
  "  SYNSEM.LKEYS.KEYREL.PRED $verb_rel$ ].",
  ""
  ] :: StringTemplate String

srLexEntries :: LexicalEntries -> [String]
srLexEntries lexEnts = do
  map (aux (lexType lexEnts)) (verbs lexEnts)  
 where 
   aux tp v = 
        toString 
        $ setAttribute "verb" v 
        $ setAttribute "type" tp 
        $ setAttribute "lemma" ("\34"++verb++"\34") 
        $ setAttribute "verb_rel" ("_\34"+verb+"_v_rel\34") enTemp


mkLexEntries :: MorphoDirPath -> LexEntriesPath -> OutPath -> OutPath -> IO ()
mkLexEntries dirpath lexpath outLexEntPath outVerbsPath = do
  paths <- listDirectory dirpath
  dicts <- mapM (lemmaDict mtags . combine dir) paths
  let morpho = foldr (M.unionWith (++)) M.empty dicts
  lexEnts <-readEntries [lexpath]
  writeFile outVerbsPath (intercalate "\n" $ concatMap (checkVerbs morpho . verbs) lexEnts)
  writeFile outLexEntPath (intercalate "\n" $ map srLexEntries lexEnts)
