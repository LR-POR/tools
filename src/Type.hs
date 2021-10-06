{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Type where

import qualified Data.Map as M
import qualified Data.Text as T

--import qualified Data.Text as T
type Dir = String

type Doc = [Entry]

data Entry = Entry 
    {form  :: Form
    ,lemma :: Lemma
    ,pos   :: POS
    ,tags  :: [Tag]
    }

type Form  = String
type Lemma = String

data POS 
    = A
    | ADV
    | N
    | V
     deriving (Enum, Eq, Read, Show)

data Tag 
    = Int
    | INF 
    | GRD
    | PTPST
    | PRS
    | IMPF
    | PRF 
    | FUT 
    | PQP
    | SBJR
    | SBJP
    | SBJF
    | IMP 
    | COND
    | M 
    | F
    | SG 
    | PL 
    | DIM 
    | AUG 
    | SUPER
    | NEG
     deriving (Enum, Eq, Read, Show)

type MorphoMap = M.Map T.Text [(T.Text, T.Text)]
type OutPath = FilePath
type MorphoDirPath = FilePath
type LexRulesPath = FilePath
type MapTagsRulesPath = FilePath
type LexEntriesPath = FilePath
