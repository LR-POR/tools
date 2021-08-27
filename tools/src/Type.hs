{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MorphoTools where

type Doc = [Entry]

data Entry = Entry 
    {lemma :: Lemma
    ,pos   :: POS
    ,tags  :: [Tag]
    }

data Lemma = T.Text

data POS 
    = A
    | ADV
    | N
    | V
     deriving (Enum, Eq, Read, Show)

data Tag 
    = INF 
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
    | 1
    | 2
    | 3
    | DIM 
    | AUG 
    | SUPER
    | NEG
     deriving (Enum, Eq, Read, Show)
