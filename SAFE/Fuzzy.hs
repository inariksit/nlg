module Fuzzy where

import PGF
import qualified SAFEQuery as NLG
import SAFEQuery hiding (Action)
import Data.Map (Map, fromList, assocs)

-- This is copied from Meng's code
data Action = MkAct { name   :: String
                    , params :: ActionParams }
            deriving (Show, Eq)

type ActionParams = Map String MyParamVal
data MyParamVal = MyString String
                | MyBool   Bool
                | MyInt    Int
                | MyChar   Char
                | MyList   [MyParamVal]
                deriving (Show, Eq)

isStr (MyString _) = True
isStr (MyList xs) = all isStr xs
isStr _ = False

strs (MyString x) = [x]
strs (MyList xs) = if all isStr xs then concatMap strs xs else []
strs _ = []

isBool (MyBool _) = True
isBool _ = False

isList (MyList _) = True
isList _ = False

-- My own example, slightly modified to better fit with GF
issueSharesEasy :: Int -> Action
issueSharesEasy numShares = MkAct "issue shares"
                      (fromList [("issues",     MyBool True)
                                ,("sells",      MyBool True)
                                ,("stock",      MyString "preferred")
                                ,("valuation",  MyList [MyString "pre-money", MyString "fixed"])
                                ,("numShares",  MyInt numShares)
                                ])

-- This code tries to transform the above format to GF trees
translateAction :: PGF -> Action -> NLG.Action
translateAction gr action =
  AComplIndir (ASlashDir pred dir) indir
  where
    kinds = kind gr `fmap` objects (params action)
    [dir,indir] = TDet ASg `fmap` kinds
    preds = toGf gr `fmap` predicates (params action) :: [ActionDirIndir] -- hardcoded type for now
    pred = ConjSlashDirIndir And (ListActionDirIndir preds) :: ActionDirIndir

-- a Kind may have one or more modifiers
kind :: PGF -> (String,[String]) -> Kind
kind gr (hd,mods) = foldr KProperty baseKind
                     [ cid2gf p :: Property
                     | p <- props, typeOf gr p == mkCId "Property"]
  where
    baseKind = toGf gr hd :: Kind
    props = map (lookupFuzzy gr) mods :: [CId]


------------------------------------------------------------------------
-- Helper functions

-- First extract just strings from ActionParams
predicates :: ActionParams -> [String]
predicates ps = [ str
                | (str,val) <- assocs $ ps
                , val == MyBool True ]

objects :: ActionParams -> [(String,[String])]
objects ps = [ (head,mods)
             | (head, val) <- assocs $ ps
             , isStr val
             , let mods = strs val ]

-- Then "parse" those strings very loosely
lookupFuzzy :: PGF -> String -> CId
lookupFuzzy gr = fst . head . lookupMorpho morpho
  where morpho = buildMorpho gr (lang gr)

-- Some more generic GF helpers
cid2gf :: (Gf a) => CId -> a
cid2gf = fg . (flip mkApp) []

toGf :: (Gf a) => PGF -> String -> a
toGf gr = cid2gf . lookupFuzzy gr

typeOf ::  PGF -> CId -> CId
typeOf gr cid = typ
 where
   (_, typ, _) = maybe (undefined, cid, undefined) unType (functionType gr cid)

lang :: PGF -> Language
lang = head . languages
