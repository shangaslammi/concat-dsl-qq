{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Concat.TH (cc) where

import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as S
import qualified Language.Haskell.Meta as M

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Arrow ((>>>))
import Data.List (foldl1', foldl')
import Data.Char (isSpace)

import qualified Language.Concat.Stack as Stack

cc = QuasiQuoter
    { quoteExp  = parseCC
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

push :: Exp -> Exp
push = AppE $ VarE 'Stack.push

liftS2 :: Exp -> Exp
liftS2 = AppE $ VarE 'Stack.liftS2

var :: String -> Exp
var = VarE . mkName

stripSpace :: String -> String
stripSpace = filter (not.isSpace)

extractQuot :: [S.Exp] -> Exp
extractQuot []  = var "nop"
extractQuot [e] = combineExps $ extractWords e

extractWords :: S.Exp -> [Exp]
extractWords exp = case exp of
    S.Lit lit -> [push $ M.toExp $ M.toLit lit]
    S.App a p -> extractWords a ++ extractWords p
    var@(S.Var (S.UnQual (S.Symbol _))) -> [liftS2 (M.toExp var)]
    var@(S.Var _) -> [M.toExp var]
    S.List q -> return $ push $ extractQuot q
    S.If (S.List cond) (S.List then_) (S.List else_) ->
        [ extractQuot cond
        , push $ extractQuot then_
        , push $ extractQuot else_
        , var "if_"
        ]
    e -> error $ "Invalid expression in DSL: " ++ prettyPrint e

combineExps :: [Exp] -> Exp
combineExps = foldl1' step where
    step r l = InfixE (Just r) arr (Just l)
    arr = VarE '(>>>)

parseCC :: String -> Q Exp
parseCC (stripSpace -> "") = return $ VarE 'id
parseCC (parseExp -> ParseOk exp) = return . combineExps . extractWords $ exp
parseCC (parseExp -> ParseFailed _ err) = error err

