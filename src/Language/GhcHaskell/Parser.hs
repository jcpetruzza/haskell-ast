module Language.GhcHaskell.Parser

where

import Control.Monad

import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.AST.HSE

import Language.GhcHaskell.AST

parsePat :: String -> ParseResult (Parsed Pat)
parsePat = P.parse >=> fromHsePat

parseExp :: String -> ParseResult (Parsed Exp)
parseExp = P.parse >=> fromHseExp

parseType :: String -> ParseResult (Parsed Type)
parseType = P.parse >=> fromHseType

parseDecl :: String -> ParseResult (Parsed Decl)
parseDecl = P.parse >=> fromHseDecl

parseModule :: String -> ParseResult (Parsed Module)
parseModule = P.parse >=> fromHseModule
