{-# LANGUAGE OverloadedStrings #-}

module Parsing.LangDef (
  Parser,
  catrinaDef
                        ) where

import Data.Functor.Identity
import Data.Text
import Parsing.Names
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

type Parser = Parsec Text ()

catrinaDef :: GenLanguageDef Text st Identity
catrinaDef = emptyDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter   -- <|> oneOf ".@"
  , identLetter     = alphaNum <|> oneOf "'_" -- <|> oneOf "_'.?"
  , opStart         = oneOf ":!#$%&*+/<=>?\\^|" 
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~" 
  , reservedOpNames = reservedOpNames'
  , reservedNames   = reservedNames'
  , caseSensitive   = True
  }
