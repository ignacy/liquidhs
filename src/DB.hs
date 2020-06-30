{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module DB where

import Hasql.Session
import Hasql.Statement
import qualified Data.Text as T
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import qualified Data.Int

getPage :: Data.Int.Int32 -> Session (Maybe T.Text)
getPage id = Hasql.Session.statement id findPageById

findPageById :: Statement Data.Int.Int32 (Maybe T.Text)
findPageById =
  Statement
    "select content from pages where id = $1"
    (E.param E.int4)
    (D.rowMaybe (D.column D.text))
    True
