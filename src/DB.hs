{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module DB where

import qualified Data.Text       as T
import qualified Hasql.Decoders  as D
import qualified Hasql.Encoders  as E
import           Hasql.Session
import           Hasql.Statement

import qualified Data.Int

getPage :: Data.Int.Int32 -> Session (Maybe T.Text)
getPage id = Hasql.Session.statement id findPageById

findPageById :: Statement Data.Int.Int32 (Maybe T.Text)
findPageById =
  Statement
    "select content from pages where id = $1"
    (E.param (E.nonNullable E.int4))
    (D.rowMaybe (D.column (D.nonNullable D.text)))
    True
