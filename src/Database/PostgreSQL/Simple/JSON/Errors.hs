{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Simple.JSON.Errors where

import           Prelude                          (Int, fromEnum)

import           Data.Function                    (($))
import           Data.Functor                     (fmap)
import           Data.Maybe                       (Maybe (Just, Nothing), maybe)
import           Data.Monoid                      ((<>))

import           Control.Applicative              (pure, (<$>), (<*>))
import           Control.Category                 ((.))
import           Control.Monad                    (fail, mzero)

import           Data.Bool                        (Bool (True, False))
import           Data.String                      (String)
import           Data.Text                        (Text, unpack)
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Text.Read                        (readMaybe)
import           Text.Show                        (show)

import           GHC.Real                         (fromIntegral)

import           Database.PostgreSQL.Simple       (ExecStatus (..), Query (..),
                                                   QueryError (..),
                                                   ResultError (..),
                                                   SqlError (..))
import           Database.PostgreSQL.Simple.Types (Oid (..))

import           Data.Aeson                       (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                       as Aeson

import           Data.JSON.Schema                 (Field (Field),
                                                   JSONSchema (schema),
                                                   LengthBound (..), Schema (Object, Value, Choice, Constant))
import           Data.JSON.Schema.Combinators     (enum, value)

execStatusToText :: ExecStatus -> Text
execStatusToText EmptyQuery    = "empty_query"
execStatusToText CommandOk     = "command_ok"
execStatusToText TuplesOk      = "tuples_ok"
execStatusToText CopyOut       = "copy_out"
execStatusToText CopyIn        = "copy_in"
execStatusToText BadResponse   = "bad_response"
execStatusToText NonfatalError = "non_fatal_error"
execStatusToText FatalError    = "fatal_error"

execStatusFromText :: Text -> Maybe ExecStatus
execStatusFromText "empty_query"     = Just EmptyQuery
execStatusFromText "command_ok"      = Just CommandOk
execStatusFromText "tuples_ok"       = Just TuplesOk
execStatusFromText "copy_out"        = Just CopyOut
execStatusFromText "copy_in"         = Just CopyIn
execStatusFromText "bad_response"    = Just BadResponse
execStatusFromText "non_fatal_error" = Just NonfatalError
execStatusFromText "fatal_error"     = Just FatalError
execStatusFromText _ = Nothing

instance FromJSON ExecStatus where
  parseJSON (Aeson.String s) =
    maybe (fail $ "Invalid ExecStatus: " <> unpack s) pure
    . execStatusFromText
    $ s
  parseJSON _ = mzero

instance ToJSON ExecStatus where
  toJSON = toJSON . execStatusToText

instance JSONSchema QueryError where
  schema _ = Object
    [ Field "message" True (Value (LengthBound Nothing Nothing))
    , Field "query"   True (Value (LengthBound Nothing Nothing))
    ]

instance FromJSON QueryError where
  parseJSON (Aeson.Object e) = QueryError
    <$> e Aeson..: "message"
    <*> e Aeson..: "query"
  parseJSON _ = mzero

instance ToJSON QueryError where
  toJSON (QueryError m q) = Aeson.object
    [ "message" Aeson..= m
    , "query"   Aeson..= q
    ]

instance FromJSON Query where
  parseJSON (Aeson.String s) =
    maybe (fail $ "Invalid Query: " <> unpack s) pure
    . readMaybe
    . unpack
    $ s
  parseJSON _ = mzero

instance ToJSON Query where
  toJSON = toJSON . show

instance JSONSchema SqlError where
  schema _ = Object
    [ Field "sqlState" True (Value (LengthBound Nothing Nothing))
    , Field "sqlExecStatus" True execSum
    ]
    where
      execSum = Choice
        . fmap (Constant . Aeson.String . execStatusToText)
        $ [EmptyQuery .. FatalError]

instance FromJSON SqlError where
  parseJSON (Aeson.Object e) = SqlError
    <$> (fmap encodeUtf8 $ e Aeson..: "sqlState")
    <*>                    e Aeson..: "sqlExecStatus"
    <*> (fmap encodeUtf8 $ e Aeson..: "sqlErrorMsg")
    <*> (fmap encodeUtf8 $ e Aeson..: "sqlErrorDetail")
    <*> (fmap encodeUtf8 $ e Aeson..: "sqlErrorHint")
  parseJSON _ = mzero

instance ToJSON SqlError where
  toJSON (SqlError st es em ed eh) = Aeson.object
    [ "sqlState"        Aeson..= decodeUtf8 st
    , "sqlExecStatus"   Aeson..= es
    , "sqlErrorMessage" Aeson..= decodeUtf8 em
    , "sqlErrorDetail"  Aeson..= decodeUtf8 ed
    , "sqlErrorHint"    Aeson..= decodeUtf8 eh
    ]

instance JSONSchema ResultError where
  schema _ = Object
    [ Field "type"        True  (enum ["incompatible","unexpectedNull","conversionFailed"])
    , Field "sqlType"     True  value
    , Field "tableOid"    False value
    , Field "sqlField"    True  value
    , Field "haskellType" True  value
    , Field "errMsg"      True  value
    ]

instance FromJSON ResultError where
  parseJSON (Aeson.Object o) = do
    t   <- o Aeson..: "type"
    con <- case (t::Text) of
      "incompatible"     -> pure Incompatible
      "unexpectedNull"   -> pure UnexpectedNull
      "conversionFailed" -> pure ConversionFailed
      _ -> fail $ "Invalid type" <> unpack t
    con
      <$> o Aeson..: "sqlType"
      <*> (fmap toOid <$> (o Aeson..: "tableOid"))
      <*> o Aeson..: "sqlField"
      <*> o Aeson..: "haskellType"
      <*> o Aeson..: "errMsg"
    where
      toOid :: Int -> Oid
      toOid = Oid . fromIntegral
  parseJSON _ = mzero

instance ToJSON ResultError where
  toJSON = resultErrorToJson . splitTuple
    where
      splitTuple :: ResultError -> (Text,String,Maybe Oid,String,String,String)
      splitTuple (Incompatible st to sf ht m) =
        ("incompatible",st,to,sf,ht,m)
      splitTuple (UnexpectedNull st to sf ht m) =
        ("unexpectedNull",st,to,sf,ht,m)
      splitTuple (ConversionFailed st to sf ht m) =
        ("conversionFailed",st,to,sf,ht,m)
      resultErrorToJson (t,st,to,sf,ht,m) = Aeson.object
        [ "type"        Aeson..= t
        , "sqlType"     Aeson..= st
        , "tableOid"    Aeson..= fmap (\ (Oid o) -> fromEnum o) to
        , "sqlField"    Aeson..= sf
        , "haskellType" Aeson..= ht
        , "errMsg"      Aeson..= m
        ]
