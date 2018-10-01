{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- | Options used to derive FromJSON/ToJSON instance. These options generally
comply to our style regarding names. Of course sometimes they don't fit one's
needs, so treat them as just sensible defaults.
-}

module Data.Aeson.Options
       ( -- * Custom options
         defaultOptions
       , leaveTagOptions
       , defaultOptionsPS
       , stripTypeOptions

         -- * Generic functions
       , genericParseJSONStripType
       , genericToJSONStripType
       ) where

import Data.Aeson.Types (Parser)
import Data.Char (isLower, isPunctuation, isUpper, toLower)
import Data.List (findIndex, isPrefixOf)
import GHC.Generics (Generic, Rep)
import Type.Reflection (Typeable, typeRep)

import qualified Data.Aeson as A

headToLower :: String -> String
headToLower []     = error "Can not use headToLower on empty String"
headToLower (x:xs) = toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
    maybe t (flip drop t . decrementSafe) $ findIndex isLower t
  where
    decrementSafe 0 = 0
    decrementSafe i = i - 1

-- | These options do the following transformations:
-- 1. Names of field
-- records are assumed to be camelCased, `camel` part is removed,
-- `Cased` part is converted to `cased`. So `camelCased` becomes
-- `cased`. Also all punctuation symbols are dropped before doing it.
-- 2. Constructors are assumed to start with some capitalized prefix
-- (which finished right before the last capital letter). This prefix
-- is dropped and then the first letter is lowercased.
defaultOptions :: A.Options
defaultOptions =
    A.defaultOptions
    { A.fieldLabelModifier = headToLower . stripFieldPrefix . dropPunctuation
    , A.constructorTagModifier = headToLower . stripConstructorPrefix
    , A.sumEncoding = A.ObjectWithSingleField
    }

-- | These options are the same as `defaultOptions`, but they don't
-- modify constructor tags.
leaveTagOptions :: A.Options
leaveTagOptions = defaultOptions { A.constructorTagModifier = id }

-- | Options used for communication with PureScript by default.
defaultOptionsPS :: A.Options
defaultOptionsPS =
    A.defaultOptions
    { A.constructorTagModifier = headToLower . stripConstructorPrefix
    }

{- | Allows to create 'A.FromJSON' instance that strips the data type name prefix
from every field. Doesn't change name of the fields that doesn't start with the
type name.

>>> data Foo = Foo { fooBar :: String, fooQuux :: Int } deriving (Generic, Show)
>>> instance FromJSON Foo where parseJSON = genericParseJSONStripType
>>> decode @Foo "{ \"bar\": \"test\", \"quux\": 42 }"
Just (Foo {fooBar = "test", fooQuux = 42})
-}
genericParseJSONStripType
    :: forall a .
       (Typeable a, Generic a, A.GFromJSON A.Zero (Rep a))
    => A.Value
    -> Parser a
genericParseJSONStripType = A.genericParseJSON (stripTypeOptions @a)

{- | Allows to create 'A.ToJSON' instance that strips the data type name prefix
from every field. Doesn't change name of the fields that doesn't start with the
type name.

>>> data Foo = Foo { fooBar :: String, fooQuux :: Int } deriving (Generic, Show)
>>> instance ToJSON Foo where toJSON = genericToJSONStripType
>>> encode $ Foo { fooBar = "test", fooQuux = 42 }
"{\"quux\":42,\"bar\":\"test\"}"
-}
genericToJSONStripType
    :: forall a .
       (Typeable a, Generic a, A.GToJSON A.Zero (Rep a))
    => a
    -> A.Value
genericToJSONStripType = A.genericToJSON (stripTypeOptions @a)

{- | Options to strip type name from the field names. See
'genericParseJSONStripType' and 'genericToJSONStripType' for examples.
-}
stripTypeOptions :: forall a . Typeable a => A.Options
stripTypeOptions = A.defaultOptions
    { A.fieldLabelModifier = stripTypeNamePrefix
    }
  where
    typeName :: String
    typeName = headToLower $ show $ typeRep @a

    stripTypeNamePrefix :: String -> String
    stripTypeNamePrefix fieldName =
        if typeName `isPrefixOf` fieldName
            then headToLower $ drop (length typeName) fieldName
            else fieldName
