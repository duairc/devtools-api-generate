{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DevTools.Meta.Protocol
    ( Protocol (Protocol)
    , Version (Version)
    , Domain (Domain)
    , Data (Data)
    , Property (Property)
    , Command (Command)
    , Event (Event)
    , Type (..)
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, ToJSON
                     , Options, defaultOptions
                     , fieldLabelModifier, omitNothingFields
                     , parseJSON, genericParseJSON
                     , toEncoding, genericToEncoding, toJSON, genericToJSON
                     , withObject, (.:), (.:?)
                     , Encoding, Value
                     )
import qualified Data.Aeson as A
import           Data.Aeson.Encoding (pair)


-- base ----------------------------------------------------------------------
import           Data.Maybe (catMaybes)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- text ----------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T


------------------------------------------------------------------------------
options :: Options
options = defaultOptions
    { fieldLabelModifier = reverse . dropWhile (== '_') . reverse
    , omitNothingFields = True
    }


------------------------------------------------------------------------------
data Protocol = Protocol
    { version :: !Version
    , domains :: ![Domain]
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Protocol where
    parseJSON = genericParseJSON options


------------------------------------------------------------------------------
instance ToJSON Protocol where
    toEncoding = genericToEncoding options
    toJSON = genericToJSON options


------------------------------------------------------------------------------
data Version = Version
    { major :: !Text
    , minor :: !Text
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Version where
    parseJSON = genericParseJSON options


------------------------------------------------------------------------------
instance ToJSON Version where
    toEncoding = genericToEncoding options
    toJSON = genericToJSON options


------------------------------------------------------------------------------
data Domain = Domain
    { domain :: !Text
    , description :: !(Maybe Text)
    , experimental :: !(Maybe Bool)
    , deprecated :: !(Maybe Bool)
    , dependencies :: !(Maybe [Text])
    , types :: !(Maybe [Data])
    , commands :: !(Maybe [Command])
    , events :: !(Maybe [Event])
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Domain where
    parseJSON = genericParseJSON options


------------------------------------------------------------------------------
instance ToJSON Domain where
    toEncoding = genericToEncoding options
    toJSON = genericToJSON options


------------------------------------------------------------------------------
data Data = Data
    { id :: !Text
    , description :: !(Maybe Text)
    , experimental :: !(Maybe Bool)
    , deprecated :: !(Maybe Bool)
    , type_ :: !Type
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Data where
    parseJSON v = do
        type_ <- parseJSON v
        withObject "type" (go type_) v
      where
        go type_ o = Data <$> o .: "id" <*> o .:? "description"
            <*> o .:? "experimental" <*> o .:? "deprecated" <*> pure type_


------------------------------------------------------------------------------
dataToPairs :: Data -> [Pair]
dataToPairs (Data i d x c t) = catMaybes
    [ "id" .=. i, "description" .=? d, "experimental" .=? x
    , "deprecated" .=? c
    ] ++ typeToPairs t


------------------------------------------------------------------------------
instance ToJSON Data where
    toEncoding = encoding . dataToPairs
    toJSON = object . dataToPairs


------------------------------------------------------------------------------
data Type
    = Any
    | Boolean
    | Integer
    | Number
    | String
    | Object
    | Enum ![Text]
    | Record ![Property]
    | Array !Type
    | Ref !Text
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Type where
    parseJSON = withObject "type" $ \o -> do
        type_ <- o .:? "type"
        case type_ of
            Just "any" -> pure Any
            Just "boolean" -> pure Boolean
            Just "integer" -> pure Integer
            Just "number" -> pure Number
            Just "string" -> maybe String Enum <$> o .:? "enum"
            Just "object" -> maybe Object Record <$> o .:? "properties"
            Just "array" -> Array <$> o .: "items"
            Nothing -> Ref <$> o .: "$ref"                
            Just x -> fail $ "unknown type: " ++ T.unpack x


------------------------------------------------------------------------------
typeToPairs :: Type -> [Pair]
typeToPairs t = case t of
    Any -> ["type" .= T.pack "any"]
    Boolean -> ["type" .= T.pack "boolean"]
    Integer -> ["type" .= T.pack "integer"]
    Number -> ["type" .= T.pack "number"]
    String -> ["type" .= T.pack "string"]
    Object -> ["type" .= T.pack "object"]
    Enum a -> ["type" .= T.pack "string", "enum" .= a]
    Record a -> ["type" .= T.pack "object", "properties" .= a]
    Array a -> ["type" .= T.pack "array", "items" .= a]
    Ref a -> ["$ref" .= a]


------------------------------------------------------------------------------
instance ToJSON Type where
    toEncoding = encoding . typeToPairs
    toJSON = object . typeToPairs


------------------------------------------------------------------------------
data Property = Property
    { name :: !Text
    , description :: !(Maybe Text)
    , optional :: !(Maybe Bool)
    , experimental :: !(Maybe Bool)
    , deprecated :: !(Maybe Bool)
    , type_ :: !Type
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Property where
    parseJSON v = do
        type_ <- parseJSON v
        withObject "property" (go type_) v
      where
        go type_ o = Property <$> o .: "name" <*> o .:? "description" 
            <*> o .:? "optional" <*> o .:? "experimental"
            <*> o .:? "deprecated" <*> pure type_


------------------------------------------------------------------------------
propertyToPairs :: Property -> [Pair]
propertyToPairs (Property n d o x c t) = catMaybes
    [ "name" .=. n, "description" .=? d, "optional" .=? o
    , "experimental" .=? x, "deprecated" .=? c
    ] ++ typeToPairs t


------------------------------------------------------------------------------
instance ToJSON Property where
    toEncoding = encoding . propertyToPairs
    toJSON = object . propertyToPairs


------------------------------------------------------------------------------
data Command = Command
    { name :: !Text
    , description :: !(Maybe Text)
    , experimental :: !(Maybe Bool)
    , deprecated :: !(Maybe Bool)
    , parameters :: !(Maybe [Property])
    , returns :: !(Maybe [Property])
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Command where
    parseJSON = genericParseJSON options


------------------------------------------------------------------------------
instance ToJSON Command where
    toEncoding = genericToEncoding options
    toJSON = genericToJSON options


------------------------------------------------------------------------------
data Event = Event
    { name :: !Text
    , description :: !(Maybe Text)
    , experimental :: !(Maybe Bool)
    , deprecated :: !(Maybe Bool)
    , parameters :: !(Maybe [Property])
    }
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable
    )


------------------------------------------------------------------------------
instance FromJSON Event where
    parseJSON = genericParseJSON options


------------------------------------------------------------------------------
instance ToJSON Event where
    toEncoding = genericToEncoding options
    toJSON = genericToJSON options


------------------------------------------------------------------------------
data Pair = Pair !Text !Encoding !Value


------------------------------------------------------------------------------
(.=) :: ToJSON a => Text -> a -> Pair
(.=) key = Pair key <$> toEncoding <*> toJSON


------------------------------------------------------------------------------
encoding :: [Pair] -> Encoding
encoding = A.pairs . foldMap go
  where
    go (Pair key value _) = pair key value


------------------------------------------------------------------------------
object :: [Pair] -> Value
object = A.object . map go
  where
    go (Pair key _ value) = (key, value)


------------------------------------------------------------------------------
(.=.) :: ToJSON a => Text -> a -> Maybe Pair
(.=.) = fmap pure . (.=)
infixr 8 .=.


------------------------------------------------------------------------------
(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
(.=?) = fmap . (.=)
infixr 8 .=?
