{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimulationTypes where

import Relude

import Web.Scotty.Trans
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.ByteString.Char8 (unpack)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Scientific (Scientific)

data Sex = Male | Female
    deriving (Show, Eq, Generic)

instance Parsable Sex where
    parseParam = parseSex

parseSex :: LText -> Either LText Sex
parseSex t
        | t' == LT.toCaseFold "male" = Right Male
        | t' == LT.toCaseFold "female" = Right Female
        | otherwise = Left "parseParam Sex: unsupported value"
        where
        t' = LT.toCaseFold t

parseSex' :: Text -> Either Text Sex
parseSex' t
        | t' == T.toCaseFold "male" = Right Male
        | t' == T.toCaseFold "female" = Right Female
        | otherwise = Left "parseParam Sex: unsupported value"
        where
        t' = T.toCaseFold t

sexToText :: Sex -> Text
sexToText Male = "male"
sexToText Female = "female"

instance FromField Sex where
    fromField f Nothing = returnError UnexpectedNull f ""
    fromField f (Just bs) = case parseSexBs of
        Left _ -> returnError ConversionFailed f (unpack bs)
        Right v -> return v
        where
            parseSexBs :: Either String Sex
            parseSexBs = let res =  parseSex . toLazy . decodeUtf8 $ bs
                in case res of
                    Left err -> Left $ toString err
                    Right v -> Right v

newtype SimulationId = SimulationId { unSimulationId :: Int64 }
    deriving (Show, Eq, Generic)
deriving newtype instance FromField SimulationId
deriving newtype instance ToField SimulationId

data Simulation = Simulation
    { simId :: !SimulationId
    , simAge :: !Int
    , simSex :: !Text
    , simWorkStart :: !Int
    , simWorkEnd :: !Int
    , simSalary :: !Scientific
    , simExpectedSalary :: !Scientific
    }
    deriving (Show, Generic)

instance FromRow Simulation where
    fromRow = Simulation <$> field <*> field <*> field <*> field <*> field <*> field <*> field

newtype ParameterId = ParameterId { unParameterId :: Int64 }
    deriving (Show, Eq, Generic)
deriving newtype instance FromField ParameterId
deriving newtype instance ToField ParameterId

data Params = Params
    { paramSalary :: !Double
    , paramExpectedSalary :: !Double
    , paramWorkEnd :: !Int
    }
    deriving (Show, Generic)
