module Simulation where

import Relude

import Web.Scotty.Trans
import qualified Data.Text.Lazy             as T

data Sex = Male | Female
    deriving (Show, Eq, Generic)

instance Parsable Sex where
    parseParam t
        | t' == T.toCaseFold "male" = Right Male
        | t' == T.toCaseFold "female" = Right Female
        | otherwise = Left "parseParam Sex: unsupported value"
        where
        t' = T.toCaseFold t
