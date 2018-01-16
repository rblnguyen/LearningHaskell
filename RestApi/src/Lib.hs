{-# LANGUAGE OverloadedStrings #-}
module Lib 
    (
        someFunc
    )where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText,encode, decode)
import Data.Aeson.Types (typeMismatch, ToJSONKey, FromJSONKey)
import Data.Text (Text)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ScoreID
    = GoodProbability
    | FirstPaymentDefault
    | FirstPaymentMissed
    | SameAsCash
    deriving (Eq, Show, Ord)

instance ToJSON ScoreID where
    toJSON = String . scoreIDToText

instance FromJSON ScoreID where
    parseJSON = withText "ScoreID" $ \t ->
        case scoreIDFromText t of
        Nothing -> typeMismatch "ScoreID" (String t)
        Just s -> pure s

scoreIDToText :: ScoreID -> Text
scoreIDToText GoodProbability = "good_probability"
scoreIDToText FirstPaymentDefault = "first_payment_default"
scoreIDToText FirstPaymentMissed = "first_payment_missed"
scoreIDToText SameAsCash = "same_as_cash"


scoreIDFromText :: Text -> Maybe ScoreID
scoreIDFromText "good_probability" = Just GoodProbability
scoreIDFromText "first_payment_default" = Just FirstPaymentDefault
scoreIDFromText "first_payment_missed" = Just FirstPaymentMissed
scoreIDFromText "same_as_cash" = Just SameAsCash
scoreIDFromText _ = Nothing

instance ToJSONKey ScoreID
instance FromJSONKey ScoreID