{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Helper.VMXTypes where

import Data.Aeson 
import Data.Maybe (fromMaybe)
import Prelude
import GHC.Generics
import Data.Typeable (Typeable)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad     (mzero)
import Data.Time (UTCTime)

data VMXObject = VMXObject {
    vmxOName  :: String,
    vmxOBB    :: [Float],
    vmxOScore :: Maybe Float,
    vmxOExtra :: Maybe [Float]
}

instance ToJSON VMXObject where
    toJSON (VMXObject name bbs score extra) =
            object ["name" .= name, "bb" .= bbs, "score" .= score, "extra" .= extra]

instance FromJSON VMXObject where
    parseJSON (Object o) =
        VMXObject <$> (o .: "name") <*> (o .: "bb") <*> (o .:? "extra") <*> (o .:? "score")
    parseJSON _ = mzero

    

instance FromJSON VMXImage where
    parseJSON (Object o) =
        VMXImage <$> (o .: "image") <*> (o .:? "time") <*> (o .:? "objects")
    parseJSON _ = mzero


data VMXImage = VMXImage {
    vmxIImage :: String,
    vmxITime  :: Maybe String,
    vmxIObjs  :: Maybe [VMXObject]
}

instance ToJSON VMXImage where
    toJSON (VMXImage image time objects) =
            object ["image" .= image, "time" .= time, "objects" .= objects']
            where
                    objects' = fromMaybe ([]) objects


data CreateModel = CreateModel {
    cmImages :: [VMXImage],
    cmParams :: Value,
    cmName   :: String
}


data VMXParams = VMXParams {                 
               train_max_negatives           ::      Maybe Int
,              train_max_positives           ::      Maybe Int
,              detect_max_overlap            ::      Maybe Double
,              detect_add_flip               ::      Maybe Bool
,              levels_per_octave             ::      Maybe Int
,              learn_threshold               ::      Maybe Double
,              learn_iterations              ::      Maybe Int
,              max_windows                   ::      Maybe Int
,              max_image_size                ::      Maybe Int
,              learn_max_positives           ::      Maybe Int
,              cell_size                     ::      Maybe Int
,              initialize_max_cells          ::      Maybe Int
,              crop_radius                   ::      Maybe Int
,              crop_threshold                ::      Maybe Double
,              display_threshold             ::      Maybe Double
,              jpeg_quality                  ::      Maybe Double
,              initialize_add_flip           ::      Maybe Bool
,              learn_mode                    ::      Maybe Bool
} deriving (Generic, Typeable)
instance ToJSON   VMXParams where
  toJSON (VMXParams a b c d e f g h i j k l m n o p q r) =
    object ["train_max_negatives" .= fromMaybe (1000) a,
            "train_max_positives" .= fromMaybe (2000) b,
            "detect_max_overlap" .= fromMaybe (0.1) c,
            "detect_add_flip" .= fromMaybe False d,
            "levels_per_octave" .= fromMaybe 10 e,
            "learn_threshold" .= fromMaybe 0.0 f,
            "learn_iterations" .= fromMaybe 10 g,
            "max_windows" .= fromMaybe 100 h,
            "max_image_size" .= fromMaybe 320 i,
            "learn_max_positives" .= fromMaybe 1 j,
            "cell_size" .= fromMaybe 4 k,
            "initialize_max_cells" .= fromMaybe 12 l,
            "crop_radius" .= fromMaybe 80 m,
            "crop_threshold" .= fromMaybe (-1.0) n,
            "display_threshold" .= fromMaybe (-1.0) o,
            "jpeg_quality" .= fromMaybe 1.0 p,
            "initialize_add_flip" .= fromMaybe False q,
            "learn_mode" .= fromMaybe False r]



instance FromJSON VMXParams where
    parseJSON (Object o) =
        VMXParams        <$> (o .:? "train_max_negatives")
                         <*> (o .:? "train_max_positives") 
                         <*> (o .:? "detect_max_overlap") 
                         <*> (o .:? "detect_add_flip") 
                         <*> (o .:? "levels_per_octave") 
                         <*> (o .:? "learn_threshold")
                         <*> (o .:? "learn_iterations")
                         <*> (o .:? "max_windows")
                         <*> (o .:? "max_image_size")
                         <*> (o .:? "learn_max_positives")
                         <*> (o .:? "cell_size")
                         <*> (o .:? "initialize_max_cells")
                         <*> (o .:? "crop_radius")
                         <*> (o .:? "crop_threshold")
                         <*> (o .:? "display_threshold")
                         <*> (o .:? "jpeg_quality")
                         <*> (o .:? "initialize_add_flip")
                         <*> (o .:? "learn_mode")
    parseJSON _ = mzero

type ErrorFlag = Int
data CreateModelResponse = CreateModelResponse ErrorFlag CreateModelData
instance FromJSON CreateModelResponse where
    parseJSON (Object o) = CreateModelResponse <$> o .: "error" 
                                               <*> o .: "data"
    parseJSON _ = mzero

instance ToJSON CreateModelResponse where
    toJSON (CreateModelResponse error data') = object ["data" .= data', "error" .= error]

data CreateModelData = CreateModelData VMXModel

instance FromJSON CreateModelData where
    parseJSON (Object o) = CreateModelData <$> o .: "model"
    parseJSON _ = mzero

instance ToJSON CreateModelData where
    toJSON (CreateModelData model) = object ["model" .= model]

data VMXModel = VMXModel {
    vmxModelUuid :: String,
    vmxModelName :: String,
    vmxModelSize :: [Int],  -- should be a tuple
    vmxModelNumPos  :: Int,
    vmxModelNumNeg  :: Int,
    vmxStartTime    :: UTCTime,
    vmxEndTime    :: UTCTime
}

instance FromJSON VMXModel where
    parseJSON (Object o) = VMXModel <$> o .: "uuid" 
                                    <*> o .: "name"
                                    <*> o .: "size"
                                    <*> o .: "num_pos"
                                    <*> o .: "num_neg"
                                    <*> o .: "start_time"
                                    <*> o .: "end_time"
    parseJSON _ = mzero

instance ToJSON VMXModel where
    toJSON (VMXModel uuid name size pos neg start end) =
            object ["uuid" .= uuid, "name" .= name, "size" .= size, "pos" .= pos, "neg" .= neg, "start" .= start, "end" .= end]
