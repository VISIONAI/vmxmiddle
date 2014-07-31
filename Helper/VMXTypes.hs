{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Helper.VMXTypes where

import Data.Aeson 
import Data.Maybe (fromMaybe)
import Prelude
import GHC.Generics
import Data.Typeable (Typeable)
import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad     (mzero)

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
               
               save_features                 ::      Bool
,              threshold                     ::      Double
,              train_max_negatives_in_cache  ::      Int
,              nms_threshold                 ::      Double
,              detect_add_flip               ::      Bool
,              levels_per_octave             ::      Int
,              ilearn_threshold              ::      Double
,              ilearn_iterations             ::      Int
,              max_windows                   ::      Int
,              max_image_size                ::      Int
,              ilearn_max_positives          ::      Int
,              sbin                          ::      Int
,              max_template_dim              ::      Int
,              crop_radius                   ::      Int
,              crop_threshold                ::      Double
,              display_threshold             ::      Double
,              jpeg_quality                  ::      Int
,              initialize_add_flip           ::      Bool
,              ilearn                        ::      Bool
} deriving (Generic, Typeable)
instance ToJSON   VMXParams
instance FromJSON VMXParams where
    parseJSON (Object o) =
        VMXParams  False (-10000) (2000) <$> (o .: "nms_threshold") 
                         <*> (o .: "detect_add_flip") 
                         <*> (o .: "levels_per_octave") 
                         <*> (o .: "ilearn_threshold")
                         <*> (o .: "ilearn_iterations")
                         <*> (o .: "max_windows")
                         <*> (o .: "max_image_size")
                         <*> (o .: "ilearn_max_positives")
                         <*> (o .: "sbin")
                         <*> (o .: "max_template_dim")
                         <*> (o .: "crop_radius")
                         <*> (o .: "crop_threshold")
                         <*> (o .: "display_threshold")
                         <*> (o .: "jpeg_quality")
                         <*> (o .: "initialize_add_flip")
                         <*> (o .: "ilearn")
    parseJSON _ = mzero
