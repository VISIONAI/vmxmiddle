{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Helper.VMXTypes where

import Data.Aeson 
import Prelude
import GHC.Generics
import Data.Typeable (Typeable)

data VMXObject = VMXObject {
    vmxOName  :: String,
    vmxOBB    :: [Float],
    vmxOScore :: Maybe Float,
    vmxOExtra :: Maybe [Float]
}

instance ToJSON VMXObject where
    toJSON (VMXObject name bbs score extra) =
            object ["name" .= name, "bb" .= bbs, "score" .= score, "extra" .= extra]


data VMXImage = VMXImage {
    vmxIImage :: String,
    vmxITime  :: String,
    vmxIObjs  :: [VMXObject]
}

instance ToJSON VMXImage where
    toJSON (VMXImage image time objects) =
            object ["image" .= image, "time" .= time, "objects" .= objects]


data CreateModel = CreateModel {
    cmImages :: [VMXImage],
    cmParams :: Value,
    cmName   :: String
}

data VMXParams = VMXParams {  
               nms_threshold                 ::      Double
,              threshold                     ::      Double
,              ilearn_threshold              ::      Double
,              ilearn_iterations             ::      Int
,              levels_per_octave             ::      Int
,              max_windows                   ::      Int
,              max_image_size                ::      Int
,              ilearn_max_positives          ::      Int
,              sbin                          ::      Int
,              max_template_dum              ::      Int
,              train_max_negatives_in_cache  ::      Int
,              crop_radius                   ::      Int
,              crop_threshold                ::      Int
,              jpeg_quality                  ::      Int
,              detect_add_flip               ::      Bool
,              initialize_add_flip           ::      Bool
,              save_features                 ::      Bool
,              ilearn                        ::      Bool
} deriving (Generic, Typeable)
