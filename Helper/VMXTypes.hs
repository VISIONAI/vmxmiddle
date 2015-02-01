{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : VMXTypes
Description : VMX Types

Common data structures used in VMX
-}
module Helper.VMXTypes where

import Data.Aeson 
import Data.Maybe (fromMaybe)
import Prelude
import GHC.Generics
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))
import Control.Monad     (mzero)

{-|
The 'VMXObject' represents an object inside an image with a
mandatory name and bounding box.  The 'vmxOScore' is optional (doesn't
make sense for a manual bounding box annotation) and the 'vmxOExtra' field
can store an extra array of numbers associated with the object.
-}
data VMXObject = VMXObject {
    vmxOName  :: String,
    vmxOBB    :: [Float],
    vmxOScore :: Maybe Float,
    vmxOExtra :: Maybe [Float]
}

instance ToJSON VMXObject where
    toJSON (VMXObject name bbs score extra) =
            object ["name" .= name,
                    "bb" .= bbs,
                    "score" .= score,
                    "extra" .= extra]

instance FromJSON VMXObject where
    parseJSON (Object o) =
        VMXObject <$> (o .: "name")
        <*> (o .: "bb")
        <*> (o .:? "extra")
        <*> (o .:? "score")
    parseJSON _ = mzero

instance FromJSON VMXImage where
    parseJSON (Object o) =
        VMXImage <$> (o .: "image")
        <*> (o .:? "time")
        <*> (o .:? "objects")
    parseJSON _ = mzero

{-|
The 'VMXImage' represents an image (along with a time) as well as an
optional array of 'VMXObject's.
-}
data VMXImage = VMXImage {
    vmxIImage :: String,
    vmxITime  :: Maybe String,
    vmxIObjs  :: Maybe [VMXObject]
}

instance ToJSON VMXImage where
    toJSON (VMXImage image time objects) =
            object ["image" .= image,
                    "time" .= time,
                    "objects" .= objects']
            where
                    objects' = fromMaybe ([]) objects


{-|
The 'VMXParams' represents the object detection parameters for VMXserver.
-}
data VMXParams = VMXParams {                 
               train_max_negatives           ::      Int
,              train_max_positives           ::      Int
,              detect_max_overlap            ::      Double
,              detect_add_flip               ::      Bool
,              levels_per_octave             ::      Int
,              learn_threshold               ::      Double
,              learn_iterations              ::      Int
,              max_windows                   ::      Int
,              max_image_size                ::      Int
,              learn_max_positives           ::      Int
,              cell_size                     ::      Int
,              initialize_max_cells          ::      Int
,              crop_radius                   ::      Int
,              crop_threshold                ::      Double
,              display_threshold             ::      Double
,              jpeg_quality                  ::      Int
,              initialize_add_flip           ::      Bool
,              learn_mode                    ::      Bool
} deriving (Generic, Typeable)

instance ToJSON   VMXParams
instance FromJSON VMXParams where
    parseJSON (Object o) =
        VMXParams        <$> (o .: "train_max_negatives")
                         <*> (o .: "train_max_positives") 
                         <*> (o .: "detect_max_overlap") 
                         <*> (o .: "detect_add_flip") 
                         <*> (o .: "levels_per_octave") 
                         <*> (o .: "learn_threshold")
                         <*> (o .: "learn_iterations")
                         <*> (o .: "max_windows")
                         <*> (o .: "max_image_size")
                         <*> (o .: "learn_max_positives")
                         <*> (o .: "cell_size")
                         <*> (o .: "initialize_max_cells")
                         <*> (o .: "crop_radius")
                         <*> (o .: "crop_threshold")
                         <*> (o .: "display_threshold")
                         <*> (o .: "jpeg_quality")
                         <*> (o .: "initialize_add_flip")
                         <*> (o .: "learn_mode")
    parseJSON _ = mzero

{-|
The 'CreateModel' represents the input to model creation: an array of
'VMXImage's, some detection parameters, as well as a model name.
-}
data CreateModel = CreateModel {
    cmImages :: [VMXImage],
    cmParams :: Value,
    cmName   :: String
}



