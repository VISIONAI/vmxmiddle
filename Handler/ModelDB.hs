module Handler.ModelDB where

import Import
import Helper.Shared

getModelDBR :: Handler TypedContent
getModelDBR = do
    mAid <- maybeAuthId
    models <- runDB $ selectList ([ModelUser ==. mAid]) []
    returnReps $ show models


postModelDBR :: Handler Html
postModelDBR = error "Not yet implemented: postModelDBR"
