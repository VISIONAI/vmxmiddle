module Handler.ModelDB where

import Import
import Helper.Shared

getModelDBR :: Handler TypedContent
getModelDBR = do
    mAid <- maybeAuthId
    models <- runDB $ selectList ([ModelUser ==. mAid]) []
    returnReps' $ object ["data" .= clean models]
    where
        clean :: [Entity Model] -> [Model]
        clean = map entityVal


postModelDBR :: Handler Html
postModelDBR = error "Not yet implemented: postModelDBR"
