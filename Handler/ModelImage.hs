module Handler.ModelImage where

import Import



getModelImageR :: ModelId -> Handler Html
getModelImageR muid = (<> "models/" <> muid <> "/image.jpg") <$> wwwDir >>= sendFile "image/jpg"
