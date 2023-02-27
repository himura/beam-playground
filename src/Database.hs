{-# LANGUAGE DeriveAnyClass #-}

module Database (
    ImageDb (..),
    imageDb,
) where

import Database.Beam
    ( Database
    , DatabaseSettings
    , Generic
    , TableEntity
    , dbModification
    , defaultDbSettings
    , fieldNamed
    , modifyTableFields
    , tableModification
    , withDbModification
    )
import Entity
    ( PrimaryKey(TagId, ImageId),
      ImageT,
      TagT,
      ImageTagT(_imagetagTagId, _imagetagImageId) )

data ImageDb f = ImageDb
    { _tableImage :: f (TableEntity ImageT)
    , _tableTag :: f (TableEntity TagT)
    , _tableImageTag :: f (TableEntity ImageTagT)
    }
    deriving stock (Generic)
    deriving anyclass (Database be)

imageDb :: DatabaseSettings be ImageDb
imageDb =
    defaultDbSettings
        `withDbModification` dbModification
            { _tableImageTag =
                modifyTableFields
                    tableModification
                        { _imagetagImageId = ImageId $ fieldNamed "image_id"
                        , _imagetagTagId = TagId $ fieldNamed "tag_id"
                        }
            }
