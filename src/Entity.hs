{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity (
    DigestMD5,
    ImageReviewStatus (..),
    Image,
    ImageT (..),
    ImageId,
    PrimaryKey (ImageId, TagId, ImageTagId),
    Tag,
    TagT (..),
    TagId,
    ImageTag,
    ImageTagT (..),
    ImageTagId,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam (
    Beamable,
    Columnar,
    FromBackendRow (fromBackendRow),
    Generic,
    Identity,
    Table (..),
 )
import Database.Beam.Backend (BeamBackend, HasSqlValueSyntax (..))

type DigestMD5 = Text

data ImageReviewStatus
    = New
    | Approved
    | Rejected
    | Deleted
    deriving (Show, Read, Eq, Ord, Enum)

instance HasSqlValueSyntax be Int32 => HasSqlValueSyntax be ImageReviewStatus where
    sqlValueSyntax = sqlValueSyntax . fromIntegral @Int @Int32 . fromEnum

instance (BeamBackend be, FromBackendRow be Int32) => FromBackendRow be ImageReviewStatus where
    fromBackendRow = toEnum . fromIntegral @Int32 @Int <$> fromBackendRow

data ImageT f = Image
    { _imageId :: Columnar f Int32
    , _imageHash :: Columnar f DigestMD5
    , _imageReviewStatus :: Columnar f ImageReviewStatus
    , _imageCreatedAt :: Columnar f LocalTime
    , _imageUpdatedAt :: Columnar f LocalTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

type Image = ImageT Identity
deriving instance Show Image
deriving instance Eq Image

type ImageId = PrimaryKey ImageT Identity
deriving instance Show ImageId
deriving instance Eq ImageId

instance Table ImageT where
    data PrimaryKey ImageT f
        = ImageId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = ImageId . _imageId

data TagT f = Tag
    { _tagId :: Columnar f Int32
    , _tagName :: Columnar f Text
    , _tagCreatedAt :: Columnar f LocalTime
    , _tagUpdatedAt :: Columnar f LocalTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

type Tag = TagT Identity
deriving instance Show Tag
deriving instance Eq Tag

type TagId = PrimaryKey TagT Identity
deriving instance Show TagId
deriving instance Eq TagId

instance Table TagT where
    data PrimaryKey TagT f
        = TagId (Columnar f Int32)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = TagId . _tagId

data ImageTagT f = ImageTag
    { _imagetagImageId :: PrimaryKey ImageT f
    , _imagetagTagId :: PrimaryKey TagT f
    , _imagetagCreatedAt :: Columnar f LocalTime
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

type ImageTag = ImageTagT Identity
deriving instance Show ImageTag
deriving instance Eq ImageTag

type ImageTagId = PrimaryKey ImageTagT Identity

instance Table ImageTagT where
    data PrimaryKey ImageTagT f
        = ImageTagId (PrimaryKey ImageT f) (PrimaryKey TagT f)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey = ImageTagId <$> _imagetagImageId <*> _imagetagTagId
