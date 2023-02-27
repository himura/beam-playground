{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Int (Int32)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Time (NominalDiffTime)
import Database (
    ImageDb (_tableImage, _tableImageTag, _tableTag),
    imageDb,
 )
import Database.Beam (
    HasSqlEqualityCheck,
    HasSqlInTable,
    Q,
    QGenExpr,
    QGroupable (group_),
    QValueContext,
    SqlEq ((==.)),
    SqlIn (in_),
    SqlOrd ((>=.)),
    SqlValable (val_),
    aggregate_,
    all_,
    as_,
    asc_,
    count_,
    default_,
    filter_,
    guard_,
    insert,
    insertExpressions,
    orderBy_,
    pk,
    references_,
    runInsert,
    runSelectReturningList,
    select,
    subselect_,
 )
import Database.Beam.MySQL (
    MySQL,
    MySQLM,
    runBeamMySQLDebug,
    runInsertRowReturning,
 )
import Database.MySQL.Base (ConnectInfo (..), MySQLConn, defaultConnectInfo, withTransaction)
import Database.MySQL.Base qualified as MySQL (close, connect)
import Entity (
    ImageReviewStatus (New),
    ImageT (Image, _imageCreatedAt),
    ImageTagT (ImageTag, _imagetagImageId, _imagetagTagId),
    PrimaryKey (ImageId, TagId),
    Tag,
    TagId,
    TagT (Tag),
 )

-- oops
instance HasSqlInTable MySQL

newMySQLPool
    :: ConnectInfo
    -> Int
    -- ^ The number of stripes (distinct sub-pools) to maintain. The smallest acceptable value is 1.
    -> NominalDiffTime
    -- ^ Amount of time for which an unused resource is kept open. The smallest acceptable value is 0.5 seconds.
    -> Int
    -- ^ Maximum number of resources to keep open per stripe. The smallest acceptable value is 1.
    -> IO (Pool MySQLConn)
newMySQLPool connectInfo = createPool (MySQL.connect connectInfo) MySQL.close

runDB :: Pool MySQLConn -> MySQLM a -> IO a
runDB pool act = do
    withResource pool $ \conn -> withTransaction conn $ runBeamMySQLDebug T.putStrLn conn act

createTag :: Text -> MySQLM (Maybe Tag)
createTag name = do
    runInsertRowReturning $
        insert (_tableTag imageDb) $
            insertExpressions [Tag default_ (val_ name) default_ default_]

tagIdsRelation
    :: (HasSqlEqualityCheck be Int32, HasSqlInTable be)
    => Bool
    -> ImageT (QGenExpr QValueContext be s)
    -> [TagId]
    -> Q be ImageDb s ()
tagIdsRelation matchAll images tagIds = do
    (m, _) <-
        subselect_ $ do
            filter_ (\(_, cnt) -> numTagsCond matchAll cnt)
                $ aggregate_
                    ( \it ->
                        let TagId tagId = _imagetagTagId it
                         in (group_ (_imagetagImageId it), as_ @Int32 $ count_ tagId)
                    )
                $ do
                    it <- all_ (_tableImageTag imageDb)
                    guard_ (_imagetagTagId it `in_` map val_ tagIds)
                    return it
    guard_ (m `references_` images)
  where
    numTagsCond True cnt = cnt ==. fromIntegral (length tagIds)
    numTagsCond False cnt = cnt >=. 1

main :: IO ()
main = do
    let connectInfo = defaultConnectInfo{ciDatabase = "testdb", ciUser = "test", ciPassword = "test"}
    connPool <- newMySQLPool connectInfo 1 10 5
    runDB connPool $
        runInsert $
            insert (_tableImage imageDb) $
                insertExpressions
                    [ Image (val_ 1) (val_ "36a65bcfa302abe39c87445988f5b273") (val_ New) default_ default_
                    , Image (val_ 2) (val_ "c905a8f04a70681a8bdc07981f00ffec") (val_ New) default_ default_
                    , Image default_ (val_ "441ce56d58b90f7293a33cc6e3000ab0") (val_ New) default_ default_
                    ]
    Just img <-
        runDB connPool $
            runInsertRowReturning $
                insert (_tableImage imageDb) $
                    insertExpressions [Image default_ (val_ "a106119c85508d85f0e6b1098701d7a7") (val_ New) default_ default_]
    print img

    allImages <-
        runDB connPool $
            runSelectReturningList $
                select $
                    do orderBy_ (asc_ . _imageCreatedAt) $ all_ (_tableImage imageDb)
    print allImages

    Just tag1 <- runDB connPool $ createTag "test"
    Just tag2 <- runDB connPool $ createTag "foo"

    runDB connPool $
        runInsert $
            insert (_tableImageTag imageDb) $
                insertExpressions
                    [ ImageTag (val_ $ ImageId 1) (val_ (pk tag1)) default_
                    , ImageTag (val_ $ ImageId 2) (val_ (pk tag1)) default_
                    , ImageTag (val_ $ ImageId 2) (val_ (pk tag2)) default_
                    , ImageTag (val_ (pk img)) (val_ (pk tag2)) default_
                    ]
    imgWithTag1 <-
        runDB connPool $
            runSelectReturningList $
                select $ do
                    images <- all_ (_tableImage imageDb)
                    m <-
                        subselect_ $ do
                            it <- all_ (_tableImageTag imageDb)
                            guard_ (_imagetagTagId it ==. val_ (pk tag1))
                            pure $ _imagetagImageId it
                    guard_ (m `references_` images)
                    return images

    print imgWithTag1
    imgWithTag2 <-
        runDB connPool $
            runSelectReturningList $
                select $ do
                    images <- all_ (_tableImage imageDb)
                    m <-
                        subselect_ $ do
                            it <- all_ (_tableImageTag imageDb)
                            guard_ (_imagetagTagId it ==. val_ (pk tag2))
                            pure $ _imagetagImageId it
                    guard_ (m `references_` images)
                    return images
    print imgWithTag2

    imgWithTag1And2 <-
        runDB connPool $
            runSelectReturningList $
                select $ do
                    images <- all_ (_tableImage imageDb)
                    tagIdsRelation True images [pk tag1, pk tag2]
                    return images
    print imgWithTag1And2

    imgWithTag1OrAnd2 <-
        runDB connPool $
            runSelectReturningList $
                select $ do
                    images <- all_ (_tableImage imageDb)
                    tagIdsRelation False images [pk tag1, pk tag2]
                    return images
    print imgWithTag1OrAnd2
