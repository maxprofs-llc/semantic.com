{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveAnyClass, DuplicateRecordFields #-}
module Command.Files
( readFile
, readBlobPairsFromHandle
, readBlobsFromHandle
, languageForFilePath
) where

import Control.Exception (catch, IOException)
import Data.Aeson
import Data.These
import Data.Functor.Both
import qualified Data.Blob as Blob
import Data.Source
import Data.String
import Language
import Prologue hiding (readFile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Prelude (fail)
import System.FilePath


-- | Read a file to a Blob, transcoding to UTF-8 along the way.
readFile :: FilePath -> Maybe Language -> IO Blob.Blob
readFile path language = do
  raw <- (Just <$> B.readFile path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe ByteString))
  pure $ fromMaybe (Blob.emptyBlob path) (Blob.sourceBlob path language . fromBytes <$> raw)

-- | Return a language based on a FilePath's extension, or Nothing if extension is not found or not supported.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . toS . takeExtension

-- | Read JSON encoded blob pairs from a handle.
readBlobPairsFromHandle :: Handle -> IO [Both Blob.Blob]
readBlobPairsFromHandle = fmap toBlobPairs . readFromHandle
  where
    toBlobPairs BlobDiff{..} = toBlobPair <$> blobs
    toBlobPair blobs = Join (fromThese empty empty (runJoin (toBlob <$> blobs)))
      where empty = Blob.emptyBlob (mergeThese const (runJoin (path <$> blobs)))

-- | Read JSON encoded blobs from a handle.
readBlobsFromHandle :: Handle -> IO [Blob.Blob]
readBlobsFromHandle = fmap toBlobs . readFromHandle
  where toBlobs BlobParse{..} = fmap toBlob blobs

readFromHandle :: FromJSON a => Handle -> IO a
readFromHandle h = do
  input <- BL.hGetContents h
  case decode input of
    Just d -> pure d
    Nothing -> die ("invalid input on " <> show h <> ", expecting JSON")

toBlob :: Blob -> Blob.Blob
toBlob Blob{..} = Blob.sourceBlob path language' (fromText content)
  where language' = case language of
          "" -> languageForFilePath path
          _ -> readMaybe language


newtype BlobDiff = BlobDiff { blobs :: [BlobPair] }
  deriving (Show, Generic, FromJSON)

newtype BlobParse = BlobParse { blobs :: [Blob] }
  deriving (Show, Generic, FromJSON)

type BlobPair = Join These Blob

data Blob = Blob
  { path :: FilePath
  , content :: Text
  , language :: String
  }
  deriving (Show, Generic, FromJSON)

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \o -> do
    before <- o .:? "before"
    after <- o .:? "after"
    case (before, after) of
      (Just b, Just a) -> pure $ Join (These b a)
      (Just b, Nothing) -> pure $ Join (This b)
      (Nothing, Just a) -> pure $ Join (That a)
      _ -> fail "Expected object with 'before' and/or 'after' keys only"