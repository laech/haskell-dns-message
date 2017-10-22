{-# LANGUAGE BinaryLiterals #-}

module Network.DNS.Message
  ( Message(..)
  , Question(..)
  , ResourceRecord(..)
  , Domain(..)
  , Label
  , mklabel
  , classIN
  , classANY
  , typeA
  , typeTXT
  , typeAAAA
  , typeOPT
  , typeANY
  , getMessage
  ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString           as ByteString
import           Data.ByteString.Char8     as Char8
import           Data.Int
import           Data.List                 as List
import           Data.Map                  as Map
import           Data.Word

data Message = Message
  { identifier  :: Word16
  , flags       :: Word16
  , questions   :: [Question]
  , answers     :: [ResourceRecord]
  , authorities :: [ResourceRecord]
  , additionals :: [ResourceRecord]
  } deriving (Show, Eq)

data Question = Question
  { qname  :: Domain
  , qtype  :: Word16
  , qclass :: Word16
  } deriving (Show, Eq)

data ResourceRecord = ResourceRecord
  { rname  :: Domain
  , rtype  :: Word16
  , rclass :: Word16
  , rttl   :: Word32
  , rdata  :: ByteString
  } deriving (Show, Eq)

newtype Label =
  Label ByteString
  deriving (Show, Eq)

newtype Domain =
  Domain [Label]
  deriving (Show, Eq)

classIN  =   1 :: Word16
classANY = 255 :: Word16

typeA    =   1 :: Word16
typeTXT  =  16 :: Word16
typeAAAA =  28 :: Word16
typeOPT  =  41 :: Word16
typeANY  = 255 :: Word16

mklabel :: String -> Label
mklabel = Label . Char8.pack -- TODO

getMessage :: Get Message
getMessage = do
  messageOffset <- bytesRead

  [identifier, flags] <- replicateM 2 getWord16be
  [qdcount, ancount, nscount, arcount] <-
    replicateM 4 (fromEnum <$> getWord16be)

  let getRRs = getResourceRecords messageOffset
  evalStateT
    (Message identifier flags
      <$> getQuestions messageOffset qdcount
      <*> getRRs ancount
      <*> getRRs nscount
      <*> getRRs arcount)
    Map.empty

-- A map of DNS labels where keys are relative offsets from the beginning of
-- the message, and values are labels at the offsets
type OffsetLabelMap = Map ByteOffset [Label]

getQuestions :: ByteOffset -> Int -> StateT OffsetLabelMap Get [Question]
getQuestions messageOffset count = replicateM count (getQuestion messageOffset)

getQuestion :: ByteOffset -> StateT OffsetLabelMap Get Question
getQuestion messageOffset = StateT $ \map -> do
  (qname, map') <- runStateT (getDomain messageOffset) map
  qtype <- getWord16be
  qclass <- getWord16be
  return (Question qname qtype qclass, map')

getResourceRecords :: ByteOffset -> Int -> StateT OffsetLabelMap Get [ResourceRecord]
getResourceRecords messageOffset count = replicateM count (getResourceRecord messageOffset)

getResourceRecord :: ByteOffset -> StateT OffsetLabelMap Get ResourceRecord
getResourceRecord messageOffset = StateT $ \map -> do
  (rname, map') <- runStateT (getDomain messageOffset) map
  rtype <- getWord16be
  rclass <- getWord16be
  rttl <- getWord32be
  rlength <- getWord16be
  rdata <- getByteString $ fromIntegral rlength
  return (ResourceRecord rname rtype rclass rttl rdata, map')

getDomain :: ByteOffset -> StateT OffsetLabelMap Get Domain
getDomain messageOffset = StateT $ \map -> do
  offset <- bytesRead
  (labels, map') <- runStateT getLabels map
  return (Domain labels, Map.insert (offset - messageOffset) labels map')

getLabels :: StateT OffsetLabelMap Get [Label]
getLabels = StateT $ \map -> do
  (result, map') <- runStateT getLabel map
  case result of
    ([], _)         -> return ([], map')
    (labels, False) -> return (labels, map')
    (labels, True)  -> runStateT ((labels++) <$> getLabels) map'

getLabel :: StateT OffsetLabelMap Get ([Label], Bool)
getLabel = StateT $ \map -> do
  prefix <- getWord8
  let high2b = prefix `shiftR` 6 .&. 0b11
      length = prefix .&. 0b00111111
  case high2b of
    0b00 -> readLabel length map
    0b11 -> findLabel length map
    0b01 -> fail "TODO 0b01"
    0b10 -> fail "TODO 0b10"

readLabel :: Word8 -> OffsetLabelMap -> Get (([Label], Bool), OffsetLabelMap)
readLabel length map = do
  bs <- getByteString (fromIntegral length)
  return $ if ByteString.null bs
  then (([], False), map)
  else (([Label bs], True), map)

findLabel :: Word8 -> OffsetLabelMap -> Get (([Label], Bool), OffsetLabelMap)
findLabel length0 map = do
  length1 <- getWord8
  let pointer = combineToInt64 length0 length1
  case Map.lookup (combineToInt64 length0 length1) map of
    Just x  -> return ((x, False), map)
    Nothing -> fail $ "Invalid pointer " ++ show pointer ++ ", valid labels are: " ++ show map

combineToInt64 :: Word8 -> Word8 -> Int64
combineToInt64 high low =
  fromIntegral $ (fromIntegral high :: Word16) `shiftL` 8 + fromIntegral low
