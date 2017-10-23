{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Control.Monad.Trans.State hiding (get, put)
import qualified Control.Monad.Trans.State as State
import           Data.Bimap                as Bimap
import           Data.Binary.Get           (ByteOffset, Get)
import qualified Data.Binary.Get           as Get
import qualified Data.Binary.Put
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
  deriving (Show, Eq, Ord)

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
getMessage = evalStateT get (MessageState 0 Bimap.empty)

type GetState a = StateT MessageState Get a

class BinaryState a where
  get :: StateT MessageState Get a

data MessageState = MessageState
  { offset :: ByteOffset
  , labels :: Bimap ByteOffset [Label]
  } deriving (Show)

increment :: Get a -> Int -> GetState a
increment g n = StateT $ \s -> do
  a <- g
  return (a, s { offset = offset s + fromIntegral n })

getWord8 = increment Get.getWord8 1
getWord16 = increment Get.getWord16be 2
getWord32 = increment Get.getWord32be 4
getByteString n = increment (Get.getByteString n) n

instance BinaryState Message where
  get = do
    [identifier, flags] <- replicateM 2 getWord16
    [qdcount, ancount, nscount, arcount] <- replicateM 4 (fromEnum <$> getWord16)
    Message identifier flags
        <$> replicateM qdcount get
        <*> replicateM ancount get
        <*> replicateM nscount get
        <*> replicateM arcount get

instance BinaryState Question where
  get = do
    qname <- get
    qtype <- getWord16
    qclass <- getWord16
    return $ Question qname qtype qclass

instance BinaryState ResourceRecord where
  get = do
    rname <- get
    rtype <- getWord16
    rclass <- getWord16
    rttl <- getWord32
    rlength <- getWord16
    rdata <- getByteString $ fromIntegral rlength
    return $ ResourceRecord rname rtype rclass rttl rdata

instance BinaryState Domain where
  get = Domain <$> get

instance BinaryState [Label] where
  get = do
    offset <- offset <$> State.get
    word <- getWord8
    let high2b = word `shiftR` 6 .&. 0b11
        length = word .&. 0b00111111

    case high2b of
      0b01 -> fail "Unsupported obsolete label type: 01"
      0b10 -> fail "Invalid label type: 10"
      0b11 -> findLabels length
      0b00 -> do
        string <- getByteString (fromIntegral length)
        if ByteString.null string
        then return []
        else (Label string :) <$> get >>= addLabels offset

    where
      addLabels :: ByteOffset -> [Label] -> GetState [Label]
      addLabels offset xs = do
        modify (\s -> s {labels = Bimap.insert offset xs (labels s)})
        return xs

      findLabels :: Word8 -> GetState [Label]
      findLabels length0 = do
        labels <- labels <$> State.get
        offset <- combineToInt64 length0 <$> getWord8
        case Bimap.lookup offset labels of
          Just x  -> return x
          Nothing -> fail $ "Invalid pointer " ++ show offset ++
            ", valid labels are: " ++ show labels

      combineToInt64 :: Word8 -> Word8 -> Int64
      combineToInt64 high low =
        fromIntegral $ (fromIntegral high :: Word16) `shiftL` 8 + fromIntegral low
