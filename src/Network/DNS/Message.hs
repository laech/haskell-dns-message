{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , putMessage
  ) where

import           Control.Monad
import           Control.Monad.Trans.State hiding (get, put)
import qualified Control.Monad.Trans.State as State
import           Data.Bimap                as Bimap
import           Data.Binary.Get           (ByteOffset, Get)
import qualified Data.Binary.Get           as Get
import           Data.Binary.Put           (PutM, Put)
import qualified Data.Binary.Put           as Put
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

putMessage :: Message -> Put
putMessage message = evalStateT (put message) (MessageState 0 Bimap.empty)

type GetState a = StateT MessageState Get a
type PutState a = StateT MessageState PutM a

class BinaryState a where
  get :: GetState a
  put :: a -> PutState ()

data MessageState = MessageState
  { offset :: ByteOffset
  , labels :: Bimap ByteOffset [Label]
  } deriving (Show)

increment :: (Monad m) => Int -> m a -> StateT MessageState m a
increment n m = StateT $ \s -> do
  a <- m
  return (a, s { offset = offset s + fromIntegral n })

getByteString n = increment n (Get.getByteString n)
putByteString s = increment (ByteString.length s) (Put.putByteString s)

instance BinaryState Word8 where
  get = increment 1   Get.getWord8
  put = increment 1 . Put.putWord8

instance BinaryState Word16 where
  get = increment 2   Get.getWord16be
  put = increment 2 . Put.putWord16be

instance BinaryState Word32 where
  get = increment 4   Get.getWord32be
  put = increment 4 . Put.putWord32be

instance BinaryState Message where
  get = do
    [identifier, flags, qdcount, ancount, nscount, arcount] <-
      replicateM 6 get
      
    let getN n = replicateM (fromEnum n) get
    Message identifier flags
        <$> getN qdcount
        <*> getN ancount
        <*> getN nscount
        <*> getN arcount

  put (Message identifier flags questions answers authorities additionals) = do
    put identifier
    put flags

    putLength questions
    putLength answers
    putLength authorities
    putLength additionals

    mapM_ put questions
    mapM_ put answers
    mapM_ put authorities
    mapM_ put additionals

    where
      putLength xs =
        let len = List.length xs
        in if len > fromIntegral (maxBound :: Word16)
        then fail "List to large"
        else put (fromIntegral len :: Word16)

instance BinaryState Question where
  get = Question <$> get <*> get <*> get
  put (Question qname qtype qclass) = do
    put qname
    put qtype
    put qclass

instance BinaryState ResourceRecord where
  get = ResourceRecord
    <$> get
    <*> get
    <*> get
    <*> get
    <*> (fromIntegral <$> (get :: GetState Word16) >>= getByteString)

  put (ResourceRecord rname rtype rclass rttl rdata) = do
    put rname
    put rtype
    put rclass
    put rttl
    putData rdata -- TODO
    where
      putData rdata =
        if ByteString.length rdata > fromIntegral (maxBound :: Word16)
        then fail "Data too big" -- TODO
        else put (fromIntegral (ByteString.length rdata) :: Word16) >>
             putByteString rdata

instance BinaryState Domain where
  get = Domain <$> get
  put (Domain xs) = put xs

instance BinaryState [Label] where
  get = do
    offset <- offset <$> State.get
    word <- get
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
        offset <- combineToInt64 length0 <$> get
        case Bimap.lookup offset labels of
          Just x  -> return x
          Nothing -> fail $ "Invalid pointer " ++ show offset ++
            ", valid labels are: " ++ show labels

      combineToInt64 :: Word8 -> Word8 -> Int64
      combineToInt64 high low =
        fromIntegral $ (fromIntegral high :: Word16) `shiftL` 8 + fromIntegral low

  put xs = do
    labels <- labels <$> State.get
    case Bimap.lookupR xs labels of
      Just offset -> putPointer offset
      Nothing     -> mapM_ putLabel xs >> put (0 :: Word8)

    where

      putPointer offset =
        if offset > 0b0011111111111111
        then fail $ "Invalid offset " ++ show offset
        else put (0b1100000000000000 `xor` (fromIntegral offset :: Word16))

      putLabel label@(Label string) = do
        putLabelLength label
        putByteString string

      putLabelLength (Label string) =
        let length = ByteString.length string
        in if length > 0b00111111
        then fail "Label too long" -- TODO
        else put (fromIntegral length :: Word8)
