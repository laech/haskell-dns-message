import           Control.Exception
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString       as B
import           Data.ByteString.Char8 as C
import           Data.ByteString.Lazy  as L
import           Network.DNS.Message
import           Numeric
import           Test.HUnit

testFile :: FilePath -> Message -> Test
testFile path expected =
  TestList
  [ testDecode path expected
  , testPutGet path expected
  ]

testDecode :: FilePath -> Message -> Test
testDecode path expected = TestLabel "testDecode" $ TestCase $ do
  input <- L.readFile path
  let result = runGetOrFail getMessage input
  case result of
    Left (_, _, info)    -> assertFailure (show info)
    Right (_, _, actual) -> assertEqual "" expected actual

testPutGet :: FilePath -> Message -> Test
testPutGet path expected = TestLabel "testPutGet" $ TestCase $ do
  expectedBytes <- L.readFile path
  let actualBytes = runPut $ putMessage expected
  test actualBytes `catch` handle expectedBytes actualBytes
  where

    test actualBytes = do
      let actual = runGet getMessage actualBytes
      assertEqual "" expected actual

    handle :: L.ByteString -> L.ByteString -> SomeException -> Assertion
    handle expectedBytes actualBytes e = fail $ show e ++
      "\n\n      Expected:\n" ++ show expected ++
      "\n\nExpected bytes:\n" ++ printHex expectedBytes ++
      "\n\n  Actual bytes:\n" ++ printHex actualBytes

printHex :: L.ByteString -> String
printHex = L.foldr (\w result -> showHex w (' ' : result)) ""

main =
  runTestTT $
  TestList
    [ testAQuery
    , testAResponse
    ]

encodeIPv4 i1 i2 i3 i4 = foldMap B.singleton [i1, i2, i3, i4]

encodeIPv6 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 =
  foldMap
    B.singleton
    [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16]

testAQuery = testFile "test-res/A.query"
  Message
  { identifier = 0x7d3c
  , flags = 0x0120
  , questions =
      [ Question
        { qname = Domain [mklabel "google", mklabel "com"]
        , qtype = typeA
        , qclass = classIN
        }
      ]
  , answers = []
  , authorities = []
  , additionals =
      [ ResourceRecord
        { rname = Domain []
        , rtype = typeOPT
        , rclass = 4096
        , rttl = 0
        , rdata = B.empty
        }
      ]
  }

testAResponse = testFile "test-res/A.response"
  Message
  { identifier = 0x7d3c
  , flags = 0x8180
  , questions =
      [ Question
        { qname = Domain [mklabel "google", mklabel "com"]
        , qtype = typeA
        , qclass = classIN
        }
      ]
  , answers =
      [ ResourceRecord
        { rname = Domain [mklabel "google", mklabel "com"]
        , rtype = typeA
        , rclass = classIN
        , rttl = 299
        , rdata = encodeIPv4 216 58 200 110
        }
      ]
  , authorities = []
  , additionals =
      [ ResourceRecord
        { rname = Domain []
        , rtype = typeOPT
        , rclass = 512
        , rttl = 0
        , rdata = B.empty
        }
      ]
  }
