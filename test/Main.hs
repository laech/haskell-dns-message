import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString       as B
import           Data.ByteString.Char8 as C
import           Data.ByteString.Lazy  as L
import           Network.DNS.Message
import           Test.HUnit

testDecode :: FilePath -> Message -> Test
testDecode path expected =
  TestCase $ do
    input <- L.readFile path
    let result = runGetOrFail getMessage input
    case result of
      Left (_, _, info)    -> assertFailure (show info)
      Right (_, _, actual) -> assertEqual "" expected actual

testPutThenGet :: Message -> Test
testPutThenGet expected = TestLabel ("put . get = id for " ++ show expected) $ TestCase $ assertEqual "" expected (runGet getMessage $ runPut $ putMessage expected)

main =
  runTestTT $
  TestList
    [ testPutThenGet
      Message
      { identifier = 1
      , flags = 101
      , questions = []
      , answers = []
      , authorities = []
      , additionals = []
      }

    , testPutThenGet
      Message
      { identifier = 11
      , flags = 1010
      , questions =
        [ Question
          { qname = Domain []
          , qtype = typeA
          , qclass = classIN
          }
        ]
      , answers = []
      , authorities = []
      , additionals = []
      }

    , testPutThenGet
      Message
      { identifier = 11
      , flags = 1010
      , questions =
        [ Question
          { qname = Domain [mklabel "google", mklabel "com"]
          , qtype = typeA
          , qclass = classIN
          }
        ]
      , answers = []
      , authorities = []
      , additionals = []
      }

    , testDecodeQueryTypeA
    , testDecodeResponseTypeA
    ]

encodeIPv4 i1 i2 i3 i4 = foldMap B.singleton [i1, i2, i3, i4]

encodeIPv6 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 =
  foldMap
    B.singleton
    [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16]

testDecodeQueryTypeA =
  testDecode
    "test-res/A.query"
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

testDecodeResponseTypeA =
  testDecode
    "test-res/A.response"
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
