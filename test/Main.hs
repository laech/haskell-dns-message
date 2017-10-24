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
    , testDecodeQueryTypeAny
--    , testDecodeResponseTypeAny
    ]

encodeIPv4 i1 i2 i3 i4 = foldMap B.singleton [i1, i2, i3, i4]

encodeIPv6 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 =
  foldMap
    B.singleton
    [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16]

testDecodeQueryTypeA =
  testDecode
    "test-res/google_com_query_type_a"
    Message
    { identifier = 0xe99f
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
    "test-res/google_com_response_type_a"
    Message
    { identifier = 0xe99f
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
          , rttl = 238
          , rdata = encodeIPv4 216 58 196 142
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

testDecodeQueryTypeAny =
  testDecode
    "test-res/google_com_query_type_any"
    Message
    { identifier = 0x3613
    , flags = 0x0120
    , questions =
        [ Question
          { qname = Domain [mklabel "google", mklabel "com"]
          , qtype = typeANY
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

--testDecodeResponseTypeAny =
--  testDecode
--    "test-res/google_com_response_type_any"
--    Message
--    { identifier = 0x3613
--    , flags = 0x8180
--    , questions =
--        [ Question
--          { qname = Domain [mklabel "google", mklabel "com"]
--          , qtype = typeANY
--          , qclass = classIN
--          }
--        ]
--    , answers =
--        [ ResourceRecord
--          { rname = Domain [mklabel "google", mklabel "com"]
--          , rtype = typeA
--          , rclass = classIN
--          , rttl = 299
--          , rdata = encodeIPv4 216 58 200 110
--          }
--        , ResourceRecord
--          { rname = Domain [mklabel "google", mklabel "com"]
--          , rtype = typeAAAA
--          , rclass = classIN
--          , rttl = 299
--          , rdata =
--              encodeIPv6
--                0x24
--                0x04
--                0x68
--                0x00
--                0x40
--                0x06
--                0x08
--                0x08
--                0x00
--                0x00
--                0x00
--                0x00
--                0x00
--                0x00
--                0x20
--                0x0e
--          }
--        , ResourceRecord
--          { rname = Domain [mklabel "google", mklabel "com"]
--          , rtype = typeTXT
--          , rclass = classIN
--          , rttl = 3599
--          , rdata = C.pack "v=spf1 include:_spf.google.com ~all"
--          }
--        ]
--    , authorities = []
--    , additionals = []
--    }
