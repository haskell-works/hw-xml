{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import           Control.Monad.Trans.Resource                        (MonadThrow)
import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Internal                            as BSI
import           Data.Conduit
import qualified Data.Vector.Storable                                as DVS
import           Data.Word
import           Foreign
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Xml.Conduit
import           HaskellWorks.Data.Xml.Conduit.Blank
import           HaskellWorks.Data.Xml.Succinct.Cursor
import           HaskellWorks.Data.BalancedParens.Simple
import           System.IO.MMap

setupEnvXml :: FilePath -> IO BS.ByteString
setupEnvXml filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadXml :: BS.ByteString -> XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadXml bs = fromByteString bs :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

xmlToInterestBits3 :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
xmlToInterestBits3 = blankXml =$= blankedXmlToInterestBits

runCon :: Conduit i [] BS.ByteString -> i -> BS.ByteString
runCon con bs = BS.concat $ runListConduit con [bs]

benchRankXml8mbConduits :: [Benchmark]
benchRankXml8mbConduits =
  [ env (setupEnvXml "corpus/8mb.xml") $ \bs -> bgroup "Xml4mb"
    [ bench "Run blankXml                    "  (whnf (runCon blankXml                  ) bs)
    , bench "Run xmlToInterestBits3          "  (whnf (runCon xmlToInterestBits3        ) bs)
    , bench "loadXml" (whnf loadXml bs)
    ]
  ]

benchRankXmlBigConduits :: [Benchmark]
benchRankXmlBigConduits =
  [ env (setupEnvXml "corpus/105mb.xml") $ \bs -> bgroup "XmlBig"
    [ bench "Run blankXml                    "  (whnf (runCon blankXml                  ) bs)
    , bench "Run xmlToInterestBits3          "  (whnf (runCon xmlToInterestBits3        ) bs)
    , bench "loadXml" (whnf loadXml bs)
    ]
  ]

setupInterestingWord8s :: IO ()
setupInterestingWord8s = do
  let !_ = interestingWord8s
  return ()

benchIsInterestingWord8 :: [Benchmark]
benchIsInterestingWord8 =
  [ env setupInterestingWord8s $ \_ -> bgroup "Interesting Word8 lookup"
    [ bench "isInterestingWord8"  (whnf isInterestingWord8 0)
    ]
  ]

main :: IO ()
main = defaultMain $ concat
  [ benchIsInterestingWord8
  , benchRankXmlBigConduits
  , benchRankXml8mbConduits
  ]
