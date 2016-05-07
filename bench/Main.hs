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
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic
import           System.IO.MMap

setupEnvBs :: Int -> IO BS.ByteString
setupEnvBs n = return $ BS.pack (take n (cycle [maxBound, 0]))

setupEnvBss :: Int -> Int -> IO [BS.ByteString]
setupEnvBss n k = setupEnvBs n >>= \v -> return (replicate k v)

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

setupEnvVectors :: Int -> Int -> IO [DVS.Vector Word64]
setupEnvVectors n k = setupEnvVector n >>= \v -> return (replicate k v)

setupEnvXml :: FilePath -> IO BS.ByteString
setupEnvXml filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadXml :: BS.ByteString -> XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadXml bs = fromByteString bs :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env (setupEnvVector 1000000) $ \bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (getCount . rank1  bv)) [0, 1000..10000000])
    ]
  ]

runCon :: Conduit i [] BS.ByteString -> i -> BS.ByteString
runCon con bs = BS.concat $ runListConduit con [bs]

runCon2 :: Conduit i [] o -> [i] -> [o]
runCon2 con is = let os = runListConduit con is in seq (length os) os

runCon3 :: Conduit i [] BS.ByteString -> [i] -> [BS.ByteString]
runCon3 con is = let os = runListConduit con is in seq (BS.length (last os)) os

xmlToInterestBits3 :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
xmlToInterestBits3 = blankXml =$= blankedXmlToInterestBits

benchRankXml40Conduits :: [Benchmark]
benchRankXml40Conduits =
  [ env (setupEnvXml "/Users/jky/Downloads/part40.xml") $ \bs -> bgroup "Xml40"
    [ bench "Run blankXml                    "  (whnf (runCon blankXml                  ) bs)
    , bench "Run xmlToInterestBits3          "  (whnf (runCon xmlToInterestBits3        ) bs)
    , bench "loadXml                         "  (whnf  loadXml                            bs)
    ]
  ]

benchRankXml80Conduits :: [Benchmark]
benchRankXml80Conduits =
  [ env (setupEnvXml "/Users/jky/Downloads/part80.xml") $ \bs -> bgroup "Xml40"
    [ bench "Run blankXml                    "  (whnf (runCon blankXml                  ) bs)
    , bench "Run xmlToInterestBits3          "  (whnf (runCon xmlToInterestBits3        ) bs)
    , bench "loadXml" (whnf loadXml bs)
    ]
  ]

benchRankXmlBigConduits :: [Benchmark]
benchRankXmlBigConduits =
  [ env (setupEnvXml "/Users/jky/Downloads/78mb.xml") $ \bs -> bgroup "XmlBig"
    [ bench "Run blankXml                    "  (whnf (runCon blankXml                  ) bs)
    , bench "Run xmlToInterestBits3          "  (whnf (runCon xmlToInterestBits3        ) bs)
    , bench "loadXml" (whnf loadXml bs)
    ]
  ]

benchBlankedXmlToBalancedParens :: [Benchmark]
benchBlankedXmlToBalancedParens =
  [ env (setupEnvXml "/Users/jky/Downloads/part40.xml") $ \bs -> bgroup "XmlBig"
    [ bench "blankedXmlToBalancedParens2" (whnf (runCon2 blankedXmlToBalancedParens) [bs])
    , bench "blankedXmlToBalancedParens2" (whnf (runCon3 blankedXmlToBalancedParens2) [bs])
    ]
  ]

main :: IO ()
main = defaultMain benchRankXml40Conduits
