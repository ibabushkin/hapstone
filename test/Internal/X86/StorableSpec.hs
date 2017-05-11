module Internal.X86.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.X86

import Internal.X86.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.X86" $ do
    x86OpMemStructSpec
    csX86OpSpec
    csX86Spec

getX86OpMemStruct :: IO X86OpMemStruct
getX86OpMemStruct = do
    ptr <- mallocArray (sizeOf x86OpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum X86RegEax :: Word32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum X86RegEdi :: Word32)
    poke (plusPtr ptr 8) (fromIntegral $ fromEnum X86RegDx :: Word32)
    poke (plusPtr ptr 12) (0x31507264 :: Int32)
    poke (plusPtr ptr 16) (0x3df1507264 :: Int64)
    peek (castPtr ptr) <* free ptr

x86OpMemStruct :: X86OpMemStruct
x86OpMemStruct =
    X86OpMemStruct X86RegEax X86RegEdi X86RegDx 0x31507264 0x3df1507264 

-- | X86OpMemStruct spec
x86OpMemStructSpec :: Spec
x86OpMemStructSpec = describe "Storable X86OpMemStruct" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: X86OpMemStruct) == 4 * 4 + 8
    it "has matching peek- and poke-implementations" $ property $
        \s@X86OpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getX86OpMemStruct `shouldReturn` x86OpMemStruct

getCsX86Op :: IO CsX86Op
getCsX86Op = do
    ptr <- mallocArray (sizeOf csX86Op) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum X86OpImm :: Int32)
    poke (plusPtr ptr 8) (0x0123456789abcdef :: Int64)
    poke (plusPtr ptr 32) (2 :: Word8)
    poke (plusPtr ptr 33) (0 :: Word8)
    poke (plusPtr ptr 36) (fromIntegral $ fromEnum X86AvxBcast4 :: Int32) 
    poke (plusPtr ptr 40) (1 :: Word8)
    peek (castPtr ptr) <* free ptr

csX86Op :: CsX86Op
csX86Op = CsX86Op (Imm 0x0123456789abcdef) 2 0 X86AvxBcast4 True

-- | CsX86ipsOp spec
csX86OpSpec :: Spec
csX86OpSpec = describe "Storable CsX86Op" $ do
    it "has a memory layout we can handle" $
        sizeOf (undefined :: CsX86Op) == 4 + 4 + 24 + 2 + 2 + 4 + 1 + 7
    it "has matching peek- and poke-implementations" $ property $
        \s ->
            alloca (\p -> poke p s >> (peek p :: IO CsX86Op)) `shouldReturn` s
    it "parses correctly" $ getCsX86Op `shouldReturn` csX86Op

getCsX86 :: IO CsX86
getCsX86 = do
    ptr <- mallocArray (sizeOf csX86) :: IO (Ptr Word8)
    pokeArray ptr ([ 0x0, 0x1, 0x2, 0x3 -- prefix
                   , 0x4, 0x5, 0x6, 0x7 -- opcode
                   , 0x0 -- rex
                   , 0x20 -- address_size
                   , 0x21 -- modrm
                   , 0x0 -- sib
                   ] :: [Word8])
    poke (plusPtr ptr 12) (0x01234567 :: Int32) -- disp
    poke (plusPtr ptr 16) (fromIntegral $ fromEnum X86RegAl :: Word32)
    poke (plusPtr ptr 20) (0x2 :: Int8) -- sibScale
    poke (plusPtr ptr 24) (fromIntegral $ fromEnum X86RegEdx :: Word32)
    poke (plusPtr ptr 28) (fromIntegral $ fromEnum X86SseCcEq :: Word32)
    poke (plusPtr ptr 32) (fromIntegral $ fromEnum X86AvxCcEq :: Word32)
    poke (plusPtr ptr 36) (0x1 :: Word8) -- avxSae
    poke (plusPtr ptr 40) (fromIntegral $ fromEnum X86AvxRmRu :: Word32)
    poke (plusPtr ptr 44) (0x1 :: Word8) -- op_count
    poke (plusPtr ptr 48) csX86Op
    peek (castPtr ptr) <* free ptr

csX86 :: CsX86
csX86 = CsX86 (Nothing, Just 0x1, Just 0x2, Just 0x3) [0x4, 0x5, 0x6, 0x7]
    0x0 0x20 0x21 Nothing (Just 0x01234567) X86RegAl 0x2 X86RegEdx
    X86SseCcEq X86AvxCcEq True X86AvxRmRu [csX86Op]

-- | CsX86 spec
csX86Spec :: Spec
csX86Spec = describe "Storable CsX86" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsX86) ==
            4 + 4 + 1 + 1 + 1 + 1 + 4 + 4 + 1 + 4 + 4 + 4 + 1 + 4 + 1 + 384 + 1
    it "has matching peek- and poke-implementations" $ property $
        \s@CsX86{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsX86 `shouldReturn` csX86
