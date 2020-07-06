module Internal.Arm64.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Arm64

import Internal.Arm64.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Arm64" $ do
    arm64OpMemStructSpec
    csArm64OpSpec
    csArm64Spec

getArm64OpMemStruct :: IO Arm64OpMemStruct
getArm64OpMemStruct = do
    ptr <- mallocArray (sizeOf arm64OpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum Arm64RegB4 :: Word32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum Arm64RegB16 :: Word32)
    poke (plusPtr ptr 8) (0x03152746 :: Int32)
    peek (castPtr ptr) <* free ptr

arm64OpMemStruct :: Arm64OpMemStruct
arm64OpMemStruct = Arm64OpMemStruct Arm64RegB4 Arm64RegB16 0x03152746

-- | Arm64OpMemStruct spec
arm64OpMemStructSpec :: Spec
arm64OpMemStructSpec = describe "Storable Arm64OpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: Arm64OpMemStruct) ==
            sizeOf (0 :: CUInt) * 2 + sizeOf (0 :: Word32)
    it "has matching peek- and poke-implementations" $ property $
        \s@Arm64OpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getArm64OpMemStruct `shouldReturn` arm64OpMemStruct

getCsArm64Op :: IO CsArm64Op
getCsArm64Op = do
    ptr <- mallocArray (sizeOf csArm64Op) :: IO (Ptr Word8)
    poke (castPtr ptr) (0x01234567 :: Word32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum Arm64Vas8b :: Int32)
    poke (plusPtr ptr 12) (fromIntegral $ fromEnum Arm64SftMsl :: Int32)
    poke (plusPtr ptr 16) (0x01234567 :: Word32)
    poke (plusPtr ptr 20) (fromIntegral $ fromEnum Arm64ExtUxtb :: Int32)
    poke (plusPtr ptr 24) (fromIntegral $ fromEnum Arm64OpImm :: Int32)
    poke (plusPtr ptr 32) (0x0123456789abcdef :: Int64)
    poke (plusPtr ptr 44) (0x1 :: Word8)
    peek (castPtr ptr) <* free ptr

csArm64Op :: CsArm64Op
csArm64Op = CsArm64Op 0x01234567 Arm64Vas8b 
    (Arm64SftMsl, 0x01234567) Arm64ExtUxtb (Imm 0x0123456789abcdef) 0x1

csArm64OpSpec :: Spec
csArm64OpSpec = describe "Storable CsArm64Op" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArm64Op) == 7*4 + 4 + 12 + 1 + 3
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArm64Op{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsArm64Op `shouldReturn` csArm64Op

getCsArm64 :: IO CsArm64
getCsArm64 = do
    ptr <- mallocArray (sizeOf csArm64) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum Arm64CcEq :: Int32)
    poke (plusPtr ptr 4) (0x1 :: Word8)
    poke (plusPtr ptr 5) (0x0 :: Word8)
    poke (plusPtr ptr 6) (0x1 :: Word8)
    poke (plusPtr ptr 8) csArm64Op
    peek (castPtr ptr) <* free ptr

csArm64 :: CsArm64
csArm64 = CsArm64 Arm64CcEq True False [csArm64Op]

csArm64Spec :: Spec
csArm64Spec = describe "Storable CsArm64" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArm64) == 4 + 3 + 1 + 8*48
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArm64{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsArm64 `shouldReturn` csArm64
