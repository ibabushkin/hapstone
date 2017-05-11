module Internal.Arm.StorableSpec where

import Foreign
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Arm

import Internal.Arm.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Arm" $ do
    armOpMemStructSpec
    csArmOpSpec
    csArmSpec

getArmOpMemStruct :: IO ArmOpMemStruct
getArmOpMemStruct = do
    ptr <- mallocArray (sizeOf armOpMemStruct) :: IO (Ptr Word8)
    poke (castPtr ptr) (fromIntegral $ fromEnum ArmRegD15 :: Word32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum ArmRegQ12 :: Word32)
    poke (plusPtr ptr 8) (0x01324657 :: Int32)
    poke (plusPtr ptr 12) (0x02143657 :: Int32)
    peek (castPtr ptr) <* free ptr

armOpMemStruct :: ArmOpMemStruct
armOpMemStruct =
    ArmOpMemStruct ArmRegD15 ArmRegQ12 0x01324657 0x02143657

-- | ArmOpMemStruct spec
armOpMemStructSpec :: Spec
armOpMemStructSpec = describe "Storable ArmOpMemStruct" $ do
    it "is a packed struct" $
        sizeOf (undefined :: ArmOpMemStruct) ==
            sizeOf (0 :: Word32) * 4
    it "has matching peek- and poke-implementations" $ property $
        \s@ArmOpMemStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getArmOpMemStruct `shouldReturn` armOpMemStruct

getCsArmOp :: IO CsArmOp
getCsArmOp = do
    ptr <- mallocArray (sizeOf csArmOp) :: IO (Ptr Word8)
    poke (castPtr ptr) (-1 :: Int32)
    poke (plusPtr ptr 4) (fromIntegral $ fromEnum ArmSftRrx :: Int32)
    poke (plusPtr ptr 8) (0x01234567 :: Word32)
    poke (plusPtr ptr 12) (fromIntegral $ fromEnum ArmOpImm :: Int32)
    poke (plusPtr ptr 16) (0x01234567 :: Word32)
    poke (plusPtr ptr 36) (0x1 :: Word8)
    poke (plusPtr ptr 37) (0x2 :: Word8)
    poke (plusPtr ptr 38) (0x3 :: Word8)
    peek (castPtr ptr) <* free ptr

csArmOp :: CsArmOp
csArmOp = CsArmOp (-1) (ArmSftRrx, 0x01234567) (Imm 0x01234567) True 0x2 0x3

-- | CsArmOp spec
csArmOpSpec :: Spec
csArmOpSpec = describe "Storable CsArmOp" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArmOp) == 4*4 + 20 + 3 + 1
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArmOp{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsArmOp `shouldReturn` csArmOp

getCsArm :: IO CsArm
getCsArm = do
    ptr <- mallocArray (sizeOf csArm) :: IO (Ptr Word8)
    poke (castPtr ptr) (0x1 :: Word8)
    poke (plusPtr ptr 4) (0x2 :: Word32)
    poke (plusPtr ptr 8) (fromIntegral $ fromEnum ArmVectordataS16 :: Word32)
    poke (plusPtr ptr 12) (fromIntegral $ fromEnum ArmCpsmodeIe :: Word32)
    poke (plusPtr ptr 16) (fromIntegral $ fromEnum ArmCpsflagNone :: Word32)
    poke (plusPtr ptr 20) (fromIntegral $ fromEnum ArmCcEq :: Word32)
    poke (plusPtr ptr 24) (0x1 :: Word8)
    poke (plusPtr ptr 25) (0x1 :: Word8)
    poke (plusPtr ptr 28) (fromIntegral $ fromEnum ArmMbNsh :: Word32)
    poke (plusPtr ptr 32) (1 :: Word8)
    poke (plusPtr ptr 40) csArmOp
    peek (castPtr ptr) <* free ptr

csArm :: CsArm
csArm = CsArm True 2 ArmVectordataS16 ArmCpsmodeIe ArmCpsflagNone ArmCcEq
    True True ArmMbNsh [csArmOp]

-- | CsArm spec
csArmSpec :: Spec
csArmSpec = describe "Storable CsArm" $ do
    it "has a memory-layout we can handle" $
        sizeOf (undefined :: CsArm) ==
            1 + 3 + 5*4 + 2 + 2 + 4 + 1 + 7 + 36*40
    it "has matching peek- and poke-implementations" $ property $
        \s@CsArm{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $ getCsArm `shouldReturn` csArm
