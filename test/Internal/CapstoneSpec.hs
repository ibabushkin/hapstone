module Internal.CapstoneSpec where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Test.Hspec
import Test.QuickCheck

import Hapstone.Internal.Capstone

import Internal.Default

-- | main spec
spec :: Spec
spec = describe "Hapstone.Internal.Capstone" $ do
    csSkipdataStructStorableSpec
    csSetSkipdataSpec
    csDetailStorableSpec
    peekDetailSpec
    csInsnStorableSpec
    peekWithArchSpec
    csInsnOffsetSpec
    csDisasmSpec
    csDisasmIterSpec

getCsSkipdataStruct :: IO CsSkipdataStruct
getCsSkipdataStruct = do
    let wordSize = sizeOf (0 :: WordPtr)
    -- space allocation
    ptr <- mallocArray (sizeOf csSkipdataStruct) :: IO (Ptr Word8)
    -- mnemonic
    str <- newCString "teststr"
    poke (castPtr ptr) (ptrToWordPtr str)
    -- callback
    poke (plusPtr ptr wordSize) (0xabcdefab :: WordPtr)
    -- user_data
    poke (plusPtr ptr (2*wordSize)) (0x01234567 :: WordPtr)
    peek (castPtr ptr) <* free ptr

csSkipdataStruct :: CsSkipdataStruct
csSkipdataStruct = CsSkipdataStruct "teststr"
    (castPtrToFunPtr . wordPtrToPtr $ fromIntegral 0xabcdefab)
    (wordPtrToPtr $ fromIntegral 0x01234567)

csSkipdataStructStorableSpec :: Spec
csSkipdataStructStorableSpec = describe "Storable CsSkipdataStruct" $ do
    it "is a packed struct" $ 
        sizeOf (undefined :: CsSkipdataStruct) == 3 * sizeOf (0 :: WordPtr)
    it "has matching peek- and poke-implementations" $ property $
        \s@CsSkipdataStruct{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $
        getCsSkipdataStruct `shouldReturn` csSkipdataStruct

csSetSkipdataSpec :: Spec
csSetSkipdataSpec = describe "csSetSkipdata" $ do
    it "resets correctly" pending
    it "sets correctly" pending
    it "throws errors correctly" pending

getCsDetail :: IO CsDetail
getCsDetail = do
    -- space allocation
    ptr <- mallocArray (sizeOf csDetail) :: IO (Ptr Word8)
    -- regs_read
    pokeArray (castPtr ptr :: Ptr Word8)
        [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB]
    -- regs_read_count
    pokeByteOff ptr 12 (12 :: Word8)
    -- regs_write
    pokeArray (plusPtr ptr 13 :: Ptr Word8)
        [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB
        , 0xC, 0xD, 0xE, 0xF, 0x0, 0x1, 0x2, 0x3
        ]
    -- regs_write_count
    pokeByteOff ptr 33 (20 :: Word8)
    -- groups
    pokeArray (plusPtr ptr 34 :: Ptr Word8) [0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7]
    -- groups_count
    pokeByteOff ptr 42 (8 :: Word8)
    peek (castPtr ptr) <* free ptr

csDetail :: CsDetail
csDetail = CsDetail
    [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB ]
    [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD
    , 0xE, 0xF, 0x0, 0x1, 0x2, 0x3
    ]
    [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7 ]
    Nothing

csDetailStorableSpec :: Spec
csDetailStorableSpec = describe "Storable CsDetail" $ do
    it "has a memory layout we can manage" $
        sizeOf (undefined :: CsDetail) == 24 + 1 + 1 + 40 + 1 + 8 + 1 + 4 + 1480
    it "has matching peek- and poke-implementations with no arch specifics" $
        property $ \s@CsDetail{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $
        getCsDetail `shouldReturn` csDetail

peekDetailSpec :: Spec
peekDetailSpec = describe "peekDetail" $ do
    it "returns results when a pointer is given" pending

getCsInsn :: IO CsInsn
getCsInsn = do
    ptr <- mallocArray (sizeOf csInsn) :: IO (Ptr Word8)
    -- id
    poke (castPtr ptr) (0x01234567 :: Word32)
    -- address
    poke (plusPtr ptr 8) (0x0123456789abcdef :: Word64)
    -- size
    poke (plusPtr ptr 16) (0x10 :: Word16)
    -- bytes
    pokeArray (plusPtr ptr 18) ([0x0..0xf] :: [Word8])
    -- mnemonic
    pokeArray (plusPtr ptr 34) ([0x1..0x20] :: [Word8])
    -- op_str
    pokeArray (plusPtr ptr 66) ([0x1..0x7f] ++ [0x0] :: [Word8])
    peek (castPtr ptr) <* free ptr

csInsn :: CsInsn
csInsn = CsInsn
    0x01234567
    0x0123456789abcdef
    [0x0..0xf]
    (map castCCharToChar [0x1..0x20])
    (map castCCharToChar [0x1..0x7f])
    Nothing

csInsnStorableSpec :: Spec
csInsnStorableSpec = describe "Storable CsInsn" $ do
    it "has a memory layout we can manage" $
        sizeOf (undefined :: CsInsn) ==
            4 + 4 + 8 + 2 + 208 + sizeOf nullPtr +
                if sizeOf nullPtr == 4 then 2 else 6 -- pointer sizes
    it "has matching peek- and poke-implementations with no detail" $
        property $ \s@CsInsn{} ->
            alloca (\p -> poke p s >> peek p) `shouldReturn` s
    it "parses correctly" $
        getCsInsn `shouldReturn` csInsn

peekWithArchSpec :: Spec
peekWithArchSpec = describe "Storable CsInsn" $ do
    it "returns results when a pointer is given" pending

csInsnOffsetSpec :: Spec
csInsnOffsetSpec = describe "csInsnOffset" $ do
    it "behaves correctly (sums match)" $ pending

csDisasmSpec :: Spec
csDisasmSpec = describe "csDisasm" $ do
    it "disassembles a correct number of instructions" pending
    it "integrates with CsArch" pending

csDisasmIterSpec :: Spec
csDisasmIterSpec = describe "csDisasmIter" $ do
    it "fulfills modification guarantees" pending
    it "integrates with CsArch" pending
