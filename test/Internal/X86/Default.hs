module Internal.X86.Default where

import Foreign
import Foreign.C.Types

import Data.List (dropWhileEnd)

import Hapstone.Internal.Util
import Hapstone.Internal.X86

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary X86Reg where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary X86OpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary X86AvxBcast where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary X86SseCc where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary X86AvxCc where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary X86AvxRm where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary X86Prefix where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary X86OpMemStruct where
    arbitrary = X86OpMemStruct <$> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsX86Op where
    arbitrary = CsX86Op <$> oneof
        [ Reg <$> arbitrary
        , Imm <$> arbitrary
        -- , Fp <$> arbitrary
        , Mem <$> arbitrary
        , pure Undefined
        ] <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsX86 where
    arbitrary = CsX86 <$> tuple <*> list <*> arbitrary <*>
        arbitrary <*> arbitrary <*> nZ <*> nZ <*> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
        arbitrary <*> (take 8 <$> arbitrary)
        where tuple = (,,,) <$> nZ <*> nZ <*> nZ <*> nZ
              nZ :: (Arbitrary a, Eq a, Num a) => Gen (Maybe a)
              nZ = fromZero <$> arbitrary
              list = (dropWhileEnd (==0) . take 4) <$> arbitrary

instance Arbitrary X86Insn where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary X86InsnGroup where
    arbitrary = elements [minBound..maxBound]
