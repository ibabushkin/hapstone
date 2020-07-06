module Internal.Arm64.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Arm64

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary Arm64Shifter where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64Extender where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64ConditionCode where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Arm64Sysreg where
    arbitrary = elements [minBound..maxBound]

--instance Arbitrary Arm64MsrReg where
--    arbitrary = elements [minBound..maxBound]

instance Arbitrary Arm64Pstate where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Arm64Vas where
    arbitrary = elements [minBound..maxBound]

-- instance Arbitrary Arm64Vess where
--    arbitrary = elements [minBound..maxBound]

instance Arbitrary Arm64BarrierOp where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64OpType where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64TlbiOp where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64AtOp where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64DcOp where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64IcOp where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64PrefetchOp where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Arm64Reg where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64Insn where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary Arm64InsnGroup where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary Arm64OpMemStruct where
    arbitrary = Arm64OpMemStruct <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsArm64OpValue where
    arbitrary = oneof
        [ Reg <$> arbitrary
        , Imm <$> arbitrary
        , CImm <$> arbitrary
        , Fp <$> arbitrary
        , Mem <$> arbitrary
        , Pstate <$> arbitrary
        , Sys <$> arbitrary
        , Prefetch <$> arbitrary
        , Barrier <$> arbitrary
        , pure Undefined
        ]

instance Arbitrary CsArm64Op where
    arbitrary = CsArm64Op <$> arbitrary <*> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsArm64 where
    arbitrary = CsArm64 <$> arbitrary <*> arbitrary <*> arbitrary <*>
        (take 8 <$> listOf arbitrary)
