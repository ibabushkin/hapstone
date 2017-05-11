module Internal.Arm.Default where

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Arm

import Test.QuickCheck
import Test.QuickCheck.Instances

-- generators for our datatypes

instance Arbitrary ArmShifter where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmConditionCode where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmSysreg where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmMemBarrier where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary ArmOpType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary ArmSetendType where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmCpsmodeType where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmCpsflagType where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmVectordataType where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary ArmOpMemStruct where
    arbitrary = ArmOpMemStruct <$> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsArmOpValue where
    arbitrary = oneof
        [ Reg <$> arbitrary
        , Sysreg <$> arbitrary
        , Imm <$> arbitrary
        , Cimm <$> arbitrary
        , Pimm <$> arbitrary
        , Fp <$> arbitrary
        , Mem <$> arbitrary
        , Setend <$> arbitrary
        , pure Undefined
        ]

instance Arbitrary CsArmOp where
    arbitrary = CsArmOp <$> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CsArm where
    arbitrary = CsArm <$> arbitrary <*> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
        arbitrary <*> arbitrary <*> (take 36 <$> listOf arbitrary)

instance Arbitrary ArmReg where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmInsn where
    arbitrary = elements [minBound..maxBound]
instance Arbitrary ArmInsnGroup where
    arbitrary = elements [minBound..maxBound]
