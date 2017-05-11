{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Arm
Description : ARM architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains ARM specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.Arm where

#include <capstone/arm.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | ARM shift type
{#enum arm_shifter as ArmShifter {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | ARM condition code
{#enum arm_cc as ArmConditionCode {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | system registers
{#enum arm_sysreg as ArmSysreg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | memory barrier operands (map directly to the 4-bit encoding of the option
-- field for Memory Barrier operations, when given as an integer)
{#enum arm_mem_barrier as ArmMemBarrier {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operand type for instruction's operands
{#enum arm_op_type as ArmOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operand type for SETEND instruction
{#enum arm_setend_type as ArmSetendType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum arm_cpsmode_type as ArmCpsmodeType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | operand type for SETEND instruction
{#enum arm_cpsflag_type as ArmCpsflagType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | data type for elements of vector instructions
{#enum arm_vectordata_type as ArmVectordataType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | ARM registers
{#enum arm_reg as ArmReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
-- associated with 'ArmOpMem' operand type
data ArmOpMemStruct = ArmOpMemStruct
    { base :: ArmReg -- ^ base register
    , index :: ArmReg -- ^ index register
    , scale :: Int32 -- ^ scale for index register (1 or -1)
    , disp :: Int32 -- ^ displacement/offset value
    } deriving (Show, Eq)

instance Storable ArmOpMemStruct where
    sizeOf _ = {#sizeof arm_op_mem#}
    alignment _ = {#alignof arm_op_mem#}
    peek p = ArmOpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get arm_op_mem->base#} p)
        <*> ((toEnum . fromIntegral) <$> {#get arm_op_mem->index#} p)
        <*> (fromIntegral <$> {#get arm_op_mem->scale#} p)
        <*> (fromIntegral <$> {#get arm_op_mem->disp#} p)
    poke p (ArmOpMemStruct b i s d) = do
        {#set arm_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set arm_op_mem->index#} p (fromIntegral $ fromEnum i)
        {#set arm_op_mem->scale#} p (fromIntegral s)
        {#set arm_op_mem->disp#} p (fromIntegral d)

-- | possible operand types (corresponding to the tagged union in the C header)
data CsArmOpValue
    = Reg Word32 -- ^ register value for 'ArmOpReg' operands
    | Sysreg Word32 -- ^ register value for 'ArmOpSysreg' operands
    | Imm Int32 -- ^ immediate value for 'ArmOpImm' operands
    | Cimm Int32 -- ^ immediate value for 'ArmOpCimm' operands
    | Pimm Int32 -- ^ immediate value for 'ArmOpPimm' operands
    | Fp Double -- ^ floating point value for 'ArmOpFp' operands
    | Mem ArmOpMemStruct -- ^ base,index,scale,disp value for
                         -- 'ArmOpMem' operands
    | Setend ArmSetendType -- ^ SETEND instruction's operand type
    | Undefined -- ^ invalid operand value, for 'ArmOpInvalid' operand
    deriving (Show, Eq)

-- | instruction operands
data CsArmOp = CsArmOp
    { vectorIndex :: Int32 -- ^ vector index for some vector operands, else -1
    , shift :: (ArmShifter, Word32) -- ^ shifter type and value
    , value :: CsArmOpValue -- ^ operand type and value
    , subtracted :: Bool -- ^ if 'True', operand is subtracted, else added
    , access :: Word8 -- ^ access mode of operand
    , neon_lane :: Int8 -- ^ neon lane index for NEON instructions, else -1
    } deriving (Show, Eq)

instance Storable CsArmOp where
    sizeOf _ = 40
    alignment _ = 8
    peek p = CsArmOp
        <$> (fromIntegral <$> {#get cs_arm_op->vector_index#} p)
        <*> ((,) <$>
            ((toEnum . fromIntegral) <$> {#get cs_arm_op->shift.type#} p) <*>
            (fromIntegral <$> {#get cs_arm_op->shift.value#} p))
        <*> do
            t <- fromIntegral <$> {#get cs_arm_op->type#} p :: IO Int
            let bP = plusPtr p 16
            case toEnum t of
              ArmOpReg -> (Reg . fromIntegral) <$> (peek bP :: IO CUInt)
              ArmOpSysreg -> (Sysreg . fromIntegral) <$> (peek bP :: IO CUInt)
              ArmOpImm -> (Imm . fromIntegral) <$> (peek bP :: IO CInt)
              ArmOpCimm -> (Cimm . fromIntegral) <$> (peek bP :: IO CInt)
              ArmOpPimm -> (Pimm . fromIntegral) <$> (peek bP :: IO CInt)
              ArmOpFp -> (Fp . realToFrac) <$> (peek bP :: IO CDouble)
              ArmOpMem -> Mem <$> (peek bP :: IO ArmOpMemStruct)
              ArmOpSetend -> (Setend . toEnum . fromIntegral) <$>
                  (peek bP :: IO CInt)
              _ -> return Undefined
        <*> (toBool <$> (peekByteOff p 36 :: IO Word8)) -- subtracted
        <*> (peekByteOff p 37 :: IO Word8) -- access
        <*> (peekByteOff p 38 :: IO Int8) -- neon_lane
    poke p (CsArmOp vI (sh, shV) val sub acc neon) = do
        {#set cs_arm_op->vector_index#} p (fromIntegral vI)
        {#set cs_arm_op->shift.type#} p (fromIntegral $ fromEnum sh)
        {#set cs_arm_op->shift.value#} p (fromIntegral shV)
        let bP = plusPtr p 16
            setType = {#set cs_arm_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke bP (fromIntegral r :: CUInt)
              setType ArmOpReg
          Sysreg r -> do
              poke bP (fromIntegral r :: CUInt)
              setType ArmOpSysreg
          Imm i -> do
              poke bP (fromIntegral i :: CInt)
              setType ArmOpImm
          Cimm i -> do
              poke bP (fromIntegral i :: CInt)
              setType ArmOpCimm
          Pimm i -> do
              poke bP (fromIntegral i :: CInt)
              setType ArmOpPimm
          Fp f -> do
              poke bP (realToFrac f :: CDouble)
              setType ArmOpFp
          Mem m -> do
              poke bP m
              setType ArmOpMem
          Setend s -> do
              poke bP (fromIntegral $ fromEnum s :: CInt)
              setType ArmOpSetend
          _ -> setType ArmOpInvalid
        pokeByteOff p 36 (fromBool sub :: Word8) -- subtracted
        pokeByteOff p 37 acc -- access
        pokeByteOff p 38 neon -- neon_lane

-- | instruction datatype
data CsArm = CsArm
    { usermode :: Bool -- ^ usermode registers to be loaded (for LDM/STM
                       -- instructions)
    , vectorSize :: Int32 -- ^ scalar size for vector instructions
    , vectorData :: ArmVectordataType -- ^ data type for elements of vector
                                      -- instructions
    , cpsMode :: ArmCpsmodeType -- ^ CPS mode for CPS instructions
    , cpsFlag :: ArmCpsflagType -- ^ CPS mode for CPS instructions
    , cc :: ArmConditionCode -- condition code
    , updateFlags :: Bool -- does this instruction update flags?
    , writeback :: Bool -- ^ does this instruction request writeback?
    , memBarrier :: ArmMemBarrier -- ^ options for some memory barrier
                                  -- instructions
    , operands :: [CsArmOp] -- ^ operand list of this instruction, *MUST*
                            -- have <= 36 elements, else you'll get a runtime
                            -- error when you (implicitly) try to write it to
                            -- memory via it's Storable instance
    } deriving (Show, Eq)

instance Storable CsArm where
    sizeOf _ = 1480
    alignment _ = 8
    peek p = CsArm
        <$> (toBool <$> (peekByteOff p 0 :: IO Word8)) -- usermode
        <*> (fromIntegral <$> {#get cs_arm->vector_size#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->vector_data#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cps_mode#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cps_flag#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cc#} p)
        <*> (toBool <$> (peekByteOff p 24 :: IO Word8)) -- update_flags
        <*> (toBool <$> (peekByteOff p 25 :: IO Word8)) -- writeback
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->mem_barrier#} p)
        <*> do num <- fromIntegral <$> {#get cs_arm->op_count#} p
               let ptr = plusPtr p 40
               peekArray num ptr
    poke p (CsArm u vS vD cM cF cc uF w m o) = do
        pokeByteOff p 0 (fromBool u :: Word8) -- usermode
        {#set cs_arm->vector_size#} p (fromIntegral vS)
        {#set cs_arm->vector_data#} p (fromIntegral $ fromEnum vD)
        {#set cs_arm->cps_mode#} p (fromIntegral $ fromEnum cM)
        {#set cs_arm->cps_flag#} p (fromIntegral $ fromEnum cF)
        {#set cs_arm->cc#} p (fromIntegral $ fromEnum cc)
        pokeByteOff p 24 (fromBool uF :: Word8) -- update_flags
        pokeByteOff p 25 (fromBool w :: Word8) -- writeback
        {#set cs_arm->mem_barrier#} p (fromIntegral $ fromEnum m)
        {#set cs_arm->op_count#} p (fromIntegral $ length o)
        if length o > 36
           then error "operands overflew 36 elements"
           else pokeArray (plusPtr p 40) o

-- | ARM instructions
{#enum arm_insn as ArmInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | ARM instruction groups
{#enum arm_insn_group as ArmInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
