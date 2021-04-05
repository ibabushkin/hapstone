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

import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.Types

import Hapstone.Internal.Util

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
    , lshift :: Maybe Int32 -- ^ left shift
    } deriving (Show, Eq)

instance Storable ArmOpMemStruct where
    sizeOf _ = {#sizeof arm_op_mem#}
    alignment _ = {#alignof arm_op_mem#}
    peek p = ArmOpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get arm_op_mem->base#} p)
        <*> ((toEnum . fromIntegral) <$> {#get arm_op_mem->index#} p)
        <*> (fromIntegral <$> {#get arm_op_mem->scale#} p)
        <*> (fromIntegral <$> {#get arm_op_mem->disp#} p)
        <*> (fromZero . fromIntegral <$> {#get arm_op_mem->lshift#} p)
    poke p (ArmOpMemStruct b i s d l) = do
        {#set arm_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set arm_op_mem->index#} p (fromIntegral $ fromEnum i)
        {#set arm_op_mem->scale#} p (fromIntegral s)
        {#set arm_op_mem->disp#} p (fromIntegral d)
        {#set arm_op_mem->lshift#} p (fromIntegral $ fromMaybe 0 l)

-- | possible operand types (corresponding to the tagged union in the C header)
data CsArmOpValue
    = Reg Int32 -- ^ register value for 'ArmOpReg' operands
    | Sysreg Int32 -- ^ register value for 'ArmOpSysreg' operands
    | Imm Int32 -- ^ immediate value for 'ArmOpImm' operands
    | CImm Int32 -- ^ immediate value for 'ArmOpCimm' operands
    | PImm Int32 -- ^ immediate value for 'ArmOpPimm' operands
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
    sizeOf _ = {#sizeof cs_arm_op#}
    alignment _ = {#alignof cs_arm_op#}
    peek p = CsArmOp
        <$> (fromIntegral <$> {#get cs_arm_op->vector_index#} p)
        <*> ((,) <$>
            ((toEnum . fromIntegral) <$> {#get cs_arm_op->shift.type#} p) <*>
            (fromIntegral <$> {#get cs_arm_op->shift.value#} p))
        <*> do
            t <- fromIntegral <$> {#get cs_arm_op->type#} p :: IO Int
            let memP = plusPtr p {#offsetof cs_arm_op->mem#}
            case toEnum t of
              ArmOpReg -> (Reg . fromIntegral) <$> {#get cs_arm_op->reg#} p
              ArmOpSysreg -> (Sysreg . fromIntegral) <$> {#get cs_arm_op->reg#} p
              ArmOpImm -> (Imm . fromIntegral) <$> {#get cs_arm_op->imm#} p
              ArmOpCimm -> (CImm . fromIntegral) <$> {#get cs_arm_op->imm#} p
              ArmOpPimm -> (PImm . fromIntegral) <$> {#get cs_arm_op->imm#} p
              ArmOpFp -> (Fp . realToFrac) <$> {#get cs_arm_op->fp#} p
              ArmOpMem -> Mem <$> (peek memP)
              ArmOpSetend -> (Setend . toEnum . fromIntegral) <$> {#get cs_arm_op->setend#} p
              _ -> return Undefined
        <*> (toBool <$> (peekByteOff p {#offsetof cs_arm_op->subtracted#} :: IO Word8))
        <*> (fromIntegral <$> {#get cs_arm_op->access#} p)
        <*> (fromIntegral <$> {#get cs_arm_op->neon_lane#} p)
    poke p (CsArmOp vI (sh, shV) val sub acc neon) = do
        {#set cs_arm_op->vector_index#} p (fromIntegral vI)
        {#set cs_arm_op->shift.type#} p (fromIntegral $ fromEnum sh)
        {#set cs_arm_op->shift.value#} p (fromIntegral shV)
        let regP = plusPtr p {#offsetof cs_arm_op->reg#}
            immP = plusPtr p {#offsetof cs_arm_op->imm#}
            fpP = plusPtr p {#offsetof cs_arm_op->fp#}
            memP = plusPtr p {#offsetof cs_arm_op->mem#}
            setendP = plusPtr p {#offsetof cs_arm_op->setend#}
            setType = {#set cs_arm_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke regP (fromIntegral r :: CUInt)
              setType ArmOpReg
          Sysreg r -> do
              poke regP (fromIntegral r :: CUInt)
              setType ArmOpSysreg
          Imm i -> do
              poke immP (fromIntegral i :: CInt)
              setType ArmOpImm
          CImm i -> do
              poke immP (fromIntegral i :: CInt)
              setType ArmOpCimm
          PImm i -> do
              poke immP (fromIntegral i :: CInt)
              setType ArmOpPimm
          Fp f -> do
              poke fpP (realToFrac f :: CDouble)
              setType ArmOpFp
          Mem m -> do
              poke memP m
              setType ArmOpMem
          Setend s -> do
              poke setendP (fromIntegral $ fromEnum s :: CInt)
              setType ArmOpSetend
          _ -> setType ArmOpInvalid
        {#set cs_arm_op->subtracted#} p sub
        {#set cs_arm_op->access#} p $ fromIntegral acc
        {#set cs_arm_op->neon_lane#} p $ fromIntegral neon

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
    sizeOf _ = {#sizeof cs_arm#}
    alignment _ = {#alignof cs_arm#}
    peek p = CsArm
        <$> (toBool <$> (peekByteOff p {#offsetof cs_arm->usermode#} :: IO Word8))
        <*> (fromIntegral <$> {#get cs_arm->vector_size#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->vector_data#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cps_mode#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cps_flag#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->cc#} p)
        <*> (toBool <$> (peekByteOff p {#offsetof cs_arm->update_flags#} :: IO Word8))
        <*> (toBool <$> (peekByteOff p {#offsetof cs_arm->writeback#} :: IO Word8))
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm->mem_barrier#} p)
        <*> do num <- fromIntegral <$> {#get cs_arm->op_count#} p
               let ptr = plusPtr p {#offsetof cs_arm->operands#}
               peekArray num ptr
    poke p (CsArm u vS vD cM cF cc uF w m o) = do
        {#set cs_arm->usermode#} p u
        {#set cs_arm->vector_size#} p (fromIntegral vS)
        {#set cs_arm->vector_data#} p (fromIntegral $ fromEnum vD)
        {#set cs_arm->cps_mode#} p (fromIntegral $ fromEnum cM)
        {#set cs_arm->cps_flag#} p (fromIntegral $ fromEnum cF)
        {#set cs_arm->cc#} p (fromIntegral $ fromEnum cc)
        {#set cs_arm->update_flags#} p uF
        {#set cs_arm->writeback#} p w
        {#set cs_arm->mem_barrier#} p (fromIntegral $ fromEnum m)
        {#set cs_arm->op_count#} p (fromIntegral $ length o)
        if length o > 36
           then error "operands overflew 36 elements"
           else pokeArray (plusPtr p {#offsetof cs_arm->operands#}) o

-- | ARM instructions
{#enum arm_insn as ArmInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | ARM instruction groups
{#enum arm_insn_group as ArmInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
