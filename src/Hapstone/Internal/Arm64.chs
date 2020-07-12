{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Arm64
Description : ARM64 architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains ARM64 specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.Arm64 where

#include <capstone/arm64.h>

{#context lib = "capstone"#}

import Control.Monad (join)

import Foreign
import Foreign.C.Types

-- | ARM64 shift type
{#enum arm64_shifter as Arm64Shifter {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | ARM64 extender type
{#enum arm64_extender as Arm64Extender {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | ARM64 condition code
{#enum arm64_cc as Arm64ConditionCode {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | system registers
{#enum arm64_sysreg as Arm64Sysreg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | more system registers

-- note that 'arm64_msr_reg' was '#if 0'ed out from capstone in commit 9d292268, "arm64: sync with LLVM 7.0.1" 2019April10
-- {#enum arm64_msr_reg as Arm64MsrReg {underscoreToCase}
--    deriving (Show, Eq, Bounded)#}

-- | system pstate field (MSR instructions)
{#enum arm64_pstate as Arm64Pstate {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | vector arrangement specifier (floating point/advanced SIMD instructions)
{#enum arm64_vas as Arm64Vas {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | vector element size specifier

-- note that 'arm_64_vess' was removed from capstone in commit 9d292268, "arm64: sync with LLVM 7.0.1" 2019April10
-- {#enum arm64_vess as Arm64Vess {underscoreToCase}
--     deriving (Show, Eq, Bounded)#}

-- | memory barrier operands
{#enum arm64_barrier_op as Arm64BarrierOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | operand type for instruction's operands
{#enum arm64_op_type as Arm64OpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | TLBI operations
{#enum arm64_tlbi_op as Arm64TlbiOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | AT operations
{#enum arm64_at_op as Arm64AtOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | DC operations
{#enum arm64_dc_op as Arm64DcOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | IC operations
{#enum arm64_ic_op as Arm64IcOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | prefetch operations (PRFM)
{#enum arm64_prefetch_op as Arm64PrefetchOp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | ARM64 registers
{#enum arm64_reg as Arm64Reg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
-- associated with 'Arm64OpMem' operand type
data Arm64OpMemStruct = Arm64OpMemStruct
    { base :: Arm64Reg -- ^ base register
    , index :: Arm64Reg -- ^ index register
    , disp :: Int32 -- ^ displacement/offset value
    } deriving (Show, Eq)

instance Storable Arm64OpMemStruct where
    sizeOf _ = {#sizeof arm64_op_mem#}
    alignment _ = {#alignof arm64_op_mem#}
    peek p = Arm64OpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get arm64_op_mem->base#} p)
        <*> ((toEnum . fromIntegral) <$> {#get arm64_op_mem->index#} p)
        <*> ((toEnum . fromIntegral) <$> {#get arm64_op_mem->disp#} p)
    poke p (Arm64OpMemStruct b i d) = do
        {#set arm64_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set arm64_op_mem->index#} p (fromIntegral $ fromEnum i)
        {#set arm64_op_mem->disp#} p (fromIntegral $ fromEnum d)

-- | possible operand types (corresponding to the tagged union in the C header)
data CsArm64OpValue
    = Reg Arm64Reg -- ^ register value for 'Arm64OpReg' operands
    | Imm Int64 -- ^ immediate value for 'Arm64OpImm' operands
    | CImm Int64 -- ^ index value for 'Arm64OpCimm' operands
    | Fp Double -- ^ floating point value for 'Arm64OpFp' operands
    | Mem Arm64OpMemStruct -- ^ base,index,disp value for 'Arm64OpMem' operands
    | Pstate Arm64Pstate -- ^ PState field of MSR instructions
    | Sys Word32 -- ^ IC/DC/AT/TLBI operation (see 'Arm64IcOp', 'Arm64DcOp',
                 -- 'Arm64AtOp', 'Arm64TlbiOp'), for 'Arm64OpSys' operands
    | Prefetch Arm64PrefetchOp -- ^ PRFM operation for 'Arm64OpPrefetch'
                               -- operands
    | Barrier Arm64BarrierOp -- ^ memory barrier operation (ISB/DMB/DSB
                             -- instructions), for 'Arm64OpBarrier' operands
    | Undefined -- ^ invalid operand value, for 'Arm64OpInvalid' operand
    deriving (Show, Eq)

-- note that 'vess' was removed from capstone in commit 9d292268, "arm64: sync with LLVM 7.0.1" 2019April10
-- | instruction operand
data CsArm64Op = CsArm64Op
    { vectorIndex :: Int32 -- ^ vector index for some vector operands, else -1
    , vas :: Arm64Vas -- ^ vector arrangement specifier
    , shift :: (Arm64Shifter, Word32) -- ^ shifter type and value
    , ext :: Arm64Extender -- ^ extender type
    , value :: CsArm64OpValue -- ^ operand type and value
    , access :: Word8 -- ^ the access mode TODO
    } deriving (Show, Eq)

instance Storable CsArm64Op where
    sizeOf _ = 48
    alignment _ = 8
    peek p = CsArm64Op
        <$> (fromIntegral <$> {#get cs_arm64_op->vector_index#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm64_op->vas#} p)
        <*> ((,) <$>
            ((toEnum . fromIntegral) <$> {#get cs_arm64_op->shift.type#} p) <*>
            (fromIntegral <$> {#get cs_arm64_op->shift.value#} p))
        <*> ((toEnum . fromIntegral) <$> {#get cs_arm64_op->ext#} p)
        <*> do
            t <- fromIntegral <$> {#get cs_arm64_op->type#} p
            let bP = plusPtr p 32
            case toEnum t of
              Arm64OpReg -> (Reg . toEnum . fromIntegral) <$>
                  (peek bP :: IO CUInt)
              Arm64OpImm -> (Imm . fromIntegral) <$> (peek bP :: IO Int64)
              Arm64OpCimm -> (CImm . fromIntegral) <$> (peek bP :: IO Int64)
              Arm64OpFp -> (Fp . realToFrac) <$> (peek bP :: IO CDouble)
              Arm64OpMem -> Mem <$> peek bP
              Arm64OpRegMsr -> (Pstate . toEnum . fromIntegral) <$>
                 (peek bP :: IO CInt)
              Arm64OpSys -> (Sys . fromIntegral) <$> (peek bP :: IO CUInt)
              Arm64OpPrefetch -> (Prefetch . toEnum . fromIntegral) <$>
                 (peek bP :: IO CInt)
              Arm64OpBarrier -> (Barrier . toEnum . fromIntegral) <$>
                 (peek bP :: IO CInt)
              _ -> return Undefined
        <*> (peekByteOff p 44 :: IO Word8) -- access
    poke p (CsArm64Op vI va (sh, shV) ext val acc) = do
        {#set cs_arm64_op->vector_index#} p (fromIntegral vI)
        {#set cs_arm64_op->vas#} p (fromIntegral $ fromEnum va)
        {#set cs_arm64_op->shift.type#} p (fromIntegral $ fromEnum sh)
        {#set cs_arm64_op->shift.value#} p (fromIntegral shV)
        {#set cs_arm64_op->ext#} p (fromIntegral $ fromEnum ext)
        let bP = plusPtr p 32
            setType = {#set cs_arm64_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CUInt)
              setType Arm64OpReg
          Imm i -> do
              poke bP (fromIntegral i :: Int64)
              setType Arm64OpImm
          CImm i -> do
              poke bP (fromIntegral i :: Int64)
              setType Arm64OpCimm
          Fp f -> do
              poke bP (realToFrac f :: CDouble)
              setType Arm64OpFp
          Mem m -> do
              poke bP m
              setType Arm64OpMem
          Pstate p -> do
              poke bP (fromIntegral $ fromEnum p :: CInt)
              setType Arm64OpRegMsr
          Sys s -> do
              poke bP (fromIntegral s :: CUInt)
              setType Arm64OpSys
          Prefetch p -> do
              poke bP (fromIntegral $ fromEnum p :: CInt)
              setType Arm64OpPrefetch
          Barrier b -> do
              poke bP (fromIntegral $ fromEnum b :: CInt)
              setType Arm64OpBarrier
          _ -> setType Arm64OpInvalid
        pokeByteOff p 44 acc

-- | instruction datatype
data CsArm64 = CsArm64
    { cc :: Arm64ConditionCode -- ^ condition code
    , updateFlags :: Bool -- ^ does this instruction update flags?
    , writeback :: Bool -- ^ does this instruction request writeback?
    , operands :: [CsArm64Op] -- ^ operand list of this instruction,
                              -- *MUST* have <= 8 elements, else you'll get a
                              -- runtime error when you (implicitly) try to
                              -- write it to memory via it's Storable instance
    } deriving (Show, Eq)

instance Storable CsArm64 where
    sizeOf _ = 392
    alignment _ = 12
    peek p = CsArm64
        <$> (toEnum . fromIntegral <$> {#get cs_arm64->cc#} p)
        <*> (toBool <$> (peekByteOff p 4 :: IO Word8)) -- update_flags
        <*> (toBool <$> (peekByteOff p 5 :: IO Word8)) -- writeback
        <*> do num <- fromIntegral <$> {#get cs_arm64->op_count#} p
               let ptr = plusPtr p {#offsetof cs_arm64.operands#}
               peekArray num ptr
    poke p (CsArm64 cc uF w o) = do
        {#set cs_arm64->cc#} p (fromIntegral $ fromEnum cc)
        pokeByteOff p 4 (fromBool uF :: Word8) -- update_flags
        pokeByteOff p 5 (fromBool w :: Word8) -- writeback
        {#set cs_arm64->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_arm64->operands#}) o

-- | ARM64 instructions
{#enum arm64_insn as Arm64Insn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | ARM64 instruction groups
{#enum arm64_insn_group as Arm64InsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
