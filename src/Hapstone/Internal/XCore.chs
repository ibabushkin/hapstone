{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.XCore
Description : XCore architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains XCore specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.XCore where

#include <capstone/xcore.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | operand type for instruction's operands
{#enum xcore_op_type as XCoreOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | XCore registers
{#enum xcore_reg as XCoreReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
-- associated with 'XcoreOpMem' operand type
data XCoreOpMemStruct = XCoreOpMemStruct
    { base :: Word8 -- ^ base register
    , index :: Word8 -- ^ index register
    , disp :: Int32 -- ^ displacement/offset value
    , direct :: Int32 -- ^ +1: forward, -1: backward
    } deriving (Show, Eq)

instance Storable XCoreOpMemStruct where
    sizeOf _ = {#sizeof xcore_op_mem#}
    alignment _ = {#alignof xcore_op_mem#}
    peek p = XCoreOpMemStruct
        <$> (fromIntegral <$> {#get xcore_op_mem->base#} p)
        <*> (fromIntegral <$> {#get xcore_op_mem->index#} p)
        <*> (fromIntegral <$> {#get xcore_op_mem->disp#} p)
        <*> (fromIntegral <$> {#get xcore_op_mem->direct#} p)
    poke p (XCoreOpMemStruct b i disp dir) = do
        {#set xcore_op_mem->base#} p (fromIntegral b)
        {#set xcore_op_mem->index#} p (fromIntegral i)
        {#set xcore_op_mem->disp#} p (fromIntegral disp)
        {#set xcore_op_mem->direct#} p (fromIntegral dir)

-- | instruction operand
data CsXCoreOp
    = Reg XCoreReg -- ^ register value for 'XcoreOpReg' operands
    | Imm Int32 -- ^ immediate value for 'XcoreOpImm' operands
    | Mem XCoreOpMemStruct -- ^ base/index/disp/direct value for 'XcoreOpMem'
                           -- operands
    | Undefined -- ^ invalid operand value, for 'XcoreOpInvalid' operand
    deriving (Show, Eq)

instance Storable CsXCoreOp where
    sizeOf _ = {#sizeof cs_xcore_op#}
    alignment _ = {#alignof cs_xcore_op#}
    peek p = do
        t <- fromIntegral <$> {#get cs_xcore_op->type#} p
        let memP = plusPtr p {#offsetof cs_xcore_op->mem#}
        case toEnum t of
          XcoreOpReg -> (Reg . toEnum . fromIntegral) <$> {#get cs_xcore_op->reg#} p
          XcoreOpImm -> (Imm . fromIntegral) <$> {#get cs_xcore_op->imm#} p
          XcoreOpMem -> Mem <$> peek memP
          _ -> return Undefined
    poke p op = do
        let regP = plusPtr p {#offsetof cs_xcore_op->reg#}
            immP = plusPtr p {#offsetof cs_xcore_op->imm#}
            memP = plusPtr p {#offsetof cs_xcore_op->mem#}
            setType = {#set cs_xcore_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> do poke regP (fromIntegral $ fromEnum r :: Int32)
                      setType XcoreOpReg
          Imm i -> poke immP i >> setType XcoreOpImm
          Mem m -> poke memP m >> setType XcoreOpMem
          _ -> setType XcoreOpInvalid

-- | instruction datatype
newtype CsXCore = CsXCore [CsXCoreOp] -- ^ operand list of this instruction,
                                      -- *MUST* have <= 8 operands, else you'll
                                      -- get a runtime error when you
                                      -- (implicitly) try to write it to memory
                                      -- via it's Storable instance
    deriving (Show, Eq)

instance Storable CsXCore where
    sizeOf _ = {#sizeof cs_xcore#}
    alignment _ = {#alignof cs_xcore#}
    peek p = do
        num <- fromIntegral <$> {#get cs_xcore->op_count#} p
        CsXCore <$> peekArray num (plusPtr p {#offsetof cs_xcore.operands#})
    poke p (CsXCore o) = do
        {#set cs_xcore->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_xcore->operands#}) o

-- | XCore instructions
{#enum xcore_insn as XCoreInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | XCore instruction groups
{#enum xcore_insn_group as XCoreInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
