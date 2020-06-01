{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.M68k
Description : M68k architecture header ported using C2HS + some boilerplate
Copyright   : (c) Khoa Nguyen Anh, 2020
License     : BSD3
Maintainer  : Khoa Nguyen Anh <mail.nganhkhoa@googlemail.com>
Stability   : experimental

This module contains M68k specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.M68k where

#include <capstone/m68k.h>

{#context lib = "capstone"#}

import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.Types

import Hapstone.Internal.Util

{#enum m68k_reg as M68kReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum m68k_address_mode as M68kAddressMode {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum m68k_op_type as M68kOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

data M68kOpMem = M68kOpMem
    { baseReg :: M68kReg
    , indexReg :: M68kReg
    , inBaseReg :: M68kReg
    , inDisp :: Word32
    , outDisp :: Word32
    , disp :: Int16
    , scale :: Word8
    , bitfield :: Word8
    , width :: Word8
    , offset :: Word8
    , indexSize :: Word8
    } deriving (Show, Eq)

instance Storable M68kOpMem where
    sizeOf _ = {#sizeof m68k_op_mem#}
    alignment _ = {#alignof m68k_op_mem#}
    peek p = M68kOpMem
        <$> ((toEnum . fromIntegral) <$> {#get m68k_op_mem->baseReg#} p)
        <*> ((toEnum . fromIntegral) <$> {#get m68k_op_mem->indexReg#} p)
        <*> ((toEnum . fromIntegral) <$> {#get m68k_op_mem->inBaseReg#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->inDisp#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->outDisp#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->disp#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->scale#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->bitfield#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->width#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->offset#} p)
        <*> (fromIntegral <$> {#get m68k_op_mem->indexSize#} p)
    poke p (M68kOpMem bR iR iBR iD oD d s b w o iS) = do
        {#set m68k_op_mem->baseReg#} p (fromIntegral $ fromEnum bR)
        {#set m68k_op_mem->indexReg#} p (fromIntegral $ fromEnum iR)
        {#set m68k_op_mem->inBaseReg#} p (fromIntegral $ fromEnum iBR)
        {#set m68k_op_mem->inDisp#} p (fromIntegral iD)
        {#set m68k_op_mem->outDisp#} p (fromIntegral oD)
        {#set m68k_op_mem->disp#} p (fromIntegral d)
        {#set m68k_op_mem->scale#} p (fromIntegral s)
        {#set m68k_op_mem->bitfield#} p (fromIntegral b)
        {#set m68k_op_mem->width#} p (fromIntegral w)
        {#set m68k_op_mem->offset#} p (fromIntegral o)
        {#set m68k_op_mem->indexSize#} p (fromIntegral iS)


{#enum m68k_op_br_disp_size as M68kOpBrDispSize {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

data M68kOpBrDisp = M68kOpBrDisp
    { disp :: Int32
    , size :: M68kOpBrDispSize
    } deriving (Show, Eq)

instance Storable M68kOpBrDisp where
    sizeOf _ = {#sizeof m68k_op_br_disp#}
    alignment _ = {#alignof m68k_op_br_disp#}
    peek p = M68kOpBrDisp
        <$> (fromIntegral <$> {#get m68k_op_br_disp->disp#} p)
        <*> ((toEnum . fromIntegral) <$> {#get m68k_op_br_disp->size#} p)
    poke p (M68kOpBrDisp d s) = do
        {#set m68k_op_br_disp->disp#} p (fromIntegral d)
        {#set m68k_op_br_disp->size#} p (fromIntegral $ fromEnum s)


data CsM68kOpValue
    = Imm Word64
    | DImm Double
    | SImm Float
    | Reg M68kReg
    | RegPair (M68kReg, M68kReg)
    | Undefined
    deriving (Show, Eq)

data CsM68kOp = CsM68kOp
    { mem :: M68kOpMem
    , brDisp :: M68kOpBrDisp
    , registerBits :: Word32
    , value :: CsM68kOpValue
    , addressMode :: M68kAddressMode
    } deriving (Show, Eq)

instance Storable CsM68kOp where
    sizeOf _ = {#sizeof cs_m68k_op#}
    alignment _ = {#alignof cs_m68k_op#}
    peek p = CsM68kOp
        <$> do
            t <- fromIntegral <$> {#get cs_m68k_op->type#} p
            let regP = plusPtr p {#offsetof cs_m68k_op->reg#}
            let reg0P = plusPtr p {#offsetof cs_m68k_op->reg_0#}
            let reg1P = plusPtr p {#offsetof cs_m68k_op->reg_1#}
            case toEnum t of
                M68kOpImm -> (Imm. toEnum . fromIntegral) <$> {#get cs_m68k_op->imm#} p
                M68kOpFpDouble -> (Reg . toEnum . fromIntegral) <$> {#get cs_m68k_op->dimm#} p
                M68kOpFpSingle -> (Reg . toEnum . fromIntegral) <$> {#get cs_m68k_op->simm#} p
                M68OpReg -> Reg <$> (peek regP)
                M68OpRegPair -> (,)
                    <$> (Reg <$> (peek reg0P))
                    <*> (Reg <$> (peek reg1P))
        <*> peek (plusPtr p {#offsetof cs_m68k_op->br_disp#})
        <*> (fromIntegral <$> {#get cs_m68k_op->registerBits#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_m64k_op->value#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_m64k_op->addressMode#} p)
    poke p (CsM68kOp m b r v a) = do
        poke (plusPtr p {#offsetof cs_m68k_op->br_disp#}) b
        {#set cs_m68k_op->value#} p (fromIntegral $ fromEnum v)
        {#set cs_m68k_op->addressMode#} p (fromIntegral $ fromEnum a)
        {#set cs_m68k_op->registerBits#} p (fromIntegral r)
        let regP = plusPtr p {#offsetof cs_m68k_op->reg#}
            immP = plusPtr p {#offsetof cs_m68k_op->imm#}
            dimmP = plusPtr p {#offsetof cs_m68k_op->dimm#}
            simmP = plusPtr p {#offsetof cs_m68k_op->simm#}
            reg0P = plusPtr p {#offsetof cs_m68k_op->reg_0#}
            reg1P = plusPtr p {#offsetof cs_m68k_op->reg_1#}
            setType = {#set cs_m68k_op->type#} p . fromIntegral . fromEnum
        case m of
          Reg r -> do
              poke regP (fromIntegral $ fromEnum r :: CUInt)
              setType M68kOpReg
          Imm i -> do
              poke immP (fromIntegral i :: Int64)
              setType M68kOpImm
          DImm i -> do
              poke dimmP (fromIntegral i :: Int64)
              setType M68kOpFpDouble
          SImm i -> do
              poke simmP (fromIntegral i :: Int64)
              setType M68kOpFpSingle
          RegPair (r0, r1) -> do
              poke reg0P (fromIntegral $ fromEnum r0 :: CUInt)
              poke reg1P (fromIntegral $ fromEnum r1 :: CUInt)
              setType M68kOpRegPair
          _ -> setType M68kOpInvalid

{#enum m68k_cpu_size as M68kCpuSize {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum m68k_fpu_size as M68kFpuSize {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum m68k_size_type as M68kSizeType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

data M68kOpSize
    = CpuSize M68kCpuSize
    | FpuSize M68kCpuSize
    | M68kOpSizeInvalid
    deriving (Show, Eq)

instance Storable M68kOpSize where
    sizeOf _ = {#sizeof m68k_op_size#}
    alignment _ = {#alignof m68k_op_size#}
    peek p = do
        t <- fromIntegral <$> {#get m68k_op_size->type#} p
        case toEnum t of
            M68kSizeTypeCpu -> (CpuSize . toEnum . fromIntegral) <$> {#get m68k_op_size->cpu_size#} p
            M68KSizeTypeFpu -> (FpuSize . toEnum . fromIntegral) <$> {#get m68k_op_size->fpu_size#} p
            _ -> M68kOpSizeInvalid
    poke p opSize = do
        let cpuP = plusPtr p {#offsetof m68_op_size->cpu_size#}
            fpuP = plusPtr p {#offsetof m68_op_size->fpu_size#}
            setType = {#set m68_op_size->type#} p . fromIntegral . fromEnum
        case opSize of
            CpuSize c -> do
                poke cpuP (fromIntegral $ fromEnum c :: CUInt)
                setType M68kSizeTypeCpu
            FpuSize f -> do
                poke fpuP (fromIntegral $ fromEnum f :: CUInt)
                setType M68kSizeTypeFpu
            _ -> setType M68kSizeInvalid


data CsM68k = CsM68k
    { operands :: [CsM68kOp]
    , size :: M68kOpSize
    } deriving (Show, Eq)

instance Storable CsM68k where
    sizeOf _ = {#sizeof cs_m68k#}
    alignment _ = {#alignof cs_m68k#}
    peek p = CsM68k
        <$> do num <- fromIntegral <$> {#get cs_m68k->op_count#} p
               let ptr = plusPtr p {#offsetof cs_m68k->operands#}
               peekArray num ptr
        <*> peek (plusPtr p {#offsetof cs_m68k->op_size#})
    poke p (CsM68k o s) = do
        poke (plusPtr p {#offsetof cs_m68k-op_size>#}) s
        {#set cs_m68k->op_count#} p (fromIntegral $ length o)
        if length o > 4
           then error "operands overflew 4 elements"
           else pokeArray (plusPtr p {#offsetof cs_m68k->operands#}) o

{#enum m68k_insn as M68kInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum m68k_group_type as M68kGroupType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
