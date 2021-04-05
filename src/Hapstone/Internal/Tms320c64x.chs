{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Tms320c64x
Description : Tms320c64x architecture header ported using C2HS + some boilerplate
Copyright   : (c) Khoa Nguyen Anh, 2020
License     : BSD3
Maintainer  : Khoa Nguyen Anh <mail.nganhkhoa@googlemail.com>
Stability   : experimental

This module contains Tms320c64x specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.Tms320c64x where

#include <capstone/tms320c64x.h>

{#context lib = "capstone"#}

import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.Types

import Hapstone.Internal.Util

{#enum tms320c64x_op_type as Tms320c64xOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum tms320c64x_mem_disp as Tms320c64xMemDisp {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum tms320c64x_mem_dir as Tms320c64xMemDir {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum tms320c64x_mem_mod as Tms320c64xMemMod {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

data Tms320c64xOpMemStruct = Tms320c64xOpMemStruct
    { base :: Word32
    , disp :: Word32
    , unit :: Word32
    , scaled :: Word32
    , disptype :: Word32
    , direction :: Word32
    , modify :: Word32
    } deriving (Show, Eq)

instance Storable Tms320c64xOpMemStruct where
    sizeOf _ = {#sizeof tms320c64x_op_mem#}
    alignment _ = {#alignof tms320c64x_op_mem#}
    peek p = Tms320c64xOpMemStruct
        <$> (fromIntegral <$> {#get tms320c64x_op_mem->base#} p)
        <*> (fromIntegral <$> {#get tms320c64x_op_mem->disp#} p)
        <*> (fromIntegral <$> {#get tms320c64x_op_mem->unit#} p)
        <*> (fromIntegral <$> {#get tms320c64x_op_mem->scaled#} p)
        <*> (fromIntegral <$> {#get tms320c64x_op_mem->disptype#} p)
        <*> (fromIntegral <$> {#get tms320c64x_op_mem->direction#} p)
        <*> (fromIntegral <$> {#get tms320c64x_op_mem->modify#} p)
    poke p (Tms320c64xOpMemStruct b d u s dt dr m) = do
        {#set tms320c64x_op_mem->base#} p (fromIntegral b)
        {#set tms320c64x_op_mem->disp#} p (fromIntegral d)
        {#set tms320c64x_op_mem->unit#} p (fromIntegral u)
        {#set tms320c64x_op_mem->scaled#} p (fromIntegral s)
        {#set tms320c64x_op_mem->disptype#} p (fromIntegral dt)
        {#set tms320c64x_op_mem->direction#} p (fromIntegral dr)
        {#set tms320c64x_op_mem->modify#} p (fromIntegral m)

data CsTms320c64xOpValue
    = Imm Int32
    | Reg Word32
    | RegPair Word32
    | Mem Tms320c64xOpMemStruct
    | CsTms320c64xOpInvalid
    deriving (Show, Eq)

data CsTms320c64xOp = CsTms320c64xOp
    { value :: CsTms320c64xOpValue
    } deriving (Show, Eq)

instance Storable CsTms320c64xOp where
    sizeOf _ = {#sizeof cs_tms320c64x_op#}
    alignment _ = {#alignof cs_tms320c64x_op#}
    peek p = CsTms320c64xOp
        <$> do
            t <- fromIntegral <$> {#get cs_tms320c64x_op->type#} p
            let memP = plusPtr p {#offsetof cs_tms320c64x_op->mem#}
            case toEnum t of
                Tms320c64xOpReg -> (Reg . fromIntegral) <$> {#get cs_tms320c64x_op->reg#} p
                Tms320c64xOpRegpair -> (RegPair . fromIntegral) <$> {#get cs_tms320c64x_op->reg#} p
                Tms320c64xOpImm -> (Imm . fromIntegral) <$> {#get cs_tms320c64x_op->reg#} p
                Tms320c64xOpMem -> Mem <$> peek memP
                _ -> return CsTms320c64xOpInvalid
    poke p (CsTms320c64xOp v) = do
        let regP = plusPtr p {#offsetof cs_tms320c64x_op->reg#}
            immP = plusPtr p {#offsetof cs_tms320c64x_op->imm#}
            memP = plusPtr p {#offsetof cs_tms320c64x_op->mem#}
            setType = {#set cs_tms320c64x_op->type#} p . fromIntegral . fromEnum
        case v of
          Reg r -> do
              poke regP (fromIntegral r :: CUInt)
              setType Tms320c64xOpReg
          RegPair r -> do
              poke regP (fromIntegral r :: CUInt)
              setType Tms320c64xOpRegpair
          Imm i -> do
              poke immP i
              setType Tms320c64xOpImm
          Mem m -> do
              poke memP m
              setType Tms320c64xOpMem
          _ -> setType Tms320c64xOpInvalid

data CsTms320c64x = CsTms320c64x
    { operands :: [CsTms320c64xOp]
    , condition :: (Word32, Word32)
    , funit :: (Word32, Word32, Word32)
    , parallel :: Word32
    } deriving (Show, Eq)

instance Storable CsTms320c64x where
    sizeOf _ = {#sizeof cs_tms320c64x#}
    alignment _ = {#alignof cs_tms320c64x#}
    peek p = CsTms320c64x
        <$> do num <- fromIntegral <$> {#get cs_tms320c64x->op_count#} p
               let ptr = plusPtr p {#offsetof cs_tms320c64x->operands#}
               peekArray num ptr
        <*> ((,)
            <$> (fromIntegral <$> {#get cs_tms320c64x->condition.reg#} p)
            <*> (fromIntegral <$> {#get cs_tms320c64x->condition.zero#} p))
        <*> ((,,)
            <$> (fromIntegral <$> {#get cs_tms320c64x->funit.unit#} p)
            <*> (fromIntegral <$> {#get cs_tms320c64x->funit.side#} p)
            <*> (fromIntegral <$> {#get cs_tms320c64x->funit.crosspath#} p))
        <*> (fromIntegral <$> {#get cs_tms320c64x->parallel#} p)
    poke p (CsTms320c64x o (r, z) (u, s, c) pr) = do
        {#set cs_tms320c64x->condition.reg#} p (fromIntegral r)
        {#set cs_tms320c64x->condition.zero#} p (fromIntegral z)
        {#set cs_tms320c64x->funit.unit#} p (fromIntegral u)
        {#set cs_tms320c64x->funit.side#} p (fromIntegral s)
        {#set cs_tms320c64x->funit.crosspath#} p (fromIntegral c)
        {#set cs_tms320c64x->parallel#} p (fromIntegral pr)
        {#set cs_tms320c64x->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_tms320c64x->operands#}) o

{#enum tms320c64x_reg as Tms320c64xReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum tms320c64x_insn as Tms320c64xInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum tms320c64x_insn_group as Tms320c64xInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum tms320c64x_funit as Tms320c64xFunit {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
