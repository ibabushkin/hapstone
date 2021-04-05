{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.M680x
Description : M680x architecture header ported using C2HS + some boilerplate
Copyright   : (c) Khoa Nguyen Anh, 2020
License     : BSD3
Maintainer  : Khoa Nguyen Anh <mail.nganhkhoa@googlemail.com>
Stability   : experimental

This module contains M680X specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.M680x where

#include <capstone/m680x.h>

{#context lib = "capstone"#}

import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.Types

import Hapstone.Internal.Util

{#enum m680x_reg as M680xReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum m680x_op_type as M680xOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

data M680xOpIdx = M680xOpIdx
    { baseReg :: M680xReg
    , offsetReg :: M680xReg
    , idxOffset :: Int16
    , offsetAddr :: Int16
    , offsetBits :: Word8
    , incDec :: Int8
    , idxFlags :: Word8
    } deriving (Show, Eq)

instance Storable M680xOpIdx where
    sizeOf _ = {#sizeof m680x_op_idx#}
    alignment _ = {#alignof m680x_op_idx#}
    peek p = M680xOpIdx
        <$> ((toEnum . fromIntegral) <$> {#get m680x_op_idx->base_reg#} p)
        <*> ((toEnum . fromIntegral) <$> {#get m680x_op_idx->offset_reg#} p)
        <*> (fromIntegral <$> {#get m680x_op_idx->offset#} p)
        <*> (fromIntegral <$> {#get m680x_op_idx->offset_addr#} p)
        <*> (fromIntegral <$> {#get m680x_op_idx->offset_bits#} p)
        <*> (fromIntegral <$> {#get m680x_op_idx->inc_dec#} p)
        <*> (fromIntegral <$> {#get m680x_op_idx->flags#} p)
    poke p (M680xOpIdx br or o oa ob id f) = do
        {#set m680x_op_idx->base_reg#} p (fromIntegral $ fromEnum br)
        {#set m680x_op_idx->offset_reg#} p (fromIntegral $ fromEnum or)
        {#set m680x_op_idx->offset#} p (fromIntegral o)
        {#set m680x_op_idx->offset_addr#} p (fromIntegral oa)
        {#set m680x_op_idx->offset_bits#} p (fromIntegral ob)
        {#set m680x_op_idx->inc_dec#} p (fromIntegral id)
        {#set m680x_op_idx->flags#} p (fromIntegral f)

data M680xOpRel = M680xOpRel
    { relAddress :: Word16
    , relOffset :: Int16
    } deriving (Show, Eq)

instance Storable M680xOpRel where
    sizeOf _ = {#sizeof m680x_op_rel#}
    alignment _ = {#alignof m680x_op_rel#}
    peek p = M680xOpRel
        <$> (fromIntegral <$> {#get m680x_op_rel->address#} p)
        <*> (fromIntegral <$> {#get m680x_op_rel->offset#} p)
    poke p (M680xOpRel a o) = do
        {#set m680x_op_rel->address#} p (fromIntegral a)
        {#set m680x_op_rel->offset#} p (fromIntegral o)

data M680xOpExt = M680xOpExt
    { extAddress :: Word16
    , indirect :: Bool
    } deriving (Show, Eq)

instance Storable M680xOpExt where
    sizeOf _ = {#sizeof m680x_op_ext#}
    alignment _ = {#alignof m680x_op_ext#}
    peek p = M680xOpExt
        <$> (fromIntegral <$> {#get m680x_op_ext->address#} p)
        <*> (toBool <$> (peekByteOff p {#offsetof m680x_op_ext->indirect#} :: IO Word8))
    poke p (M680xOpExt a i) = do
        {#set m680x_op_ext->address#} p (fromIntegral a)
        {#set m680x_op_ext->indirect#} p i

data CsM680xOpValue
    = Imm Int32
    | Reg M680xReg
    | Idx M680xOpIdx
    | Rel M680xOpRel
    | Ext M680xOpExt
    | Addr Word8
    | Val Word8
    | CsM680xOpInvalid
    deriving (Show, Eq)

data CsM680xOp = CsM680xOp
    { value :: CsM680xOpValue
    , size :: Word8
    , access :: Word8
    } deriving (Show, Eq)

instance Storable CsM680xOp where
    sizeOf _ = {#sizeof cs_m680x_op#}
    alignment _ = {#alignof cs_m680x_op#}
    peek p = CsM680xOp
        <$> do
            t <- fromIntegral <$> {#get cs_m680x_op->type#} p
            let idxP = plusPtr p {#offsetof cs_m680x_op->idx#}
            let relP = plusPtr p {#offsetof cs_m680x_op->rel#}
            let extP = plusPtr p {#offsetof cs_m680x_op->ext#}
            case toEnum t of
                M680xOpRegister -> (Reg . toEnum . fromIntegral) <$> {#get cs_m680x_op->reg#} p
                M680xOpImmediate -> (Imm . fromIntegral) <$> {#get cs_m680x_op->imm#} p
                M680xOpIndexed -> Idx <$> peek idxP
                M680xOpExtended -> Ext <$> peek extP
                M680xOpDirect -> (Addr . fromIntegral) <$> {#get cs_m680x_op->direct_addr#} p
                M680xOpRelative -> Rel <$> peek relP
                M680xOpConstant -> (Val . fromIntegral) <$> {#get cs_m680x_op->const_val#} p
                _ -> return CsM680xOpInvalid
        <*> (fromIntegral <$> {#get cs_m680x_op->size#} p)
        <*> (fromIntegral <$> {#get cs_m680x_op->access#} p)
    poke p (CsM680xOp v s a) = do
        {#set cs_m680x_op->size#} p (fromIntegral s)
        {#set cs_m680x_op->access#} p (fromIntegral a)
        let regP = plusPtr p {#offsetof cs_m680x_op->reg#}
            immP = plusPtr p {#offsetof cs_m680x_op->imm#}
            idxP = plusPtr p {#offsetof cs_m680x_op->idx#}
            relP = plusPtr p {#offsetof cs_m680x_op->rel#}
            extP = plusPtr p {#offsetof cs_m680x_op->ext#}
            addrP = plusPtr p {#offsetof cs_m680x_op->direct_addr#}
            valP = plusPtr p {#offsetof cs_m680x_op->const_val#}
            setType = {#set cs_m680x_op->type#} p . fromIntegral . fromEnum
        case v of
            Imm i -> do
              poke immP (fromIntegral i :: CUInt)
              setType M680xOpImmediate
            Reg r -> do
              poke regP (fromIntegral $ fromEnum r :: CUInt)
              setType M680xOpRegister
            Idx i -> do
              poke idxP i
              setType M680xOpIndexed
            Rel r -> do
              poke relP r
              setType M680xOpRelative
            Ext e -> do
              poke extP e
              setType M680xOpExtended
            Addr a -> do
              poke addrP (fromIntegral a :: CUInt)
              setType M680xOpDirect
            Val v -> do
              poke valP (fromIntegral a :: CUInt)
              setType M680xOpConstant
            _ -> setType M680xOpInvalid


data CsM680x = CsM680x
    { flags :: Word8
    , operands :: [CsM680xOp]
    } deriving (Show, Eq)

instance Storable CsM680x where
    sizeOf _ = {#sizeof cs_m680x#}
    alignment _ = {#alignof cs_m680x#}
    peek p = CsM680x
        <$> (fromIntegral <$> {#get cs_m680x->flags#} p)
        <*> do num <- fromIntegral <$> {#get cs_m680x->op_count#} p
               let ptr = plusPtr p {#offsetof cs_m680x->operands#}
               peekArray num ptr
    poke p (CsM680x f o) = do
        {#set cs_m680x->flags#} p (fromIntegral f)
        if length o > 9
           then error "operands overflew 9 elements"
           else pokeArray (plusPtr p {#offsetof cs_m680x->operands#}) o

-- | M680X instructions
{#enum m680x_insn as M680xInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
