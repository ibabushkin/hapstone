{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.X86
Description : x86 architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains x86 specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.X86 where

-- ugly workaround because... capstone doesn't import stdbool.h
#include <stdbool.h> 
#include <capstone/x86.h>

{#context lib = "capstone"#}

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)

import Foreign
import Foreign.C.Types

import Hapstone.Internal.Util

-- | x86 registers
{#enum x86_reg as X86Reg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- TODO: add X86_EFLAGS_* flags as enum

-- | operand type for instruction's operands
{#enum x86_op_type as X86OpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | AVX broadcast
{#enum x86_avx_bcast as X86AvxBcast {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | SSE condition code
{#enum x86_sse_cc as X86SseCc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | AVX condition code
{#enum x86_avx_cc as X86AvxCc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | AVX static rounding mode
{#enum x86_avx_rm as X86AvxRm {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | instruction prefix
{#enum x86_prefix as X86Prefix {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | memory access operands
data X86OpMemStruct = X86OpMemStruct
    { segment :: X86Reg -- ^ segment register
    , base :: X86Reg -- ^ base register
    , index :: X86Reg -- ^ index register
    , scale :: Int32 -- ^ scale for index register
    , disp' :: Int64 -- ^ displacement/offset value
    } deriving (Show, Eq)

instance Storable X86OpMemStruct where
    sizeOf _ = {#sizeof x86_op_mem#}
    alignment _ = {#alignof x86_op_mem#}
    peek p = X86OpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get x86_op_mem->segment#} p)
        <*> ((toEnum . fromIntegral) <$> {#get x86_op_mem->base#} p)
        <*> ((toEnum . fromIntegral) <$> {#get x86_op_mem->index#} p)
        <*> (fromIntegral <$> {#get x86_op_mem->scale#} p)
        <*> (fromIntegral <$> {#get x86_op_mem->disp#} p)
    poke p (X86OpMemStruct se b i sc d) = do
        {#set x86_op_mem->segment#} p (fromIntegral $ fromEnum se)
        {#set x86_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set x86_op_mem->index#} p (fromIntegral $ fromEnum i)
        {#set x86_op_mem->scale#} p (fromIntegral sc)
        {#set x86_op_mem->disp#} p (fromIntegral d)

-- | possible operand types (corresponding to the tagged union in the C header)
data CsX86OpValue
    = Reg X86Reg -- ^ register value for 'X86OpReg' operands
    | Imm Word64 -- ^ immediate value for 'X86OpImm' operands
    -- | Fp Double ^ floating point value for 'X86OpFp' operands
    | Mem X86OpMemStruct -- ^ segment,base,index,scale,disp value for
                         -- 'X86OpMem' operands
    | Undefined -- ^ invalid operand value, for 'X86OpInvalid' operand
    deriving (Show, Eq)

-- | instruction operand
data CsX86Op = CsX86Op
    { value :: CsX86OpValue -- ^ operand type and value
    , size :: Word8 -- ^ size of this operand in bytes
    , access :: Word8 -- ^ access mode of this operand
    , avxBcast :: X86AvxBcast -- ^ AVX broadcast type
    , avxZeroOpmask :: Bool -- ^ AVX zero opmask
    } deriving (Show, Eq)

instance Storable CsX86Op where
    sizeOf _ = 48
    alignment _ = 8
    peek p = CsX86Op
        <$> do
            t <- fromIntegral <$> {#get cs_x86_op->type#} p
            let bP = plusPtr p 8
            case toEnum t of
              X86OpReg -> (Reg . toEnum . fromIntegral) <$>
                  (peek bP :: IO CInt)
              X86OpImm -> Imm <$> peek bP
              -- X86OpFp -> (Fp . realToFrac) <$> (peek bP :: IO CDouble)
              X86OpMem -> Mem <$> peek bP
              _ -> return Undefined
        <*> (peekByteOff p 32) -- size
        <*> (peekByteOff p 33) -- access
        <*> ((toEnum . fromIntegral) <$>
                (peekByteOff p 36 :: IO CInt)) -- avx_bcast
        <*> (toBool <$> (peekByteOff p 40 :: IO Word8)) -- avx_zero_opmask
    poke p (CsX86Op val s a ab az) = do
        let bP = plusPtr p 8
            setType = {#set cs_x86_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CInt)
              setType X86OpReg
          Imm i -> do
              poke bP i
              setType X86OpImm
          {- Fp f -> do
              poke bP (realToFrac f :: CDouble)
              setType X86OpFp -}
          Mem m -> do
              poke bP m
              setType X86OpMem
          Undefined -> setType X86OpInvalid
        pokeByteOff p 32 s -- size
        pokeByteOff p 33 a -- access
        pokeByteOff p 36 (fromIntegral $ fromEnum ab :: CInt) -- avx_bcast
        pokeByteOff p 40 (fromBool az :: Word8) -- avx_zero_opmask

-- instructions
data CsX86 = CsX86
    { prefix :: (Maybe Word8, Maybe Word8, Maybe Word8, Maybe Word8)
      -- ^ instruction prefix, up to 4 bytes. Each prefix byte is optional.
      -- first byte: REP/REPNE/LOCK, second byte: segment override,
      -- third byte: operand-size override
      -- fourth byte: address-size override
    , opcode :: [Word8] -- ^ instruction opcode, 1-4 bytes long
    , rex :: Word8 -- ^ REX prefix, only non-zero values relevant for x86_64
    , addrSize :: Word8 -- ^ address size
    , modRM :: Word8 -- ^ ModR/M byte
    , sib :: Maybe Word8 -- ^ optional SIB value
    , disp :: Maybe Int32 -- ^ optional displacement value
    , sibIndex :: X86Reg -- ^ SIB index register, possibly irrelevant
    , sibScale :: Int8 -- ^ SIB scale, possibly irrelevant
    , sibBase :: X86Reg -- ^ SIB base register, possibly irrelevant
    , sseCc :: X86SseCc -- ^ SSE condition code
    , avxCc :: X86AvxCc -- ^ AVX condition code
    , avxSae :: Bool -- ^ AXV Supress all Exception
    , avxRm :: X86AvxRm -- ^ AVX static rounding mode
    , operands :: [CsX86Op] -- ^ operand list for this instruction, *MUST*
                            -- have <= 8 elements, else you'll get a runtime
                            -- error when you (implicitly) try to write it to
                            -- memory via it's Storable instance
    } deriving (Show, Eq)

instance Storable CsX86 where
    sizeOf _ = 424
    alignment _ = 8
    peek p = CsX86
        <$> do let bP = plusPtr p {#offsetof cs_x86->prefix#}
               [p0, p1, p2, p3] <- peekArray 4 bP :: IO [Word8]
               return (fromZero p0, fromZero p1, fromZero p2, fromZero p3)
        <*> (dropWhileEnd (== 0) <$>
            peekArray 4 (plusPtr p {#offsetof cs_x86->opcode#}))
        <*> (fromIntegral <$> {#get cs_x86->rex#} p)
        <*> (fromIntegral <$> {#get cs_x86->addr_size#} p)
        <*> (fromIntegral <$> {#get cs_x86->modrm#} p)
        <*> ((fromZero . fromIntegral) <$> {#get cs_x86->sib#} p)
        <*> ((fromZero . fromIntegral) <$> {#get cs_x86->disp#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->sib_index#} p) -- 16
        <*> (fromIntegral <$> {#get cs_x86->sib_scale#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->sib_base#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->sse_cc#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->avx_cc#} p)
        <*> (toBool <$> (peekByteOff p {#offsetof cs_x86->avx_sae#} :: IO Word8)) -- avx_sae
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->avx_rm#} p)
        <*> do num <- (fromIntegral <$> {#get cs_x86->op_count#} p)
               let ptr = plusPtr p {#offsetof cs_x86->operands#}
               peekArray num ptr
    poke p (CsX86 (p0, p1, p2, p3) op r a m s d sI sS sB sC aC aS aR o) =
        do
        let p' = [ fromMaybe 0 p0
                 , fromMaybe 0 p1
                 , fromMaybe 0 p2
                 , fromMaybe 0 p3
                 ]
            op' = op ++ replicate (4 - length op) 0
        pokeArray (plusPtr p {#offsetof cs_x86->prefix#}) p'
        pokeArray (plusPtr p {#offsetof cs_x86->opcode#}) op'
        {#set cs_x86->rex#} p (fromIntegral r)
        {#set cs_x86->addr_size#} p (fromIntegral a)
        {#set cs_x86->modrm#} p (fromIntegral m)
        {#set cs_x86->sib#} p (fromIntegral $ fromMaybe 0 s)
        {#set cs_x86->disp#} p (fromIntegral $ fromMaybe 0 d)
        {#set cs_x86->sib_index#} p (fromIntegral $ fromEnum sI)
        {#set cs_x86->sib_scale#} p (fromIntegral sS)
        {#set cs_x86->sib_base#} p (fromIntegral $ fromEnum sB)
        {#set cs_x86->sse_cc#} p (fromIntegral $ fromEnum sC)
        {#set cs_x86->avx_cc#} p (fromIntegral $ fromEnum aC)
        pokeByteOff p {#offsetof cs_x86->avx_sae#} (fromBool aS :: Word8) -- avx_sae
        {#set cs_x86->avx_rm#} p (fromIntegral $ fromEnum aR)
        {#set cs_x86->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_x86->operands#}) o

-- | x86 instructions
{#enum x86_insn as X86Insn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | x86 instruction groups
{#enum x86_insn_group as X86InsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
