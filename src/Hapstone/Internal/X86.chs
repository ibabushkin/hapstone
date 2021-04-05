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

-- | x86 eflags
{#enum define X86EFlags
    { X86_EFLAGS_MODIFY_AF    as X86EflagsModifyAf
    , X86_EFLAGS_MODIFY_CF    as X86EflagsModifyCf
    , X86_EFLAGS_MODIFY_SF    as X86EflagsModifySf
    , X86_EFLAGS_MODIFY_ZF    as X86EflagsModifyZf
    , X86_EFLAGS_MODIFY_PF    as X86EflagsModifyPf
    , X86_EFLAGS_MODIFY_OF    as X86EflagsModifyOf
    , X86_EFLAGS_MODIFY_TF    as X86EflagsModifyTf
    , X86_EFLAGS_MODIFY_IF    as X86EflagsModifyIf
    , X86_EFLAGS_MODIFY_DF    as X86EflagsModifyDf
    , X86_EFLAGS_MODIFY_NT    as X86EflagsModifyNt
    , X86_EFLAGS_MODIFY_RF    as X86EflagsModifyRf
    , X86_EFLAGS_PRIOR_OF     as X86EflagsPriorOf
    , X86_EFLAGS_PRIOR_SF     as X86EflagsPriorSf
    , X86_EFLAGS_PRIOR_ZF     as X86EflagsPriorZf
    , X86_EFLAGS_PRIOR_AF     as X86EflagsPriorAf
    , X86_EFLAGS_PRIOR_PF     as X86EflagsPriorPf
    , X86_EFLAGS_PRIOR_CF     as X86EflagsPriorCf
    , X86_EFLAGS_PRIOR_TF     as X86EflagsPriorTf
    , X86_EFLAGS_PRIOR_IF     as X86EflagsPriorIf
    , X86_EFLAGS_PRIOR_DF     as X86EflagsPriorDf
    , X86_EFLAGS_PRIOR_NT     as X86EflagsPriorNt
    , X86_EFLAGS_RESET_OF     as X86EflagsResetOf
    , X86_EFLAGS_RESET_CF     as X86EflagsResetCf
    , X86_EFLAGS_RESET_DF     as X86EflagsResetDf
    , X86_EFLAGS_RESET_IF     as X86EflagsResetIf
    , X86_EFLAGS_RESET_SF     as X86EflagsResetSf
    , X86_EFLAGS_RESET_AF     as X86EflagsResetAf
    , X86_EFLAGS_RESET_TF     as X86EflagsResetTf
    , X86_EFLAGS_RESET_NT     as X86EflagsResetNt
    , X86_EFLAGS_RESET_PF     as X86EflagsResetPf
    , X86_EFLAGS_SET_CF       as X86EflagsSetCf
    , X86_EFLAGS_SET_DF       as X86EflagsSetDf
    , X86_EFLAGS_SET_IF       as X86EflagsSetIf
    , X86_EFLAGS_TEST_OF      as X86EflagsTestOf
    , X86_EFLAGS_TEST_SF      as X86EflagsTestSf
    , X86_EFLAGS_TEST_ZF      as X86EflagsTestZf
    , X86_EFLAGS_TEST_PF      as X86EflagsTestPf
    , X86_EFLAGS_TEST_CF      as X86EflagsTestCf
    , X86_EFLAGS_TEST_NT      as X86EflagsTestNt
    , X86_EFLAGS_TEST_DF      as X86EflagsTestDf
    , X86_EFLAGS_UNDEFINED_OF as X86EflagsUndefinedOf
    , X86_EFLAGS_UNDEFINED_SF as X86EflagsUndefinedSf
    , X86_EFLAGS_UNDEFINED_ZF as X86EflagsUndefinedZf
    , X86_EFLAGS_UNDEFINED_PF as X86EflagsUndefinedPf
    , X86_EFLAGS_UNDEFINED_AF as X86EflagsUndefinedAf
    , X86_EFLAGS_UNDEFINED_CF as X86EflagsUndefinedCf
    , X86_EFLAGS_RESET_RF     as X86EflagsResetRf
    , X86_EFLAGS_TEST_RF      as X86EflagsTestRf
    , X86_EFLAGS_TEST_IF      as X86EflagsTestIf
    , X86_EFLAGS_TEST_TF      as X86EflagsTestTf
    , X86_EFLAGS_TEST_AF      as X86EflagsTestAf
    , X86_EFLAGS_RESET_ZF     as X86EflagsResetZf
    , X86_EFLAGS_SET_OF       as X86EflagsSetOf
    , X86_EFLAGS_SET_SF       as X86EflagsSetSf
    , X86_EFLAGS_SET_ZF       as X86EflagsSetZf
    , X86_EFLAGS_SET_AF       as X86EflagsSetAf
    , X86_EFLAGS_SET_PF       as X86EflagsSetPf
    , X86_EFLAGS_RESET_0F     as X86EflagsReset0F
    , X86_EFLAGS_RESET_AC     as X86EflagsResetAc
    }
    deriving (Show, Eq, Bounded)#}

-- | x86 fpu flags
{#enum define X86FpuFlags
    { X86_FPU_FLAGS_MODIFY_C0    as X86FpuFlagsModifyC0
    , X86_FPU_FLAGS_MODIFY_C1    as X86FpuFlagsModifyC1
    , X86_FPU_FLAGS_MODIFY_C2    as X86FpuFlagsModifyC2
    , X86_FPU_FLAGS_MODIFY_C3    as X86FpuFlagsModifyC3
    , X86_FPU_FLAGS_RESET_C0     as X86FpuFlagsResetC0
    , X86_FPU_FLAGS_RESET_C1     as X86FpuFlagsResetC1
    , X86_FPU_FLAGS_RESET_C2     as X86FpuFlagsResetC2
    , X86_FPU_FLAGS_RESET_C3     as X86FpuFlagsResetC3
    , X86_FPU_FLAGS_SET_C0       as X86FpuFlagsSetC0
    , X86_FPU_FLAGS_SET_C1       as X86FpuFlagsSetC1
    , X86_FPU_FLAGS_SET_C2       as X86FpuFlagsSetC2
    , X86_FPU_FLAGS_SET_C3       as X86FpuFlagsSetC3
    , X86_FPU_FLAGS_UNDEFINED_C0 as X86FpuFlagsUndefinedC0
    , X86_FPU_FLAGS_UNDEFINED_C1 as X86FpuFlagsUndefinedC1
    , X86_FPU_FLAGS_UNDEFINED_C2 as X86FpuFlagsUndefinedC2
    , X86_FPU_FLAGS_UNDEFINED_C3 as X86FpuFlagsUndefinedC3
    , X86_FPU_FLAGS_TEST_C0      as X86FpuFlagsTestC0
    , X86_FPU_FLAGS_TEST_C1      as X86FpuFlagsTestC1
    , X86_FPU_FLAGS_TEST_C2      as X86FpuFlagsTestC2
    , X86_FPU_FLAGS_TEST_C3      as X86FpuFlagsTestC3
    }
    deriving (Show, Eq, Bounded)#}

-- | operand type for instruction's operands
{#enum x86_op_type as X86OpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | XOP code condition
{#enum x86_xop_cc as X86XopCc {underscoreToCase}
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
    sizeOf _ = {#sizeof cs_x86_op#}
    alignment _ = {#alignof cs_x86_op#}
    peek p = CsX86Op
        <$> do
            t <- fromIntegral <$> {#get cs_x86_op->type#} p
            case toEnum t of
              X86OpReg -> (Reg . toEnum . fromIntegral) <$> {#get cs_x86_op->reg#} p
              X86OpImm -> (Imm . fromIntegral) <$> {#get cs_x86_op->imm#} p
              X86OpMem -> Mem <$> (peek (plusPtr p {#offsetof cs_x86_op->mem#}))
              _ -> return Undefined
        <*> (fromIntegral <$> {#get cs_x86_op->size#}  p)
        <*> (fromIntegral <$> {#get cs_x86_op->access#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86_op->avx_bcast#} p)
        <*> (toBool <$> (peekByteOff p {#offsetof cs_x86_op->avx_zero_opmask#} :: IO Word8))
    poke p (CsX86Op val s a ab az) = do
        let regP = plusPtr p {#offsetof cs_x86_op->reg#}
            immP = plusPtr p {#offsetof cs_x86_op->imm#}
            memP = plusPtr p {#offsetof cs_x86_op->mem#}
            setType = {#set cs_x86_op->type#} p . fromIntegral . fromEnum
        case val of
          Reg r -> do
              poke regP (fromIntegral $ fromEnum r :: CInt)
              setType X86OpReg
          Imm i -> do
              poke immP i
              setType X86OpImm
          Mem m -> do
              poke memP m
              setType X86OpMem
          Undefined -> setType X86OpInvalid
        {#set cs_x86_op->size#} p (fromIntegral s)
        {#set cs_x86_op->access#} p (fromIntegral a)
        {#set cs_x86_op->access#} p (fromIntegral $ fromEnum ab)
        {#set cs_x86_op->avx_zero_opmask#} p az

data CsX86Encoding = CsX86Encoding
    { modRMOffset :: Word8
    , dispOffset :: Word8
    , dispSize :: Word8
    , immOffset :: Word8
    , immSize :: Word8
    } deriving (Show, Eq)

instance Storable CsX86Encoding where
    sizeOf _ = {#sizeof cs_x86_encoding#}
    alignment _ = {#alignof cs_x86_encoding#}
    peek p = CsX86Encoding
        <$> (fromIntegral <$> {#get cs_x86_encoding->modrm_offset#} p)
        <*> ((fromIntegral <$> {#get cs_x86_encoding->disp_offset#} p))
        <*> (fromIntegral <$> {#get cs_x86_encoding->disp_size#} p)
        <*> (fromIntegral <$> {#get cs_x86_encoding->imm_offset#} p)
        <*> (fromIntegral <$> {#get cs_x86_encoding->imm_size#} p)
    poke p (CsX86Encoding moff doff dsize ioff isize) = do
        {#set cs_x86_encoding->modrm_offset#} p (fromIntegral moff)
        {#set cs_x86_encoding->disp_size#} p (fromIntegral doff)
        {#set cs_x86_encoding->disp_size#} p (fromIntegral dsize)
        {#set cs_x86_encoding->imm_offset#} p (fromIntegral ioff)
        {#set cs_x86_encoding->imm_size#} p (fromIntegral isize)

data CsX86Flags
    = EFlags Word64
    | FpuFlags Word64

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
    , disp :: Maybe Int64 -- ^ optional displacement value
    , sibIndex :: X86Reg -- ^ SIB index register, possibly irrelevant
    , sibScale :: Int8 -- ^ SIB scale, possibly irrelevant
    , sibBase :: X86Reg -- ^ SIB base register, possibly irrelevant
    , xopCc :: X86XopCc -- ^ XOP code instruction
    , sseCc :: X86SseCc -- ^ SSE condition code
    , avxCc :: X86AvxCc -- ^ AVX condition code
    , avxSae :: Bool -- ^ AXV Supress all Exception
    , avxRm :: X86AvxRm -- ^ AVX static rounding mode
    , flags :: Word64 -- ^ flags updated by this instruction
    , operands :: [CsX86Op] -- ^ operand list for this instruction, *MUST*
                            -- have <= 8 elements, else you'll get a runtime
                            -- error when you (implicitly) try to write it to
                            -- memory via it's Storable instance
    , encoding :: CsX86Encoding
    } deriving (Show, Eq)

instance Storable CsX86 where
    sizeOf _ = {#sizeof cs_x86#}
    alignment _ = {#alignof cs_x86#}
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
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->xop_cc#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->sse_cc#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->avx_cc#} p)
        <*> (toBool <$> (peekByteOff p {#offsetof cs_x86->avx_sae#} :: IO Word8))
        <*> ((toEnum . fromIntegral) <$> {#get cs_x86->avx_rm#} p)
        <*> (fromIntegral <$> {#get cs_x86->eflags#} p)
        <*> do num <- (fromIntegral <$> {#get cs_x86->op_count#} p)
               let ptr = plusPtr p {#offsetof cs_x86->operands#}
               peekArray num ptr
        <*> (peekByteOff p {#offsetof cs_x86->encoding#})
        -- <*> ({#get cs_x86->encoding#} p)
    poke p (CsX86 (p0, p1, p2, p3) op r a m s d sI sS sB xC sC aC aS aR f o e) =
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
        {#set cs_x86->xop_cc#} p (fromIntegral $ fromEnum xC)
        {#set cs_x86->sse_cc#} p (fromIntegral $ fromEnum sC)
        {#set cs_x86->avx_cc#} p (fromIntegral $ fromEnum aC)
        {#set cs_x86->avx_sae#} p aS
        {#set cs_x86->avx_rm#} p (fromIntegral $ fromEnum aR)
        {#set cs_x86->eflags#} p (fromIntegral f)
        {#set cs_x86->op_count#} p (fromIntegral $ length o)
        pokeByteOff p {#offsetof cs_x86->encoding#} e
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_x86->operands#}) o

-- | x86 instructions
{#enum x86_insn as X86Insn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | x86 instruction groups
{#enum x86_insn_group as X86InsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
