{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Capstone
Description : capstone's API header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, 2016
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains capstone's public API, with the necessary datatypes and
functions, and some boilerplate to make it usable. Thus, it exposes an IO-based
interface to capstone, which is a rough 1:1 translation of the capstone C
header to Haskell. Obviously, it isn't very ideomatic to use, so a higher-level
API is present in "Hapstone.Capstone". The approach there is to wrap all
necessary cleanup and initialization and expose an ideomatic (but heavily
abstracted) interface to capstone.

This module, on the other hand, is intended to be used when performance is more
critical or greater versatility is needed. This means that the abstractions
introduced in the C version of the library are still present, but their use has
been restricted to provide more reasonable levels of safety.
-}
module Hapstone.Internal.Capstone 
    ( -- * Datatypes
      Csh
    , CsArch(..)
    , CsSupport(..)
    , CsMode(..)
    , CsOption(..)
    , CsOptionState(..)
    , CsOperand(..)
    , CsGroup(..)
      -- * Skipdata setup
      -- $skipdata
    , CsSkipdataCallback
    , CsSkipdataStruct(..)
    , csSetSkipdata
      -- * Instruction representation
    , ArchInfo(..)
      -- $instructions
    , CsDetail(..)
    , peekDetail
    , CsInsn(..)
    , peekArch
    , peekArrayArch
      -- * Capstone API
    , csInsnOffset
    , CsErr(..)
    , csSupport
    , csOpen
    , csClose
    , csOption
    , csErrno
    , csStrerror
    , csDisasm
    , csDisasmIter
    , csFree
    , csMalloc
    , csRegName
    , csInsnName
    , csGroupName
    , csInsnGroup
    , csRegRead
    , csRegWrite
    , csOpCount
    , csOpIndex
    ) where

#include <capstone/capstone.h>

{#context lib = "capstone"#}

import Control.Monad (join, (>=>))

import Foreign
import Foreign.C.Types
import Foreign.C.String ( CString, peekCString
                        , newCString, castCharToCChar, castCCharToChar)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr

import Hapstone.Internal.Util
import qualified Hapstone.Internal.Arm64 as Arm64
import qualified Hapstone.Internal.Arm as Arm
import qualified Hapstone.Internal.Mips as Mips
import qualified Hapstone.Internal.Ppc as Ppc
import qualified Hapstone.Internal.Sparc as Sparc
import qualified Hapstone.Internal.SystemZ as SystemZ
import qualified Hapstone.Internal.X86 as X86
import qualified Hapstone.Internal.XCore as XCore

import System.IO.Unsafe (unsafePerformIO)

-- | capstone's weird^M^M^M^M^Mopaque handle type
type Csh = CSize
{#typedef csh Csh#}

-- | supported architectures
{#enum cs_arch as CsArch {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | support constants
{#enum define CsSupport
    { CS_SUPPORT_DIET as CsSupportDiet
    , CS_SUPPORT_X86_REDUCE as CsSupportX86Reduce}
    deriving (Show, Eq, Bounded)#}

-- | working modes
{#enum cs_mode as CsMode {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- TODO: we will skip user defined dynamic memory routines for now

-- TODO: add cs_opt_mnem struct (yay for naming)

-- | options are, interestingly, represented by different types: an option
{#enum cs_opt_type as CsOption {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | ... and a state of an option
{#enum cs_opt_value as CsOptionState {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | arch-uniting operand type
{#enum cs_op_type as CsOperand {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- TODO: find a place for cs_ac_type

-- | arch-uniting instruction group type
{#enum cs_group_type as CsGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- $skipdata
-- SKIPDATA is an option supported by the capstone disassembly engine, that
-- allows to skip data which can't be disassembled and to represent it in form
-- of pseudo-instructions. The types and functions given here attempt to mirror
-- capstone's setup of this option, and a more high-level interface is
-- available in "Hapstone.Capstone".

-- | callback type for user-defined SKIPDATA work
type CsSkipdataCallback =
    FunPtr (Ptr Word8 -> CSize -> CSize -> Ptr () -> IO CSize)

-- | user-defined SKIPDATA setup
data CsSkipdataStruct = CsSkipdataStruct String CsSkipdataCallback (Ptr ())
    deriving (Show, Eq)

instance Storable CsSkipdataStruct where
    sizeOf _ = {#sizeof cs_opt_skipdata#}
    alignment _ = {#alignof cs_opt_skipdata#}
    peek p = CsSkipdataStruct
        <$> (peekCString =<< {#get cs_opt_skipdata->mnemonic#} p)
        <*> (castFunPtr <$> {#get cs_opt_skipdata->callback#} p)
        <*> {#get cs_opt_skipdata->user_data#} p
    poke p (CsSkipdataStruct s c d) = do
        newCString s >>= {#set cs_opt_skipdata->mnemonic#} p
        {#set cs_opt_skipdata->callback#} p (castFunPtr c)
        {#set cs_opt_skipdata->user_data#} p d

-- | safely set SKIPDATA options (reset on Nothing)
csSetSkipdata :: Csh -> Maybe CsSkipdataStruct -> IO CsErr
csSetSkipdata h Nothing = csOption h CsOptSkipdata CsOptOff
csSetSkipdata h (Just s) = do
    csOption h CsOptSkipdata CsOptOn
    with s (csOption h CsOptSkipdataSetup . fromIntegral . ptrToWordPtr)

-- | architecture specific information
data ArchInfo
    = X86 X86.CsX86 -- ^ x86 architecture
    | Arm64 Arm64.CsArm64 -- ^ ARM64 architecture
    | Arm Arm.CsArm -- ^ ARM architecture
    | Mips Mips.CsMips -- ^ MIPS architecture
    | Ppc Ppc.CsPpc -- ^ PPC architecture
    | Sparc Sparc.CsSparc -- ^ SPARC architecture
    | SysZ SystemZ.CsSysZ -- ^ SystemZ architecture
    | XCore XCore.CsXCore -- ^ XCore architecture
    deriving (Show, Eq)

-- | instruction information
data CsDetail = CsDetail
    { regsRead :: [Word8] -- ^ registers read by this instruction
    , regsWrite :: [Word8] -- ^ registers written by this instruction
    , groups :: [Word8] -- ^ instruction groups this instruction belongs to
    , archInfo :: Maybe ArchInfo -- ^ (optional) architecture-specific info
    } deriving (Show, Eq)

-- $instructions
-- The union holding architecture-specific info is not tagged. Thus, we have
-- no way to determine what kind of data is stored in it without resorting to
-- some kind of context lookup, as the corresponding C code would do. Thus, the
-- peek implementation does not get architecture information, use peekDetail
-- for that.

instance Storable CsDetail where
    sizeOf _ = 1560
    alignment _ = 8
    peek p = CsDetail
        <$> do num <- fromIntegral <$> {#get cs_detail->regs_read_count#} p
               let ptr = plusPtr p {#offsetof cs_detail.regs_read#}
               peekArray num ptr
        <*> do num <- fromIntegral <$> {#get cs_detail->regs_write_count#} p
               let ptr = plusPtr p {#offsetof cs_detail.regs_write#}
               peekArray num ptr
        <*> do num <- fromIntegral <$> {#get cs_detail->groups_count#} p
               let ptr = plusPtr p {#offsetof cs_detail.groups#}
               peekArray num ptr
        <*> pure Nothing
    poke p (CsDetail rR rW g a) = do
        {#set cs_detail->regs_read_count#} p (fromIntegral $ length rR)
        if length rR > 12
           then error "regs_read overflew 12 bytes"
           else pokeArray (plusPtr p {#offsetof cs_detail.regs_read#}) rR
        {#set cs_detail->regs_write_count#} p (fromIntegral $ length rW)
        if length rW > 20
           then error "regs_write overflew 20 bytes"
           else pokeArray (plusPtr p {#offsetof cs_detail.regs_write#}) rW
        {#set cs_detail->groups_count#} p (fromIntegral $ length g)
        if length g > 8
           then error "groups overflew 8 bytes"
           else pokeArray (plusPtr p {#offsetof cs_detail.groups#}) g
        let bP = plusPtr p ({#offsetof cs_detail.groups_count#} + 1)
        case a of
          Just (X86 x) -> poke bP x
          Just (Arm64 x) -> poke bP x
          Just (Arm x) -> poke bP x
          Just (Mips x) -> poke bP x
          Just (Ppc x) -> poke bP x
          Just (Sparc x) -> poke bP x
          Just (SysZ x) -> poke bP x
          Just (XCore x) -> poke bP x
          Nothing -> return ()

-- | an arch-sensitive peek for cs_detail
peekDetail :: CsArch -> Ptr CsDetail -> IO CsDetail
peekDetail arch p = do
    detail <- peek p
    let bP = plusPtr p 48
    aI <- case arch of
            CsArchX86 -> X86 <$> peek bP
            CsArchArm64 -> Arm64 <$> peek bP
            CsArchArm -> Arm <$> peek bP
            CsArchMips -> Mips <$> peek bP
            CsArchPpc -> Ppc <$> peek bP
            CsArchSparc -> Sparc <$> peek bP
            CsArchSysz -> SysZ <$> peek bP
            CsArchXcore -> XCore <$> peek bP
    return detail { archInfo = Just aI }

-- | instructions
data CsInsn = CsInsn
    { insnId :: Word32 -- ^ instruction ID
    , address :: Word64 -- ^ instruction's address in memory
    , bytes :: [Word8] -- ^ raw byte representation
    , mnemonic :: String -- ^ instruction's mnemonic
    , opStr :: String -- ^ operands
    , detail :: Maybe CsDetail -- ^ (optional) detailed info
    } deriving (Show, Eq)

-- The untagged-union-problem propagates here as well
instance Storable CsInsn where
    sizeOf _ = {#sizeof cs_insn#}
    alignment _ = {#alignof cs_insn#}
    peek p = CsInsn
        <$> (fromIntegral <$> {#get cs_insn->id#} p)
        <*> (fromIntegral <$> {#get cs_insn->address#} p)
        <*> do num <- fromIntegral <$> {#get cs_insn->size#} p
               let ptr = plusPtr p {#offsetof cs_insn->bytes#}
               peekArray num ptr
        <*> ((map castCCharToChar . takeWhile (/=0)) <$>
            peekArray 32 (plusPtr p {#offsetof cs_insn->mnemonic#}))
        <*> ((map castCCharToChar . takeWhile (/=0)) <$>
            peekArray 160 (plusPtr p {#offsetof cs_insn->op_str#}))
        <*> return Nothing
        --(castPtr <$> {#get cs_insn->detail#} p >>= peekMaybe)
    poke p (CsInsn i a b m o d) = do
        {#set cs_insn->id#} p (fromIntegral i)
        {#set cs_insn->address#} p (fromIntegral a)
        {#set cs_insn->size#} p (fromIntegral $ length b)
        if length b > 16
           then error "bytes overflew 16 bytes"
           else pokeArray (plusPtr p {#offsetof cs_insn.bytes#}) b
        if length m >= 32
           then error "mnemonic overflew 32 bytes"
           else do pokeArray (plusPtr p {#offsetof cs_insn.mnemonic#})
                             (map castCharToCChar m)
                   poke (plusPtr p ({#offsetof cs_insn.mnemonic#} + length m))
                        (0 :: Word8)
        if length o >= 160
           then error "op_str overflew 160 bytes"
           else do pokeArray (plusPtr p {#offsetof cs_insn.op_str#})
                             (map castCharToCChar o)
                   poke (plusPtr p ({#offsetof cs_insn.op_str#} + length o))
                        (0 :: Word8)
        case d of
          Nothing -> {#set cs_insn->detail#} p nullPtr
          Just d' ->  do csDetailPtr <- malloc
                         poke csDetailPtr d'
                         {#set cs_insn->detail#} p (castPtr csDetailPtr)

-- | an arch-sensitive peek for cs_insn 
peekArch :: CsArch -> Ptr CsInsn -> IO CsInsn
peekArch arch p = do
    insn <- peek p
    bP <- castPtr <$> {#get cs_insn->detail#} p
    if bP /= nullPtr
       then do
           det <- peekDetail arch bP
           return insn { detail = Just det }
       else return insn

-- | an arch-sensitive peekElemOff for cs_insn
peekElemOffArch :: CsArch -> Ptr CsInsn -> Int -> IO CsInsn
peekElemOffArch arch ptr off =
    peekArch arch (plusPtr ptr (off * sizeOf (undefined :: CsInsn)))

-- | an arch-sensitive peekArray for cs_insn
peekArrayArch :: CsArch -> Int -> Ptr CsInsn -> IO [CsInsn]
peekArrayArch arch num ptr
    | num <= 0 = return []
    | otherwise = f (num-1) []
  where
    f 0 acc = do e <- peekElemOffArch arch ptr 0; return (e:acc)
    f n acc = do e <- peekElemOffArch arch ptr n; f (n-1) (e:acc)

-- | our own port of the CS_INSN_OFFSET macro
csInsnOffset :: Ptr CsInsn -> Int -> Int
csInsnOffset p n = unsafePerformIO $
    (-) <$> getAddr (plusPtr p (n * {#sizeof cs_insn#})) <*> getAddr p
    where getAddr p = fromIntegral <$> {#get cs_insn->address#} p

-- | possible error conditions
{#enum cs_err as CsErr {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

-- | get the library version
{#fun pure cs_version as ^
    {alloca- `Int' peekNum*, alloca- `Int' peekNum*} -> `Int'#}

foreign import ccall "capstone/capstone.h cs_support"
    csSupport' :: CInt -> Bool
-- | get information on supported features
csSupport :: Enum a => a -> Bool
csSupport = csSupport' . fromIntegral . fromEnum

-- | open a new disassembly handle
{#fun cs_open as ^
    {`CsArch', combine `[CsMode]', alloca- `Csh' peek*} -> `CsErr'#}

-- | close a handle obtained by cs_open/'csOpen'
{#fun cs_close as csClose' {id `Ptr Csh'} -> `CsErr'#}
csClose :: Csh -> IO CsErr
csClose = new >=> csClose'

-- | set an option on a handle
{#fun cs_option as ^ `Enum a' =>
    {`Csh', `CsOption', getCULongFromEnum `a'} -> `CsErr'#}

-- | get the last error from a handle
{#fun cs_errno as ^ {`Csh'} -> `CsErr'#}

-- | get the description of an error
{#fun pure cs_strerror as ^ {`CsErr'} -> `String'#}

foreign import ccall "capstone/capstone.h cs_disasm"
    csDisasm' :: Csh -- handle
              -> Ptr CUChar -> CSize -- buffer to disassemble
              -> CULong -- address to start at
              -> CSize -- number of instructins to disassemble
              -> Ptr (Ptr CsInsn) -- where to put the instructions
              -> IO CSize -- number of succesfully disassembled instructions

-- | disassemble a buffer
csDisasm :: CsArch -> Csh -> [Word8] -> Word64 -> Int -> IO [CsInsn]
csDisasm arch handle bytes addr num = do
    array <- newArray $ map fromIntegral bytes
    passedPtr <- malloc :: IO (Ptr (Ptr CsInsn))
    resNum <- fromIntegral <$> csDisasm' handle array
        (fromIntegral $ length bytes) (fromIntegral addr)
        (fromIntegral num) passedPtr
    resPtr <- peek passedPtr
    free passedPtr
    res <- peekArrayArch arch resNum resPtr
    csFree resPtr resNum
    return res

-- | free an instruction struct array
{#fun cs_free as ^ {castPtr `Ptr CsInsn', `Int'} -> `()'#}

-- | allocate space for an instruction structure
{#fun cs_malloc as ^ {`Csh'} -> `Ptr CsInsn' castPtr#}

foreign import ccall "capstone/capstone.h cs_disasm_iter"
    csDisasmIter' :: Csh -- handle
                  -> Ptr (Ptr CUChar) -> Ptr CSize -- buffer description
                  -> Ptr CULong -- address to start at
                  -> Ptr CsInsn -- output buffer
                  -> IO Bool -- success

-- | disassemble one instruction at a time
csDisasmIter :: Csh -> [Word8] -> Word64
             -> IO ([Word8], Word64, Either CsErr CsInsn)
csDisasmIter handle bytes addr = do
    array <- newArray (map fromIntegral bytes) :: IO (Ptr CUChar)
    arrayPtr <- new array
    sizePtr <- new . fromIntegral $ length bytes
    addrPtr <- new $ fromIntegral addr
    insnPtr <- csMalloc handle
    success <- csDisasmIter' handle arrayPtr sizePtr addrPtr insnPtr
    bytes' <- join $
        peekArray <$> (fromIntegral <$> peek sizePtr) <*> peek arrayPtr
    addr' <- peek addrPtr
    free arrayPtr
    free sizePtr
    free addrPtr
    result <- if success
                 then Right <$> peek insnPtr
                 else Left <$> csErrno handle
    return (map fromIntegral bytes', fromIntegral addr', result)

-- | get a register's name as a String
{#fun pure cs_reg_name as csRegName' {`Csh', `Int'} -> `CString'#}
csRegName :: Enum e => Csh -> e -> Maybe String
csRegName h = stringLookup . csRegName' h . fromEnum

-- | get a instruction's name as a String
{#fun pure cs_insn_name as csInsnName' {`Csh', `Int'} -> `CString'#}
csInsnName :: Enum e => Csh -> e -> Maybe String
csInsnName h = stringLookup . csInsnName' h . fromEnum

-- | get a instruction group's name as a String
{#fun pure cs_group_name as csGroupName' {`Csh', `Int'} -> `CString'#}
csGroupName :: Enum e => Csh -> e -> Maybe String
csGroupName h = stringLookup . csGroupName' h . fromEnum

foreign import ccall "capstone/capstone.h cs_insn_group"
    csInsnGroup' :: Csh -> Ptr CsInsn -> IO Bool
-- | check whether an instruction is member of a group
csInsnGroup :: Csh -> CsInsn -> Bool
csInsnGroup h i = unsafePerformIO . withCast i $ csInsnGroup' h

foreign import ccall "capstone/capstone.h cs_reg_read"
    csRegRead' :: Csh -> Ptr CsInsn -> CUInt -> IO Bool
-- | check whether an instruction reads from a register
csRegRead :: Csh -> CsInsn -> Int -> Bool
csRegRead h i =
    unsafePerformIO . withCast i . flip (csRegRead' h) . fromIntegral

foreign import ccall "capstone/capstone.h cs_reg_write"
    csRegWrite' :: Csh -> Ptr CsInsn -> CUInt -> IO Bool
-- | check whether an instruction writes to a register
csRegWrite :: Csh -> CsInsn -> Int -> Bool
csRegWrite h i =
    unsafePerformIO . withCast i . flip (csRegWrite' h) . fromIntegral

-- | return the number of operands of given type an instruction has
{#fun pure cs_op_count as ^
    {`Csh', withCast* `CsInsn', `Int'} -> `Int'#}

-- | return the position of the first operand of given type an instruction has,
-- given an inclusive search range
{#fun pure cs_op_index as ^
    {`Csh', withCast* `CsInsn', `Int', `Int'} -> `Int'#}

-- TODO: bind to cs_regs_access
