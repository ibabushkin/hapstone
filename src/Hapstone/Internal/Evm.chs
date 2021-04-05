{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Evm
Description : EVM architecture header ported using C2HS + some boilerplate
Copyright   : (c) Khoa Nguyen Anh, 2020
License     : BSD3
Maintainer  : Khoa Nguyen Anh <mail.nganhkhoa@googlemail.com>
Stability   : experimental

This module contains EVM specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.Evm where

#include <capstone/evm.h>

{#context lib = "capstone"#}

import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.Types

import Hapstone.Internal.Util

data CsEvm = CsEvm
    { pop :: Word8
    , push :: Word8
    , fee :: Word32
    } deriving (Show, Eq)

instance Storable CsEvm where
    sizeOf _ = {#sizeof cs_evm#}
    alignment _ = {#alignof cs_evm#}
    peek p = CsEvm
        <$> (fromIntegral <$> {#get cs_evm->pop#} p)
        <*> (fromIntegral <$> {#get cs_evm->push#} p)
        <*> (fromIntegral <$> {#get cs_evm->fee#} p)
    poke p (CsEvm pop push fee) = do
        {#set cs_evm->pop#} p (fromIntegral pop)
        {#set cs_evm->push#} p (fromIntegral push)
        {#set cs_evm->fee#} p (fromIntegral fee)

-- | EVM instruction
{#enum evm_insn as EvmInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
-- | EVM instruction group
{#enum evm_insn_group as EvmInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
