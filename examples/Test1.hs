{-|
Description : X86 Example using Hapstone
Copyright   : (c) Garret Wassermann, 2017
License     : BSD3
Maintainer  : Garret Wassermann <gwasser@gmail.com>
Stability   : experimental

This is example code that shows how to use the Hapstone bindings,
based on an X86 example provided with the python bindings to Capstone.
For more information, see http://www.capstone-engine.org/lang_python.html.
-}
module Main 
where

import Data.Word
import Numeric (showHex)

import Hapstone.Capstone
import Hapstone.Internal.Capstone as Capstone

-- use example from Capstone: http://www.capstone-engine.org/lang_python.html
intel_asm_buf = [0x55, 0x48, 0x8b, 0x05, 0xb8, 0x13, 0x00, 0x00] :: [Word8]

myAction :: Capstone.Csh -> Capstone.CsInsn -> IO ()
myAction handle insn = putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
    where m = mnemonic insn
          o = opStr insn
          a = (showHex $ address insn) ""

myDisasm = Disassembler { 
    arch = Capstone.CsArchX86 -- ^ Options: CsArchArm, CsArchArm64, CsArchMips, CsArchX86, CsArchPpc, CsArchSparc, CsArchSysz, CsArchXcore
    , modes = [Capstone.CsMode64] -- ^ Modes (some may be combined by adding to the list): CsModeLittleEndian, CsModeArm, CsMode16 (16-bit x86), CsMode32 (32-bit x86), CsMode64 (64-bit x86-64/amd64 or PPC), CsModeThumb, CsModeMclass, CsModeV8 (ARMv8 A32), CsModeMicro, CsModeMips3, CsModeMips32r6, CsModeMipsGp64, CsModeV9 (SparcV9 mode), CsModeBigEndian, CsModeMips32, CsModeMips64
    , buffer = intel_asm_buf -- ^ buffer to disassemble, as [Word8]
    , addr = 0x1000 -- ^ address of first byte in the buffer, as Word64
    , num = 0 -- ^ number of instructions to disassemble (0 for maximum)
    , Hapstone.Capstone.detail = True -- ^ include detailed information? True/False
    , skip = Just (defaultSkipdataStruct) -- ^ setup SKIPDATA options, as Maybe CsSkipdataStruct
    , action = myAction -- ^ action to run on each instruction, a function with signature Csh -> CsInsn -> IO a; default is defaultAction
    }

-- disasmIO has signature Disassembler a -> IO (Either CsErr [(CsInsn, a)])
main = disasmIO myDisasm

