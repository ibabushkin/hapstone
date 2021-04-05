module Main where

import           Control.Monad
import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone
import           Hapstone.Internal.Mips        as Mips

mips_code =
  [ 0x0C, 0x10, 0x00, 0x97, 0x00, 0x00, 0x00, 0x00, 0x24
  , 0x02, 0x00, 0x0c, 0x8f, 0xa2, 0x00, 0x00, 0x34, 0x21
  , 0x34, 0x56
  ]

mips_code2 =
  [ 0x56, 0x34, 0x21, 0x34, 0xc2, 0x17, 0x01, 0x00
  ]

mips_32r6m =
  [ 0x00, 0x07, 0x00, 0x07, 0x00, 0x11, 0x93, 0x7c, 0x01
  , 0x8c, 0x8b, 0x7c, 0x00, 0xc7, 0x48, 0xd0
  ]

mips_32r6 =
  [ 0xec, 0x80, 0x00, 0x19, 0x7c, 0x43, 0x22, 0xa0
  ]

print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = do
  putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  Just detail <- pure $ Capstone.detail insn
  Just (Mips arch) <- pure $ archInfo detail
  printArchInsnInfo arch
  putStrLn ""
 where
  m = mnemonic insn
  o = opStr insn
  a = (showHex $ address insn) ""

  printArchInsnInfo (CsMips operands) = do
    putStrLn ("\topcount: " ++ ((show . length) operands))
    mapM_ printOperandDetail $ zip [0..] operands
   where
    printOperandDetail :: (Int, Mips.CsMipsOp) -> IO ()
    printOperandDetail (i, op) =
      case op of
        Reg reg -> do
          let Just reg_name = Capstone.csRegName handle reg
          putStrLn $ printf "\t\toperands[%u].type: REG = %s" i reg_name
        Imm imm ->
          putStrLn $ printf "\t\toperands[%u].type: IMM = 0x%x" i imm
        Mem mem -> do
          putStrLn $ printf "\t\toperands[%u].type: MEM" i
          when (base mem /= MipsRegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle (base mem)
              putStrLn $ printf "\t\t\toperands[%u].mem.base: REG = %s" i reg_name
          when (disp mem /= 0)
            $ putStrLn $ printf "\t\t\toperands[%u].mem.disp: 0x%x" i (disp mem)

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchMips
                   , modes                    = [CsModeBigEndian, CsModeMips32]
                   , buffer                   = mips_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "MIPS-32 (Big-endian)"
    )
  , ( Disassembler { arch                     = Capstone.CsArchMips
                   , modes                    = [CsModeLittleEndian, CsModeMips64]
                   , buffer                   = mips_code2
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "MIPS-64-EL (Little-endian)"
    )
  , ( Disassembler { arch                     = Capstone.CsArchMips
                   , modes                    = [CsModeBigEndian, CsModeMicro, CsModeMips32r6]
                   , buffer                   = mips_32r6m
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "MIPS-32R6 | Micro (Big-endian)"
    )
  , ( Disassembler { arch                     = Capstone.CsArchMips
                   , modes                    = [CsModeBigEndian, CsModeMips32r6]
                   , buffer                   = mips_32r6
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "MIPS-32R6 (Big-endian)"
    )
  ]

main :: IO ()
main = do
  mapM test_disasm all_tests
  pure ()
 where
  test_disasm (dis, platform) = do
    putStrLn $ replicate 16 '*'
    putStrLn $ "Platform: " ++ platform
    putStrLn $ "Code: " ++ to_hex (buffer dis)
    putStrLn "Disasm:"
    disasmIO $ dis

  to_hex code = unwords (map (printf "0x%02X") code)
