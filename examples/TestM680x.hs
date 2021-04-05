module Main where

import           Control.Monad
import           Data.Bits                      ( (.&.) )
import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone
import           Hapstone.Internal.M680x       as M680x


m6800_code =
  [ 0x01, 0x09, 0x36, 0x64, 0x7f, 0x74, 0x10, 0x00, 0x90
  , 0x10, 0xA4, 0x10, 0xb6, 0x10, 0x00, 0x39
  ]

m6801_code =
  [ 0x04, 0x05, 0x3c, 0x3d, 0x38, 0x93, 0x10, 0xec, 0x10
  , 0xed, 0x10, 0x39
  ]

m6805_code =
  [ 0x04, 0x7f, 0x00, 0x17, 0x22, 0x28, 0x00, 0x2e, 0x00
  , 0x40, 0x42, 0x5a, 0x70, 0x8e, 0x97, 0x9c, 0xa0, 0x15
  , 0xad, 0x00, 0xc3, 0x10, 0x00, 0xda, 0x12, 0x34, 0xe5
  , 0x7f, 0xfe
  ]

m6808_code =
  [ 0x31, 0x22, 0x00, 0x35, 0x22, 0x45, 0x10, 0x00, 0x4b
  , 0x00, 0x51, 0x10, 0x52, 0x5e, 0x22, 0x62, 0x65, 0x12
  , 0x34, 0x72, 0x84, 0x85, 0x86, 0x87, 0x8a, 0x8b, 0x8c
  , 0x94, 0x95, 0xa7, 0x10, 0xaf, 0x10, 0x9e, 0x60, 0x7f
  , 0x9e, 0x6b, 0x7f, 0x00, 0x9e, 0xd6, 0x10, 0x00, 0x9e
  , 0xe6, 0x7f
  ]

hcs08_code =
  [ 0x32, 0x10, 0x00, 0x9e, 0xae, 0x9e, 0xce, 0x7f, 0x9e
  , 0xbe, 0x10, 0x00, 0x9e, 0xfe, 0x7f, 0x3e, 0x10, 0x00
  , 0x9e, 0xf3, 0x7f, 0x96, 0x10, 0x00, 0x9e, 0xff, 0x7f
  , 0x82
  ]

hd6301_code =
  [ 0x6b, 0x10, 0x00, 0x71, 0x10, 0x00, 0x72, 0x10, 0x10
  , 0x39
  ]

m6809_code =
  [ 0x06, 0x10, 0x19, 0x1a, 0x55, 0x1e, 0x01, 0x23, 0xe9
  , 0x31, 0x06, 0x34, 0x55, 0xa6, 0x81, 0xa7, 0x89, 0x7f
  , 0xff, 0xa6, 0x9d, 0x10, 0x00, 0xa7, 0x91, 0xa6, 0x9f
  , 0x10, 0x00, 0x11, 0xac, 0x99, 0x10, 0x00, 0x39, 0xA6
  , 0x07, 0xA6, 0x27, 0xA6, 0x47, 0xA6, 0x67, 0xA6, 0x0F
  , 0xA6, 0x10, 0xA6, 0x80, 0xA6, 0x81, 0xA6, 0x82, 0xA6
  , 0x83, 0xA6, 0x84, 0xA6, 0x85, 0xA6, 0x86, 0xA6, 0x88
  , 0x7F, 0xA6, 0x88, 0x80, 0xA6, 0x89, 0x7F, 0xFF, 0xA6
  , 0x89, 0x80, 0x00, 0xA6, 0x8B, 0xA6, 0x8C, 0x10, 0xA6
  , 0x8D, 0x10, 0x00, 0xA6, 0x91, 0xA6, 0x93, 0xA6, 0x94
  , 0xA6, 0x95, 0xA6, 0x96, 0xA6, 0x98, 0x7F, 0xA6, 0x98
  , 0x80, 0xA6, 0x99, 0x7F, 0xFF, 0xA6, 0x99, 0x80, 0x00
  , 0xA6, 0x9B, 0xA6, 0x9C, 0x10, 0xA6, 0x9D, 0x10, 0x00
  , 0xA6, 0x9F, 0x10, 0x00
  ]

m6811_code =
  [ 0x02, 0x03, 0x12, 0x7f, 0x10, 0x00, 0x13, 0x99, 0x08
  , 0x00, 0x14, 0x7f, 0x02, 0x15, 0x7f, 0x01, 0x1e, 0x7f
  , 0x20, 0x00, 0x8f, 0xcf, 0x18, 0x08, 0x18, 0x30, 0x18
  , 0x3c, 0x18, 0x67, 0x18, 0x8c, 0x10, 0x00, 0x18, 0x8f
  , 0x18, 0xce, 0x10, 0x00, 0x18, 0xff, 0x10, 0x00, 0x1a
  , 0xa3, 0x7f, 0x1a, 0xac, 0x1a, 0xee, 0x7f, 0x1a, 0xef
  , 0x7f, 0xcd, 0xac, 0x7f
  ]

cpu12_code =
  [ 0x00, 0x04, 0x01, 0x00, 0x0c, 0x00, 0x80, 0x0e, 0x00
  , 0x80, 0x00, 0x11, 0x1e, 0x10, 0x00, 0x80, 0x00, 0x3b
  , 0x4a, 0x10, 0x00, 0x04, 0x4b, 0x01, 0x04, 0x4f, 0x7f
  , 0x80, 0x00, 0x8f, 0x10, 0x00, 0xb7, 0x52, 0xb7, 0xb1
  , 0xa6, 0x67, 0xa6, 0xfe, 0xa6, 0xf7, 0x18, 0x02, 0xe2
  , 0x30, 0x39, 0xe2, 0x10, 0x00, 0x18, 0x0c, 0x30, 0x39
  , 0x10, 0x00, 0x18, 0x11, 0x18, 0x12, 0x10, 0x00, 0x18
  , 0x19, 0x00, 0x18, 0x1e, 0x00, 0x18, 0x3e, 0x18, 0x3f
  , 0x00
  ]

hd6309_code =
  [ 0x01, 0x10, 0x10, 0x62, 0x10, 0x10, 0x7b, 0x10, 0x10
  , 0x00, 0xcd, 0x49, 0x96, 0x02, 0xd2, 0x10, 0x30, 0x23
  , 0x10, 0x38, 0x10, 0x3b, 0x10, 0x53, 0x10, 0x5d, 0x11
  , 0x30, 0x43, 0x10, 0x11, 0x37, 0x25, 0x10, 0x11, 0x38
  , 0x12, 0x11, 0x39, 0x23, 0x11, 0x3b, 0x34, 0x11, 0x8e
  , 0x10, 0x00, 0x11, 0xaf, 0x10, 0x11, 0xab, 0x10, 0x11
  , 0xf6, 0x80, 0x00
  ]


print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = do
  putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  Just detail <- pure $ Capstone.detail insn
  Just (M680x arch) <- pure $ archInfo detail
  igroups <- pure $ groups detail
  printArchInsnInfo arch
  when (length igroups /= 0)
    $ putStrLn $ printf "\tgroups_count: %u" (length igroups)
  putStrLn ""
 where
  m = mnemonic insn
  o = opStr insn
  a = (showHex $ address insn) ""

  printArchInsnInfo arch = do
    let operands = M680x.operands arch
    mapM_ printOperandDetail $ zip [0..] operands
   where
    printOperandDetail :: (Int, M680x.CsM680xOp) -> IO ()
    printOperandDetail (i, op) = do
      case value op of
        Imm imm -> do
          putStrLn $ printf "\t\toperands[%u].type: IMMEDIATE = #%d" i imm
        Reg reg -> do
          let f = flags arch
              comment1 = i == 0 && (f .&. 1 == 1)
              comment2 = i == 1 && (f .&. 2 == 1)
              Just reg_name = Capstone.csRegName handle reg
          if (comment1 || comment2)
             then putStrLn $ printf "\t\toperands[%u].type: REGISTER = %s (in mnemonic)" i reg_name
             else putStrLn $ printf "\t\toperands[%u].type: REGISTER = %s" i reg_name
        Idx idx -> do
          let post_pre = if ((idxFlags idx) .&. 4 == 1)
                            then "post"
                            else "pre"
              inc_dec = if (incDec idx > 0)
                            then "increment"
                            else "decrement"
              Just base_reg = Capstone.csRegName handle $ baseReg idx
              Just offset_reg = Capstone.csRegName handle $ offsetReg idx
          if ((idxFlags idx) .&. 1 == 1)
             then putStrLn $ printf "\t\toperands[%u].type: INDEXED INDIRECT" i
             else putStrLn $ printf "\t\toperands[%u].type: INDEXED" i
          when (baseReg idx /= M680xRegInvalid)
            $ putStrLn $ printf "\t\t\tbase register: %s" base_reg
          when (offsetReg idx /= M680xRegInvalid)
            $ putStrLn $ printf "\t\t\tbase register: %s" offset_reg
          when (offsetBits idx /= 0 && offsetReg idx == M680xRegInvalid && incDec idx == 0)
            $ do
              putStrLn $ printf "\t\t\toffset: %u" (idxOffset idx)
              when (baseReg idx == M680xRegPc)
                $ putStrLn $ printf "\t\t\toffset address: 0x%04x" (offsetAddr idx)
              putStrLn $ printf "\t\t\toffset bits: %u" (offsetBits idx)
          when (incDec idx /= 0)
            $ putStrLn $ printf "\t\t\t%s %s: %d" post_pre inc_dec (abs $ incDec idx)
        Rel rel ->
          putStrLn $ printf "\t\toperands[%u].type: RELATIVE = 0x%04x" i (relAddress rel)
        Ext ext ->
          if (indirect ext)
             then putStrLn $ printf "\t\toperands[%u].type: EXTENDED INDIRECT = 0x%04x" i (extAddress ext)
             else putStrLn $ printf "\t\toperands[%u].type: EXTENDED = 0x%04x" i (extAddress ext)
        Addr addr ->
          putStrLn $ printf "\t\toperands[%u].type: DIRECT = 0x%02x" i addr
        Val val ->
          putStrLn $ printf "\t\toperands[%u].type: CONSTANT = %u" i val
        _ -> pure ()
      when (size op /= 0)
        $ putStrLn $ printf "\t\t\tsize: %d" (size op)
      when (access op /= 0)
        $ putStrLn $ printf "\t\t\taccess: %d" (access op)

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6301]
                   , buffer                   = hd6301_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_HD6301"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6309]
                   , buffer                   = hd6309_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_HD6309"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6800]
                   , buffer                   = m6800_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_M6800"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6801]
                   , buffer                   = m6801_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_MX6801"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6805]
                   , buffer                   = m6805_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_M68HC05"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6808]
                   , buffer                   = m6808_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_M68HC08"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6809]
                   , buffer                   = m6809_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_M6809"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680x6811]
                   , buffer                   = m6811_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_M68HC11"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680xCpu12]
                   , buffer                   = cpu12_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_CPU12"
    )
  , ( Disassembler { arch                     = Capstone.CsArchM680x
                   , modes                    = [CsModeM680xHcs08]
                   , buffer                   = hcs08_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M680X_HCS08"
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
