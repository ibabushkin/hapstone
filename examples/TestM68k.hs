module Main where

import           Control.Monad
import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone
import           Hapstone.Internal.M68k        as M68k

m68k_code =
  [ 0x4c, 0x00, 0x54, 0x04, 0x48, 0xe7, 0xe0, 0x30, 0x4c
  , 0xdf, 0x0c, 0x07, 0xd4, 0x40, 0x87, 0x5a, 0x4e, 0x71
  , 0x02, 0xb4, 0xc0, 0xde, 0xc0, 0xde, 0x5c, 0x00, 0x1d
  , 0x80, 0x71, 0x12, 0x01, 0x23, 0xf2, 0x3c, 0x44, 0x22
  , 0x40, 0x49, 0x0e, 0x56, 0x54, 0xc5, 0xf2, 0x3c, 0x44
  , 0x00, 0x44, 0x7a, 0x00, 0x00, 0xf2, 0x00, 0x0a, 0x28
  , 0x4e, 0xb9, 0x00, 0x00, 0x00, 0x12, 0x4e, 0x75
  ]


print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = do
  putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  Just detail <- pure $ Capstone.detail insn
  Just (M68k arch) <- pure $ archInfo detail
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
    let operands = M68k.operands arch
    putStrLn ("\topcount: " ++ ((show . length) operands))
    mapM_ printOperandDetail $ zip [0..] operands
   where
    printOperandDetail :: (Int, M68k.CsM68kOp) -> IO ()
    printOperandDetail (i, op) =
      case value op of
        Imm imm ->
          putStrLn $ printf "\t\toperands[%u].type: IMM = 0x%x" i imm
        DImm dimm -> do
          putStrLn $ printf "\t\toperands[%u].type: FP_DOUBLE" i
          putStrLn $ printf "\t\toperands[%u].simm: %lf" i dimm
        SImm simm -> do
          putStrLn $ printf "\t\toperands[%u].type: FP_SINGLE" i
          putStrLn $ printf "\t\toperands[%u].simm: %f" i simm
        Reg reg -> do
          let Just reg_name = Capstone.csRegName handle reg
          putStrLn $ printf "\t\toperands[%u].type: REG = %s" i reg_name
        BrDisp brdisp -> do
          putStrLn $ printf "\t\toperands[%u].br_disp.disp: 0x%x" i (brDisp brdisp)
          putStrLn $ printf "\t\toperands[%u].br_disp.disp_size: %s" i (show $ brSize brdisp)
        Mem mem -> do
          let baseReg_ = baseReg mem
              indexReg_ = indexReg mem
          putStrLn $ printf "\t\toperands[%u].type: MEM" i
          when (baseReg_ /= M68kRegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle baseReg_
              putStrLn $ printf "\t\t\toperands[%u].mem.base: REG = %s" i reg_name
          when (indexReg_ /= M68kRegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle indexReg_
              putStrLn $ printf "\t\t\toperands[%u].mem.index: REG = %s" i reg_name
              if (indexSize mem > 0)
                 then putStrLn $ printf "\t\t\toperands[%u].mem.index: size = l" i
                 else putStrLn $ printf "\t\t\toperands[%u].mem.index: size = w" i
          when (memDisp mem /= 0)
            $ putStrLn $ printf "\t\t\toperands[%u].mem.disp: 0x%x" i (memDisp mem)
          when (scale mem /= 0)
            $ putStrLn $ printf "\t\t\toperands[%u].mem.scale: %d" i (scale mem)
          putStrLn $ printf "\t\taddress mode: %s" (show $ addressMode op)
        RegPair (reg1, reg2) -> pure ()
        _ -> pure ()

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchM68k
                   , modes                    = [CsModeBigEndian, CsModeM68k040]
                   , buffer                   = m68k_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "M68K"
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
