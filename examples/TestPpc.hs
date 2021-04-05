module Main where

import           Control.Monad
import           Data.Bits                      ( (.&.) )
import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone
import           Hapstone.Internal.Ppc         as Ppc


ppc_code =
  [ 0x43, 0x20, 0x0c, 0x07, 0x41, 0x56, 0xff, 0x17, 0x80
  , 0x20, 0x00, 0x00, 0x80, 0x3f, 0x00, 0x00, 0x10, 0x43
  , 0x23, 0x0e, 0xd0, 0x44, 0x00, 0x80, 0x4c, 0x43, 0x22
  , 0x02, 0x2d, 0x03, 0x00, 0x80, 0x7c, 0x43, 0x20, 0x14
  , 0x7c, 0x43, 0x20, 0x93, 0x4f, 0x20, 0x00, 0x21, 0x4c
  , 0xc8, 0x00, 0x21, 0x40, 0x82, 0x00, 0x14
  ]

ppc_code2 =
  [ 0x10, 0x60, 0x2a, 0x10, 0x10, 0x64, 0x28, 0x88, 0x7c
  , 0x4a, 0x5d, 0x0f
  ]

print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = do
  putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  Just detail <- pure $ Capstone.detail insn
  Just (Ppc arch) <- pure $ archInfo detail
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
    let operands = Ppc.operands arch
    mapM_ printOperandDetail $ zip [0..] operands
    when (bc arch /= PpcBcInvalid)
      $ putStrLn $ printf "\tBranch code: %s" (show $ bc arch)
    when (bh arch /= PpcBhInvalid)
      $ putStrLn $ printf "\tBranch hint: %s" (show $ bh arch)
    when (updateCr0 arch)
      $ putStrLn "\tUpdate-CR0: True"
   where
    printOperandDetail :: (Int, Ppc.CsPpcOp) -> IO ()
    printOperandDetail (i, op) = do
      case op of
        Imm imm -> do
          putStrLn $ printf "\t\toperands[%u].type: IMM = #%d" i imm
        Reg reg -> do
          let Just reg_name = Capstone.csRegName handle reg
          putStrLn $ printf "\t\toperands[%u].type: REG = %s" i reg_name
        Mem mem -> do
          putStrLn $ printf "\t\toperands[%u].type: MEM" i
          when (base mem /= PpcRegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle $ base mem
              putStrLn $ printf "\t\t\toperands[%u].mem.base: REG = %s" i reg_name
          when (disp mem /= 0)
            $ putStrLn $ printf "\t\t\toperands[%u].mem.disp: 0x%x" i (disp mem)
        Crx crx -> do
          putStrLn $ printf "\t\toperands[%u].type: CRX" i
          putStrLn $ printf "\t\t\toperands[%u].crx.scale: = %u" i (scale crx)
          when (reg crx /= PpcRegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle $ reg crx
              putStrLn $ printf "\t\t\toperands[%u].crx.reg: REG = %s" i reg_name
          putStrLn $ printf "\t\t\toperands[%u].crx.cond: %s" i (show $ cond crx)
        _ -> pure ()

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchPpc
                   , modes                    = [CsModeBigEndian]
                   , buffer                   = ppc_code
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "PPC-64"
    )
  , ( Disassembler { arch                     = Capstone.CsArchPpc
                   , modes                    = [CsModeBigEndian, CsModeQpx]
                   , buffer                   = ppc_code2
                   , addr                     = 0x1000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "PPC-64 + QPX"
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
