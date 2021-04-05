module Main where

import           Control.Monad
import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone
import           Hapstone.Internal.Arm64       as Arm64

arm_code =
  [ 0x09, 0x00, 0x38, 0xd5, 0xbf, 0x40, 0x00, 0xd5, 0x0c
  , 0x05, 0x13, 0xd5, 0x20, 0x50, 0x02, 0x0e, 0x20, 0xe4
  , 0x3d, 0x0f, 0x00, 0x18, 0xa0, 0x5f, 0xa2, 0x00, 0xae
  , 0x9e, 0x9f, 0x37, 0x03, 0xd5, 0xbf, 0x33, 0x03, 0xd5
  , 0xdf, 0x3f, 0x03, 0xd5, 0x21, 0x7c, 0x02, 0x9b, 0x21
  , 0x7c, 0x00, 0x53, 0x00, 0x40, 0x21, 0x4b, 0xe1, 0x0b
  , 0x40, 0xb9, 0x20, 0x04, 0x81, 0xda, 0x20, 0x08, 0x02
  , 0x8b, 0x10, 0x5b, 0xe8, 0x3c
  ]

print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = do
  putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  Just detail <- pure $ Capstone.detail insn
  Just (Arm64 arch) <- pure $ archInfo detail
  printArchInsnInfo arch
  putStrLn ""
 where
  m = mnemonic insn
  o = opStr insn
  a = (showHex $ address insn) ""

  printArchInsnInfo arch = do
    let operands = Arm64.operands arch
    when (length operands > 0)
      $ putStrLn ("\topcount: " ++ ((show . length) operands))
    mapM_ printOperandDetail $ zip [0..] operands
    when (updateFlags arch)
      $ putStrLn "\tUpdate-flags: True"
    when (writeback arch)
      $ putStrLn "\tWrite-back: True"
    case cc arch of
      Arm64CcAl -> pure ()
      Arm64CcInvalid -> pure ()
      _ -> putStrLn $ printf "\tCode-condition: %s" (show $ cc arch)

   where
    printOperandDetail :: (Int, Arm64.CsArm64Op) -> IO ()
    printOperandDetail (i, op) = do
      case value op of
        Reg reg -> do
          let Just reg_name = Capstone.csRegName handle reg
          putStrLn (printf "\t\toperands[%u].type: REG = %s" i reg_name)
        Imm imm ->
          putStrLn (printf "\t\toperands[%u].type: IMM = 0x%x" i imm)
        CImm imm ->
          putStrLn (printf "\t\toperands[%u].type: C-IMM = %u" i imm)
        Fp fp ->
          putStrLn (printf "\t\toperands[%u].type: FP = %f" i fp)
        Mem mem -> do
          let base_ = base mem
              index_ = index mem
              disp_ = disp mem
          putStrLn (printf "\t\toperands[%u].type: MEM" i)
          when (base_ /= Arm64RegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle base_
              putStrLn (printf "\t\t\toperands[%u].mem.base: REG = %s" i reg_name)
          when (index_ /= Arm64RegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle index_
              putStrLn (printf "\t\t\toperands[%u].mem.index: REG = %s" i reg_name)
          when (disp_ /= 0)
            $ putStrLn (printf "\t\t\toperands[%u].mem.disp: 0x%x" i disp_)
        Pstate state ->
          putStrLn (printf "\t\toperands[%u].type: PSTATE = %s" i (show state))
        Sys sys ->
          putStrLn (printf "\t\toperands[%u].type: SYS = 0x%x" i sys)
        Prefetch prefetch ->
          putStrLn (printf "\t\t\toperands[%u].type: PREFETCH = %s" i (show prefetch))
        Barrier barrier ->
          putStrLn (printf "\t\toperands[%u].type: BARRIER = %s" i (show barrier))
        _ -> pure ()

      let shift_ = shift op
          ext_ = ext op
          vess_ = vess op
          vas_ = vas op
          vector_index_ = vectorIndex op

      when ((fst shift_ /= Arm64SftInvalid) && (snd shift_ /= 0))
        $ putStrLn (printf "\t\t\tShift: type = %s, value = %u" (show $ fst shift_) (snd shift_))
      when (ext_ /= Arm64ExtInvalid)
        $ putStrLn (printf "\t\t\tExt: %s" (show ext_))
      when (vas_ /= Arm64VasInvalid)
        $ putStrLn (printf "\t\t\tVector Arrangement Specifier: %s" (show vas_))
      when (vess_ /= Arm64VessInvalid)
        $ putStrLn (printf "\t\t\tVector Element Size Specifier: %s" (show vess_))
      when (vector_index_ /= -1)
        $ putStrLn (printf "\t\t\tVector Index: %u" vector_index_)

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchArm64
                   , modes                    = [Capstone.CsModeArm]
                   , buffer                   = arm_code
                   , addr                     = 0x2c
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "ARM-64"
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
