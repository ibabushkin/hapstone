module Main where

import           Control.Monad
import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone
import           Hapstone.Internal.Arm         as Arm

arm_code =
  [ 0x86 , 0x48 , 0x60 , 0xf4 , 0xED , 0xFF , 0xFF , 0xEB
  , 0x04 , 0xe0 , 0x2d , 0xe5 , 0x00 , 0x00 , 0x00 , 0x00
  , 0xe0 , 0x83 , 0x22 , 0xe5 , 0xf1 , 0x02 , 0x03 , 0x0e
  , 0x00 , 0x00 , 0xa0 , 0xe3 , 0x02 , 0x30 , 0xc1 , 0xe7
  , 0x00 , 0x00 , 0x53 , 0xe3 , 0x00 , 0x02 , 0x01 , 0xf1
  , 0x05 , 0x40 , 0xd0 , 0xe8 , 0xf4 , 0x80 , 0x00 , 0x00
  ]

arm_code2 =
  [ 0xd1 , 0xe8 , 0x00 , 0xf0 , 0xf0 , 0x24 , 0x04 , 0x07
  , 0x1f , 0x3c , 0xf2 , 0xc0 , 0x00 , 0x00 , 0x4f , 0xf0
  , 0x00 , 0x01 , 0x46 , 0x6c
  ]

thumb_code =
  [ 0x70 , 0x47 , 0x00 , 0xf0 , 0x10 , 0xe8 , 0xeb , 0x46
  , 0x83 , 0xb0 , 0xc9 , 0x68 , 0x1f , 0xb1 , 0x30 , 0xbf
  , 0xaf , 0xf3 , 0x20 , 0x84 , 0x52 , 0xf8 , 0x23 , 0xf0
  ]

thumb_code2 =
  [ 0x4f , 0xf0 , 0x00 , 0x01 , 0xbd , 0xe8 , 0x00 , 0x88
  , 0xd1 , 0xe8 , 0x00 , 0xf0 , 0x18 , 0xbf , 0xad , 0xbf
  , 0xf3 , 0xff , 0x0b , 0x0c , 0x86 , 0xf3 , 0x00 , 0x89
  , 0x80 , 0xf3 , 0x00 , 0x8c , 0x4f , 0xfa , 0x99 , 0xf6
  , 0xd0 , 0xff , 0xa2 , 0x01
  ]

thumb_mclass = [0xef, 0xf3, 0x02, 0x80]

armv8 =
  [ 0xe0, 0x3b, 0xb2, 0xee, 0x42, 0x00, 0x01, 0xe1, 0x51
  , 0xf0, 0x7f, 0xf5
  ]

print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = do
  putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  Just detail <- pure $ Capstone.detail insn
  Just (Arm arch) <- pure $ archInfo detail
  printArchInsnInfo arch
  putStrLn ""
 where
  m = mnemonic insn
  o = opStr insn
  a = (showHex $ address insn) ""

  printArchInsnInfo arch = do
    let operands = Arm.operands arch
    when (length operands > 0)
      $ putStrLn ("\topcount: " ++ ((show . length) operands))
    mapM_ printOperandDetail $ zip [0..] operands
    when (updateFlags arch)
      $ putStrLn "\tUpdate-flags: True"
    when (writeback arch)
      $ putStrLn "\tWrite-back: True"
    case cc arch of
      ArmCcAl -> pure ()
      ArmCcInvalid -> pure ()
      _ -> putStrLn (printf "\tCode condition: %s" (show $ cc arch))
    case cpsMode arch of
      ArmCpsmodeInvalid -> pure ()
      _ ->
        putStrLn (printf "\tCPSI-mode: %s" (show $ cpsMode arch))
    case cpsFlag arch of
      ArmCpsflagInvalid -> pure ()
      _ ->
        putStrLn (printf "\tCPSI-flag: %s" (show $ cpsFlag arch))
    case vectorData arch of
      ArmVectordataInvalid -> pure ()
      _ -> putStrLn (printf "\tVector-data: %s" (show $ vectorData arch))
    when (vectorSize arch /= 0)
      $ putStrLn (printf "\tVector-size: %u" (vectorSize arch))
    when (usermode arch)
      $ putStrLn (printf "\tUser-mode: True")
    case memBarrier arch of
      ArmMbInvalid -> pure ()
      _ -> putStrLn (printf "\tMem-barrier: %s" (show $ memBarrier arch))

    -- TODO: read/writer register, wait cs_reg_access
   where
    printOperandDetail :: (Int, Arm.CsArmOp) -> IO ()
    printOperandDetail (i, op) = do
      case value op of
        Reg reg ->
          let Just reg_name = Capstone.csRegName handle reg in
            putStrLn (printf "\t\toperands[%u].type: REG = %s" i reg_name)
        Sysreg reg ->
          putStrLn (printf "\t\toperands[%u].type: SYSREG = %u" i reg)
        Imm imm ->
          putStrLn (printf "\t\toperands[%u].type: IMM = 0x%x" i imm)
        CImm imm ->
          putStrLn (printf "\t\toperands[%u].type: C-IMM = %u" i imm)
        PImm imm ->
          putStrLn (printf "\t\toperands[%u].type: P-IMM = %u" i imm)
        Fp fp ->
          putStrLn (printf "\t\toperands[%u].type: FP = %f" i fp)
        Setend setend ->
          case setend of
            ArmSetendBe ->
              putStrLn (printf "\t\toperands[%u].type: SETEND = be" i)
            ArmSetendLe ->
              putStrLn (printf "\t\toperands[%u].type: SETEND = le" i)
        Mem mem -> do
          let base_ = base mem
              index_ = index mem
              scale_ = scale mem
              disp_ = disp mem
              lshift_ = lshift mem
          putStrLn (printf "\t\toperands[%u].type: MEM" i)
          when (base_ /= ArmRegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle base_
              putStrLn (printf "\t\t\toperands[%u].mem.base: REG = %s" i reg_name)

          when (index_ /= ArmRegInvalid)
            $ do
              let Just reg_name = Capstone.csRegName handle index_
              putStrLn (printf "\t\t\toperands[%u].mem.index: REG = %s" i reg_name)
          when (scale_ /= 1)
            $ putStrLn (printf "\t\t\toperands[%u].mem.scale: %u" i scale_)
          when (disp_ /= 0)
            $ putStrLn (printf "\t\t\toperands[%u].mem.disp: 0x%x" i disp_)
          case lshift_ of
            Just ls -> putStrLn (printf "\t\t\toperands[%u].mem.lshift: 0x%x" i ls)
            Nothing -> pure ()
        _ ->
          putStrLn (printf "\t\toperands[%u].type: UNKNOWN" i)

      let neon_lane_ = neon_lane op
          access_ = access op
          shift_ = shift op
          subtracted_ = subtracted op
          vector_index_ = vectorIndex op

      when (neon_lane_ /= -1)
        $ putStrLn (printf "\t\toperands[%u].neon_lane = %u" neon_lane_)

      -- TODO: wait for access enum
      -- case access_ of
      --   csAcRead ->
      --     putStrLn (printf "\t\toperands[%u].access: READ" i)
      --     putStrLn ""
      --   csAcWrite ->
      --     putStrLn (printf "\t\toperands[%u].access: WRITE" i)
      --     putStrLn ""
      --   csAcReadWrite ->
      --     putStrLn (printf "\t\toperands[%u].access: READ | WRITE" i)
      --     putStrLn ""

      when ((fst shift_ /= ArmSftInvalid) && (snd shift_ /= 0))
        $ putStrLn (printf "\t\t\tShift: %s = %u" (show $ fst shift_) (snd shift_))
      when (vector_index_ /= -1)
        $ putStrLn (printf "\t\t\toperands[%u].vector_index = %u" i vector_index_)
      when subtracted_
        $ putStrLn (printf "\t\t\toperands[%u].subtracted = True" i)


all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchArm
                   , modes                    = [Capstone.CsModeArm]
                   , buffer                   = arm_code
                   , addr                     = 0x80001000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "ARM"
    )
  , ( Disassembler { arch                     = Capstone.CsArchArm
                   , modes                    = [Capstone.CsModeThumb]
                   , buffer                   = thumb_code
                   , addr                     = 0x80001000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "Thumb"
    )
  , ( Disassembler { arch                     = Capstone.CsArchArm
                   , modes                    = [Capstone.CsModeThumb]
                   , buffer                   = arm_code2
                   , addr                     = 0x80001000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "Thumb-mixed"
    )
  , ( Disassembler { arch                     = Capstone.CsArchArm
                   , modes                    = [Capstone.CsModeThumb]
                   , buffer                   = thumb_code2
                   , addr                     = 0x80001000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "Thumb-2 & register named with numbers"
    )
  , ( Disassembler { arch                     = Capstone.CsArchArm
                   , modes = [Capstone.CsModeThumb, Capstone.CsModeMclass]
                   , buffer                   = thumb_mclass
                   , addr                     = 0x80001000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "Thumb-MClass"
    )
  , ( Disassembler { arch                     = Capstone.CsArchArm
                   , modes = [Capstone.CsModeArm, Capstone.CsModeV8]
                   , buffer                   = armv8
                   , addr                     = 0x80001000
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "Arm-V8"
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
