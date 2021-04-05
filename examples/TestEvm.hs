module Main where

import           Control.Monad
import           Data.Word
import           Text.Printf
import           Numeric                        ( showHex )

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone    as Capstone
import           Hapstone.Internal.Evm         as Evm

evm_code =
  [ 0x60
  , 0x61
  , 0x55
  ]

print_insn_detail :: Capstone.Csh -> Capstone.CsInsn -> IO ()
print_insn_detail handle insn = do
  putStrLn ("0x" ++ a ++ ":\t" ++ m ++ "\t" ++ o)
  Just detail <- pure $ Capstone.detail insn
  Just (Evm arch) <- pure $ archInfo detail
  igroups <- pure $ groups detail
  when (pop arch > 0)
    $ putStrLn $ printf "\tPop:     %u" (pop arch)
  when (push arch > 0)
    $ putStrLn $ printf "\tPush:    %u" (pop arch)
  when (fee arch > 0)
    $ putStrLn $ printf "\tGas fee: %u" (pop arch)
  when (length igroups > 0)
    $ do
      putStr "\tThis instruction belongs to groups:"
      mapM_ printGroup igroups
  putStrLn ""
 where
  m = mnemonic insn
  o = opStr insn
  a = (showHex $ address insn) ""

  printGroup g = do
    Just name <- pure $ csGroupName handle g
    putStr $ printf " %s" name

all_tests =
  [ ( Disassembler { arch                     = Capstone.CsArchEvm
                   , modes                    = []
                   , buffer                   = evm_code
                   , addr                     = 0x100
                   , num                      = 0
                   , Hapstone.Capstone.detail = True
                   , skip                     = Just (defaultSkipdataStruct)
                   , action                   = print_insn_detail
                   }
    , "EVM"
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
