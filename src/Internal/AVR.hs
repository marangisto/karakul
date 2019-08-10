{-# LANGUAGE RecordWildCards #-}
module Internal.AVR (toolChain) where

import Internal.ToolChain
import Data.List (intercalate)

toolChain :: FilePath -> MCU -> ToolChain
toolChain _ mcu = ToolChain{..}
    where name = "avr-gcc"
          cc = ("avr-gcc", \_ -> ccFlags mcu)
          cpp = ("avr-g++", \_ -> cppFlags mcu)
          asm = ("avr-gcc", \_ -> cppFlags mcu ++ asmFlags mcu)
          ld = ("avr-gcc", \xs -> ldFlags mcu ++ xs)
          ar = ("avr-ar", \_ -> [])
          objcopy = ("avr-objcopy", \_ -> copyFlags mcu)
          objdump = ("avr-objdump", \_ -> [])
          size = ("avr-size", \_ -> [ "--mcu=" ++ mcuStr mcu, "--format=avr" ])
          format = Hex

ccFlags mcu =
    [ "-mmcu=" ++ mcuStr mcu
    , "-ffunction-sections"
    , "-fdata-sections"
    ]

cppFlags mcu = ccFlags mcu ++
    [ "-std=c++11"
    , "-fno-exceptions"
    , "-fno-threadsafe-statics"
    ]

asmFlags mcu =
    [ "-xassembler-with-cpp"
    ]

ldFlags mcu =
    [ "-mmcu=" ++ mcuStr mcu
    , "-Wl,--gc-sections"
    , "-Wl,-u,vfprintf"
    , "-lprintf_flt"
    , "-lm"
    ]

copyFlags _ =
    [ "-R.eeprom"
    ]

