{-# LANGUAGE RecordWildCards #-}
module ShakeMCU.AVR (toolChain) where

import ShakeMCU.ToolChain

toolChain :: MCU -> ToolChain
toolChain mcu = ToolChain{..}
    where name = "avr-gcc"
          cc = ("avr-gcc", ccFlags mcu)
          cpp = ("avr-g++", cppFlags mcu)
          ld = ("avr-gcc", ldFlags mcu)
          ar = ("avr-ar", [])
          objcopy = ("avr-objcopy", [])
          objdump = ("avr-objdump", [])
          size = ("avr-size", [ "--mcu=" ++ mcuStr mcu, "--format=avr" ])

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

ldFlags mcu =
    [ "-mmcu=" ++ mcuStr mcu
    , "-Wl,--gc-sections"
    , "-Wl,-u,vfprintf"
    , "-lprintf_flt"
    , "-lm"
    ]

