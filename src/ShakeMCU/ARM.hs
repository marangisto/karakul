{-# LANGUAGE RecordWildCards #-}
module ShakeMCU.ARM (toolChain) where

import ShakeMCU.ToolChain

toolChain :: MCU -> ToolChain
toolChain mcu = ToolChain{..}
    where name = "arm-none-eabi-gcc"
          cc = ("arm-none-eabi-gcc", ccFlags mcu)
          cpp = ("arm-none-eabi-g++", cppFlags mcu)
          ld = ("arm-none-eabi-gcc", ldFlags mcu)
          ar = ("arm-none-eabi-ar", [])
          objcopy = ("arm-none-eabi-objcopy", [])
          objdump = ("arm-none-eabi-objdump", [])
          size = ("arm-none-eabi-size", [])

ccFlags mcu =
    [ "-ffunction-sections"
    , "-fdata-sections"
    , "-nostdlib"
    , "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=hard"
    , "-mfpu=fpv4-sp-d16"
    , "-fsingle-precision-constant"
    , "-D__" ++ show mcu ++ "__"
    , "-DUSB_SERIAL"
    , "-DLAYOUT_US_ENGLISH"
    , "-DARDUINO=10600"
    , "-DTEENSYDUINO=121"
    ]

cppFlags _ =
    [ "-std=gnu++0x"
    , "-felide-constructors"
    , "-fno-exceptions"
    , "-fno-rtti"
    ]

ldFlags mcu =
    [ "-Wl,--gc-sections,--relax,--defsym=__rtc_localtime=1476636451"
    , "-TC:/tmp/" ++ mcuStr mcu ++ ".ld"   -- FIXME: need this file somewhere!
    , "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=hard"
    , "-mfpu=fpv4-sp-d16"
    , "-fsingle-precision-constant"
    ]

