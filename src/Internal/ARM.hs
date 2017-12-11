{-# LANGUAGE RecordWildCards #-}
module Internal.ARM (toolChain) where

import Internal.ToolChain

samDir :: FilePath
samDir = "c:/Users/marten/AppData/Local/Arduino15/packages/arduino/hardware/sam/1.6.11"

toolChain :: MCU -> ToolChain
toolChain mcu = ToolChain{..}
    where name = "arm-none-eabi-gcc"
          cc = ("arm-none-eabi-gcc", ccFlags mcu)
          cpp = ("arm-none-eabi-g++", ccFlags mcu ++ cppFlags mcu)
          ld = ("arm-none-eabi-ld", ldFlags mcu)
          ar = ("arm-none-eabi-ar", [])
          objcopy = ("arm-none-eabi-objcopy", copyFlags mcu)
          objdump = ("arm-none-eabi-objdump", [])
          size = ("arm-none-eabi-size", [])

ccFlags SAM3X8E =
    [ "-D__SAM3X8E__"
    , "-mcpu=cortex-m3"
    , "-mthumb"
    , "-I../ARM"
    , "-I" ++ samDir ++ "/system/CMSIS/CMSIS/include"
    , "-I" ++ samDir ++ "/system/CMSIS/Device/ATMEL/sam3xa/include"
    , "-ffunction-sections"
    , "-fdata-sections"
    ]
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
    , "-I../Teensy3"
    ]

cppFlags MK64FX512 =
    [ "-std=gnu++11"
    , "-felide-constructors"
    , "-fno-exceptions"
    , "-fno-rtti"
    ]
cppFlags MK66FX1M0 = cppFlags MK64FX512
cppFlags SAM3X8E =
    [ "-std=gnu++11"
    , "-fno-threadsafe-statics"
    , "-fno-exceptions"
    ]

ldFlags mcu@MK64FX512 = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6   -- FIXME: need this file somewhere!
ldFlags mcu@MK66FX1M0 = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6   -- FIXME: need this file somewhere!
ldFlags SAM3X8E =
    [ "--gc-sections"
    , "-T" ++ samDir ++ "/variants/arduino_due_x/linker_scripts/gcc/flash.ld"
    , "--entry=Reset_Handler"
    ]

ldFlagsMK6 =
    [ "-Wl,--gc-sections,--relax,--defsym=__rtc_localtime=1476636451"
    , "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=hard"
    , "-mfpu=fpv4-sp-d16"
    , "-fsingle-precision-constant"
    ]

copyFlags SAM3X8E =
    [ "-Obinary"
    ]
copyFlags _ =
    [ "-Oihex"
    , "-R.eeprom"
    ]

