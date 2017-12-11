{-# LANGUAGE RecordWildCards #-}
module Internal.ARM (toolChain) where

import Internal.ToolChain

samDir :: FilePath
samDir = "c:/Users/marten/AppData/Local/Arduino15/packages/arduino/hardware/sam/1.6.11"

toolChain :: MCU -> ToolChain
toolChain mcu = ToolChain{..}
    where name = "arm-none-eabi-gcc"
          cc = ("arm-none-eabi-gcc", \_ -> ccFlags mcu)
          cpp = ("arm-none-eabi-g++", \_ -> ccFlags mcu ++ cppFlags mcu)
          ld = ("arm-none-eabi-gcc", \objs -> ldFlags mcu objs)
          ar = ("arm-none-eabi-ar", \_ -> [])
          objcopy = ("arm-none-eabi-objcopy", \_ -> copyFlags mcu)
          objdump = ("arm-none-eabi-objdump", \_ -> [])
          size = ("arm-none-eabi-size", \_ -> [])

ccFlags SAM3X8E =
    [ "-D__SAM3X8E__"
    , "-mcpu=cortex-m3"
    , "-mthumb"
    , "-I../ARM"
    , "-I" ++ samDir ++ "/system/CMSIS/CMSIS/include"
    , "-I" ++ samDir ++ "/system/CMSIS/Device/ATMEL"
    , "-I" ++ samDir ++ "/system/libsam"
    , "-I" ++ samDir ++ "/cores/arduino"
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

cppFlags SAM3X8E =
    [ "-std=gnu++11"
    , "-fno-threadsafe-statics"
    , "-fno-exceptions"
    ]
cppFlags MK64FX512 =
    [ "-std=gnu++11"
    , "-felide-constructors"
    , "-fno-exceptions"
    , "-fno-rtti"
    ]
cppFlags MK66FX1M0 = cppFlags MK64FX512

ldFlags SAM3X8E objs =
    [ "-mcpu=cortex-m3"
    , "-mthumb"
    , "-Wl,--gc-sections"
    , "-T" ++ samDir ++ "/variants/arduino_due_x/linker_scripts/gcc/flash.ld"
    , "-Wl,--check-sections"
    , "-Wl,--entry=Reset_Handler"
    , "-Wl,--unresolved-symbols=report-all"
    , "-Wl,--warn-common"
    , "-Wl,--warn-section-align"
    , "-Wl,--start-group"
    , "-u _sbrk"
    , "-u link"
    , "-u _close"
    , "-u _fstat"
    , "-u _isatty"
    , "-u _lseek"
    , "-u _read"
    , "-u _write"
    , "-u _exit"
    , "-u kill"
    , "-u _getpid"
    ] ++ objs ++
    [ samDir ++ "/variants/arduino_due_x/libsam_sam3x8e_gcc_rel.a"
    , "-Wl,--end-group"
    , "-lm"
    , "-gcc"
    ]
ldFlags mcu@MK64FX512 objs = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6 objs  -- FIXME: need this file somewhere!
ldFlags mcu@MK66FX1M0 objs = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6 objs   -- FIXME: need this file somewhere!

ldFlagsMK6 objs =
    [ "-Wl,--gc-sections,--relax,--defsym=__rtc_localtime=1476636451"
    , "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=hard"
    , "-mfpu=fpv4-sp-d16"
    , "-fsingle-precision-constant"
    ] ++ objs

copyFlags SAM3X8E =
    [ "-Obinary"
    ]
copyFlags _ =
    [ "-Oihex"
    , "-R.eeprom"
    ]

