{-# LANGUAGE RecordWildCards #-}
module Internal.ARM (toolChain) where

import Internal.ToolChain
import System.FilePath

samDir :: FilePath
samDir = "c:/Users/marten/AppData/Local/Arduino15/packages/arduino/hardware/sam/1.6.11"

toolChain :: FilePath -> MCU -> ToolChain
toolChain baseDir mcu = ToolChain{..}
    where name = "arm-none-eabi-gcc"
          cc = ("arm-none-eabi-gcc", \_ -> ccFlags baseDir mcu)
          cpp = ("arm-none-eabi-g++", \_ -> ccFlags baseDir mcu ++ cppFlags mcu)
          ld = ("arm-none-eabi-gcc", \objs -> ldFlags baseDir mcu objs)
          ar = ("arm-none-eabi-ar", \_ -> [])
          objcopy = ("arm-none-eabi-objcopy", \_ -> copyFlags mcu)
          objdump = ("arm-none-eabi-objdump", \_ -> [ "--disassemble-all" ])
          size = ("arm-none-eabi-size", \_ -> [])
          format = Binary

ccFlags baseDir STM32F051 =
    [ "-DSTM32F0"
    , "-DSTM32F0x1"
    , "-DSTM32F05x"
    , "-DSTM32F051"
    , "-mcpu=cortex-m0"
    , "-mthumb"
    , "-ffunction-sections"
    , "-fdata-sections"
    ]
ccFlags _ SAM3X8E =
    [ "-D__SAM3X8E__"
    , "-mcpu=cortex-m3"
    , "-mthumb"
    , "-I../ARM"
    , "-I" ++ samDir ++ "/system/CMSIS/CMSIS/include"
    , "-I" ++ samDir ++ "/system/CMSIS/Device/ATMEL"
    , "-I" ++ samDir ++ "/system/CMSIS/Device/ATMEL/sam3xa/include"
    , "-I" ++ samDir ++ "/system/libsam"
    , "-I" ++ samDir ++ "/cores/arduino"
    , "-ffunction-sections"
    , "-fdata-sections"
    ]
ccFlags _ mcu =
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

cppFlags STM32F051 =
    [ "-std=gnu++17"
    , "-fno-threadsafe-statics"
    , "-fno-exceptions"
    , "-fno-rtti"
    ]
cppFlags SAM3X8E =
    [ "-std=gnu++11"
    , "-fno-threadsafe-statics"
    , "-fno-exceptions"
    , "-fno-rtti"
    ]
cppFlags MK64FX512 =
    [ "-std=gnu++11"
    , "-felide-constructors"
    , "-fno-exceptions"
    , "-fno-rtti"
    ]
cppFlags MK66FX1M0 = cppFlags MK64FX512

ldFlags baseDir STM32F051 objs =
    [ "-mcpu=cortex-m0"
    , "-mthumb"
    , "-Wl,--gc-sections"
    , "-T" ++ baseDir </> "stm32f0/link/stm32f051.ld"
    , "-Wl,--check-sections"
    , "-Wl,--entry=Reset_HDLR"
    , "-Wl,--unresolved-symbols=report-all"
    , "-Wl,--warn-common"
    , "-Wl,--warn-section-align"
    , "-Wl,--start-group"
    ] ++ objs ++
    [ "-Wl,--end-group"
    , "-lm"
    ]
ldFlags _ SAM3X8E objs =
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
    [ "-Wl,--end-group"
    , "-lm"
    , "-gcc"
    ]
ldFlags _ mcu@MK64FX512 objs = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6 objs  -- FIXME: need this file somewhere!
ldFlags _ mcu@MK66FX1M0 objs = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6 objs   -- FIXME: need this file somewhere!

ldFlagsMK6 objs =
    [ "-Wl,--gc-sections,--relax,--defsym=__rtc_localtime=1476636451"
    , "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=hard"
    , "-mfpu=fpv4-sp-d16"
    , "-fsingle-precision-constant"
    ] ++ objs

copyFlags _ =
    [
    ]

