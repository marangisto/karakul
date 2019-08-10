{-# LANGUAGE RecordWildCards #-}
module Internal.ARM (toolChain) where

import Internal.ToolChain
import System.FilePath

samDir :: FilePath
samDir = "c:/Users/marten/AppData/Local/Arduino15/packages/arduino/hardware/sam/1.6.11"

toolChain :: FilePath -> MCU -> ToolChain
toolChain baseDir mcu = ToolChain{..}
    where name = "arm-none-eabi-gcc"
          cc = ("arm-none-eabi-gcc", \_ -> ccFlags mcu)
          cpp = ("arm-none-eabi-g++", \_ -> ccFlags mcu ++ cppFlags mcu)
          asm = ("arm-none-eabi-gcc", \_ -> ccFlags mcu ++ asmFlags mcu)
          ld = ("arm-none-eabi-gcc", \objs -> ldFlags baseDir mcu objs)
          ar = ("arm-none-eabi-ar", \_ -> [])
          objcopy = ("arm-none-eabi-objcopy", \_ -> copyFlags mcu)
          objdump = ("arm-none-eabi-objdump", \_ -> [ "--disassemble-all" ])
          size = ("arm-none-eabi-size", \_ -> [])
          format = Binary

ccFlags mcu = mcuFlags mcu ++
    [ "-mthumb"
    , "-ffunction-sections"
    , "-fdata-sections"
    ]

mcuFlags STM32F051 =
    [ "-DSTM32F051"
    , "-mcpu=cortex-m0"
    ]
mcuFlags STM32F103 =
    [ "-DSTM32F103"
    , "-mcpu=cortex-m3"
    ]
mcuFlags STM32F411 =
    [ "-DSTM32F411"
    , "-mcpu=cortex-m4"
    ]
mcuFlags STM32G070 =
    [ "-DSTM32G070"
    , "-mcpu=cortex-m0plus" -- .small-multiply"
    ]
mcuFlags STM32G431 =
    [ "-DSTM32G431"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=hard"
    , "-mfpu=fpv4-sp-d16"
    , "-fsingle-precision-constant"
    ]
mcuFlags SAM3X8E =
    [ "-D__SAM3X8E__"
    , "-mcpu=cortex-m3"
    , "-I../ARM"
    , "-I" ++ samDir ++ "/system/CMSIS/CMSIS/include"
    , "-I" ++ samDir ++ "/system/CMSIS/Device/ATMEL"
    , "-I" ++ samDir ++ "/system/CMSIS/Device/ATMEL/sam3xa/include"
    , "-I" ++ samDir ++ "/system/libsam"
    , "-I" ++ samDir ++ "/cores/arduino"
    ]
mcuFlags mcu =
    [ "-nostdlib"
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

cppFlags _ =
    [ "-std=gnu++17"
    , "-fno-threadsafe-statics"
    , "-fno-exceptions"
    , "-fno-rtti"
    ]

asmFlags _ =
    [ "-xassembler-with-cpp"
    ]

ldFlags baseDir STM32F051 objs =
    [ "-mcpu=cortex-m0"
    , "-mthumb"
    , "-specs=nosys.specs"  -- to get gcc _sbrk, etc to link
    , "-Wl,--gc-sections"
    , "-T" ++ baseDir </> "hal/link/stm32f051.ld"
    , "-Wl,--check-sections"
    , "-Wl,--entry=__reset"
    , "-Wl,--unresolved-symbols=report-all"
    , "-Wl,--warn-common"
    , "-Wl,--warn-section-align"
    , "-Wl,--start-group"
    ] ++ objs ++
    [ "-Wl,--end-group"
    , "-lm"
    ]
ldFlags baseDir STM32F103 objs =
    [ "-mcpu=cortex-m3"
    , "-mthumb"
    , "-specs=nosys.specs"  -- to get gcc _sbrk, etc to link
    , "-Wl,--gc-sections"
    , "-T" ++ baseDir </> "hal/link/stm32f103.ld"
    , "-Wl,--check-sections"
    , "-Wl,--entry=__reset"
    , "-Wl,--unresolved-symbols=report-all"
    , "-Wl,--warn-common"
    , "-Wl,--warn-section-align"
    , "-Wl,--start-group"
    ] ++ objs ++
    [ "-Wl,--end-group"
    , "-lm"
    ]
ldFlags baseDir STM32F411 objs =
    [ "-mcpu=cortex-m4"
    , "-mthumb"
    , "-specs=nosys.specs"  -- to get gcc _sbrk, etc to link
    , "-Wl,--gc-sections"
    , "-T" ++ baseDir </> "hal/link/stm32f411.ld"
    , "-Wl,--check-sections"
    , "-Wl,--entry=__reset"
    , "-Wl,--unresolved-symbols=report-all"
    , "-Wl,--warn-common"
    , "-Wl,--warn-section-align"
    , "-Wl,--start-group"
    ] ++ objs ++
    [ "-Wl,--end-group"
    , "-lm"
    ]
ldFlags baseDir STM32G070 objs =
    [ "-mcpu=cortex-m0plus"
    , "-mthumb"
    , "-specs=nosys.specs"  -- to get gcc _sbrk, etc to link
    , "-Wl,--gc-sections"
    , "-T" ++ baseDir </> "hal/link/stm32g070.ld"
    , "-Wl,--check-sections"
    , "-Wl,--entry=__reset"
    , "-Wl,--unresolved-symbols=report-all"
    , "-Wl,--warn-common"
    , "-Wl,--warn-section-align"
    , "-Wl,--start-group"
    ] ++ objs ++
    [ "-Wl,--end-group"
    , "-lm"
    ]
ldFlags baseDir STM32G431 objs =
    [ "-mcpu=cortex-m4"
    , "-mfloat-abi=hard"
    , "-mfpu=fpv4-sp-d16"
    , "-fsingle-precision-constant"
    , "-mthumb"
    , "-specs=nosys.specs"  -- to get gcc _sbrk, etc to link
    , "-Wl,--gc-sections"
    , "-T" ++ baseDir </> "hal/link/stm32g431.ld"
    , "-Wl,--check-sections"
    , "-Wl,--entry=__reset"
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
    , "-Wl,--entry=__reset"
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

