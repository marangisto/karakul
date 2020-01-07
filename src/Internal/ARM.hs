{-# LANGUAGE RecordWildCards #-}
module Internal.ARM (toolChain) where

import Internal.ToolChain
import System.FilePath

samDir :: FilePath
samDir = "c:/Users/marten/AppData/Local/Arduino15/packages/arduino/hardware/sam/1.6.11"

toolChain :: ToolConfig -> ToolChain
toolChain tc@ToolConfig{..} = ToolChain{..}
    where name = "arm-none-eabi-gcc"
          cc = ("arm-none-eabi-gcc", \_ -> ccFlags tc)
          cpp = ("arm-none-eabi-g++", \_ -> ccFlags tc ++ cppFlags mcu)
          asm = ("arm-none-eabi-gcc", \_ -> ccFlags tc ++ asmFlags mcu)
          ld = ("arm-none-eabi-gcc", \objs -> ldFlags tc objs)
          ar = ("arm-none-eabi-ar", \_ -> [])
          objcopy = ("arm-none-eabi-objcopy", \_ -> copyFlags mcu)
          objdump = ("arm-none-eabi-objdump", \_ -> [ "--disassemble-all" ])
          size = ("arm-none-eabi-size", \_ -> [])
          format = Binary

ccFlags ToolConfig{..} = mcuFlags mcu ++
    [ "-mthumb"
    , "-ffunction-sections"
    , "-fdata-sections"
    ]

mcuFlags STM32F051 =
    [ "-DSTM32F051"
    , "-DSTM32F0"
    , "-mcpu=cortex-m0"
    ]
mcuFlags STM32F072 =
    [ "-DSTM32F072"
    , "-DSTM32F0"
    , "-mcpu=cortex-m0"
    ]
mcuFlags STM32F103 =
    [ "-DSTM32F103"
    , "-DSTM32F1"
    , "-mcpu=cortex-m3"
    ]
mcuFlags STM32F411 =
    [ "-DSTM32F411"
    , "-DSTM32F4"
    , "-mcpu=cortex-m4"
    ]
mcuFlags STM32F767 =
    [ "-DSTM32F767"
    , "-DSTM32F7"
    , "-mcpu=cortex-m7"
    ]
mcuFlags STM32H743 =
    [ "-DSTM32H743"
    , "-DSTM32H7"
    , "-mcpu=cortex-m7"
    ]
mcuFlags STM32G070 =
    [ "-DSTM32G070"
    , "-DSTM32G0"
    , "-mcpu=cortex-m0plus" -- .small-multiply"
    ]
mcuFlags STM32G431 =
    [ "-DSTM32G431"
    , "-DSTM32G4"
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

ldFlags ToolConfig{..} objs = ldFlagsMCU mcu ++
    [ "-mthumb"
    , "-specs=nosys.specs"  -- to get gcc _sbrk, etc to link
    , "-Wl,--gc-sections"
    , "-T" ++ link
    , "-Wl,--check-sections"
    , "-Wl,--entry=" ++ entry
    , "-Wl,--unresolved-symbols=report-all"
    , "-Wl,--warn-common"
    , "-Wl,--warn-section-align"
    , "-Wl,--start-group"
    ] ++ objs ++
    [ "-Wl,--end-group"
    , "-lm"
    ]

ldFlagsMCU STM32F051 = [ "-mcpu=cortex-m0" ]
ldFlagsMCU STM32F072 = [ "-mcpu=cortex-m0" ]
ldFlagsMCU STM32F103 = [ "-mcpu=cortex-m3" ]
ldFlagsMCU STM32F411 = [ "-mcpu=cortex-m4" ]
ldFlagsMCU STM32F767 = [ "-mcpu=cortex-m7" ]
ldFlagsMCU STM32H743 = [ "-mcpu=cortex-m7" ]
ldFlagsMCU STM32G070 = [ "-mcpu=cortex-m0plus" ]
ldFlagsMCU STM32G431 = [ "-mcpu=cortex-m4", "-mfloat-abi=hard", "-mfpu=fpv4-sp-d16", "-fsingle-precision-constant" ]
ldFlagsMCU SAM3X8E = [ "-mcpu=cortex-m3" ]

--ldFlags _ mcu@MK64FX512 objs = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6 objs  -- FIXME: need this file somewhere!
--ldFlags _ mcu@MK66FX1M0 objs = ("-T../Teensy3/" ++ mcuStr mcu ++ ".ld") : ldFlagsMK6 objs   -- FIXME: need this file somewhere!

ldFlagsMK6 _ objs =
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

