module Internal.ToolChain where

import Data.Char (toLower)

data MCU
    = ATMEGA328
    | ATMEGA328P
    | ATMEGA328PB
    | ATMEGA32U4
    | ATMEGA2560
    | ATTINY85
    | ATTINY84
    | MK64FX512
    | MK66FX1M0
    | SAM3X8E
    | STM32F051
    | STM32F103
    | STM32F411
    | STM32F412
    | STM32G070
    | STM32G431
    deriving (Read, Show)

mcuStr :: MCU -> String
mcuStr = map toLower . show

data ARCH = AVR | ARM deriving (Read, Show)

arch :: MCU -> ARCH
arch ATMEGA328      = AVR
arch ATMEGA328P     = AVR
arch ATMEGA328PB    = AVR
arch ATMEGA32U4     = AVR
arch ATMEGA2560     = AVR
arch ATTINY85       = AVR
arch ATTINY84       = AVR
arch MK64FX512      = ARM
arch MK66FX1M0      = ARM
arch SAM3X8E        = ARM
arch STM32F051      = ARM
arch STM32F103      = ARM
arch STM32F411      = ARM
arch STM32F412      = ARM
arch STM32G070      = ARM
arch STM32G431      = ARM

data Board
    = Uno
    | Mega2560
    | Leonardo
    | Due
    | TrinketPro
    | Teensy35
    | Teensy36
    deriving (Read, Show)

type Freq = Double

boardMCU :: Board -> (MCU, Freq)
boardMCU Uno            = (ATMEGA328P,      16e6)
boardMCU Mega2560       = (ATMEGA2560,      16e6)
boardMCU Leonardo       = (ATMEGA32U4,      16e6)
boardMCU Due            = (SAM3X8E,         84e6)
boardMCU TrinketPro     = (ATMEGA328P,      16e6)
boardMCU Teensy35       = (MK64FX512,       120e6)
boardMCU Teensy36       = (MK66FX1M0,       180e6)

data Format = Binary | Hex

type Tool = (String, [String] -> [String])

data ToolChain = ToolChain
    { name      :: String
    , cc        :: Tool
    , cpp       :: Tool
    , asm       :: Tool
    , ld        :: Tool
    , ar        :: Tool
    , objcopy   :: Tool
    , objdump   :: Tool
    , size      :: Tool
    , format    :: Format
    }

