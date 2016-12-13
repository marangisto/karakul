module Internal.ToolChain where

import Data.Char (toLower)

data MCU
    = ATMEGA328
    | ATMEGA328P
    | ATMEGA32U4
    | ATTINY85
    | MK64FX512
    | MK66FX1M0
    deriving (Read, Show)

mcuStr :: MCU -> String
mcuStr = map toLower . show

data ARCH = AVR | ARM deriving (Read, Show)

arch :: MCU -> ARCH
arch ATMEGA328      = AVR
arch ATMEGA328P     = AVR
arch ATMEGA32U4     = AVR
arch ATTINY85       = AVR
arch MK64FX512      = ARM
arch MK66FX1M0      = ARM

data Board
    = Uno
    | Leonardo
    | TrinketPro
    | Teensy35
    | Teensy36
    deriving (Read, Show)

type Freq = Double

boardMCU :: Board -> (MCU, Freq)
boardMCU Uno            = (ATMEGA328P,      16e6)
boardMCU Leonardo       = (ATMEGA32U4,      16e6)
boardMCU TrinketPro     = (ATMEGA328P,      16e6)
boardMCU Teensy35       = (MK64FX512,       120e6)
boardMCU Teensy36       = (MK66FX1M0,       180e6)

type Tool = (String, [String])

data ToolChain = ToolChain
    { name      :: String
    , cc        :: Tool
    , cpp       :: Tool
    , ld        :: Tool
    , ar        :: Tool
    , objcopy   :: Tool
    , objdump   :: Tool
    , size      :: Tool
    }

