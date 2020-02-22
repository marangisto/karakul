module Internal.Program where

import Internal.Config
import Internal.ToolChain
import Internal.USBSerial
import Development.Shake
import Control.Applicative
import Data.Maybe (fromMaybe)
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad

programmer :: Maybe Board -> MCU -> FilePath -> Action Tool
programmer Nothing mcu bin
    | mcu `elem`
        [ STM32F051
        , STM32F072
        , STM32F103
        , STM32F411
        , STM32F412
        , STM32F767
        , STM32H743
        , STM32G070
        , STM32G431
        ]
    = return ("STM32_Programmer_CLI", \_ ->
        [ "--connect port=SWD mode=UR"
        , "--write"
        , bin
        , "0x8000000"
        , "--verify"
        , "-hardRst"
        ])
programmer Nothing mcu hex = return
    ("atprogram", \_ ->
        [ "-t"
        , "avrispmk2"
        , "-d"
        , mcuStr mcu
        , "-i"
        , "isp"
        , "program"
        , "-c"
        , "--verify"
        , "-f"
        , hex
        ])
programmer (Just Uno) mcu hex = do
    port <- fmap (fromMaybe "COM3") $ liftIO $ findPort 0x2341 0x43
    port <- fmap (fromMaybe port) $ getPort
    return ("avrdude", \_ ->
        [ "-c" ++ "arduino"
        , "-C" ++ avrdudeConf
        , "-p" ++ mcuStr mcu
        , "-P" ++ port
        , "-b" ++ "115200"
        , "-D"
        , "-Uflash:w:" ++ hex ++ ":i"
        ])
programmer (Just Mega2560) mcu hex = do
    port <- fmap (fromMaybe "COM3") $ liftIO $ findPort 0x2341 0x43 -- FIXME: use correct id!
    port <- fmap (fromMaybe port) $ getPort
    return ("avrdude", \_ ->
        [ "-c" ++ "arduino"
        , "-C" ++ avrdudeConf
        , "-p" ++ mcuStr mcu
        , "-P" ++ port
        --, "-b" ++ "115200"
        , "-b" ++ "9600"
        , "-D"
        , "-Uflash:w:" ++ hex ++ ":i"
        ])
programmer (Just Leonardo) mcu hex = do
    b <- getManualBoot
    unless b $ do
        port <- fmap (fromMaybe "COM3") $ liftIO $ findPort 0x2341 0x8036
        port <- fmap (fromMaybe port) $ getPort
        liftIO $ putStrLn $ "resetting " ++ port
        liftIO $ closeSerial =<< openSerial port defaultSerialSettings { commSpeed = CS1200 }
        liftIO $ threadDelay 4000000 -- FIXME: wait for device change
    port <- fmap (fromMaybe "COM4") $ liftIO $ findPort 0x2341 0x36
    bootPort <- getBootPort
    port <- return $ fromMaybe port bootPort
    return ("avrdude", \_ ->
        [ "-c" ++ "avr109"
        , "-C" ++ avrdudeConf
        , "-p" ++ mcuStr mcu
        , "-P" ++ port
        , "-b" ++ "57600"
        , "-D"
        , "-Uflash:w:" ++ hex ++ ":i"
        ])
programmer (Just TrinketPro) mcu hex = return
    ("avrdude", \_ ->
        [ "-c" ++ "usbtiny"
        , "-C" ++ avrdudeConf
        , "-p" ++ mcuStr mcu
        , "-D"
        , "-Uflash:w:" ++ hex ++ ":i"
        ])
programmer (Just Teensy35) mcu hex = return
    ("teensy_loader_cli", \_ ->
        [ "--mcu=" ++ mcuStr mcu
        , "-v"
        , hex
        ])
programmer (Just Due) _ hex = do
    port <- fmap (fromMaybe "COM3") $ liftIO $ findPort 0x2341 0x003d
    liftIO $ withSerial port defaultSerialSettings { commSpeed = CS1200 } $ \port -> threadDelay 2000000
    return ("bossac", \_ ->
        [ "-i"
        , "--port=" ++ port
        , "-Ufalse"
        , "-e"
        , "-w"
        , "-v"
        , "-b"
        , hex
        , "-R"
        ])
programmer (Just b) _ _ = error $ "don't know how to program board: " ++ show b

avrdudeConf :: FilePath
avrdudeConf = "c:/Program Files (x86)/Arduino/hardware/tools/avr/etc/avrdude.conf"

reset :: Maybe Board -> MCU -> Action Tool
reset Nothing mcu
    | mcu `elem`
        [ STM32F051
        , STM32F072
        , STM32F103
        , STM32F411
        , STM32F412
        , STM32F767
        , STM32H743
        , STM32G070
        , STM32G431
        ]
    = return ("STM32_Programmer_CLI", \_ ->
        [ "--connect port=SWD mode=UR"
        , "-hardRst"
        ])
    | otherwise = error "don't know how to reset this mcu"
