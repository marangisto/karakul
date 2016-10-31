module ShakeMCU.Program where

import ShakeMCU.Config
import ShakeMCU.ToolChain
import ShakeMCU.USBSerial
import Development.Shake
import Control.Applicative
import Data.Maybe (fromMaybe)
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad

programmer :: Maybe Board -> MCU -> FilePath -> Action Tool
programmer Nothing mcu hex = return
    ("atprogram",
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
    return ("avrdude",
        [ "-c" ++ "arduino"
        , "-p" ++ mcuStr mcu
        , "-P" ++ port
        , "-b" ++ "115200"
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
    return ("avrdude",
        [ "-c" ++ "avr109"
        , "-p" ++ mcuStr mcu
        , "-P" ++ port
        , "-b" ++ "57600"
        , "-D"
        , "-Uflash:w:" ++ hex ++ ":i"
        ])
programmer (Just TrinketPro) mcu hex = return
    ("avrdude",
        [ "-c" ++ "usbtiny"
        , "-p" ++ mcuStr mcu
        , "-D"
        , "-Uflash:w:" ++ hex ++ ":i"
        ])
programmer (Just Teensy35) mcu hex = return
    ("teensy_loader_cli",
        [ "--mcu=" ++ mcuStr mcu
        , "-v"
        , hex
        ])
programmer (Just b) _ _ = error $ "don't know how to program board: " ++ show b

