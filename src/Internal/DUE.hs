{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Internal.DUE (main) where

import System.Console.CmdArgs
import System.Hardware.Serialport
import Control.Concurrent

data Options = Options
    { port :: FilePath
    } deriving (Show, Data, Typeable)

main :: IO ()
main = do
    Options{..} <- cmdArgs $ Options "COM6"
    withSerial port defaultSerialSettings { commSpeed = CS1200 } $ \port -> run port 0
 
run port i = do
    threadDelay 10000

