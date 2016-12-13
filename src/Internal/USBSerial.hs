{-# LANGUAGE RecordWildCards #-}
module Internal.USBSerial (USBSerial(..), findPort, usbSerials) where

--import System.Win32.Registry (hKEY_LOCAL_MACHINE, regOpenKey, regCloseKey, regQueryValue, regQueryValueEx)
--import System.Win32.Types (DWORD, HKEY)
import Control.Exception (handle, bracket, SomeException(..))
import Foreign (toBool, Storable(peek, sizeOf), castPtr, alloca) 
import Data.List.Split (splitOn)
import Data.List (stripPrefix)
import Numeric (readHex, showHex)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad (forM)

data USBSerial = USBSerial
    { key           :: String
    , vendorId      :: Int
    , productId     :: Int
    , portName      :: String
    , friendlyName  :: String
    }

instance Show USBSerial where
    show USBSerial{..} = unwords [ portName, toHex vendorId, toHex productId, friendlyName ]
        where toHex x = let s = showHex x "" in replicate (4 - length s) '0' ++ s

findPort :: Int -> Int -> IO (Maybe String)
findPort vendorId productId = fmap (fmap portName . listToMaybe) $ usbSerials (Just vendorId) (Just productId)

usbSerials :: Maybe Int -> Maybe Int -> IO [USBSerial]
usbSerials mVendorId mProductId = undefined

keyToVidPid :: String -> Maybe (Int, Int)
keyToVidPid name
    | (_:s:_) <- splitOn "\\" name
    , (v:p:_) <- splitOn "&" s
    , Just v <- fromHex =<< stripPrefix "VID_" v
    , Just p <- fromHex =<< stripPrefix "PID_" p = Just (v, p)
    | otherwise = Nothing
    where fromHex s = case readHex s of
            [(x, "")] -> Just x
            _         -> Nothing

