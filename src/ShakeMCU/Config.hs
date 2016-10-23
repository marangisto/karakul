module ShakeMCU.Config where

import ShakeMCU.ToolChain
import Development.Shake
import Development.Shake.Config
import Control.Applicative
import Data.Maybe (fromMaybe)

getBoard :: Action (Maybe Board)
getBoard = fmap (fmap read) $ getConfig "BOARD"

getMCU :: Action MCU
getMCU = do
    mcu <- fmap (fmap read) $ getConfig "MCU"
    board <- getBoard
    return $ fromMaybe (error "don't know how to determine MCU") $ mcu <|> fmap (fst . boardMCU) board

getF_CPU :: Action Freq
getF_CPU = do
    freq <- getConfig "F_CPU"
    board <- getBoard
    return $ fromMaybe 16e6 $ fmap read freq <|> fmap (snd . boardMCU) board

getPort :: Action (Maybe FilePath)
getPort = getConfig "PORT"

getBootPort :: Action (Maybe FilePath)
getBootPort = getConfig "BOOT_PORT"

getManualBoot :: Action Bool
getManualBoot = fmap (fromMaybe False . fmap read) $ getConfig "MANUAL_BOOT"

