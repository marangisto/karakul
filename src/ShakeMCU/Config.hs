module ShakeMCU.Config where

import ShakeMCU.ToolChain
import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Control.Applicative
import Data.Maybe (fromMaybe)

getBoard :: Action (Maybe Board)
getBoard = fmap read <$> getConfig "BOARD"

getMCU :: Action MCU
getMCU = do
    mcu <- fmap read <$> getConfig "MCU"
    board <- getBoard
    return $ fromMaybe (error "don't know how to determine MCU") $ mcu <|> fmap (fst . boardMCU) board

getF_CPU :: Action Freq
getF_CPU = do
    freq <- getConfig "F_CPU"
    board <- getBoard
    return $ fromMaybe 16e6 $ fmap read freq <|> fmap (snd . boardMCU) board

getLibs :: MCU -> Action [String]
getLibs mcu = fromMaybe (defLibs $ arch mcu) . fmap words <$> getConfig "LIBS"
    where defLibs AVR = [ "AVR" ]
          defLibs _ = []

getPort :: Action (Maybe FilePath)
getPort = getConfig "PORT"

getBootPort :: Action (Maybe FilePath)
getBootPort = getConfig "BOOT_PORT"

getManualBoot :: Action Bool
getManualBoot = fromMaybe False . fmap read <$> getConfig "MANUAL_BOOT"

