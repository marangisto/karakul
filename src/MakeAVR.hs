import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util
import Data.Maybe (fromMaybe)
import System.Hardware.Serialport
import Control.Applicative
import Control.Concurrent

ccflags =
    [ "-c"
    , "-g"
    , "-O3"
    , "-w"
    , "-std=c++11"
    , "-fno-exceptions"
    , "-ffunction-sections"
    , "-fdata-sections"
    , "-fno-threadsafe-statics"
    , "-MMD"
    , "-I.."
    ]

ldflags =
    [ "-Os"
    , "-Wl,--gc-sections"
    ]

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    usingConfigFile "build.mk"

    want [ buildDir </> "image" <.> "hex", buildDir </> "image" <.> "s" ]

    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir [ "//*" ]

    buildDir </> "image" <.> "elf" %> \out -> do
        cs <- getDirectoryFiles "" [ "//*.c", "//*.cpp" ]
        mcu <- getMCU
        let os = [ buildDir </> c -<.> "o" | c <- cs ]
        need os
        cmd "avr-g++" ldflags ("-mmcu=" ++ mcu) "-o" [ out ] os

    buildDir </> "image" <.> "hex" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        cmd "avr-objcopy" [ "-Oihex" , "-R.eeprom" ] [ elf ] [ out ]

    buildDir </> "image" <.> "s" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        Stdout res <- cmd "avr-objdump" "-S" [ elf ]
        writeFile' out res

    buildDir </> "//*.o" %> \out -> do
        let c = dropDirectory1 $ out -<.> "cpp"
            m = out -<.> "m"
        mcu <- getMCU
        freq <- getF_CPU
        putNormal $ "MCU=" ++ mcu ++ ", F_CPU=" ++ show freq
        () <- cmd "avr-g++" ccflags
            ("-mmcu=" ++ mcu) ("-DF_CPU=" ++ show (round freq) ++ "L")
            [ c ] "-o" [ out ] "-MMD -MF" [ m ]
        needMakefileDependencies m

    phony "upload" $ do
        let hex = buildDir </> "image" <.> "hex"
        need [ hex ]
        mcu <- getMCU
        port <- fmap (fromMaybe "COM3") $ getConfig "PORT"
        board <- getConfig "BOARD"
        case board of
            Nothing -> cmd "atprogram"
                [ "-t", "avrispmk2", "-d", mcu, "-i", "isp" ]
                [ "program", "-c", "--verify", "-f", hex ]
            Just "uno" -> cmd "avrdude"
                [ "-c" ++ "arduino", "-p" ++ mcu, "-P" ++ port ]
                [ "-b" ++ "115200", "-D" ]
                ("-Uflash:w:" ++ hex ++ ":i")
            Just "leonardo" -> do
                port <- liftIO $ leonardoBootPort port
                cmd "avrdude"
                    [ "-c" ++ "avr109", "-p" ++ mcu, "-P" ++ port ]
                    [ "-b" ++ "57600", "-D" ]
                    ("-Uflash:w:" ++ hex ++ ":i")
            Just "trinket-pro" -> cmd "avrdude"
                [ "-c" ++ "usbtiny", "-p" ++ mcu, "-D" ]
                ("-Uflash:w:" ++ hex ++ ":i")
            Just b -> error $ "don't know how to program BOARD: " ++ b

leonardoBootPort :: FilePath -> IO FilePath
leonardoBootPort port = do
    putStrLn $ "resetting " ++ port
    closeSerial =<<  openSerial port defaultSerialSettings { commSpeed = CS1200 }
    threadDelay 4000000 -- FIXME: wait for device change
    return "COM4" -- FIXME: look at device changes

buildDir = "_build"

getMCU = do
    mcu <- getConfig "MCU"
    board <- getConfig "BOARD"
    return $ fromMaybe (error "don't know how to determine MCU") $ mcu <|> (f =<< board)
    where f = fmap fst . flip lookup boards

getF_CPU = do
    freq <- getConfig "F_CPU"
    board <- getConfig "BOARD"
    return $ fromMaybe 16e6 $ fmap read freq <|> (f =<< board)
    where f = fmap snd . flip lookup boards

getProgrammer = fmap (fromMaybe "avrispmk2") $ getConfig "PROGRAMMER"

boards =
    [ ("uno",         ("atmega328p",   16e6))
    , ("leonardo",    ("atmega32u4",   16e6))
    , ("trinket-pro", ("atmega328p",   16e6))
    ]

