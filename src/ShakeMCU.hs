import ShakeMCU.Config
import ShakeMCU.ToolChain
import ShakeMCU.Program
import ShakeMCU.USBSerial
import qualified ShakeMCU.AVR as AVR
import qualified ShakeMCU.ARM as ARM
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    usingConfigFile "build.mk"

    want [ buildDir </> "image" <.> "hex", buildDir </> "image" <.> "s" ]

    buildDir </> "image" <.> "elf" %> \out -> do
        cs <- filterGarbageFiles <$> getDirectoryFiles "" [ "//*.c" ]
        cpps <- filterGarbageFiles <$> getDirectoryFiles "" [ "//*.cpp" ]
        mcu <- getMCU
        let objs = [ buildDir </> c <.> "o" | c <- cs ++ cpps ]
--            libs = [ buildDir </> "mk64fx512.o" ] -- FIXME: lib / shared location and conditional on architecture and mcu
        need $ objs -- ++ libs
        (command, flags) <- fmap (ld . toolChain) getMCU
        () <- cmd command flags "-o" [ out ] objs -- libs -- FIXME: -l lib flags
        (command, flags) <- fmap (size . toolChain) getMCU
        cmd command flags [ out ]

    buildDir </> "image" <.> "hex" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        (command, flags) <- fmap (objcopy . toolChain) getMCU
        cmd command flags [ "-Oihex" , "-R.eeprom" ] [ elf ] [ out ]

    buildDir </> "image" <.> "s" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        (command, flags) <- fmap (objdump . toolChain) getMCU
        cmd (FileStdout out) command flags "-S" [ elf ]

    let compile tool out = do
        let src = dropExtension . dropDirectory1 $ out
            m = out -<.> "m"
        freq <- getF_CPU
        (command, flags) <- fmap (tool . toolChain) getMCU
        let include = [ "-I.." ]
        () <- cmd command flags
            [ "-c", "-g", "-O3", "-w" ] include
            ("-DF_CPU=" ++ show (round freq) ++ "L")
            [ src ] "-o" [ out ] "-MMD -MF" [ m ]
        needMakefileDependencies m

    buildDir <//> "*.c.o" %> compile cc
    buildDir <//> "*.cpp.o" %> compile cpp

    phony "upload" $ do
        let hex = buildDir </> "image" <.> "hex"
        need [ hex ]
        board <- getBoard
        mcu <- getMCU
        (command, flags) <- programmer board mcu hex
        cmd command flags

    phony "ports" $ liftIO $ usbSerials Nothing Nothing >>= mapM_ print

    phony "clean" $ do
        putNormal $ "Cleaning files in " ++ buildDir
        removeFilesAfter buildDir [ "//*" ]

toolChain :: MCU -> ToolChain
toolChain mcu = case arch mcu of
    AVR -> AVR.toolChain mcu
    ARM -> ARM.toolChain mcu

filterGarbageFiles :: [FilePath] -> [FilePath]
filterGarbageFiles = filter $ \p -> not $ any (`isPrefixOf` takeFileName p) ["#", ".#"]

buildDir :: FilePath
buildDir = "_build"

