module ShakeMCU (main) where

import Internal.Config
import Internal.ToolChain
import Internal.Program
import Internal.USBSerial
import qualified Internal.AVR as AVR
import qualified Internal.ARM as ARM
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util
import System.FilePath (takeBaseName)
import System.Directory (getCurrentDirectory)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    usingConfigFile "build.mk"

    want [ buildDir </> "image" <.> "s" ]

    buildDir </> "image" <.> "elf" %> \out -> do
        name <- fmap takeBaseName $ liftIO getCurrentDirectory
        cs <- filterGarbageFiles <$> getDirectoryFiles "" [ "//*.c" ]
        cpps <- filterGarbageFiles <$> getDirectoryFiles "" [ "//*.cpp" ]
        mcu <- getMCU
        let objs = [ buildDir </> name </> c <.> "o" | c <- cs ++ cpps ]
        libs <- (map (\l -> buildDir </> l </> l <.> "a")) <$> getLibs mcu
        need $ objs ++ libs
        (command, flags) <- ld . toolChain <$> getMCU
        () <- cmd command (flags $ objs ++ libs) "-o" [ out ]
        (command, flags) <- size . toolChain <$> getMCU
        cmd command (flags []) [ out ]

    buildDir </> "image" <.> "hex" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        (command, flags) <- objcopy . toolChain <$> getMCU
        cmd command (flags []) [ elf ] "-Oihex" [ out ]

    buildDir </> "image" <.> "bin" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        (command, flags) <- objcopy . toolChain <$> getMCU
        cmd command (flags []) [ elf ] "-Obinary" [ out ]

    buildDir </> "image" <.> "s" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        (command, flags) <- objdump . toolChain <$> getMCU
        cmd (FileStdout out) command (flags []) "-S" [ elf ]

    buildDir <//> "*.a" %> \out -> do
        let libName = takeBaseName out
        let libDir = ".." </> libName
        cs <- filterGarbageFiles <$> getDirectoryFiles libDir [ "//*.c" ]
        cpps <- filterGarbageFiles <$> getDirectoryFiles libDir [ "//*.cpp" ]
        let objs = [ buildDir </> libName </> c <.> "o" | c <- cs ++ cpps ]
        need objs
        (command, flags) <- ar . toolChain <$> getMCU
        cmd command (flags []) "rcs" out objs

    let compile tool out = do
            let src = ".." </> dropDirectory1 (dropExtension out)
                m = out -<.> "m"
            freq <- getF_CPU
            (command, flags) <- tool . toolChain <$> getMCU
            let include = [ "-I.." ]
            () <- cmd command (flags [])
                [ "-c", "-g", "-Werror", "-Wall", "-Os" ] include
                ("-DF_CPU=" ++ show (round freq) ++ "L")
                [ src ] "-o" [ out ] "-MMD -MF" [ m ]
            needMakefileDependencies m

    buildDir <//> "*.c.o" %> compile cc
    buildDir <//> "*.cpp.o" %> compile cpp

    phony "upload" $ do
        tc <- toolChain <$> getMCU
        let payload = case format tc of
                Hex -> buildDir </> "image" <.> "hex"
                Binary -> buildDir </> "image" <.> "bin"
        need [ payload ]
        board <- getBoard
        mcu <- getMCU
        (command, flags) <- programmer board mcu payload
        cmd command (flags [])

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

