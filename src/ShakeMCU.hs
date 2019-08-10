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
import System.Directory as D (getCurrentDirectory, doesFileExist)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, isInfixOf)

main :: IO ()
main = do
  let configFile = "build.mk"
  exists <- D.doesFileExist configFile
  if exists then shakeArgs shakeOptions{ shakeFiles = buildDir } $ do
    usingConfigFile configFile
    let getBaseDir = fromMaybe "../.." <$> getConfig "BASE_DIR"

    want [ buildDir </> "image" <.> "s" ]

    buildDir </> "image" <.> "elf" %> \out -> do
        name <- fmap takeBaseName $ liftIO getCurrentDirectory
        cs <- filterGarbageFiles <$> getDirectoryFiles "" [ "//*.c" ]
        cpps <- filterGarbageFiles <$> getDirectoryFiles "" [ "//*.cpp" ]
        asms <- filterGarbageFiles <$> getDirectoryFiles "" [ "//*.asm" ]
        mcu <- getMCU
        let objs = [ buildDir </> name </> c <.> "o" | c <- cs ++ cpps ++ asms ]
        libs <- (map (\l -> buildDir </> l </> l <.> "a")) <$> getLibs mcu
        need $ objs ++ libs
        baseDir <- getBaseDir
        (command, flags) <- ld . toolChain baseDir <$> getMCU
        () <- cmd command (flags $ objs ++ libs) "-o" [ out ]
        (command, flags) <- size . toolChain baseDir <$> getMCU
        cmd command (flags []) [ out ]

    buildDir </> "image" <.> "hex" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        baseDir <- getBaseDir
        (command, flags) <- objcopy . toolChain baseDir <$> getMCU
        cmd command (flags []) [ elf ] "-Oihex" [ out ]

    buildDir </> "image" <.> "bin" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        baseDir <- getBaseDir
        (command, flags) <- objcopy . toolChain baseDir <$> getMCU
        cmd command (flags []) [ elf ] "-Obinary" [ out ]

    buildDir </> "image" <.> "s" %> \out -> do
        let elf = out -<.> ".elf"
        need [ elf ]
        baseDir <- getBaseDir
        (command, flags) <- objdump . toolChain baseDir <$> getMCU
        cmd (FileStdout out) command (flags []) "-S" [ elf ]

    buildDir <//> "*.a" %> \out -> do
        baseDir <- getBaseDir
        let libName = takeBaseName out
        let libDir = baseDir </> libName </> "src"
        cs <- filterGarbageFiles <$> getDirectoryFiles libDir [ "/*.c" ]
        cpps <- filterGarbageFiles <$> getDirectoryFiles libDir [ "/*.cpp" ]
        let objs = [ buildDir </> libName </> c <.> "o" | c <- cs ++ cpps ]
        need objs
        (command, flags) <- ar . toolChain baseDir <$> getMCU
        cmd command (flags []) "rcs" out objs

    let compile tool out = do
            baseDir <- getBaseDir
            this <- fmap takeBaseName $ liftIO getCurrentDirectory
            let src | [ buildDir, lib, name ] <- splitDirectories out
                    , lib /= this
                    = baseDir </> lib </> "src" </> dropExtension name
                    | otherwise = ".." </> dropDirectory1 (dropExtension out)
                m = out -<.> "m"
            freq <- getF_CPU
            (command, flags) <- tool . toolChain baseDir <$> getMCU
            libs <- getLibs =<< getMCU
            defs <- getDefs
            let include = [ "-I" <> baseDir </> lib </> "include" | lib <- libs ] ++ [ "-I." ]
                define = [ "-D" <> def | def <- defs ]
            () <- cmd command (flags [])
                [ "-c", "-g", "-Werror", "-Wall", "-Os" ] include define
                ("-DF_CPU=" ++ show (round freq) ++ "L")
                [ src ] "-o" [ out ] "-MMD -MF" [ m ]
                ("-Wa,-adhln=" ++ out -<.> "s")
            needMakefileDependencies m

    buildDir <//> "*.c.o" %> compile cc
    buildDir <//> "*.cpp.o" %> compile cpp
    buildDir <//> "*.asm.o" %> compile asm

    phony "upload" $ do
        baseDir <- getBaseDir
        tc <- toolChain baseDir <$> getMCU
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
  else
    putStrLn $ "no " <> configFile <> " present, bailing out..."


toolChain :: FilePath -> MCU -> ToolChain
toolChain baseDir mcu = case arch mcu of
    AVR -> AVR.toolChain baseDir mcu
    ARM -> ARM.toolChain baseDir mcu

filterGarbageFiles :: [FilePath] -> [FilePath]
filterGarbageFiles = filter $ \p -> not $ any (`isPrefixOf` takeFileName p) ["#", ".#"]

buildDir :: FilePath
buildDir = "_build"

