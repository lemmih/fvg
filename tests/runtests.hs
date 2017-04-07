module Main where

import AbsSyn
import Parser
import Interpreter

import System.Directory
import System.FilePath
import System.Environment
import Control.Exception
import Control.Monad
import Text.Printf
import Data.List

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      inp <- readFile file
      let outFile = replaceExtension file "out"
      expr <- evaluate (runScript inp)
      writeFile outFile (show expr)
    _ -> do
      files <- getDirectoryContents "."
      forM_ (sort files) $ \file ->
        when (takeExtension file == ".fvg") $ do
          inp <- readFile file
          let outFile = replaceExtension file "out"
          hasOut <- doesFileExist outFile
          if hasOut
            then do
              out <- readFile outFile
              ret <- try $ evaluate (runScript inp)
              case ret :: Either SomeException Expr of
                Left{}  -> unexpectedFailure file
                Right expr
                  | show expr == out -> ok file
                  | otherwise        -> mismatch file
            else do
              ret <- try $ evaluate (runScript inp)
              case ret :: Either SomeException Expr of
                Left{}  -> ok file
                Right{} -> expectedFailure file

ok file = printf "%15s: [OK]\n" (takeBaseName file)
expectedFailure file = printf "%15s: [BAD, EXPECTED FAILURE]\n" (takeBaseName file)
unexpectedFailure file = printf "%15s: [BAD, UNEXPECTED FAILURE]\n" (takeBaseName file)
mismatch file = printf "%15s: [BAD, OUTPUT MISMATCH]\n" (takeBaseName file)
