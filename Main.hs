module Main where

import Control.Logging hiding (debug)
import Control.Monad hiding (sequence)
import Data.C2Hsc (C2HscOptions(..), runArgs)
import Data.List as L
import Prelude hiding (concat, sequence, mapM, mapM_, foldr)
import System.Console.CmdArgs
import System.Environment

version :: String
version = "0.7.0"

copyright :: String
copyright = "2012-2014"

c2hscSummary :: String
c2hscSummary = "c2hsc v" ++ version ++ ", (C) John Wiegley " ++ copyright

c2hscOptions :: C2HscOptions
c2hscOptions = C2HscOptions
    { gcc       = def &= typFile
                  &= help "Specify explicit path to gcc or cpp"
    , cppopts   = def &= typ "OPTS"
                  &= help "Pass OPTS to the preprocessor"
    , prefix    = def &= typ "PREFIX"
                  &= help "Use PREFIX when naming modules"
    , filePrefix = def &= typ "FILE_PREFIX"
                  &= help "Process included headers whose paths match this prefix"
    , overrides = def &= typFile
                  &= help "FILE contains \"C type -> FFI type\" translations"
    , verbose   = def &= name "v"
                  &= help "Report progress verbosely"
    , debug     = def &= name "D"
                  &= help "Report debug information"
    , files     = def &= args &= typFile } &=
    summary c2hscSummary &=
    program "c2hsc" &=
    help "Create an .hsc Bindings-DSL file from a C API header file"

------------------------------ IMPURE FUNCTIONS ------------------------------

-- Parsing of C headers begins with finding gcc so we can run the
-- preprocessor.

main :: IO ()
main = getArgs >>= \mainArgs -> do
  opts <- withArgs (if null mainArgs then ["--help"] else mainArgs)
          (cmdArgs c2hscOptions)
  withStderrLogging $ runArgs opts Nothing False
