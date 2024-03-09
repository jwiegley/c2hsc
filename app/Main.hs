module Main where

import Control.Logging hiding (debug)
import Control.Monad hiding (sequence)
import Data.C2Hsc (C2HscOptions(..), runArgs)
import Data.List (null)
import Data.Semigroup
import Data.Time.Calendar
import Data.Time.Clock
import Prelude hiding (concat, sequence, mapM, mapM_, foldr)
import System.Console.CmdArgs
import System.Environment


version :: String
version = "0.8.0"

copyright :: Integer -> String
copyright year = "2012-" <> (show year)

c2hscSummary :: Integer -> String
c2hscSummary year = "c2hsc v" <> version <> ", (C) John Wiegley " <> (copyright year)

c2hscOptions :: Integer -> C2HscOptions
c2hscOptions year = C2HscOptions
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
    summary (c2hscSummary year) &=
    program "c2hsc" &=
    help "Create an .hsc Bindings-DSL file from a C API header file"

------------------------------ IMPURE FUNCTIONS ------------------------------

-- Parsing of C headers begins with finding gcc so we can run the
-- preprocessor.

main :: IO ()
main = getArgs >>= \mainArgs -> do
  (year, _, _) <- getCurrentTime >>= pure . toGregorian . utctDay
  opts <- withArgs (if null mainArgs then ["--help"] else mainArgs)
          (cmdArgs $ c2hscOptions year)
  withStderrLogging $ runArgs opts Nothing False
