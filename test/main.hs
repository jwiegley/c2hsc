{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception
import Control.Logging
import Data.C2Hsc
import Data.Char
import Data.Monoid
import Data.String.Here
import Data.Text (Text, pack)
import Prelude hiding (log)
import Test.Hspec

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

main :: IO ()
main = withStdoutLogging $ hspec $ do
    describe "sanity tests" $ do
        it "maps fundamental types" $
            matches [here|
typedef int an_int;
|] [here|
{- typedef int an_int; -}
#synonym_t an_int , CInt
|]
        it "handles issue #12" $
            matches [here|
struct st {
  int i;
};

enum e {
  CONST
};

union u {
  char c;
};
|] [here|
{- struct st {
    int i;
}; -}
#starttype struct st
#field i , CInt
#stoptype
{- enum e {
    CONST
}; -}
#integral_t enum e
#num CONST
{- union u {
    char c;
}; -}
#starttype union u
#field c , CChar
#stoptype
|]

matches :: String -> String -> IO ()
matches input output = do
    res <- processString input
    trim res `shouldBe` output

tshow :: String -> Text
tshow = pack . show

trim :: String -> String
trim = trimTail . dropWhile isSpace

trimTail :: String -> String
trimTail "" = ""
trimTail s = take (lastNonBlank s) s
  where lastNonBlank = (+1) . fst . foldl acc (0, 0)
        acc (l, n) c | isSpace c = (l, n + 1)
                     | otherwise = (n, n + 1)
