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
        it "handles issue #15" $ do
            matches [here|
struct MyTypeImpl;
typedef struct MyTypeImpl* MyType;

typedef struct MyStruct {
  int x;
} MyStructType;

typedef struct MyStructEmpty MyStructEmptyType;
|] [here|
{- struct MyTypeImpl; -}
#opaque_t struct MyTypeImpl
{- typedef struct MyTypeImpl * MyType; -}
#synonym_t MyType , <struct MyTypeImpl>
{- typedef struct MyStruct {
            int x;
        } MyStructType; -}
#starttype struct MyStruct
#field x , CInt
#stoptype
#synonym_t MyStructType , <struct MyStruct>
{- typedef struct MyStructEmpty MyStructEmptyType; -}
#opaque_t struct MyStructEmpty
#synonym_t MyStructEmptyType , <struct MyStructEmpty>
|]

        it "primitive type: float" $ do
            matches [here|
float ordinary_float;
|] [here|
#globalvar ordinary_float , CFloat
|]
        it "primitive type: double" $ do
            matches [here|
double ordinary_double;
|] [here|
#globalvar ordinary_double , CDouble
|]
        -- test disabled until https://ghc.haskell.org/trac/ghc/ticket/3353 is resolved.
        -- 
        -- it "primitive type: long double" $ do
        --     matches [here|
        -- long double ordinary_long_double;
        -- |] [here|
        -- #globalvar ordinary_long_double , CLongDouble
        -- |]
        it "primitive type: char" $ do
            matches [here|
char ordinary_signed_char;
|] [here|
#globalvar ordinary_signed_char , CChar
|]
        it "primitive type: unsigned char" $ do
            matches [here|
unsigned char unsigned_char;
|] [here|
#globalvar unsigned_char , CUChar
|]
        it "primitive type: short" $ do
            matches [here|
short ordinary_signed_short;
|] [here|
#globalvar ordinary_signed_short , CShort
|]
        it "primitive type: signed short" $ do
            matches [here|
signed short explicit_signed_short;
|] [here|
#globalvar explicit_signed_short , CShort
|]
        it "primitive type: unsigned short" $ do
            matches [here|
unsigned short unsigned_short;
|] [here|
#globalvar unsigned_short , CUShort
|]
        it "primitive type: int" $ do
            matches [here|
int ordinary_signed_int;
|] [here|
#globalvar ordinary_signed_int , CInt
|]
        it "primitive type: signed int" $ do
            matches [here|
signed int explicit_signed_int;
|] [here|
#globalvar explicit_signed_int , CInt
|]
        it "primitive type: unsigned int" $ do
            matches [here|
unsigned int unsigned_int;
|] [here|
#globalvar unsigned_int , CUInt
|]
        it "primitive type: long" $ do
            matches [here|
long ordinary_signed_long;
|] [here|
#globalvar ordinary_signed_long , CLong
|]
        it "primitive type: signed long" $ do
            matches [here|
signed long explicit_signed_long;
|] [here|
#globalvar explicit_signed_long , CLong
|]
        it "primitive type: unsigned long" $ do
            matches [here|
unsigned long unsigned_long;
|] [here|
#globalvar unsigned_long , CULong
|]
        it "primitive type: long long" $ do
            matches [here|
long long ordinary_signed_long_long;
|] [here|
#globalvar ordinary_signed_long_long , CLlong
|]
        it "primitive type: signed long long" $ do
            matches [here|
signed long long explicit_signed_long_long;
|] [here|
#globalvar explicit_signed_long_long , CLlong
|]
        it "primitive type: unsigned long long" $ do
            matches [here|
unsigned long long unsigned_long_long;
|] [here|
#globalvar unsigned_long_long , CULlong
|]
-- // pointers
--   // primitive types which cannot be signed
--     void* ordinary_void_pointer;
--     bool* ordinary_bool_pointer;
--     float* ordinary_float_pointer;
--     double* ordinary_double_pointer;
--     long double* ordinary_long_double_pointer;
--   // types which can be signed
--     // char
--       char *ordinary_signed_char_pointer;
--       signed char *explicit_signed_char_pointer;
--       unsigned char *unsigned_char_pointer;
--     // short
--       short *ordinary_signed_short_pointer;
--       signed short *explicit_signed_short_pointer;
--       unsigned short *unsigned_short_pointer;
--     // int
--       int* ordinary_signed_int_pointer;
--       signed int* explicit_signed_int_pointer;
--       unsigned int* unsigned_int_pointer;
--     // long
--       long *ordinary_signed_long_pointer;
--       signed long *explicit_signed_long_pointer;
--       unsigned long *unsigned_long_pointer;
--     // long long
--       long long* ordinary_signed_long_long_pointer;
--       signed long long* explicit_signed_long_long_pointer;
--       unsigned long long* unsigned_long_long_pointer;
-- // arrays
--   // primitive types which cannot be signed
--     //void ordinary_void_array[10];
--     bool ordinary_bool_array[10];
--     float ordinary_float_array[10];
--     double ordinary_double_array[10];
--     long double ordinary_long_double_array[10];
--   // types which can be signed
--     // char
--       char ordinary_signed_char_array[10];
--       signed char explicit_signed_char_array[10];
--       unsigned char unsigned_char_array[10];
--     // short
--       short ordinary_signed_short_array[10];
--       signed short explicit_signed_short_array[10];
--       unsigned short unsigned_short_array[10];
--     // int
--       int ordinary_signed_int_array[10];
--       signed int explicit_signed_int_array[10];
--       unsigned int unsigned_int_array[10];
--     // long
--       long ordinary_signed_long_array[10];
--       signed long explicit_signed_long_array[10];
--       unsigned long unsigned_long_array[10];
--     // long long
--       long long ordinary_signed_long_long_array[10];
--       signed long long explicit_signed_long_long_array[10];
--       unsigned long long unsigned_long_long_array[10];
--   // pointers
--     // primitive types which cannot be signed
--       void* ordinary_void_pointer_array[10];
--       bool* ordinary_bool_pointer_array[10];
--       float* ordinary_float_pointer_array[10];
--       double* ordinary_double_pointer_array[10];
--       long double* ordinary_long_double_pointer_array[10];
--     // types which can be signed
--       // char
--         char *ordinary_signed_char_pointer_array[10];
--         signed char *explicit_signed_char_pointer_array[10];
--         unsigned char *unsigned_char_pointer_array[10];
--       // short
--         short *ordinary_signed_short_pointer_array[10];
--         signed short *explicit_signed_short_pointer_array[10];
--         unsigned short *unsigned_short_pointer_array[10];
--       // int
--         int* ordinary_signed_int_pointer_array[10];
--         signed int* explicit_signed_int_pointer_array[10];
--         unsigned int* unsigned_int_pointer_array[10];
--       // long
--         long *ordinary_signed_long_pointer_array[10];
--         signed long *explicit_signed_long_pointer_array[10];
--         unsigned long *unsigned_long_pointer_array[10];
--       // long long
--         long long* ordinary_signed_long_long_pointer_array[10];
--         signed long long* explicit_signed_long_long_pointer_array[10];
--         unsigned long long* unsigned_long_long_pointer_array[10];
-- // structs
--   // primitive types which cannot be signed
--     //void ordinary_void;
--     struct ordinary_bool_struct {bool ordinary_bool_member;};
--     struct ordinary_float_struct {float ordinary_float_member;};
--     struct ordinary_double_struct {double ordinary_double_member;};
--     struct ordinary_long_double_struct {long double ordinary_long_double_member;};
--   // types which can be signed
--     // char
--       struct ordinary_signed_char_struct {char ordinary_signed_char_member;};
--       struct explicit_signed_char_struct {signed char explicit_signed_char_member;};
--       struct unsigned_char_struct {unsigned char unsigned_char_member;};
--     // short
--       struct ordinary_signed_short_struct {short ordinary_signed_short_member;};
--       struct explicit_signed_short_struct {signed short explicit_signed_short_member;};
--       struct unsigned_short_struct {unsigned short unsigned_short_member;};
--     // int
--       struct ordinary_signed_int_struct {int ordinary_signed_int_member;};
--       struct explicit_signed_int_struct {signed int explicit_signed_int_member;};
--       struct unsigned_int_struct {unsigned int unsigned_int_member;};
--     // long
--       struct ordinary_signed_long_struct {long ordinary_signed_long_member;};
--       struct explicit_signed_long_struct {signed long explicit_signed_long_member;};
--       struct unsigned_long_struct {unsigned long unsigned_long_member;};
--     // long long
--       struct ordinary_signed_long_long_struct {long long ordinary_signed_long_long_member;};
--       struct explicit_signed_long_long_struct {signed long long explicit_signed_long_long_member;};
--       struct unsigned_long_long_struct {unsigned long long unsigned_long_long_member;};
--   // pointers
--     // primitive types which cannot be signed
--       struct ordinary_void_pointer_struct {void* ordinary_void_pointer_member;};
--       struct ordinary_bool_pointer_struct {bool* ordinary_bool_pointer_member;};
--       struct ordinary_float_pointer_struct {float* ordinary_float_pointer_member;};
--       struct ordinary_double_pointer_struct {double* ordinary_double_pointer_member;};
--       struct ordinary_long_double_pointer_struct {long double* ordinary_long_double_pointer_member;};
--     // types which can be signed
--       // char
--         struct ordinary_signed_char_pointer_struct {char *ordinary_signed_char_pointer_member;};
--         struct explicit_signed_char_pointer_struct {signed char *explicit_signed_char_pointer_member;};
--         struct unsigned_char_pointer_struct {unsigned char *unsigned_char_pointer_member;};
--       // short
--         struct ordinary_signed_short_pointer_struct {short *ordinary_signed_short_pointer_member;};
--         struct explicit_signed_short_pointer_struct {signed short *explicit_signed_short_pointer_member;};
--         struct unsigned_short_pointer_struct {unsigned short *unsigned_short_pointer_member;};
--       // int
--         struct ordinary_signed_int_pointer_struct {int* ordinary_signed_int_pointer_member;};
--         struct explicit_signed_int_pointer_struct {signed int* explicit_signed_int_pointer_member;};
--         struct unsigned_int_pointer_struct {unsigned int* unsigned_int_pointer_member;};
--       // long
--         struct ordinary_signed_long_pointer_struct {long *ordinary_signed_long_pointer_member;};
--         struct explicit_signed_long_pointer_struct {signed long *explicit_signed_long_pointer_member;};
--         struct unsigned_long_pointer_struct {unsigned long *unsigned_long_pointer_member;};
--       // long long
--         struct ordinary_signed_long_long_pointer_struct {long long* ordinary_signed_long_long_pointer_member;};
--         struct explicit_signed_long_long_pointer_struct {signed long long* explicit_signed_long_long_pointer_member;};
--         struct unsigned_long_long_pointer_struct {unsigned long long* unsigned_long_long_pointer_member;};
--   // arrays
--     // primitive types which cannot be signed
--       //struct ordinary_void_array_struct {void ordinary_void_array_member[10];};
--       struct ordinary_bool_array_struct {bool ordinary_bool_array_member[10];};
--       struct ordinary_float_array_struct {float ordinary_float_array_member[10];};
--       struct ordinary_double_array_struct {double ordinary_double_array_member[10];};
--       struct ordinary_long_double_array_struct {long double ordinary_long_double_array_member[10];};
--     // types which can be signed
--       // char
--         struct ordinary_signed_char_array_struct {char ordinary_signed_char_array_member[10];};
--         struct explicit_signed_char_array_struct {signed char explicit_signed_char_array_member[10];};
--         struct unsigned_char_array_struct {unsigned char unsigned_char_array_member[10];};
--       // short
--         struct ordinary_signed_short_array_struct {short ordinary_signed_short_array_member[10];};
--         struct explicit_signed_short_array_struct {signed short explicit_signed_short_array_member[10];};
--         struct unsigned_short_array_struct {unsigned short unsigned_short_array_member[10];};
--       // int
--         struct ordinary_signed_int_array_struct {int ordinary_signed_int_array_member[10];};
--         struct explicit_signed_int_array_struct {signed int explicit_signed_int_array_member[10];};
--         struct unsigned_int_array_struct {unsigned int unsigned_int_array_member[10];};
--       // long
--         struct ordinary_signed_long_array_struct {long ordinary_signed_long_array_member[10];};
--         struct explicit_signed_long_array_struct {signed long explicit_signed_long_array_member[10];};
--         struct unsigned_long_array_struct {unsigned long unsigned_long_array_member[10];};
--       // long long
--         struct ordinary_signed_long_long_array_struct {long long ordinary_signed_long_long_array_member[10];};
--         struct explicit_signed_long_long_array_struct {signed long long explicit_signed_long_long_array_member[10];};
--         struct unsigned_long_long_array_struct {unsigned long long unsigned_long_long_array_member[10];};
--     // pointers
--       // primitive types which cannot be signed
--         struct ordinary_void_pointer_array_struct {void* ordinary_void_pointer_array_member[10];};
--         struct ordinary_bool_pointer_array_struct {bool* ordinary_bool_pointer_array_member[10];};
--         struct ordinary_float_pointer_array_struct {float* ordinary_float_pointer_array_member[10];};
--         struct ordinary_double_pointer_array_struct {double* ordinary_double_pointer_array_member[10];};
--         struct ordinary_long_double_pointer_array_struct {long double* ordinary_long_double_pointer_array_member[10];};
--       // types which can be signed
--         // char
--           struct ordinary_signed_char_pointer_array_struct {char *ordinary_signed_char_pointer_array_member[10];};
--           struct explicit_signed_char_pointer_array_struct {signed char *explicit_signed_char_pointer_array_member[10];};
--           struct unsigned_char_pointer_array_struct {unsigned char *unsigned_char_pointer_array_member[10];};
--         // short
--           struct ordinary_signed_short_pointer_array_struct {short *ordinary_signed_short_pointer_array_member[10];};
--           struct explicit_signed_short_pointer_array_struct {signed short *explicit_signed_short_pointer_array_member[10];};
--           struct unsigned_short_pointer_array_struct {unsigned short *unsigned_short_pointer_array_member[10];};
--         // int
--           struct ordinary_signed_int_pointer_array_struct {int* ordinary_signed_int_pointer_array_member[10];};
--           struct explicit_signed_int_pointer_array_struct {signed int* explicit_signed_int_pointer_array_member[10];};
--           struct unsigned_int_pointer_array_struct {unsigned int* unsigned_int_pointer_array_member[10];};
--         // long
--           struct ordinary_signed_long_pointer_array_struct {long *ordinary_signed_long_pointer_array_member[10];};
--           struct explicit_signed_long_pointer_array_struct {signed long *explicit_signed_long_pointer_array_member[10];};
--           struct unsigned_long_pointer_array_struct {unsigned long *unsigned_long_pointer_array_member[10];};
--         // long long
--           struct ordinary_signed_long_long_pointer_array_struct {long long* ordinary_signed_long_long_pointer_array_member[10];};
--           struct explicit_signed_long_long_pointer_array_struct {signed long long* explicit_signed_long_long_pointer_array_member[10];};
--           struct unsigned_long_long_pointer_array_struct {unsigned long long* unsigned_long_long_pointer_array_member[10];};

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
