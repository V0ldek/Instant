module Jvm.Emit where

import           Data.Int (Int32)

-- Indent code by two spaces.
ind :: String -> String
ind s = "  " ++ s

classHeader :: String -> [String]
classHeader name =
  [ ".class public " ++ name,
    ".super java/lang/Object"
  ]

defaultCtor :: [String]
defaultCtor =
  [ ".method public <init>()V",
    ind ".limit stack 1",
    ind ".limit locals 1",
    ind "aload_0",
    ind "invokespecial java/lang/Object/<init>()V",
    ind "return",
    ".end method"
  ]

mainHeader :: Int -> Int -> [String]
mainHeader localCount stackLimit =
  [ ".method public static main([Ljava/lang/String;)V",
    ind ".limit stack " ++ show (max stackLimit 1),
    ind ".limit locals " ++ show (max localCount 1)
  ]

endMain :: [String]
endMain =
  [ ind "return",
    ".end method"
  ]

printInt :: [String]
printInt = ind <$> ["invokestatic Runtime/printInt(I)V"]

loadInt :: Int32 -> [String]
loadInt n =
  ind <$> case n of
    -1                             -> ["iconst_m1"]
    n | n >= 0 && n <= 5           -> ["iconst_" ++ show n]
    n | n >= lbyte && n <= ubyte   -> ["bipush " ++ show n]
    n | n >= lshort && n <= ushort -> ["sipush " ++ show n]
    n                              -> ["ldc " ++ show n]
  where
    lbyte = - (2 ^ 7)
    ubyte = 2 ^ 7 - 1
    lshort = - (2 ^ 15)
    ushort = 2 ^ 15 - 1

store :: Int -> [String]
store loc =
  ind <$> case loc of
    n | n >= 0 && n <= 3 -> ["istore_" ++ show n]
    n                    -> ["istore " ++ show n]

load :: Int -> [String]
load loc =
  ind <$> case loc of
    n | n >= 0 && n <= 3 -> ["iload_" ++ show n]
    n                    -> ["iload " ++ show n]

add :: [String]
add = ind <$> ["iadd"]

sub :: [String]
sub = ind <$> ["isub"]

mul :: [String]
mul = ind <$> ["imul"]

div :: [String]
div = ind <$> ["idiv"]
