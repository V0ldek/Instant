module Llvm.Emit where

newtype RegVal = RegVal Int

-- Locations in LLVM that can be either registers
-- or a constant value (since constants cannot be put into a register).
data LlvmParam = Const Int | Reg RegVal

instance Show RegVal where
  show (RegVal n) = "%t" ++ show n

instance Show LlvmParam where
  show x = case x of
    Const n -> show n
    Reg reg -> show reg

-- Indent code by two spaces.
ind :: String -> String
ind s = "  " ++ s

-- Emit the definition of the printInt runtime function.
printIntDefinition :: [String]
printIntDefinition =
  [ "declare i32 @printf(i8*, ...)",
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
    "",
    "define void @printInt(i32 %x) {",
    ind "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
    ind "call i32 (i8*, ...) @printf(i8* %t0, i32 %x) ",
    ind "ret void",
    "}"
  ]

mainHeader :: [String]
mainHeader = ["define i32 @main() {"]

mainEnd :: [String]
mainEnd =
  [ ind "ret i32 0",
    "}"
  ]

printInt :: LlvmParam -> [String]
printInt param = ind <$> ["call void @printInt(i32 " ++ show param ++ ")"]

addInts :: RegVal -> LlvmParam -> LlvmParam -> [String]
addInts reg param1 param2 = ind <$> [show reg ++ " = add i32 " ++ show param1 ++ ", " ++ show param2]

subInts :: RegVal -> LlvmParam -> LlvmParam -> [String]
subInts reg param1 param2 = ind <$> [show reg ++ " = sub i32 " ++ show param1 ++ ", " ++ show param2]

mulInts :: RegVal -> LlvmParam -> LlvmParam -> [String]
mulInts reg param1 param2 = ind <$> [show reg ++ " = mul i32 " ++ show param1 ++ ", " ++ show param2]

divInts :: RegVal -> LlvmParam -> LlvmParam -> [String]
divInts reg param1 param2 = ind <$> [show reg ++ " = sdiv i32 " ++ show param1 ++ ", " ++ show param2]

allocInt :: RegVal -> [String]
allocInt reg = ind <$> [show reg ++ " = alloca i32, align 4"]

storeInt :: RegVal -> LlvmParam -> [String]
storeInt reg param = ind <$> ["store i32 " ++ show param ++ ", i32* " ++ show reg]

loadInt :: RegVal -> RegVal -> [String]
loadInt reg src = ind <$> [show reg ++ " = load i32, i32* " ++ show src]
