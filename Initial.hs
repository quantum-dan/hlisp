-- Definitions for the compiled code
-- Some repetition compared to the other modules, since this is supposed to be standalone with the compiler
module Initial where

-- Lisp data

data LispData =
  LispInt Integer
  | LispPair LispData LispData
  | LispUnit
  | LispString String
  | LispSymbol String
  | LispFloat Double
  | LispFraction Integer Integer
  | LispLambda ([LispData] -> LispData)
  | LispBool Bool

instance Eq LispData where
  (LispInt x) == (LispInt y) = x == y
  LispUnit == LispUnit = True
  (LispPair x y) == (LispPair a b) = x == a && y == b
  (LispString x) == (LispString y) = x == y
  (LispSymbol x) == (LispSymbol y) = x == y
  (LispFloat x) == (LispFloat y) = x == y
  (LispFraction a c) == (LispFraction b d) = (quot a c) == (quot b d)
  (LispBool x) == (LispBool y) = x == y
  _ == _ = False

showPair :: LispData -> String
showPair (LispPair x lp@(LispPair _ _)) = (show x) ++ " " ++ (showPair lp)
showPair (LispPair x y) = (show x) ++ " . " ++ (show y)
showPair _ = "not a pair"

instance Show LispData where
  show (LispInt x) = show x
  show lp@(LispPair x y) = "(" ++ showPair lp ++ ")"
  show LispUnit = "unit"
  show (LispString x) = show x
  show (LispSymbol x) = x
  show (LispFloat x) = show x
  show (LispFraction x y) = show x ++ "/" ++ show y
  show (LispLambda _) = "Lambda"
  show (LispBool True) = "#t"
  show (LispBool False) = "#f"

-- Function application
apply :: LispData -> [LispData] -> LispData
apply (LispLambda f) x = f x

-- Primitive functions
type LLI = [LispData] -> LispData

lispAdd :: LispData -> LispData -> LispData
lispAdd (LispInt x) (LispInt y) = LispInt $ x + y
lispAdd (LispInt x) lf@(LispFloat _) = lispAdd (LispFloat $ fromIntegral x) lf
lispAdd (LispInt x) lf@(LispFraction _ _) = lispAdd (LispFraction x 1) lf
lispAdd (LispFloat x) (LispFloat y) = LispFloat $ x + y
lispAdd lf@(LispFloat _) (LispInt x) = lispAdd lf (LispFloat $ fromIntegral x)
lispAdd lf@(LispFloat _) (LispFraction x y) = lispAdd lf (LispFloat $ fromIntegral x / fromIntegral y)
lispAdd (LispFraction a c) (LispFraction b d) = LispFraction (a * d + b * c) (c * d)
lispAdd lf@(LispFraction _ _) x = lispAdd x lf

add :: LLI
add [x, y] = lispAdd x y
add (x:xs) = lispAdd x (add xs)

lispMult :: LispData -> LispData -> LispData
lispMult (LispInt x) (LispInt y) = LispInt $ x * y
lispMult (LispInt x) lf@(LispFloat _) = lispMult (LispFloat $ fromIntegral x) lf
lispMult (LispInt x) lf@(LispFraction _ _) = lispMult (LispFraction x 1) lf
lispMult (LispFloat x) (LispFloat y) = LispFloat $ x * y
lispMult lf@(LispFloat _) (LispFraction x y) = lispMult lf (LispFloat $ fromIntegral x / fromIntegral y)
lispMult lf@(LispFloat _) x = lispMult x lf
lispMult (LispFraction a c) (LispFraction b d) = LispFraction (a * b) (c * d)
lispMult lf@(LispFraction _ _) x = lispMult x lf

mult :: LLI
mult [x, y] = lispMult x y
mult (x:xs) = lispMult x (mult xs)

lispNegate :: LispData -> LispData
lispNegate (LispInt x) = LispInt $ -x
lispNegate (LispFloat x) = LispFloat $ -x
lispNegate (LispFraction x y) = LispFraction (-x) y

negate :: LLI
negate = lispNegate . head

lispInvert :: LispData -> LispData
lispInvert (LispInt _) = LispInt 0
lispInvert (LispFloat x) = LispFloat $ 1 / x
lispInvert (LispFraction x y) = LispFraction y x

invert :: LLI
invert = lispInvert . head

lispCons :: LispData -> LispData -> LispData
lispCons = LispPair

cons :: LLI
cons [x, y] = lispCons x y

mkList' :: LLI
mkList' [] = LispUnit
mkList' (x:xs) = lispCons x (mkList' xs)

mkList :: LispData
mkList = LispLambda mkList'

lispCar :: LispData -> LispData
lispCar (LispPair x _) = x

car :: LLI
car = lispCar . head

lispCdr :: LispData -> LispData
lispCdr (LispPair _ x) = x

cdr :: LLI
cdr = lispCdr . head

lispEq :: LispData -> LispData -> LispData
lispEq a b = LispBool $ a == b

eq :: LLI
eq [x, y] = lispEq x y

lispIf :: LispData -> LispData -> LispData -> LispData
lispIf (LispBool t) a z = if t then a else z

if' :: LLI
if' [x, y, z] = lispIf x y z
