module Primitives where

import Parse

data Var = Var {
  varName :: String,
  varData :: LispData
               }
  deriving (Show, Eq)

type PF = LispData -> LispData
lispAdd' :: LispData -> LispData -> LispData
lispAdd' (LispInt x) (LispInt y) = LispInt $ x + y
lispAdd' (LispFloat x) (LispFloat y) = LispFloat $ x + y
lispAdd' (LispInt x) (LispFloat y) = LispFloat $ (fromIntegral x) + y
lispAdd' (LispFloat x) (LispInt y) = LispFloat $ x + (fromIntegral y)
lispAdd' _ _ = LispError "Error: can't add non-numeric values"

lispAdd :: [LispData] -> LispData
lispAdd = foldl lispAdd' (LispInt 0)

lispEq :: [LispData] -> LispData
lispEq [] = LispBool True
lispEq (x:[]) = LispBool True
lispEq (x:y:xs) = LispBool $ (x == y) && (lispEq xs == LispBool True)

lispIf :: [LispData] -> LispData
lispIf ld
  | length ld == 3 = if (head ld) == (LispBool True) then (ld !! 1) else (last ld)
  | otherwise = LispError "Error: if takes exactly 3 arguments"

lispNegate :: PF
lispNegate (LispInt x) = LispInt (-x)
lispNegate (LispFloat x) = LispFloat (-x)
lispNegate _ = LispError "Error: can't negate non-numeric values"

lispMult' :: LispData -> LispData -> LispData
lispMult' (LispInt x) (LispInt y) = LispInt $ x * y
lispMult' (LispFloat x) (LispFloat y) = LispFloat $ x * y
lispMult' (LispInt x) (LispFloat y) = LispFloat $ (fromIntegral x) * y
lispMult' (LispFloat x) (LispInt y) = LispFloat $ x * (fromIntegral y)
lispMult' _ _ = LispError "Error: can't multiply non-numeric values"

lispMult :: [LispData] -> LispData
lispMult = foldl lispMult' (LispInt 1)

lispInvert :: PF
lispInvert (LispFloat x) = LispFloat $ 1 / x
lispInvert _ = LispError "Error: can't invert non-floating point values"

lispCons :: [LispData] -> LispData
lispCons items@(x:xs)
  | length items == 2 = LispPair (head items) (last items)
  | otherwise = foldr LispPair LispUnit items

lispCar :: PF
lispCar (LispPair a b) = a
lispCar _ = LispError "Error: car must be applied to a pair"

lispCdr :: PF
lispCdr (LispPair a b) = b
lispCdr _ = LispError "Error: cdr must be applied to a pair"

primitives :: [Var]
primitives = map (\x -> Var (show x) (LispPrimitive x)) primList

getMultiArgsPrim :: Primitive -> ([LispData] -> LispData)
getMultiArgsPrim LispAdd = lispAdd
getMultiArgsPrim LispMult = lispMult
getMultiArgsPrim LispCons = lispCons
getMultiArgsPrim LispEq = lispEq
getMultiArgsPrim LispIf = lispIf

getUnaryPrim :: Primitive -> (LispData -> LispData)
getUnaryPrim LispNegate = lispNegate
getUnaryPrim LispInvert = lispInvert
getUnaryPrim LispCar = lispCar
getUnaryPrim LispCdr = lispCdr

applyPrimitive :: Primitive -> [LispData] -> LispData
applyPrimitive primitive args
  | primitive `elem` [LispAdd, LispMult, LispCons, LispEq, LispIf] = getMultiArgsPrim primitive args
  | primitive `elem` [LispNegate, LispInvert, LispCar, LispCdr] && length args == 1 = getUnaryPrim primitive $ head args
  | otherwise = LispError $ "Wrong number of arguments to function " ++ show primitive ++ " or implementation bug"
