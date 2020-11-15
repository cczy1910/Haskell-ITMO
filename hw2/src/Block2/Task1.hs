module Block2.Task1
       (Expr(..),
        ArithmeticError(..),
       eval) where

import           Control.Monad

data Expr = Const Int | Add Expr Expr | Subt Expr Expr | Mul Expr Expr | Div Expr Expr | Pow Expr Expr

newtype ArithmeticError = ArithmeticError String deriving(Show, Eq)

eval :: Expr -> Either ArithmeticError Int

eval (Const x) = Right x
eval (Add l r) = liftM2 (+) (eval l) (eval r)
eval (Subt l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)
eval (Div l r) = join (liftM2 safeDiv (eval l) (eval r))
eval (Pow l r) = join (liftM2 safePow (eval l) (eval r))

safeDiv :: Int -> Int -> Either ArithmeticError Int
safeDiv a b
    | b == 0    = Left (ArithmeticError "Division by zero")
    | otherwise = Right (div a b)

safePow :: Int -> Int -> Either ArithmeticError Int
safePow a b
    | b < 0     = Left (ArithmeticError "Neative power")
    | otherwise = Right (a ^ b)