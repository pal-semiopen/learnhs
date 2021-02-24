{-# LANGUAGE RankNTypes #-}

module Main where

import Lib (someFunc)
import Data.List
-- ↓ここから第二回↓
main :: IO ()
main = someFunc

list  = [1,2,3,4] -- リスト
list' = 1:(2:(3:(4:[]))) -- ↑の糖衣構文を剥いだ

string   = "(^_^)" -- 文字列
string'  = ['(','^','_','^',')'] -- ↑の糖衣構文を剥いだ

listoflist = [["1"],["2","3"],["4","5","6"]] :: [[String]]

func1    = \x -> x * 2 -- 関数定義
func1' x =       x * 2 -- の糖衣構文

func2        = \x -> \y -> \z -> x + y + z -- 関数定義
func2' x y z =                   x + y + z -- の糖衣構文

func3 l = head l
func3' = head

func1'' = (* 2)

-- (* 2) = \x -> x * 2

second :: [a] -> a
second l = head $ tail l

second' :: [a] -> a
second'  = head.tail

first' :: [a] -> a
first'  = head

third' :: [a] -> a
third'  = head.tail.tail

flist = [second, second'] --型が同じなのでコンパイルが通る

bakuretsu a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac = ()
-- 爆裂しない。代わりにゼミが爆裂する（クソッ）

--data List a = Nil | Cons a (List a)
data Light = R | Y | G

func4 R = "┌(┌^o^)┐"
func4 Y = "…┌(┌^o^)┐"
func4 G = "三┌(┌^o^)┐"

func4' c = case c of
            R -> "┌(┌^o^)┐"
            Y -> "…┌(┌^o^)┐"
            G -> "三┌(┌^o^)┐"

data Manju = I Int | S String -- 新しい饅頭というデータ型を製造している（饅頭に入ってる）

manjus = [I 1, I 2, S "Cake", S "Cookie"]
manjus :: [Manju]

{-
manjus2 = [1, 2, "Cake", "Cookie"]
    → No instance for (Num [Char]) arising from the literal ‘1’（死）
-}

data IntGaSemeDeStringGaUke = Love Int String
data HanpenChan = HC (Int -> String)

type I2S = Int -> String -- 型に新しい名前をつけて読みやすくする
--type String = [Char]

type IntPlusString = forall p. ((Int -> p) -> (String -> p) -> p)

a $#$+ b = (head a) : (tail b) 
gucho a b = head a : tail b

drunk name department = name ++ ": I belong to " ++ department

data List a = NilHanpen | Cons a (List a)
-- Nil → [], Cons → :

testlist = Cons "1" $ Cons "2" $ Cons "3" NilHanpen


myHead NilHanpen  = ""
myHead (Cons x _) = x

myHead2 []  = ""
myHead2 (x:_) = x


myConcat []   ys = ys
myConcat (x:xs) ys = x:(myConcat xs ys)
