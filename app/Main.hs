{-# LANGUAGE RankNTypes #-}

module Main where

import Lib (someFunc)
import Data.List
-- ↓ここから第二回↓
main :: IO ()
main = do
  putStrLn "ﾌﾟﾙﾌﾟﾙ、ぼくわるいはんぺんじゃないよ"
  putStrLn "うるせぇゆでるぞ"
  putStrLn "ぴぇ（死）"

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

-- ↓第三回ここから↓

data MyMaybe a = MyNothing | MyJust a

myHead3 :: [a] -> Maybe a
myHead3 []    = Nothing
myHead3 (x:_) = Just x

showHead :: [String] -> String
showHead xs = 
  case myHead3 xs of
    Just x  -> x
    Nothing -> "Nothing..."

-- (a -> b) -> Maybe a -> Maybe b ができたらうれしい……！
-- Functor: オーラ(レベル1). Maybeにくるまれた値を普通の関数に突っ込んでMaybeを返させることができる
--          具体的には <$> :: (a -> b) -> f a -> f b が使える
-- 例1: (\x -> x*2) <$> Just 3 = Just 6

-- Maybe (a -> b) -> Maybe a -> Maybe b ができたらうれしい……！
-- Applicative: オーラ(レベル2). Maybeにくるまれた値を "Maybeに入った関数" に突っ込んでMaybeを返させることができる
--              具体的には <*> :: f (a -> b) -> f a -> f b が使える
-- 例2: (Just (\x -> x*2)) <*> Just 3 = Just 6
-- 例3: (\x y-> x*y) <$> Just 2 <*> Just 3 = Just 6

-- (a -> Maybe b) -> Maybe a -> Maybe b ができたらうれしい……！
-- 次回: 忍び寄るレベル3の影 ~Monad~ 「do(おまじない)の正体に迫りつつあるぱる研一行。謎の概念Monadが彼らを襲う！！」
-- ヒント: (a -> Maybe b) と (b -> Maybe c) を合成したい……


type Variable = String
data Term = Var Variable | App Term Term | Lam Variable Term  deriving (Show, Eq)
data Status = Complete Term | Incomplete [Status] | Waiting String

bracketStart = "("
bracketEnd   = ")"
lambdaStart  = "\\"
lambdaEnd    = "."

getStrUntilFirstX :: String -> String -> String
getStrUntilFirstX x []  = []
getStrUntilFirstX x str = if isPrefixOf x str then [] else (head str):getStrUntilFirstX x (tail str)

getStrFromFirstX :: String -> String -> String
getStrFromFirstX x []  = []
getStrFromFirstX x str = if isPrefixOf x str then dropX x str else getStrFromFirstX x (tail str)

splitWithFirstX :: String -> String -> (String, String)
splitWithFirstX x str = (getStrUntilFirstX x str, getStrFromFirstX x str)

stripl :: String -> String
stripl (' ':xs) = stripl xs
stripl xs       = xs

stripr :: String -> String
stripr = reverse . stripl . reverse

strip :: String -> String
strip = stripl . stripr

spaceBefore :: String -> String -> String
spaceBefore x "" = ""
spaceBefore x (sh:st) = if x `isPrefixOf` (sh:st) then ' ':sh:spaceBefore x st else sh:spaceBefore x st

compress :: String -> String
compress ""           = ""
compress (x1:[])      = x1:[]
compress (x1:(x2:xs)) = if x1 == ' ' && x2 == ' ' then x2:xs else x1:compress (x2:xs)

dropX x str = drop (length x) str

findArgs :: String -> Term -> Term
findArgs raw =
  case strip raw of
    ""  -> id
    str -> (Lam (getStrUntilFirstX " " str)) . findArgs (getStrFromFirstX " " str)


stringToTerm :: String -> Maybe Term
stringToTerm str = 
  let (mt,s) = readStr str
  in if s == "" then joinTerms mt else Nothing

joinTerms :: [Maybe Term] -> Maybe Term
joinTerms []         = Nothing
joinTerms (t1:[])    = t1
joinTerms (t1:t2:ts) = joinTerms $ ((App<$>t1<*>t2) : ts)

joinResult r (lr,ll) = (r:lr, ll)

readStr :: String -> ([Maybe Term], String)
readStr raw = 
    let str     = compress $ strip
                  $ spaceBefore bracketStart 
                  $ spaceBefore bracketEnd 
                  $ spaceBefore lambdaStart 
                  $ spaceBefore lambdaEnd 
                  $ raw
    in  if str == ""
        then ([],"")
        else
          if bracketStart `isPrefixOf` str
          then (\(r, l) -> joinTerms r `joinResult` readStr l) $ readStr (dropX bracketEnd str)
          else 
            if bracketEnd `isPrefixOf` str
            then ([], dropX bracketStart str)
            else
              if lambdaStart `isPrefixOf` str
              then let  args = findArgs $ getStrFromFirstX lambdaStart $ getStrUntilFirstX lambdaEnd str
                        (leftResult, leftLeft) = readStr $ getStrFromFirstX lambdaEnd str
                    in  (([args <$> (joinTerms leftResult)]), leftLeft)
              else (Just $ Var $ getStrUntilFirstX " " str) `joinResult` readStr (getStrFromFirstX " " str)

termToString term =
  case term of
    Var v -> v
    App a b -> "(" ++ (termToString a) ++ " " ++ (termToString b) ++ ")"
    Lam v t -> "(λ " ++ v ++ ". " ++ (termToString t) ++ ")"

{-
replace x t1 t2 = 
  case t1 of
    Var v   -> if v == x then t2 else Ver v
    App a b -> App (replace x a t2) (replace x b t2)
    Lam v t -> if v == x then Lam v t else Lam v (replace x t t2)

calc term =
  case term of
    App (Lam x t1) t2 -> replace x t1 t2
    App t1 t2         -> replace
    -}