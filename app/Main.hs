module Main where

import Lib (someFunc)
import Data.List
main :: IO ()
main = do
  putStrLn("わ")

type Varible = Char
data Term = Var Varible | App Term Term | Abs Varible Term

--ここから括弧処理

data StrTree = Tree [StrTree] | Chr Char

--split_by_close のサポート
s_b_c' :: (String->Maybe(String,String)) -> Char -> String -> Maybe(String, String)
s_b_c' = \f c s -> case c of
  ')' -> (Just([], s))
  x   -> case f s of
    Nothing -> Nothing
    Just xxx -> (Just(  (x:(fst xxx)) , (snd xxx)  ))

split_by_close ::String -> Maybe(String, String)
split_by_close = \s -> case s of
    []   -> Nothing --Just([], [])
    c:s' -> s_b_c' split_by_close c s'




str_to_strtree :: String -> Maybe [StrTree]
str_to_strtree = \s -> case s of     --例のMaybe monad の話で大分綺麗になる気がする
    []     -> (Just [])
    '(':s' -> let inside_tree     = (str_to_strtree =<< (fst <$> (split_by_close s')))  --括弧の中
                  following_trees = (str_to_strtree =<< (snd <$> (split_by_close s'))) in  --括弧閉じの後ろ側
                    ((\i f -> ((Tree i):f)) <$> (inside_tree)) <*> following_trees
    x:s'   -> ((Chr x):) <$> (str_to_strtree s')


strtree_to_str::StrTree -> String --デバッガ
strtree_to_str = \t -> case t of
  Tree t' -> ("<"++(strtree_to_str =<< t')++">")
  Chr  c  -> [c]



{-    結局=<<そのものだった
to_str_each :: (a -> String) -> [a] -> String  --デバッガ補助
to_str_each = \f vec -> case vec of        --listをmonadとして使えばよいのでは？？
  []   -> []
  x:v' -> ((f x)++(to_str_each f v'))
-}


{-

to_str = \t -> case t of --t:Term
  Var x -> [x]
  App t_1 t_2 -> (to_str t_1)++(to_str t_2)
  Abs x t' -> "\\"++[x]++"."++(to_str t')

str_to_term :: String -> Maybe Term
str_to_term = \s -> case s of --s: string of a term
    []      -> Nothing
    '\\':s' -> abs s'
    '(':s'  -> 

abs :: String -> Maybe Term
abs = \s -> case s of --(\s): str of an abstructed term 
    []      -> Nothing
    '.':s'  -> Just (str_to_term s')
    x:s'    -> Just (Abs (Var (char_to_var x) (str_to_term s')))

-}
