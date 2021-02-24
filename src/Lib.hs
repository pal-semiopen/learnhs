module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--カンペ

list  = [0,1,2,3]
list' = 0:1:2:3:[]
areListsSame = list == list'

string  = "(^_^)"
string' = ['(','^','_','^',')']
areStringsSame = string == string'

firstElem     = head list
notFirstElems = tail list

func1  = \l -> head l
func1' = head