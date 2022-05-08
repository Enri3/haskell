borrarUlt:: [a] -> [a]
borrarUlt [] = []
borrarUlt (x:[]) = []
borrarUlt (x:xs) = x:borrarUlt xs