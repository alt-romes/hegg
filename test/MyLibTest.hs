module Main (main) where

main :: IO ()
main = do
    putStrLn "runEGS emptyEGraph $ reprExpr (("a"*2)/2) >> merge 1 2"
    putStrLn "runEGS emptyEGraph $ reprExpr (("a"*2)/2+0) >> reprExpr (2/2) >>= \idiv -> reprExpr 1 >>= \i1 -> reprExpr ("a"*1) >>= \imul1 -> reprExpr "a" >>= \ia -> reprExpr ("a"*(2/2)) >>= \i -> merge imul1 ia >> merge 3 i >> merge i1 idiv >> merge 3 5 >> rebuild"
