module DTS.ForNRI (
  nriAxiom
) where

import qualified Data.Text.Lazy as T
import qualified DTS.DTTdeBruijn as DTT

nriAxiom :: DTT.Signature
nriAxiom = [("dummy", DTT.Entity)
            {-
            NRIが固有名詞
            ("kanshin-kyomi", (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "関心") (DTT.Var 1)) (DTT.Var 0)) (DTT.App (DTT.App (DTT.Con "＃ヘ") (DTT.Var 3)) (DTT.Var 2)))) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ノ") (DTT.Var 3)) (DTT.Var 1)) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.Con "高める/たかめる/ガヲ") (DTT.Var 3)) (DTT.Con "NRI")) (DTT.Var 0)))))) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "興味") (DTT.Var 1)) (DTT.Var 0)))) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.App (DTT.Con "＃存在/ガガニ") (DTT.Var 3)) (DTT.Proj (DTT.Fst) (DTT.Var 1))) (DTT.Var 4)) (DTT.Var 0))))))))
            
            eventを最初に取る
            ("kanshin-kyomi", (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "関心") (DTT.Var 1)) (DTT.Var 0)) (DTT.App (DTT.App (DTT.Con "＃ヘ") (DTT.Var 3)) (DTT.Var 2)))) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ノ") (DTT.Var 3)) (DTT.Var 1)) (DTT.App (DTT.App (DTT.App (DTT.Con "高める/たかめる/ガヲ") (DTT.Var 2)) (DTT.Con "NRI")) (DTT.Var 5))))) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "興味") (DTT.Var 1)) (DTT.Var 0)))) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.App (DTT.Con "＃存在/ガガニ") (DTT.Var 3)) (DTT.Proj (DTT.Fst) (DTT.Var 1))) (DTT.Var 4)) (DTT.Var 0)))))))))
            
            Sigma (event 高まる)
            ("kanshin-kyomi", (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "関心") (DTT.Var 1)) (DTT.Var 0)) (DTT.App (DTT.App (DTT.Con "＃ヘ") (DTT.Var 3)) (DTT.Var 2)))) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ノ") (DTT.Var 3)) (DTT.Var 1)) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.Con "高める/たかめる/ガヲ") (DTT.Var 3)) (DTT.Con "NRI")) (DTT.Var 0)))))) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "興味") (DTT.Var 1)) (DTT.Var 0)))) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.App (DTT.Con "＃存在/ガガニ") (DTT.Var 3)) (DTT.Proj (DTT.Fst) (DTT.Var 1))) (DTT.Var 4)) (DTT.Var 0))))))))
            
            x0:entityに顧客を適用
            ("kanshin-kyomi", (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "関心") (DTT.Var 1)) (DTT.Var 0)) (DTT.App (DTT.App (DTT.Con "＃ヘ") (DTT.Var 3)) (DTT.Var 2)))) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ノ") (DTT.Con "顧客/こきゃく")) (DTT.Var 1)) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.Con "高める/たかめる/ガヲ") (DTT.Var 3)) (DTT.Con "NRI")) (DTT.Var 0)))))) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "興味") (DTT.Var 1)) (DTT.Var 0)))) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.App (DTT.Con "＃存在/ガガニ") (DTT.Var 3)) (DTT.Proj (DTT.Fst) (DTT.Var 1))) (DTT.Con "顧客/こきゃく")) (DTT.Var 0)))))))
            
            x0:entityに顧客をx1:entityに投資を適用
            ("kanshin-kyomi", (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "関心") (DTT.Var 1)) (DTT.Var 0)) (DTT.App (DTT.App (DTT.Con "＃ヘ") (DTT.Con "投資")) (DTT.Var 2)))) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ノ") (DTT.Con "顧客/こきゃく")) (DTT.Var 1)) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.Con "高める/たかめる/ガヲ") (DTT.Var 3)) (DTT.Con "NRI")) (DTT.Var 0)))))) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "興味") (DTT.Var 1)) (DTT.Var 0)))) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.App (DTT.Con "＃存在/ガガニ") (DTT.Con "投資")) (DTT.Proj (DTT.Fst) (DTT.Var 1))) (DTT.Con "顧客/こきゃく")) (DTT.Var 0))))))
            -}
            ]


 