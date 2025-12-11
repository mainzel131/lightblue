module DTS.ForNRI (
  nriLexicon
  , nriAxiom
) where

import qualified Data.Text.Lazy as T
import Parser.CCG
import Parser.Language.Japanese.Templates
import qualified DTS.DTTdeBruijn as DTT --lightblue
import DTS.UDTTdeBruijn as UDTT --lightblue

type Signature = DTT.Signature

terminator :: UDTT.Preterm
terminator = UDTT.Ann (UDTT.Lam UDTT.Top) (DTT.Pi DTT.Entity DTT.Type)

mylex :: [T.Text] -> T.Text -> Cat -> (UDTT.Preterm, Signature) -> [Node]
mylex wds num cat' (sem',sig') = [(lexicalitem wd num 100 cat' (sem',sig')) | wd <- wds ]

nriLexicon :: [Node]
nriLexicon = concat $ [
  mylex ["NRI"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "NRI"),
  mylex ["投資"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "投資"),
  mylex ["関心"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "関心"),
  mylex ["セミナー"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "セミナー"),
  mylex ["ために"] "Mai" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[Term,NStem],F[P,M],F[P,M],F[P,M],F[M],F[M]]))
        ((Lam (Lam (Lam (Pi (App (Var 1) terminator) (App (Var 3) (Var 1)))))),[])
  ]

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
            
            パーズ結果に合わせて描いた「顧客は投資に興味がある」
            (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "興味") (DTT.Var 1)) (DTT.Var 0)) (DTT.Top)))) (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.App (DTT.App (DTT.Con "＃存在/ガガニ") (DTT.Con "投資")) (DTT.Proj (DTT.Fst) (DTT.Var 1))) (DTT.Con "顧客/こきゃく")) (DTT.Var 0)) (DTT.Top))))
            
            パーズ結果に合わせて描いた「NRIは顧客の投資への関心を高める」
            (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "関心") (DTT.Var 1)) (DTT.Var 0)) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ヘ") (DTT.Con "投資")) (DTT.Var 2)) (DTT.Top)))) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ノ") (DTT.Con "顧客/こきゃく")) (DTT.Var 1)) (DTT.Sigma (DTT.Entity) (DTT.Sigma(DTT.App (DTT.App (DTT.App (DTT.Con "高める/たかめる/ガヲ") (DTT.Var 3)) (DTT.Con "NRI")) (DTT.Var 0)) (DTT.Top))))))
            
            パーズ結果に合わせて描いた公理←これを使う
            ("kanshin-kyomi", (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "関心") (DTT.Var 1)) (DTT.Var 0)) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ヘ") (DTT.Con "投資")) (DTT.Var 2)) (DTT.Top)))) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "＃ノ") (DTT.Con "顧客/こきゃく")) (DTT.Var 1)) (DTT.Sigma (DTT.Entity) (DTT.Sigma(DTT.App (DTT.App (DTT.App (DTT.Con "高める/たかめる/ガヲ") (DTT.Var 3)) (DTT.Con "NRI")) (DTT.Var 0)) (DTT.Top)))))) (DTT.Sigma (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.Con "興味") (DTT.Var 1)) (DTT.Var 0)) (DTT.Top)))) (DTT.Sigma (DTT.Entity) (DTT.Sigma (DTT.App (DTT.App (DTT.App (DTT.App (DTT.Con "＃存在/ガガニ") (DTT.Con "投資")) (DTT.Proj (DTT.Fst) (DTT.Var 1))) (DTT.Con "顧客/こきゃく")) (DTT.Var 0)) (DTT.Top))))))))
            -}
            ]
