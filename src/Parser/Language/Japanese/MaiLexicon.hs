module Parser.Language.Japanese.MaiLexicon (
  verbLexicon,
  nriLexicon
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

verbLexicon :: [Node]
verbLexicon = concat $ [
  -- for 695
  mylex ["最中"] "new" (N) (predSR 1 "最中/さいちゅう"),
  -- for 697
  mylex ["つつ"] "new" (((T False 1 modifiableS `BS` NP[F[Ga]]) `SL` (T False 1 modifiableS `BS` NP[F[Ga]])) `BS` (S [F verb, F[Cont], F[M],F[M],F[M],F[M],F[M]] `BS` NP[F[Ga]])) 
        ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) terminator) (App (App (Var 3) (Var 2)) (Var 1))))))), []),
  -- for 699
  mylex ["打ち合わせ"] "new" (N) (predSR 1 "打ち合わせ/うちあわせ")
  ]

nriLexicon :: [Node]
nriLexicon = concat $ [
  -- mylex ["NRI"] "Mai" (N) (predSR 1 "NRI"),
  mylex ["NRI"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "NRI"),
  mylex ["投資"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "投資"),
  mylex ["関心"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "関心"),
  mylex ["セミナー"] "Mai" ((T True 1 modifiableS `SL` (T True 1 modifiableS `BS` NP [F[Nc]]))) (properNameSR "セミナー"),
  mylex ["user"] "Mai" (N) (predSR 1 "user"),
  mylex ["ために"] "Mai" ((T False 1 modifiableS `SL` T False 1 modifiableS) `BS` (S [F anyPos,F[Term,NStem],F[P,M],F[P,M],F[P,M],F[M],F[M]]))
        ((Lam (Lam (Lam (Pi (App (Var 1) terminator) (App (Var 3) (Var 1)))))),[])
  ]