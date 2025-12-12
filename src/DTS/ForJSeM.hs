module DTS.ForNRI (
  verbsLexicon
  , verbsAxiom
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

verbsLexicon :: [Node]
verbsLexicon = concat $ [
  -- for 695
  mylex ["最中"] "new" (N) (predSR 1 "最中/さいちゅう"),
  -- for 697
  mylex ["つつ"] "new" (((T False 1 modifiableS `BS` NP[F[Ga]]) `SL` (T False 1 modifiableS `BS` NP[F[Ga]])) `BS` (S [F verb, F[Cont], F[M],F[M],F[M],F[M],F[M]] `BS` NP[F[Ga]])) 
        ((Lam (Lam (Lam (Lam (Sigma (App (App (Var 3) (Var 1)) terminator) (App (App (Var 3) (Var 2)) (Var 1))))))), []),
  -- for 699
  mylex ["打ち合わせ"] "new" (N) (predSR 1 "打ち合わせ/うちあわせ")
  ]

verbsAxiom :: DTT.Signature
verbsAxiom = [("dummy", DTT.Entity)
            {-
            -- 727の公理(破くー破れる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("yabuku",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.App (DTT.Con "破く/やぶく/ガヲ") (DTT.Var 0)) (DTT.Var 1)) (DTT.Var 2)) (DTT.App (DTT.App (DTT.Con "破れる/やぶれる/ガ") (DTT.Var 1)) (DTT.Var 3))))))] [] sentences
            -- 727の公理2(上手くいっていないが要検討)(破くー破れる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("yabuku", DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.App (DTT.Con "破く/やぶく/ガヲ") (DTT.Var 1)) (DTT.Var 2)) (DTT.Var 0))) (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "破れる/やぶれる/ガ") (DTT.Var 2)) (DTT.Var 0))))))] [] sentences
            -- 728の公理(閉めるー閉まる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("shimeru",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.App (DTT.Con "閉める/しめる/ガヲ") (DTT.Var 0)) (DTT.Var 1)) (DTT.Var 2)) (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 1)) (DTT.Var 3))))))] [] sentences
            -- 519の公理(小さなー大きな)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("chiisana", DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.Con "小さな/ちいさな") (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.Con "大きな/おおきな") (DTT.Var 1)) (DTT.Bot))))] [] sentences
            -- 520の公理(大きなー小さな)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("ookina", DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.Con "大きな/おおきな") (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.Con "小さな/ちいさな") (DTT.Var 1)) (DTT.Bot))))] [] sentences
            -- 523の公理(開くー閉まる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("hiraku",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.Con "開く/ひらく/ガ") (DTT.Var 1)) (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 2)) (DTT.Var 1)) (DTT.Bot)))))] [] sentences
            -- 524の公理(閉まるー開く)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("shimaru",DTT.Pi (DTT.Entity) (DTT.Pi (DTT.Entity) (DTT.Pi (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 1)) (DTT.Var 0)) (DTT.Pi (DTT.App (DTT.App (DTT.Con "開く/あく/ガ") (DTT.Var 2)) (DTT.Var 1)) (DTT.Bot)))))] [] sentences
            -- 開くの公理2(開くー閉まる)
            -- parseResult = NLI.parseWithTypeCheck parseSetting prover [("dummy",DTT.Entity), ("hitraku2", DTT.Pi(DTT.Entity) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "開く/ひらく/ガ") (DTT.Var 1)) (DTT.Var 0))) (DTT.Pi (DTT.Sigma (DTT.Entity) (DTT.App (DTT.App (DTT.Con "閉まる/しまる/ガ") (DTT.Var 2)) (DTT.Var 0))) (DTT.Bot))))] [] sentences
            -}
            ]