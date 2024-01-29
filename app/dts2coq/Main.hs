{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

--import Interface.Text 
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified DTS.UDTTdeBruijn as U
import qualified DTS.UDTTvarName as D
import qualified DTS.TypeQuery as TQ
import qualified DTS.Prover.Coq.DTS2Coq as Coq

type UDTTpreterm = D.Preterm U.DTT
type DTTpreterm  = D.Preterm U.DTT

sampleTypes :: [DTTpreterm]
sampleTypes = [
  D.Sigma (D.VarName 'x' 1) (D.Con "entity") (D.App (D.Con "dog") (D.Var (D.VarName 'x' 1)))
  ]



--　test3用(Sigma)

sigsample :: [(T.Text, U.Preterm U.DTT)]
sigsample = [("A", U.Type), ("B", U.Type)]

ctxsample :: [U.Preterm U.DTT]
ctxsample = [U.Con "A", U.Sigma (U.Con "A") (U.Con "B")]

typsample :: U.Preterm U.DTT
typsample  = U.Con "B"



-- test4用(Pi)
sigsample' :: [(T.Text, U.Preterm U.DTT)]
sigsample' = [("A", U.Type), ("B", U.Type)]

ctxsample' :: [U.Preterm U.DTT]
ctxsample' = [U.Pi (U.Con "A") (U.Con "B")]

typsample' :: U.Preterm U.DTT
typsample'  = U.Con "A"



-- test5用(太郎はパンとご飯を食べた。→太郎はパンを食べた。)これは途中だから一旦無視
sigsample'' :: [(T.Text, U.Preterm U.DTT)]
sigsample'' = [("太郎/たろう;太郎/たろう", U.Con "entity"),
              ("＃ト", U.Pi (U.Con "entity") (U.Pi (U.Con "evt") (U.Type))),
              ("花子/はなこ", U.Con "entity"),
              ("パン/ぱん", U.Pi (U.Con "entity") (U.Pi (U.Con "evt") (U.Type))),
              ("食べる/たべる/ガヲニ", U.Pi (U.Con "entity") (U.Pi (U.Con "entity") (U.Pi (U.Con "entity") (U.Pi (U.Con "evt") (U.Type))))),
　　　　　      ("＃タ", U.Pi (U.Con "evt") (U.Type))]
-- [太郎/たろう;太郎/たろう:entity,
--  ＃ト:(x0:entity)→ (e0:evt)→ type,
--  花子/はなこ:entity,
--  パン/ぱん:(x0:entity)→ (e0:evt)→ type,
--  食べる/たべる/ガヲニ:(x0:entity)→ (x1:entity)→ (x2:entity)→ (e0:evt)→ type,
--  ＃タ:(e0:evt)→ type]

ctxsample'' :: [U.Preterm U.DTT]
ctxsample'' = [U.Pi (U.Con "A") (U.Con "B")]

typsample'' :: U.Preterm U.DTT
typsample''  = U.Con "A"
--  λx0.λx1.(u0:(x2:entity)× (e0:evt)× (パン/ぱん(e0,x2)))× (e1:evt)× (u2:食べる/たべる/ガヲニ(e1,太郎/たろう;太郎/たろう,π1(u0),x0))× (u3:＃タ(e1))× x1(e1)

-- (u0:(x2:entity)× (e0:evt)× (パン/ぱん(e0,x2)))

-- U.Sigma (U.Con "entity") (U.Sigma (U.Con "evt") (U.Con "パン/ぱん"))

-- (u2:食べる/たべる/ガヲニ(e1,太郎/たろう;太郎/たろう,π1(u0),x0))× (u3:＃タ(e1))× x1(e1)

-- (U.Sigma (U.Con "＃タ") (U.Var 1))



-- test6用(あるイタリア人が世界最高のテノール歌手になった。->世界最高のテノール歌手になったイタリア人がいた。)
signaturesample :: [(T.Text, U.Preterm U.DTT)]
signaturesample = [("イタリア~人", U.Pi (U.Con "entity") (U.Pi (U.Con "evt") (U.Type))),
                   ("世界最高/せかいさいこう", U.Pi (U.Con "entity") (U.Pi (U.Con "evt") (U.Type))),
                   ("テノール~歌手", U.Pi (U.Con "entity") (U.Pi (U.Con "evt") (U.Type))),
                   ("成る/なる", U.Pi (U.Pi (U.Con "entity") (U.Type)) (U.Pi (U.Con "entity") (U.Pi (U.Con "evt") (U.Type)))),
                   ("＃タ", U.Pi (U.Con "evt") (U.Type))]

-- [イタリア~人:(x0:entity)→ (e0:evt)→ type,
--  世界最高/せかいさいこう:(x0:entity)→ (e0:evt)→ type,
--  テノール~歌手:(x0:entity)→ (e0:evt)→ type,
--  成る/なる:(u0:(x0:entity)→ type)→ (x1:entity)→ (e0:evt)→ type,
--  ＃タ:(e0:evt)→ type]



contextsample :: [U.Preterm U.DTT]
contextsample = [U.Sigma (U.Sigma (U.Con "entity") (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "イタリア~人") (U.Var 0)) (U.Var 1))))
                 (U.Sigma (U.Con "evt") (U.Sigma (U.App (U.App (U.App (U.Con "成る/なる") (U.Var 0)) (U.Proj U.Fst (U.Var 1)))
                 (U.Lam (U.Sigma (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "世界最高/せかいさいこう") (U.Var 0)) (U.Var 1)))
                 (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "テノール~歌手") (U.Var 0)) (U.Var 2)))))) (U.App (U.Con "＃タ") (U.Var 1))))]

-- (u0:(x0:entity)× (e0:evt)× (イタリア~人(e0,x0)))× (e1:evt)× (u2:成る/なる(e1,π1(u0),λx1.(u3:(e2:evt)× (世界最高/せかいさいこう(e2,x1)))× (e3:evt)× (テノール~歌手(e3,x1))))× (＃タ(e1))



typesample :: U.Preterm U.DTT  --語彙項目変更
typesample = U.Sigma (U.Con "entity") (U.Sigma (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "世界最高/せかいさいこう") (U.Var 0)) (U.Var 1)))
            (U.Sigma (U.Sigma (U.Con "evt") (U.Sigma (U.App (U.App (U.App (U.Con "成る/なる") (U.Var 0)) (U.Var 2))
            (U.Lam (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "テノール~歌手") (U.Var 0)) (U.Var 1))))) (U.App (U.Con "＃タ") (U.Var 1)))) (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "イタリア~人") (U.Var 0)) (U.Var 3)))))


-- U.Sigma (U.Con "entity") (U.Sigma (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "世界最高/せかいさいこう") (U.Var 0)) (U.Var 1))) (U.Sigma (U.Sigma (U.Sigma (U.Con "evt") (U.Sigma (U.App (U.App (U.App (U.Con "成る/なる") (U.Var 0)) (U.Var 2)) (U.Lam (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "テノール~歌手") (U.Var 0)) (U.Var 1))))) (U.App (U.Con "＃タ") (U.Var 1)))  ) (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "イタリア~人") (U.Var 0)) (U.Var 3))))))

-- ((x0:entity)× (u1:(e0:evt)× (世界最高/せかいさいこう(e0,x0)))× (u3:(e1:evt)× (u4:成る/なる(e1,x0,λx1.(e2:evt)× (テノール~歌手(e2,x1))))× (＃タ(e1)))× (e3:evt)× (イタリア~人(e3,x0)))



typesample' :: U.Preterm U.DTT  --asp出てくる
typesample' = U.Sigma (U.Sigma (U.Con "entity") (U.Sigma (U.Sigma (U.Con "evt") (U.Sigma (U.App (U.App (U.App (U.Con "成る/なる") (U.Var 0))
             (U.Var 1)) (U.Lam (U.Sigma (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "世界最高/せかいさいこう") (U.Var 0)) (U.Var 1)))
             (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "テノール~歌手") (U.Var 0)) (U.Var 2)))))) (U.App (U.Con "＃タ") (U.Var 1))))
             (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "イタリア~人") (U.Var 0)) (U.Var 2)))))
             (U.Sigma (U.Con "evt") (U.Sigma (U.App (U.App (U.Con "＃存在") (U.Var 0)) (U.Proj U.Fst (U.Var 1)))
             (U.Sigma (U.App (U.App (U.Con "＃場所") (U.Var 1)) (U.Con "asp")) (U.App (U.Con "＃タ") (U.Var 2)))))

-- (u0:(x0:entity)× (u1:(e0:evt)× (u2:成る/なる(e0,x0,λx1.(u3:(e1:evt)× (世界最高/せかいさいこう(e1,x1)))× (e2:evt)× (テノール~歌手(e2,x1))))× (＃タ(e0)))× (e3:evt)× (イタリア~人(e3,x0)))× (e4:evt)× (u8:＃存在(e4,π1(u0)))× (u9:＃場所(e4,@0:entity))× (＃タ(e4))



typesample'' :: U.Preterm U.DTT  -- asp出てこない
typesample'' = U.Lam (U.Lam (U.Sigma (U.Sigma (U.Con "entity") (U.Sigma (U.Sigma (U.Con "evt")
             (U.Sigma (U.App (U.App (U.App (U.Con "成る/なる") (U.Var 0)) (U.Var 1))
             (U.Lam (U.Sigma (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "世界最高/せかいさいこう") (U.Var 0)) (U.Var 1)))
             (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "テノール~歌手") (U.Var 0)) (U.Var 2)))))) (U.App (U.Con "＃タ") (U.Var 1))))
             (U.Sigma (U.Con "evt") (U.App (U.App (U.Con "イタリア~人") (U.Var 0)) (U.Var 2)))))
             (U.Sigma (U.Con "evt") (U.Sigma (U.App (U.App (U.Con "＃存在") (U.Var 0)) (U.Proj U.Fst (U.Var 1)))
             (U.Sigma (U.App (U.App (U.Con "＃場所") (U.Var 1)) (U.Var 4)) (U.Sigma (U.App (U.Con "＃タ") (U.Var 2))
             (U.App (U.Var 5) (U.Var 3))))))))

-- λx0.λx1.(u0:(x2:entity)× (u1:(e0:evt)× (u2:成る/なる(e0,x2,λx3.(u3:(e1:evt)× (世界最高/せかいさいこう(e1,x3)))× (e2:evt)× (テノール~歌手(e2,x3))))× (＃タ(e0)))× (e3:evt)× (イタリア~人(e3,x2)))× (e4:evt)× (u8:＃存在(e4,π1(u0)))× (u9:＃場所(e4,x0))× (u10:＃タ(e4))× x1(e4)






-- | The main function
main :: IO ()
main = do
  -- | test 1
  -- mapM_ (T.putStrLn . Coq.convcoq) sampleTypes
  -- | test 2
  -- Coq.coqProver (TQ.ProofSearchSetting Nothing Nothing) (TQ.ProofSearchQuery [] [] (D.toDeBruijn [] D.Type))
  -- | test 3
  -- Coq.coqProver (TQ.ProofSearchSetting Nothing Nothing)
                -- (TQ.ProofSearchQuery sigsample ctxsample typsample)
  -- | test 4
  -- Coq.coqProver (TQ.ProofSearchSetting Nothing Nothing)
                -- (TQ.ProofSearchQuery sigsample' ctxsample' typsample')
  -- | test 5
  -- Coq.coqProver (TQ.ProofSearchSetting Nothing Nothing)
                -- (TQ.ProofSearchQuery sigsample'' ctxsample'' typsample'')
  -- | test 5
  Coq.coqProver (TQ.ProofSearchSetting Nothing Nothing)
                (TQ.ProofSearchQuery signaturesample contextsample typesample)



