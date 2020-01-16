module DTS.Alligator.Prover
(
  prove
) where

import qualified DTS.DTT as DT            -- DTT
import qualified DTS.UDTT as UD            -- DTT
import qualified Data.Text.Lazy as T      -- text
import qualified Data.List as L           -- base
import qualified Control.Applicative as M -- base
import qualified Control.Monad as M       -- base
import qualified DTS.UDTTwithName as VN
import qualified DTS.Prover.Judgement as J
import Data.Time　as Ti
import System.Timeout

data ArrowSelector = Arrow_Fst | Arrow_Snd deriving (Eq, Show)

data Arrowterm =
  Conclusion DT.Preterm |
  Arrow_Sigma Arrowterm Arrowterm |
  Arrow_App Arrowterm Arrowterm |
  Arrow_Pair Arrowterm Arrowterm |
  Arrow_Proj ArrowSelector Arrowterm |
  Arrow_Lam Arrowterm |
  Arrow [Arrowterm]  Arrowterm
  deriving (Eq)

instance Show Arrowterm where
  show = show . arrow2DT
  -- show (Conclusion p)=   (show p)
  -- show (Arrow env r) ="[ " ++ (tail (foldr (\a -> \b -> "," ++ a  ++ b) "" $ map show env)) ++ " ] =>" ++  (show r)

arrow_notat_selector :: DT.Selector -> ArrowSelector
arrow_notat_selector DT.Fst = Arrow_Fst
arrow_notat_selector DT.Snd = Arrow_Snd

dt_notat_selector :: ArrowSelector -> DT.Selector
dt_notat_selector Arrow_Fst = DT.Fst
dt_notat_selector Arrow_Snd = DT.Snd

arrow2DT :: Arrowterm -> DT.Preterm
arrow2DT (Conclusion a) = a
arrow2DT (Arrow_Sigma h t)= DT.Sigma (arrow2DT h) (arrow2DT t)
arrow2DT (Arrow_Pair h t)= DT.Pair (arrow2DT h) (arrow2DT t)
arrow2DT (Arrow_App a b) = DT.App (arrow2DT a) (arrow2DT b)
arrow2DT (Arrow_Proj s p) = DT.Proj (dt_notat_selector s) (arrow2DT p)
arrow2DT (Arrow_Lam p) = DT.Lam (arrow2DT p)
arrow2DT (Arrow [] t) = arrow2DT t
arrow2DT (Arrow (f:r) t) = arrow2DT (Arrow r (Conclusion (DT.Pi (arrow2DT f)  (arrow2DT t))))

-- arrow_notat4biop_hojo :: DT.Preterm -> DT.Preterm -> DT.Preterm -> DT.Preterm
-- arrow_notat4biop_hojo (DT.Sigma _ _) h t= DT.Sigma h t
-- arrow_notat4biop_hojo (DT.App _ _) h t= DT.App h t
-- arrow_notat4biop_hojo (DT.Pair _ _) h t= DT.Pair h t
--
-- arrow_notat4biop :: DT.Preterm -> Arrowterm -> Arrowterm -> Arrowterm
-- arrow_notat4biop op (Conclusion arrow_h) (Conclusion arrow_t) =
--   Conclusion $ arrow_notat4biop_hojo op arrow_h arrow_t
-- arrow_notat4biop (DT.Sigma _ _) arrow_h arrow_t =
--   Arrow_Sigma arrow_h arrow_t
-- arrow_notat4biop (DT.App _ _) arrow_h arrow_t =
--   Arrow_App arrow_h arrow_t
-- arrow_notat4biop (DT.Pair _ _) arrow_h arrow_t =
--   Arrow_Pair arrow_h arrow_t
-- arrow_notat4biop op h t = undefined

--ex)
--print $ arrow_notat $ DT.Pi (DT.Pi (DT.Sigma (DT.Con (T.pack "p")) (DT.Con (T.pack "q"))) (DT.Con (T.pack "r"))) (DT.Pi (DT.Con (T.pack "p")) (DT.Pi (DT.Con (T.pack "q")) (DT.Con (T.pack "r"))))
arrow_notat :: DT.Preterm -> Arrowterm
-- --入力にArrowtermがあることはないとする
arrow_notat (DT.Type) = Conclusion $DT.Type
arrow_notat (DT.Var i) = Conclusion $ DT.Var i
arrow_notat (DT.Con i) = Conclusion $ DT.Con i
arrow_notat (DT.Not i) =
  Arrow [arrow_notat i] $Conclusion DT.Bot
arrow_notat (DT.Pi h t) =
  Arrow [arrow_notat h] (arrow_notat t)
arrow_notat (DT.Sigma h t) =
  Arrow_Sigma (arrow_notat h) (arrow_notat t)
  -- arrow_notat4biop (DT.Sigma h t) (arrow_notat h) (arrow_notat t)
arrow_notat (DT.App a b) =
  Arrow_App (arrow_notat a) (arrow_notat b)
  -- arrow_notat4biop (DT.App a b) (arrow_notat a) (arrow_notat b)
arrow_notat (DT.Pair a b) =
  Arrow_Pair (arrow_notat a) (arrow_notat b)
  -- arrow_notat4biop (DT.Pair a b) (arrow_notat a) (arrow_notat b)
arrow_notat (DT.Proj selector p) =
  Arrow_Proj (arrow_notat_selector selector) (arrow_notat p)
  -- case arrow_notat p of
  --   Conclusion arrow_p ->  Conclusion $ DT.Proj selector arrow_p
  --   arrow_p -> Arrow_Proj (arrow_notat_selector selector) arrow_p
arrow_notat (DT.Lam p) =
  Arrow_Lam (arrow_notat p)
  -- case arrow_notat p of
  --   Conclusion arrow_p ->  Conclusion $ DT.Lam arrow_p
  --   arrow_p-> Arrow_Lam arrow_p
arrow_notat dt= Conclusion dt

type TEnv = [DT.Preterm]
type SUEnv = [(T.Text,DT.Preterm)]
type AEnv = [Arrowterm]

data AJudgement =
  AJudgement AEnv Arrowterm Arrowterm
    deriving (Eq, Show)

typefromAJudgement :: AJudgement -> Arrowterm
typefromAJudgement ( AJudgement env aterm atype) = Arrow env atype

termfromAJudgement :: AJudgement -> Arrowterm
termfromAJudgement ( AJudgement env aterm atype) = Arrow env aterm

fromAJudgement2dtpreterm  :: AJudgement -> DT.Preterm
fromAJudgement2dtpreterm   = arrow2DT . typefromAJudgement

dne =   DT.Lam ((DT.Pi (DT.Con (T.pack "Prop")) (DT.Pi (DT.Pi (DT.Pi (DT.Var 1)  DT.Bot) (DT.Bot)) (DT.Var 2))))
efq = DT.Lam (DT.Pi DT.Bot (DT.Var 1))

classic = [dne,efq]

getAxiom :: String -> TEnv
getAxiom "classic"= classic ++ []

subst :: DT.Preterm -> DT.Preterm -> DT.Preterm -> DT.Preterm
subst preterm l i =
  if preterm == i then
    l
  else
    case preterm of
      DT.Pi a b -> DT.Pi (subst a l i) (subst b (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0))  (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.Not m -> DT.Not (subst m l i)
      DT.Lam m -> DT.Lam (subst m (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0)) (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.App m n    -> DT.App (subst m l i) (subst n l i)
      DT.Sigma a b  -> DT.Sigma (subst a l i) (subst b (DT.toDTT (UD.shiftIndices (DT.toUDTT l) 1 0)) (DT.toDTT (UD.shiftIndices (DT.toUDTT i) 1 0)))
      DT.Pair m n   -> DT.Pair (subst m l i) (subst n l i)
      DT.Proj s m   -> DT.Proj s (subst m l i)
      DT.Eq a m n   -> DT.Eq (subst a l i) (subst m l i) (subst n l i)
      others -> others

{-
prove([p:prop,q:prop,r:prop,s:pi(X:false,p)],_X: (p \/ ~p) ).
reverse([p:prop,q:prop,r:prop,s:pi(X:false,p)],I_Context),get_act_contexts(Base_Context),append(I_Context,Base_Context,Context),initialize_fresh_vars(Context,[v,a]),standard_context_notat(Context,N_Context),reduce_context(N_Context,NR_Context),check_context(NR_Context),copy_term(NR_Context,[X:T1|Tail]),check_type(T1,_T3,Tail).
-}

prove ::  TEnv -> SUEnv -> DT.Preterm -> DT.Preterm
--input_env : [DT.Preterm] preterm : リストを使っていない(DT.Pi(DT.Pi ... ...))というような形
-- @alligator prove([p:prop,q:set,r:prop],_X:p -> r -> p & r). / prove([p:prop,q:set,r:prop],_X:pi(X:p,pi(Y:r,sigma(Z:p,r)))).
-- @DTS.Alligator.Prover prove
  --[(DT.Con (T.pack "prop")),(DT.Con (T.pack "set")),(DT.Con (T.pack "prop"))]
  --(DT.Pi (DT.Var 0) (DT.Sigma (DT.Var 0,DT.Var 3)))

prove var_env sig_env preterm =
  --TEnvをPTEnvに変える
  let var_env' = (reverse var_env) ++ (getAxiom "classic")
      reduce_env = map (DT.toDTT . UD.betaReduce . DT.toUDTT) var_env' in
      -- if(check_context reduce_env sig_env) then
        let arrow_env = map arrow_notat reduce_env
            arrow_term = (arrow_notat . DT.toDTT . UD.betaReduce . DT.toUDTT) preterm
        in undefined
      -- else undefined--check_contextでtypeが分からないものが出てきた
--initialize_fresh_vars

-- pi_rules = [(Type Set, Type Set),  (Type Set, Kind Type_prop),  (Kind Type_prop, Kind Type_prop),  (Type Prop, Type Prop),  (Type Set, Type Prop),  (Kind Type_prop, Type Prop)]
-- sigma_rules = [(Type Set,Type Prop),(Type Prop, Type Prop)]
pi_rules = [(DT.Type, DT.Type),  (DT.Type, DT.Kind),  (DT.Kind, DT.Kind),   (DT.Kind, DT.Type)]
sigma_rules = [(DT.Type,DT.Type)]

--forwardができてからやる
forward_context :: AEnv -> [AJudgement]
forward_context [] = []
forward_context (f:r) =
  (to_forward (length (f:r)) f) ++ forward_context r

to_forward :: Int -> Arrowterm -> [AJudgement]
to_forward num aterm =
  forward aterm (DT.Con (T.pack $ show num)) aterm

sigma_forward :: Arrowterm -> DT.Preterm -> DT.Selector -> DT.Preterm -> [AJudgement]
sigma_forward origin base  selector (DT.Sigma a b) = forward origin (DT.Proj selector base) $Conclusion (DT.Sigma a b)
sigma_forward origin base selector preterm_a =  (AJudgement [origin] (Conclusion $ DT.Proj selector base) (Conclusion $ preterm_a)) : (forward origin (DT.Proj selector base) $Conclusion preterm_a)

lam_sigma_forward_hojo :: [Arrowterm] -> AJudgement -> AJudgement
lam_sigma_forward_hojo hs (AJudgement env (Conclusion term) (a_type)) =
  AJudgement env  (Conclusion (foldr (\x -> \y -> DT.Lam y) term hs)) (Arrow hs a_type)

lam_sigma_forward ::  [Arrowterm] -> Arrowterm -> DT.Preterm ->  Arrowterm -> [AJudgement]
lam_sigma_forward  [] origin base (Arrow a (Conclusion ( DT.Sigma preterm_a preterm_b))) =
  map (lam_sigma_forward_hojo a) (sigma_forward origin base DT.Snd (subst preterm_b (DT.Proj DT.Fst (base)) (DT.Var 0))) ++ (sigma_forward origin base DT.Fst preterm_a)
lam_sigma_forward (f:r) origin base (Arrow a b) =
  lam_sigma_forward  r origin (DT.App base (DT.Con (((T.pack . (\x -> (show base )++"_"++x) . show) (length (f:r)))) )) (Arrow a b)

show_forward :: Arrowterm -> TEnv
show_forward aterm =
  map  fromAJudgement2dtpreterm $forward aterm (DT.Con (T.pack "p")) aterm

forward :: Arrowterm -> DT.Preterm -> Arrowterm  ->  [AJudgement]
-- sigma = DT.Sigma (DT.Sigma (DT.Con (T.pack "b")) (DT.Con (T.pack "c"))) (DT.Sigma (DT.Var 0) (DT.App (DT.Var 0) (DT.Var 1)))
-- lamsig = Arrow [Conclusion DT.Type,Conclusion DT.Type] (Conclusion sigma)
forward origin base (Conclusion (DT.Sigma preterm_a preterm_b)) =
  (sigma_forward origin base DT.Fst preterm_a) ++ (sigma_forward origin base DT.Snd  (subst preterm_b (DT.Proj DT.Fst (base)) (DT.Var 0)) )
forward origin base (Arrow a (Conclusion (DT.Sigma preterm_a preterm_b))) =
  lam_sigma_forward a origin base (Arrow a (Conclusion (DT.Sigma preterm_a preterm_b)))
forward origin base (Arrow a (Arrow b c)) =
  forward origin base (Arrow (a ++ b) c)
forward origin base arrowterm = []


forward' :: Arrowterm -> [Arrowterm]
-- forward' (Arrow_Sigma arrow_h arrow_t) =
--   let h = arrow2DT arrow_h
--       t = arrow2
forward' (Arrow a (Arrow b c)) =
  forward' (Arrow (b ++ a) c)
forward' _ = []

maxdepth = 100

membership :: [AJudgement] ->  Arrowterm -> Int -> [Arrowterm]
membership context arrow_type depth =
    if (or $ map ((== arrow_type) . termfromAJudgement) context) --var(shift_indiceについて考える必要がある)
      then
        map (fst) $ filter (snd) $ map (\x -> ((typefromAJudgement x),((==arrow_type) . termfromAJudgement) x)) context
      else
        []

pi_form :: [AJudgement]-> [Arrowterm] -> [Arrowterm] -> Arrowterm->Int->[Arrowterm]

pi_form context type_terms _ (Conclusion DT.Type) depth =
  --type型を持つ項a1,...,anについて一つ一つextendした[(a1,[AJudgement]),(a2,[AJudgement])...]
  --type型を持つ項a1,...,anについてありえる(as,b1),...,(as,bm)を並べた[[(Arrowterm,Arrowterm)]]
  let extendedContexts = map (\aterm -> (aterm, (forward_context [aterm] ++ context))) type_terms
      a_bss = map (\(aterm,aenv) -> (map (\b -> Arrow [aterm] b ) (deduce aenv (Conclusion DT.Type) (depth + 1)))) extendedContexts
  in foldr (++) [] a_bss
pi_form context _ kind_terms (Conclusion DT.Kind) depth =
  let extendedContexts = map (\aterm -> (aterm, (forward_context [aterm] ++ context))) kind_terms 
      a_bss = map (\(aterm,aenv) -> (map (\b -> Arrow [aterm] b ) (deduce aenv (Conclusion DT.Kind) (depth + 1)))) extendedContexts
  in foldr (++) [] a_bss
pi_form _ _ _ _ _= []

pi_intro :: [AJudgement] -> Arrowterm -> Int -> [Arrowterm]
pi_intro context arrow_type depth = undefined


deduce :: [AJudgement]->Arrowterm->Int->[Arrowterm]
--context,target,depth
--type-ax
deduce _  (Conclusion DT.Kind) depth = if depth < maxdepth then [Conclusion DT.Type] else []

deduce context arrow_type depth =
  let type_terms = [undefined]
      kind_terms = [undefined]
    in (membership context arrow_type depth) ++ (pi_form context type_terms kind_terms arrow_type depth) ++ []