{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

--import Interface.Text 
import qualified Data.Text.Lazy.IO as T
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
              
-- | The main function
main :: IO ()
main = do
  -- | test 1
  mapM_ (T.putStrLn . Coq.convcoq) sampleTypes
  -- | test 2
  Coq.coqProver (TQ.ProofSearchSetting Nothing Nothing)
                (TQ.ProofSearchQuery [] [] (D.toDeBruijn [] D.Type))


