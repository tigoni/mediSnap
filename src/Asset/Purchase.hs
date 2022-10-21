{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Asset.Purchase where 

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise ( serialise )
import           Prelude (Semigroup (..), Show (..), String)
import           Data.Aeson           (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)

import           Ledger
import           Ledger.Address
import qualified Ledger.Typed.Scripts as Scripts
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Value
import           PlutusTx.Builtins.Class
import qualified Plutus.V1.Ledger.Contexts as PVC

import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified Common.Utils             as U

data TransferParams = TransferParams {
    asset :: !TokenName
   ,minterAddress :: !Address 
   ,minterCurrency :: !AssetClass
   ,minterAmount :: !Integer
   ,collateralCurrency :: !AssetClass
   ,collateralAmount :: !Integer
} deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''TransferParams
-- PlutusTx.makeIsDataIndexed ''TransferParamsDatum [('TransferParamsDatum, 0)]

--pragma {# INLINABLE func #}: allows the compiler to inline the definition of `transferValidator` inside the `||` brackets
--Any function that is to be used for on-chain code will need this validator.
{-# INLINABLE transferValidator #-}

--this function will be supplied to `mkTypedValidator` which will compile it into Plutus Core.
transferValidator :: TransferParams -> () -> () -> ScriptContext -> Bool 
transferValidator p () () ctx  = validate 
    where
        validate ::  Bool
        validate =   txHasOneScInputOnly && validateTxOuts && sellerIsPaid 

--Only one input should exist pointing to a validator
        txHasOneScInputOnly :: Bool
        txHasOneScInputOnly =
          length (filter isJust $ toValidatorHash . txOutAddress . txInInfoResolved <$> txInfoInputs (scriptContextTxInfo ctx)) == 1

--At least one output must contain a collateral amount of 2 Ada or less
        validateTxOuts :: Bool
        validateTxOuts = any txOutValidate (txInfoOutputs (scriptContextTxInfo ctx))

        txOutValidate :: TxOut -> Bool
        txOutValidate txo = containsRequiredCollateralAmount txo
        
        -- collateral added is at least 2 Ada 
        containsRequiredCollateralAmount :: TxOut -> Bool
        containsRequiredCollateralAmount txo =
          collateralAmount p <= assetClassValueOf (txOutValue txo) (collateralCurrency p)

        txVal :: Value 
        txVal = assetClassValue (assetClass adaSymbol adaToken) 2000000 

        txOut :: PVC.TxOut
        txOut = PVC.TxOut (minterAddress p) txVal Nothing

        sellerIsPaid :: Bool
        sellerIsPaid =  txVal `elem` (fmap txOutValue $ filter (\x -> PVC.txOutValue x == txVal) (PVC.txInfoOutputs (PVC.scriptContextTxInfo ctx)))

        -- beneficiaryIsPaid :: Bool
        -- beneficiaryIsPaid = assetClassValueOf (valuePaidToAddress ctx (beneficiary p)) (beneficiaryCurrency p) == beneficiaryAmount p

        -- --AR address must have 2 Ada deposited.
        -- minterAddress :: Address
        -- minterAddress = pubKeyHashAddress (PaymentPubKeyHash $  PubKeyHash $ stringToBuiltinByteString "bebe8013168a1f3607bddb3a170b0adb12400316a8bcf34b7efedf0a") Nothing 
        -- minterIsPaid :: Bool
        -- minterIsPaid =  elem minterAddress (txOutAddress (filter (\x -> txOutAddress x == minterAddress) (txInfoOutputs (scriptContextTxInfo ctx))))  && 2000000 == minterAmount p
        -- -- minterIsPaid = assetClassValueOf (U.valuePaidToAddress ctx (minterAddress p)) (minterCurrency p) == minterAmount p
        
--for typed validators, we need to inform the Plutus compiler by creating a new type that encodes 
--the information about the datum and redeemer that plutus core expects.
data TypedValidator
instance Scripts.ValidatorTypes TypedValidator where
    type instance DatumType TypedValidator = ()
    type instance RedeemerType TypedValidator = () 

typedValidator :: TransferParams -> Scripts.TypedValidator TypedValidator
typedValidator p = Scripts.mkTypedValidator @TypedValidator
    ($$(PlutusTx.compile [|| transferValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: TransferParams -> Validator
validator = Scripts.validatorScript . typedValidator

--generate hash of the validator
valHash :: TransferParams -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

--generate address from the validator
scrAddress :: TransferParams -> Ledger.Address
scrAddress = scriptAddress . validator
