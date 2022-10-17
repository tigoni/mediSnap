{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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

import           Data.Aeson             (ToJSON, FromJSON)
import           GHC.Generics           (Generic)
import           Prelude              (Show) 

import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..),unless)
import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Plutus.V1.Ledger.Value
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
        validate =   txHasOneScInputOnly && validateTxOuts && minterIsPaid 

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

        -- beneficiaryIsPaid :: Bool
        -- beneficiaryIsPaid = assetClassValueOf (valuePaidToAddress ctx (beneficiary p)) (beneficiaryCurrency p) == beneficiaryAmount p

        --AR address must have 2 Ada deposited.
        minterIsPaid :: Bool
        minterIsPaid = assetClassValueOf (U.valuePaidToAddress ctx (minterAddress p)) (minterCurrency p) == minterAmount p
        
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
