{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Nft.Mint
    ( tokenPolicy
    , tokenCurSymbol
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value

--This policy script defines the constraints under which the tokens can be minted.
--In this case:
    -- the specified UTXO (oref) is part of the selected spending inputs for this tx
    -- the minting info for previous mint operations matches the current inputs on   token name and amount.
{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == amt
        _                -> False

tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.wrapMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt

tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn