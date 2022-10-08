{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Asset.Deploy
    ( writeJSON
    , writeValidator
    , writeAssetPurchaseValidator
    , writeUnit
    , writeRedeemer
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger as L
import qualified Ledger.Value
import PlutusTx.Builtins.Internal

import          Asset.Purchase 

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> L.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . L.unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "token/lock/unit.json" ()

writeRedeemer :: IO () 
writeRedeemer = writeJSON "token/lock/redeemer.json" ()

writeAssetPurchaseValidator :: IO (Either (FileError ()) ())
writeAssetPurchaseValidator = writeValidator "token/lock/mds-lock.plutus" $ validator $ AssetPurchase 
                                {
                                    nft= Ledger.Value.tokenName $ TE.encodeUtf8 $ T.pack "MediSnap"
                                   ,minter = L.pubKeyHashAddress (L.PaymentPubKeyHash $  L.PubKeyHash $ BuiltinByteString $  TE.encodeUtf8 $ T.pack "bebe8013168a1f3607bddb3a170b0adb12400316a8bcf34b7efedf0a") Nothing 
                                   ,minterCurrency = Ledger.Value.assetClass (Ledger.Value.currencySymbol "") (Ledger.Value.tokenName  $ TE.encodeUtf8 $ T.pack "")
                                   ,minterAmount = 2000000
                                   ,beneficiary = L.pubKeyHashAddress (L.PaymentPubKeyHash $  L.PubKeyHash $ BuiltinByteString $  TE.encodeUtf8 $ T.pack "482421baa0219f801aa40f91dd8ec5d6ded4631a73f09b9d9b29848c") Nothing
                                   ,beneficiaryCurrency = Ledger.Value.assetClass (Ledger.Value.currencySymbol "") (Ledger.Value.tokenName  $ TE.encodeUtf8 $ T.pack "")
                                   ,beneficiaryAmount = 2000000
                                   ,collateral = Ledger.Value.assetClass (Ledger.Value.currencySymbol "") (Ledger.Value.tokenName $ TE.encodeUtf8 $ T.pack "")
                                   ,collateralAmnt = 2000000
                                }