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
import Data.Hex
import Data.String (IsString (..)) 

import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Prelude as PP
import qualified PlutusTx.Builtins.Class as PBC
import qualified Ledger  as L             hiding (singleton)
import qualified Ledger.Address as LA
import qualified Plutus.V1.Ledger.Value as LV
import           Plutus.V1.Ledger.Api as LP
import           Plutus.V1.Ledger.Bytes (getLedgerBytes)

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
writeAssetPurchaseValidator = writeValidator "token/lock/mds-lock.plutus" $ validator $ TransferParams 
                                {
                                    asset= LV.TokenName $ getLedgerBytes $ fromString $ hex "MediSnap#10" --use PlutusTx for this
                                   ,minterAddress = LA.pubKeyHashAddress (LA.PaymentPubKeyHash $  PubKeyHash $ PBC.stringToBuiltinByteString "bebe8013168a1f3607bddb3a170b0adb12400316a8bcf34b7efedf0a") Nothing 
                                   ,minterCurrency = LV.assetClass adaSymbol (TokenName $ PBC.stringToBuiltinByteString "")
                                   ,minterAmount = 2000000
                                   ,collateralCurrency = LV.assetClass adaSymbol (TokenName $ PBC.stringToBuiltinByteString "")
                                   ,collateralAmount = 2000000
                                }



-- {-# INLINABLE toTokenName #-} toTokenName :: String -> TokenName toTokenName tn = TokenName { unTokenName = getLedgerBytes $ fromString $ hex tn } 