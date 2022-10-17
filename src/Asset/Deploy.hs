{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Asset.Deploy
    ( writeJSON
    , writeValidator
    , writeAssetPurchaseValidator
    , writeUnit
    , writeRedeemer
    ) where


import           Prelude (IO, Show(..), FilePath)
import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Data.Hex
import Data.String (IsString (..)) 

import           PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..))
import           PlutusTx.Builtins.Class
import           Ledger
import           Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api
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

writeValidator :: FilePath -> Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "token/lock/unit.json" ()

writeRedeemer :: IO () 
writeRedeemer = writeJSON "token/lock/redeemer.json" ()

writeAssetPurchaseValidator :: IO (Either (FileError ()) ())
writeAssetPurchaseValidator = writeValidator "token/lock/mds-lock.plutus" $ validator $ TransferParams 
                                {
                                    asset= TokenName $ getLedgerBytes $ fromString $ hex "MediSnap#10" --use PlutusTx for this
                                   ,minterAddress = pubKeyHashAddress (PaymentPubKeyHash $  PubKeyHash $ stringToBuiltinByteString "bebe8013168a1f3607bddb3a170b0adb12400316a8bcf34b7efedf0a") Nothing 
                                   ,minterCurrency = assetClass adaSymbol (TokenName $ stringToBuiltinByteString "")
                                   ,minterAmount = 2000000
                                   ,collateralCurrency = assetClass adaSymbol (TokenName $ stringToBuiltinByteString "")
                                   ,collateralAmount = 2000000
                                }



-- {-# INLINABLE toTokenName #-} toTokenName :: String -> TokenName toTokenName tn = TokenName { unTokenName = getLedgerBytes $ fromString $ hex tn } 