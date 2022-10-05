# # #address2 unlocks FaberCastell#02 NFT from script1
cardano-cli transaction build-raw \
     --babbage-era \
    --tx-in cc64916cb5f01b4f951cfca9337e2e07f1ce39a5a727f8954c85228b607d4130#0 \
    --tx-in-datum-file "../af/mint/unit.json" \
    --tx-in-redeemer-file "../af/mint/unit.json" \
    --tx-in-script-file "../af/lock-script/af-purchase.plutus" \
    --tx-out $(cat ../wallets/wallet2/payment.addr)+"1829818 lovelace + 1 9756702ae3befaa2282ffaa6d992b31a910fcf0b6d757e88d4ae24cc.41463033" \
    --tx-out $(cat ../wallets/beneficiary1/beneficiary.addr)+2000000 \
    --tx-out $(cat ../wallets/aggregator/aggregator.addr)+2000000 \
    --tx-in-collateral "da35e53daba28004e788c470a3cecd920a591bcffd2fc3f3078a31334b96329a#0" \
    --tx-in-execution-units="(1000000000, 10000000)" \
    --protocol-params-file "../af/mint/protocol-params.json" \
    --fee 992317 \
    --out-file "../af/unlock-script/tx.body"

cardano-cli transaction sign \
    --tx-body-file "../af/unlock-script/tx.body" \
    --signing-key-file "../wallets/wallet2/payment.skey" \
    --$TS \
    --out-file "../af/unlock-script/tx.signed"

cardano-cli transaction submit \
    --$TS \
   --tx-file "../af/unlock-script/tx.signed"


#    cardano-cli transaction calculate-min-fee \
#     --tx-body-file  "../af/unlock-script/tx.body" \
#     --tx-in-count 1 \
#     --tx-out-count 3 \
#     --witness-count 1 \
#     --$TS \
#     --protocol-params-file "../af/mint/protocol-params.json"



