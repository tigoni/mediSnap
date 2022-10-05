cardano-cli transaction build-raw \
    --babbage-era \
    --tx-in f3701be5137aa74e9256886f2e8327a93d51c14b462dfdbe52a4dbaa1cee2254#1  \
    --tx-out-datum-hash-file "../af/mint/unit.json" \
    --tx-out $(cat ../wallets/scripts/af-purchase-script.addr)+6822135+" 1 9756702ae3befaa2282ffaa6d992b31a910fcf0b6d757e88d4ae24cc.41463033" \
    --tx-out  $(cat ../wallets/wallet1/payment.addr)+1000000 \
    --out-file "../af/lock-script/tx.raw" \
    --fee 177865

cardano-cli transaction sign \
--signing-key-file "../wallets/wallet1/payment.skey" \
--tx-body-file  "../af/lock-script/tx.raw" \
--out-file "../af/lock-script/tx.signed"  \
--$TS 

cardano-cli transaction submit --tx-file "../af/lock-script/tx.signed" --$TS 


# cardano-cli transaction calculate-min-fee \
# --tx-body-file  "../af/lock-script/tx.raw" \
# --tx-in-count 1 \
# --tx-out-count 2 \
# --witness-count 1 \
# --$TS \
# --protocol-params-file "../af/mint/protocol-params.json"

