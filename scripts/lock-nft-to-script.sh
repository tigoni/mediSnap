cardano-cli transaction build-raw \
    --babbage-era \
    --tx-in 711ba39ed1e5cea705ef8bb094bca230d7d6a78b523e7cfa0beb53be5bf614e4#1 \
    --tx-out-datum-hash-file "../token/lock/unit.json" \
    --tx-out $(cat ../token/lock/script.addr)+8000000+" 1 4cfaf7079202490e972c0f18b27018342bb6a69bb37fa7ee55ce8076.4d656469536e6170233133" \
    --tx-out $(cat ../wallets/minter/minter.addr)+1821871 \
    --out-file "../token/lock/tx.raw" \
    --fee 178129

cardano-cli transaction sign \
--signing-key-file "../wallets/minter/minter.skey" \
--tx-body-file  "../token/lock/tx.raw" \
--out-file "../token/lock/tx.signed"  \
--$TS 

cardano-cli transaction submit --tx-file "../token/lock/tx.signed" --$TS 


# cardano-cli transaction calculate-min-fee \
# --tx-body-file  "../token/lock/tx.raw" \
# --tx-in-count 1 \
# --tx-out-count 2 \
# --witness-count 1 \
# --$TS \
# --protocol-params-file "../token/lock/protocol-params.json"

