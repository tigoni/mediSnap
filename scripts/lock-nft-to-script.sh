cardano-cli transaction build-raw \
    --babbage-era \
    --tx-in 295fb73b56a192d8f983b975a09f5e4ee63fae75ca210148e247d9c3403c25dc#1 \
    --tx-out-datum-hash-file "../token/lock/unit.json" \
    --tx-out $(cat ../token/lock/mds-lock.addr)+8000000+" 1 c317ee36aaa3e6947435bc337928ca80387b8d6ddbda1fc147604a30.4d656469536e6170233135" \
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

