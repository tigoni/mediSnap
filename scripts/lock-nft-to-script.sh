cardano-cli transaction build-raw \
    --babbage-era \
    --tx-in aeea9c9def9bb0118fc78ac405e118e9ad942693898d11ba81c6f53ef9ca62a2#1 \
    --tx-out-datum-hash-file "../token/lock/unit.json" \
    --tx-out $(cat ../token/lock/mds-lock.addr)+8000000+" 1 cc9b25586a29e084962fc74619af931450e2168bd1cd20161aedd537.4d656469536e6170233232" \
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

