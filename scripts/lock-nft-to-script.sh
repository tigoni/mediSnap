cardano-cli transaction build-raw \
    --babbage-era \
    --tx-in d749922e035b2956e9d38b7fb9fba9dba9bce3cff3a62698778c2cb710f9dd5c#1  \
    --tx-out-datum-hash-file "../token/lock/unit.json" \
    --tx-out $(cat ../token/lock/script.addr)+250000000+" 1 b34ebf7f6f88c5c9c743992ce2b4e3ad41cc3e01f2c998b12abc7a98.4d656469536e6170" \
    --tx-out  $(cat ../wallets/minter/minter.addr)+49821959 \
    --out-file "../token/lock/tx.raw" \
    --fee 178041

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
# --protocol-params-file "../token/protocol-params.json"

