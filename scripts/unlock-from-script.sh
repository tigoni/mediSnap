# # #address2 unlocks FaberCastell#02 NFT from script1
cardano-cli transaction build-raw \
     --babbage-era \
    --tx-in 80b06a39bb4d5b67239ff10b30190a17eed5dd176498d1c55053919074d3b9bd#0 \
    --tx-in-datum-file "../token/lock/unit.json" \
    --tx-in-redeemer-file "../token/lock/unit.json" \
    --tx-in-script-file "../token/lock/mds-lock.plutus" \
    --tx-out $(cat ../wallets/buyer/buyer.addr)+"245007331 lovelace + 1 b34ebf7f6f88c5c9c743992ce2b4e3ad41cc3e01f2c998b12abc7a98.4d656469536e6170" \
    --tx-out $(cat ../wallets/beneficiary/beneficiary.addr)+2000000 \
    --tx-out $(cat ../wallets/minter/minter.addr)+2000000 \
    --tx-in-collateral "dc6fba11e10917236072a200c5e9e8b2c5c95d317d9b924bd736952fb5153c39#0" \
    --tx-in-execution-units="(1000000000, 10000000)" \
    --protocol-params-file "../token/protocol-params.json" \
    --fee 992669  \
    --out-file "../token/unlock/tx.body"

cardano-cli transaction sign \
    --tx-body-file "../token/unlock/tx.body" \
    --signing-key-file "../wallets/buyer/buyer.skey" \
    --$TS \
    --out-file "../token/unlock/tx.signed"

cardano-cli transaction submit \
    --$TS \
   --tx-file "../token/unlock/tx.signed"


#    cardano-cli transaction calculate-min-fee \
#     --tx-body-file  "../token/unlock/tx.body" \
#     --tx-in-count 1 \
#     --tx-out-count 3 \
#     --witness-count 1 \
#     --$TS \
#     --protocol-params-file "../token/protocol-params.json"



