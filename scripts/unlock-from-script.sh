# # #address2 unlocks FaberCastell#02 NFT from script1
cardano-cli transaction build \
     --babbage-era \
    --tx-in e9bf5fde641f039acdc86399190e83ff413e2c122d135570540c516dee4bdc7b#0 \
    --tx-in-datum-file "../token/lock/unit.json" \
    --tx-in-redeemer-file "../token/lock/unit.json" \
    --tx-in-script-file "../token/lock/mds-lock.plutus" \
    --tx-out $(cat ../wallets/buyer/buyer.addr)+"2000000 lovelace + 1 27c0e23065d174355b8db2615d569b41eb01c2fb40cc9144b18d15e5.4d656469536e6170233132"\
    --tx-out $(cat ../wallets/beneficiary/beneficiary.addr)+2000000 \
    --tx-out $(cat ../wallets/minter/minter.addr)+2000000 \
    --tx-in-collateral "dc6fba11e10917236072a200c5e9e8b2c5c95d317d9b924bd736952fb5153c39#0" \
    --change-address $(cat ../wallets/buyer/buyer.addr) \
    --protocol-params-file "../token/lock/protocol-params.json" \
    --out-file "../token/unlock/tx.body" \
    --$TS

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


