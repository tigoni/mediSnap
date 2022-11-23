# # #address2 unlocks FaberCastell#02 NFT from script1
cardano-cli transaction build \
     --babbage-era \
    --tx-in ac48e4f3cb21ed61b2bd65251dd5f24a50a1582bd88af22926b19b495d5561df#0 \
    --tx-in-datum-file "../token/lock/unit.json" \
    --tx-in-redeemer-file "../token/lock/unit.json" \
    --tx-in-script-file "../token/lock/mds-lock.plutus" \
    --tx-out $(cat ../wallets/buyer/buyer.addr)+"2000000 lovelace + 1 cc9b25586a29e084962fc74619af931450e2168bd1cd20161aedd537.4d656469536e6170233232"\
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



