# Guide: Publishing Modified Hydra Scripts to Preprod

## Overview
We need to publish our modified Hydra contracts (with `openDatumHash` functionality) to the preprod testnet so the hydra-node can reference them.

## Generated Credentials

### Payment Address (Preprod)
```
addr_test1vz5n6l67xxjfn4hspcrfqdg37q3nk8lrhm4pam4l646dlmqxzvz5s
```

### Key Files Location
- **Signing Key**: `keys/publish-scripts/payment.skey`
- **Verification Key**: `keys/publish-scripts/payment.vkey`
- **Address File**: `keys/publish-scripts/payment.addr`

## Steps to Publish Scripts

### 1. Fund the Address
1. Go to the Cardano Preprod Faucet: https://docs.cardano.org/cardano-testnets/tools/faucet/
2. Request **at least 60 ADA** to the address above
3. Wait for the transaction to be confirmed (usually 1-2 minutes)
4. Verify funds arrived using a preprod explorer:
   - https://preprod.cardanoscan.io/address/addr_test1vz5n6l67xxjfn4hspcrfqdg37q3nk8lrhm4pam4l646dlmqxzvz5s

### 2. Set Up Blockfrost (Recommended)
1. Get a free Blockfrost API key for preprod from: https://blockfrost.io/
2. Create the file `blockfrost-preprod.txt` in the sailfish-network directory
3. Put your API key (just the key, nothing else) in that file:
   ```bash
   echo "preprodXXXXXXXXXXXXXXXXXXXXXXXXXXXX" > blockfrost-preprod.txt
   ```

### 3. Run the Publish Script
```bash
cd /Users/hoangvu/hade/no-witness-labs/sailfish-network
./scripts/publish-scripts-preprod.sh
```

The script will:
- Check that you have funded the address
- Publish the modified Hydra scripts to preprod
- Output transaction ID(s) that you need for the next step
- Save the TX ID to `hydra-scripts-tx-id.txt`

### 4. Update Docker Compose Configuration
Add the transaction ID to `docker-compose.yml` in the hydra-node command section:

```yaml
command:
  - --node-id
  - ${HYDRA_NODE_ID:-sailfish-node-1}
  # ... other options ...
  - --hydra-scripts-tx-id
  - <YOUR_TRANSACTION_ID_HERE>  # Add this line with your TX ID
  # ... rest of configuration ...
```

### 5. Restart Hydra Node
```bash
docker-compose restart hydra-node
```

## What This Accomplishes

1. **Deploys Modified Scripts**: The modified Head validator with `openDatumHash` computation will be deployed on-chain
2. **Gets Reference Scripts**: These scripts will be available as reference scripts that hydra-node can use
3. **Fixes MissingScript Error**: Hydra-node will be able to find the scripts with the correct hashes

## Script Details

The publish-scripts command deploys three validators:
1. **Initial Validator** (from Aiken/plutus.json)
2. **Commit Validator** (from Aiken/plutus.json)
3. **Head Validator** (from modified Haskell code with openDatumHash)

## Troubleshooting

### If you don't have Blockfrost
You'll need either:
- A running cardano-node synced to preprod with accessible socket
- Or get a free Blockfrost API key (recommended)

### If the script fails
Check that:
- The address has sufficient funds (60+ ADA)
- The hydra-node binary exists at the expected path
- Network connectivity to preprod is working

### Checking Script Deployment
After publishing, you can verify the scripts are deployed by checking the transaction on a preprod explorer using the TX ID output by the script.

## Cost
Publishing scripts costs approximately 50 ADA:
- Script deposits (locked in UTxOs)
- Transaction fees

The ADA is not lost but locked in the script UTxOs.