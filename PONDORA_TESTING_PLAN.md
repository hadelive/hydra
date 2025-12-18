# Testing Plan: Pondora NFT Integration with Sailfish Network

## Overview

This document outlines the testing strategy for integrating Pondora NFT verification into Hydra's close transaction, using the sailfish-network Docker environment.

## Current State

- **Sailfish Network**: Uses official `ghcr.io/cardano-scaling/hydra-node:0.22.4` Docker image
- **Hydra Modifications**: Added `referenceUTxO` parameter through the close transaction flow
- **Pondora Policy ID**: Hardcoded in `hydra-node/src/Hydra/Chain/Direct/State.hs:207`
  ```haskell
  pondoraPolicyId = "a1b2c3d4e5f6789012345678901234567890123456789012345678901234"
  ```

## Architecture Changes

### Data Flow
```
ChainStateAt {spendableUTxO, referenceUTxO, recordedAt}
    ↓ extracted in Handlers
prepareTxToPost (timeHandle, wallet, ctx, spendableUTxO, referenceUTxO, tx)
    ↓ for CloseTx
close (ctx, spendableUTxO, referenceUTxO, headId, ...)
    ↓ finds Pondora NFT
findPondoraRefInput (referenceUTxO) → Maybe TxIn
    ↓ passed to
closeTx (..., Maybe TxIn) → includes as reference input
```

### Modified Files
- `hydra-tx/src/Hydra/Tx/Close.hs` - Added `Maybe TxIn` parameter for Pondora reference input
- `hydra-node/src/Hydra/Chain/Direct/State.hs` - Added `findPondoraRefInput` and `referenceUTxO` field
- `hydra-node/src/Hydra/Chain/Direct/Handlers.hs` - Thread `referenceUTxO` through transaction posting

---

## Phase 1: Build Custom Hydra Docker Image

### Task 1.1: Verify Build System
**Prerequisites:**
- Nix with Flakes enabled
- ~10GB free disk space for Nix store
- Docker daemon running

**Verify Nix installation:**
```bash
nix --version
nix flake show /Users/hoangvu/hade/no-witness-labs/hydra
```

### Task 1.2: Build Custom Docker Image
```bash
cd /Users/hoangvu/hade/no-witness-labs/hydra

# Build the docker image package
nix build .#docker-hydra-node

# Load into Docker
docker load < result

# Tag for easy reference
docker tag hydra-node:latest hydra-node:pondora-integration

# Verify image
docker images | grep hydra-node
```

**Expected output:**
```
hydra-node    pondora-integration    <hash>    <size>
hydra-node    latest                 <hash>    <size>
```

### Task 1.3: Verify Image Contents
```bash
# Test the image runs
docker run --rm hydra-node:pondora-integration --version

# Inspect entrypoint
docker inspect hydra-node:pondora-integration | jq '.[0].Config.Entrypoint'
```

**Build Reference:**
- Docker image definition: `nix/hydra/docker.nix:9-23`
- Uses `hydra-node-static` binary from packages

---

## Phase 2: Configure Sailfish Network

### Task 2.1: Update docker-compose.yml
**File:** `/Users/hoangvu/hade/no-witness-labs/sailfish-network/docker-compose.yml`

**Change line 38:**
```yaml
# FROM:
image: ghcr.io/cardano-scaling/hydra-node:${HYDRA_NODE_VERSION:-latest}

# TO:
image: hydra-node:pondora-integration
```

### Task 2.2: Prepare Test Environment
```bash
cd /Users/hoangvu/hade/no-witness-labs/sailfish-network

# Stop and clean existing state
docker compose down -v

# Remove persistence data
rm -rf data/hydra/persistence/*
rm -rf data/cardano/db/* # Optional: only if full reset needed

# Verify cleanup
ls -la data/hydra/persistence
ls -la data/cardano/db
```

### Task 2.3: Start Network with Custom Image
```bash
# Pull latest configs (if needed)
./scripts/download-cardano-configs.sh

# Start all services
docker compose up -d

# Watch logs
docker compose logs -f hydra-node

# Verify hydra-node is using custom image
docker inspect sailfish-hydra-node | jq '.[0].Config.Image'
```

**Expected:** Should show `hydra-node:pondora-integration`

---

## Phase 3: Prepare Pondora NFT Reference Input

### Task 3.1: Understand the Requirement
The close transaction needs to reference a UTxO containing a Pondora NFT to verify the snapshot hash. This requires:
1. A UTxO with Pondora NFT exists on-chain
2. Hydra node tracks this UTxO in `referenceUTxO`
3. The UTxO is passed as reference input (not consumed)

### Task 3.2: Options for Pondora NFT

**Option A: Use Existing Pondora NFT on Preprod**
- Requires updating hardcoded policy ID in `State.hs:207` to match actual Pondora NFT
- Query existing Pondora NFTs:
  ```bash
  docker compose exec cardano-node cardano-cli query utxo \
    --address <pondora-nft-address> \
    --testnet-magic 1
  ```

**Option B: Mint Test NFT with Target Policy (Recommended for Testing)**
- Create a test policy that matches your hardcoded ID (for isolated testing)
- Mint a single NFT
- Ensure it's in a known UTxO

**Option C: Mock Reference for Development**
- For initial development, test without actual Pondora validation
- Focus on reference input plumbing
- Add actual Pondora validation later

### Task 3.3: Track Pondora NFT UTxO in Chain State

**Critical Issue Identified:** In `Handlers.hs:372`, the code incorrectly sets:
```haskell
referenceUTxO = adjustUTxO tx spendableUTxO  -- WRONG: copies spendableUTxO
```

**Required Changes:**

1. **Maintain separate tracking for reference UTxOs**
2. **Add configuration to specify Pondora NFT location**
3. **Update `referenceUTxO` from chain observations**

**Implementation approaches:**

#### Approach A: Configuration-based (Recommended)
Add Pondora NFT TxIn to hydra-node configuration:
```bash
# Add to docker-compose.yml command section:
- --pondora-ref-input
- <tx-hash>#<tx-ix>
```

Modify `ChainContext` or `Env` to include:
```haskell
data ChainContext = ChainContext
  { ...
  , pondoraTxIn :: Maybe TxIn
  }
```

Update `Handlers.hs` to use configured TxIn:
```haskell
let pondoraUTxO = case pondoraTxIn of
      Just txIn -> UTxO.filter ((== txIn) . fst) <existing-utxo-set>
      Nothing -> mempty

    newChainState = ChainStateAt
      { spendableUTxO = adjustUTxO tx spendableUTxO
      , referenceUTxO = pondoraUTxO  -- Use filtered Pondora UTxO
      , recordedAt = Just point
      }
```

#### Approach B: Automatic Discovery
Scan the UTxO set for Pondora NFT automatically:
```haskell
let pondoraUTxO = UTxO.filter hasPondoraNFT allObservedUTxOs

    newChainState = ChainStateAt
      { spendableUTxO = adjustUTxO tx spendableUTxO
      , referenceUTxO = pondoraUTxO
      , recordedAt = Just point
      }
```

Where `hasPondoraNFT` uses the same logic as `findPondoraRefInput`.

### Task 3.4: Implementation Plan for Phase 3 Fix

**Step 1:** Add Pondora TxIn configuration to `ChainContext` or `Environment`
**Step 2:** Update `Handlers.hs` to properly populate `referenceUTxO`
**Step 3:** Add logging to verify Pondora NFT tracking
**Step 4:** Update tests to verify reference UTxO population

---

## Phase 4: Add Pondora NFT Tracking

### Task 4.1: Update Chain State Observation

**File:** `hydra-node/src/Hydra/Chain/Direct/Handlers.hs`

**Locations to modify:**
- Line ~372: Fix `referenceUTxO` population
- Add Pondora-specific UTxO filtering logic
- Ensure `referenceUTxO` persists across state updates

### Task 4.2: Add Configuration Support

**Update Command Line Options:**
```haskell
-- In hydra-node/src/Hydra/Options.hs (or similar)
data Options = Options
  { ...
  , pondoraRefTxIn :: Maybe TxIn
  }

-- Parser
pondoraRefTxInParser :: Parser (Maybe TxIn)
pondoraRefTxInParser =
  optional $
    option (eitherReader parseTxIn) $
      long "pondora-ref-input"
      <> metavar "TX-HASH#TX-IX"
      <> help "Reference input containing Pondora NFT for snapshot verification"
```

**Update docker-compose.yml:**
```yaml
command:
  # ... existing commands ...
  - --pondora-ref-input
  - "${PONDORA_REF_INPUT}"
```

**Update .env:**
```bash
# Pondora NFT Reference Input
PONDORA_REF_INPUT=<tx-hash>#0
```

### Task 4.3: Add Logging and Observability

Add trace statements:
```haskell
-- In State.hs
close ctx spendableUTxO pondoraUTxO headId params ... = do
  let pondoraRefInput = findPondoraRefInput pondoraUTxO
  traceWith tracer $ PondoraRefInput pondoraRefInput
  -- ... continue
```

Monitor during close:
```bash
docker compose logs -f hydra-node | grep -i pondora
```

---

## Phase 5: Testing Strategy

### Task 5.1: Unit Tests

**File:** `hydra-node/test/Hydra/Chain/Direct/StateSpec.hs`

**Test cases to add:**
```haskell
describe "findPondoraRefInput" $ do
  it "finds UTxO with Pondora NFT" $ do
    let utxo = -- construct UTxO with Pondora NFT
    findPondoraRefInput utxo `shouldBe` Just expectedTxIn

  it "returns Nothing when Pondora NFT not present" $ do
    findPondoraRefInput mempty `shouldBe` Nothing

describe "close with Pondora reference" $ do
  it "includes reference input in close transaction" $ do
    let pondoraUTxO = -- UTxO with Pondora NFT
    case close ctx spendableUTxO pondoraUTxO headId params ... of
      Right tx ->
        -- verify tx includes reference input
        txReferenceInputs tx `shouldContain` [expectedPondoraTxIn]
      Left err -> expectationFailure $ show err
```

**Run tests:**
```bash
cd /Users/hoangvu/hade/no-witness-labs/hydra
cabal test hydra-node:test:tests --test-options="--match 'findPondoraRefInput'"
```

### Task 5.2: Integration Test with Sailfish Network

**Scenario: Complete Head Lifecycle with Close**

```bash
cd /Users/hoangvu/hade/no-witness-labs/sailfish-network

# 1. Ensure Pondora NFT exists and is configured
echo "PONDORA_REF_INPUT=<tx-hash>#0" >> .env

# 2. Start network
docker compose up -d
docker compose logs -f hydra-node

# 3. Wait for cardano-node to sync (or use Mithril)
# Check sync status:
docker compose exec cardano-node cardano-cli query tip --testnet-magic 1

# 4. Initialize head
./scripts/init-head.sh

# 5. Commit funds (all parties must commit)
./scripts/commit-funds.sh 10000000  # 10 ADA in lovelace

# 6. Perform L2 transactions (optional)
./scripts/submit-l2-tx.sh

# 7. Close head (this triggers Pondora NFT validation)
./scripts/close-head.sh

# 8. Monitor for errors
docker compose logs hydra-node | grep -i "error\|failed\|pondora"

# 9. Verify close transaction on-chain
# Get close tx hash from logs:
CLOSE_TX=$(docker compose logs hydra-node | grep "CloseTx" | tail -1 | grep -oP '[a-f0-9]{64}')

# Query the transaction
docker compose exec cardano-node cardano-cli query utxo \
  --tx-in ${CLOSE_TX}#0 \
  --testnet-magic 1
```

### Task 5.3: Verify Reference Input Inclusion

**Check the close transaction structure:**
```bash
# If transaction is still in mempool or you have the CBOR:
cardano-cli transaction view --tx-file close.tx

# Look for "reference inputs" section
# Should include: <pondora-tx-hash>#<pondora-tx-ix>
```

**Verify on-chain (after submission):**
```bash
# Use Blockfrost or Koios API
curl "https://preprod.koios.rest/api/v1/tx_info?_tx_hash=${CLOSE_TX}"

# Check "reference_inputs" array
```

### Task 5.4: Test Failure Scenarios

**Test Case 1: Close without Pondora NFT**
- Don't configure `PONDORA_REF_INPUT`
- Attempt to close
- **Expected:** Transaction builds but `pondoraRefInput = Nothing`

**Test Case 2: Close with invalid Pondora NFT**
- Configure wrong TxIn
- Attempt to close
- **Expected:** `findPondoraRefInput` returns `Nothing` (TxIn not in UTxO set)

**Test Case 3: Close with non-Pondora NFT**
- Configure TxIn with different policy ID
- Attempt to close
- **Expected:** `findPondoraRefInput` returns `Nothing` (wrong policy)

---

## Phase 6: Validation Checklist

### Build Validation
- [ ] Custom Docker image builds successfully with `nix build`
- [ ] Image loads into Docker without errors
- [ ] Hydra-node binary version matches expected
- [ ] Image size is reasonable (~200-500MB)

### Configuration Validation
- [ ] docker-compose.yml uses custom image
- [ ] Pondora reference input configuration is accepted
- [ ] Environment variables are properly passed to container
- [ ] Logs show configuration was loaded

### Runtime Validation
- [ ] Hydra node starts successfully with custom image
- [ ] Cardano node syncs to preprod
- [ ] `referenceUTxO` field is populated correctly
- [ ] `findPondoraRefInput` returns expected TxIn
- [ ] Close transaction builds without errors

### Transaction Validation
- [ ] Close transaction includes Pondora NFT as reference input
- [ ] Reference input has correct policy ID
- [ ] Transaction validates and submits on-chain
- [ ] On-chain transaction shows reference input
- [ ] Contestation period starts successfully

### Code Quality
- [ ] All unit tests pass
- [ ] No compilation warnings introduced
- [ ] Logging provides visibility into Pondora tracking
- [ ] Error handling is appropriate
- [ ] Documentation is updated

---

## Known Issues and Risks

### Critical Issues

1. **referenceUTxO Population Bug (FIXED)**
   - **Location:** `Handlers.hs:372`
   - **Issue:** `referenceUTxO = adjustUTxO tx spendableUTxO` copies spendable UTxO
   - **Impact:** Reference UTxO doesn't contain Pondora NFT
   - **Status:** Being fixed in Phase 3

2. **Hardcoded Policy ID**
   - **Location:** `State.hs:207`
   - **Issue:** Policy ID must match actual Pondora NFT on preprod
   - **Workaround:** Update hardcoded value or use test NFT
   - **Long-term:** Make configurable

### Medium Priority

3. **No Configuration Mechanism**
   - **Issue:** No way to specify Pondora NFT location
   - **Impact:** Can't easily change or configure reference input
   - **Solution:** Add command-line option (Phase 4)

4. **No Automatic Discovery**
   - **Issue:** Requires manual configuration of Pondora TxIn
   - **Impact:** Less user-friendly, prone to misconfiguration
   - **Solution:** Consider automatic discovery by policy ID

### Low Priority

5. **Nix Build Complexity**
   - **Issue:** Nix builds can be slow and require disk space
   - **Impact:** Longer iteration time during development
   - **Mitigation:** Use Nix binary cache, ensure adequate disk space

6. **Testing on Preprod**
   - **Issue:** Preprod state can be unpredictable
   - **Impact:** Tests may fail due to chain state, not code issues
   - **Mitigation:** Use private testnet or local cluster for core testing

---

## Timeline and Effort Estimates

### Phase 1: Build Docker Image
- **Effort:** 1-2 hours (first time), 15 minutes (subsequent)
- **Blockers:** Nix installation, disk space
- **Deliverable:** Working Docker image

### Phase 2: Configure Sailfish
- **Effort:** 30 minutes
- **Blockers:** None
- **Deliverable:** Sailfish network running custom image

### Phase 3: Fix referenceUTxO Bug
- **Effort:** 2-4 hours
- **Blockers:** Understanding chain state tracking logic
- **Deliverable:** Properly populated referenceUTxO

### Phase 4: Add Configuration
- **Effort:** 3-6 hours
- **Blockers:** Understanding options parsing and environment setup
- **Deliverable:** Command-line option for Pondora TxIn

### Phase 5: Testing
- **Effort:** 4-8 hours
- **Blockers:** Pondora NFT availability, preprod sync time
- **Deliverable:** Verified close transaction with Pondora reference

### Phase 6: Validation
- **Effort:** 2-4 hours
- **Blockers:** On-chain confirmation times
- **Deliverable:** Complete validation checklist

**Total Estimated Effort:** 12-24 hours

---

## Resources and References

### Documentation
- [Hydra Head Protocol](https://hydra.family/head-protocol/)
- [Cardano Reference Inputs](https://docs.cardano.org/plutus/reference-inputs/)
- [Nix Flakes](https://nixos.wiki/wiki/Flakes)
- [Docker Compose](https://docs.docker.com/compose/)

### Code References
- Hydra Docker image: `nix/hydra/docker.nix:9-23`
- Close transaction: `hydra-tx/src/Hydra/Tx/Close.hs:66`
- State management: `hydra-node/src/Hydra/Chain/Direct/State.hs:207`
- Handlers: `hydra-node/src/Hydra/Chain/Direct/Handlers.hs:372`

### Sailfish Network
- Repository: `/Users/hoangvu/hade/no-witness-labs/sailfish-network`
- Docker Compose: `docker-compose.yml`
- Scripts: `scripts/`
- Configuration: `.env`

### Useful Commands
```bash
# Nix build
nix build .#docker-hydra-node

# Docker operations
docker compose up -d
docker compose logs -f hydra-node
docker compose down -v

# Cardano queries
cardano-cli query tip --testnet-magic 1
cardano-cli query utxo --address <addr> --testnet-magic 1

# Debugging
docker inspect sailfish-hydra-node
docker exec -it sailfish-hydra-node sh
```

---

## Next Steps

### Immediate (Phase 3 Fix)
1. ✅ Write this plan to file
2. 🔄 Fix `referenceUTxO` population in `Handlers.hs:372`
3. ⏳ Verify compilation and basic functionality

### Short-term (Phases 1-2)
4. Build custom Docker image
5. Deploy to sailfish-network
6. Verify basic operation

### Medium-term (Phases 4-5)
7. Add Pondora TxIn configuration
8. Implement unit tests
9. Perform integration testing

### Long-term (Phase 6)
10. Complete validation checklist
11. Document findings
12. Prepare for production deployment

---

## Contact and Support

For questions or issues:
- Review Hydra logs: `docker compose logs -f hydra-node`
- Check Cardano node status: `docker compose ps cardano-node`
- Consult this plan for troubleshooting steps
- Review GitHub issues for similar problems
