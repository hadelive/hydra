#!/bin/bash

# Script to publish modified Hydra contracts to preprod testnet
# This will deploy our modified Head validator with openDatumHash functionality

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
NETWORK_MAGIC=1  # preprod testnet
SIGNING_KEY="/Users/hoangvu/hade/no-witness-labs/sailfish-network/keys/publish-scripts/payment.skey"
HYDRA_NODE="/Users/hoangvu/hade/no-witness-labs/hydra/hydra-node-built/bin/hydra-node"
PAYMENT_ADDR="addr_test1vz5n6l67xxjfn4hspcrfqdg37q3nk8lrhm4pam4l646dlmqxzvz5s"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Hydra Scripts Publisher for Preprod${NC}"
echo -e "${BLUE}========================================${NC}\n"

# Check if hydra-node exists
if [ ! -f "$HYDRA_NODE" ]; then
    echo -e "${RED}❌ Error: hydra-node not found at $HYDRA_NODE${NC}"
    echo "Please build hydra-node first with: cd /Users/hoangvu/hade/no-witness-labs/hydra && nix build .#hydra-node"
    exit 1
fi

# Check if signing key exists
if [ ! -f "$SIGNING_KEY" ]; then
    echo -e "${RED}❌ Error: Signing key not found at $SIGNING_KEY${NC}"
    exit 1
fi

echo -e "${YELLOW}📍 Payment Address:${NC}"
echo -e "   ${GREEN}$PAYMENT_ADDR${NC}\n"

echo -e "${YELLOW}⚠️  Prerequisites:${NC}"
echo "   1. Get at least 60 ADA from the preprod faucet"
echo "   2. Faucet URL: https://docs.cardano.org/cardano-testnets/tools/faucet/"
echo "   3. Wait for funds to arrive (check with a blockchain explorer)\n"

read -p "Have you funded the address with at least 60 ADA? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo -e "\n${YELLOW}Please fund the address first:${NC}"
    echo -e "${GREEN}$PAYMENT_ADDR${NC}"
    echo -e "\nYou can get testnet ADA from:"
    echo "https://docs.cardano.org/cardano-testnets/tools/faucet/"
    exit 0
fi

echo -e "\n${BLUE}🚀 Publishing modified Hydra scripts to preprod...${NC}"

# Use Blockfrost if available
BLOCKFROST_FILE="/Users/hoangvu/hade/no-witness-labs/sailfish-network/blockfrost-preprod.txt"

if [ -f "$BLOCKFROST_FILE" ]; then
    echo -e "${GREEN}📡 Using Blockfrost API...${NC}\n"
    
    TX_ID=$("$HYDRA_NODE" publish-scripts \
        --testnet-magic "$NETWORK_MAGIC" \
        --blockfrost "$BLOCKFROST_FILE" \
        --cardano-signing-key "$SIGNING_KEY" 2>&1)
else
    # Try using local node socket
    echo -e "${YELLOW}📡 Attempting to use local cardano-node socket...${NC}\n"
    
    # Check common socket locations
    SOCKET_PATH=""
    if [ -S "/var/run/cardano-node/node.socket" ]; then
        SOCKET_PATH="/var/run/cardano-node/node.socket"
    elif [ -S "$HOME/cardano-node/node.socket" ]; then
        SOCKET_PATH="$HOME/cardano-node/node.socket"
    elif [ -S "/tmp/cardano-node.socket" ]; then
        SOCKET_PATH="/tmp/cardano-node.socket"
    fi
    
    if [ -n "$SOCKET_PATH" ]; then
        echo -e "${GREEN}✅ Found socket at: $SOCKET_PATH${NC}\n"
        TX_ID=$("$HYDRA_NODE" publish-scripts \
            --testnet-magic "$NETWORK_MAGIC" \
            --node-socket "$SOCKET_PATH" \
            --cardano-signing-key "$SIGNING_KEY" 2>&1)
    else
        echo -e "${RED}❌ No cardano-node socket found${NC}"
        echo -e "\n${YELLOW}To use Blockfrost instead:${NC}"
        echo "1. Get a Blockfrost API key for preprod from https://blockfrost.io/"
        echo "2. Save it to: $BLOCKFROST_FILE"
        echo "3. Run this script again"
        echo -e "\n${YELLOW}To use a local node:${NC}"
        echo "1. Start a cardano-node synced to preprod"
        echo "2. Ensure the socket is accessible"
        echo "3. Run with custom socket path:"
        echo -e "\n${GREEN}$HYDRA_NODE publish-scripts \\"
        echo "  --testnet-magic $NETWORK_MAGIC \\"
        echo "  --node-socket /path/to/node.socket \\"
        echo -e "  --cardano-signing-key $SIGNING_KEY${NC}"
        exit 1
    fi
fi

# Check if command was successful
if [ $? -eq 0 ]; then
    echo -e "\n${GREEN}✅ Scripts published successfully!${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo -e "${YELLOW}📋 Transaction ID(s):${NC}"
    echo -e "${GREEN}$TX_ID${NC}"
    echo -e "${BLUE}========================================${NC}\n"
    
    # Save the TX ID to a file
    echo "$TX_ID" > /Users/hoangvu/hade/no-witness-labs/sailfish-network/hydra-scripts-tx-id.txt
    echo -e "${GREEN}✅ TX ID saved to: hydra-scripts-tx-id.txt${NC}\n"
    
    echo -e "${YELLOW}📝 Next Steps:${NC}"
    echo "1. Add this TX ID to docker-compose.yml in the hydra-node command:"
    echo -e "   ${GREEN}--hydra-scripts-tx-id${NC}"
    echo -e "   ${GREEN}$TX_ID${NC}"
    echo ""
    echo "2. Or set it as an environment variable:"
    echo -e "   ${GREEN}export HYDRA_SCRIPTS_TX_ID=$TX_ID${NC}"
    echo ""
    echo "3. Restart hydra-node with the new scripts"
else
    echo -e "\n${RED}❌ Failed to publish scripts${NC}"
    echo "Please check the error message above"
    exit 1
fi