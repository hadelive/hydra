#!/bin/bash

# Script to publish modified Hydra contracts to preprod testnet
# This will deploy our modified Head validator with openDatumHash functionality

set -e

# Configuration
NETWORK_MAGIC=1  # preprod testnet
CARDANO_SIGNING_KEY="/Users/hoangvu/hade/no-witness-labs/sailfish-network/keys/sailfish-node-1-cardano.sk"
HYDRA_NODE="/Users/hoangvu/hade/no-witness-labs/hydra/hydra-node-built/bin/hydra-node"

# Check if hydra-node exists
if [ ! -f "$HYDRA_NODE" ]; then
    echo "Error: hydra-node not found at $HYDRA_NODE"
    echo "Please build hydra-node first"
    exit 1
fi

# Check if signing key exists
if [ ! -f "$CARDANO_SIGNING_KEY" ]; then
    echo "Error: Cardano signing key not found at $CARDANO_SIGNING_KEY"
    exit 1
fi

echo "🚀 Publishing modified Hydra scripts to preprod testnet..."
echo "📝 Using signing key: $CARDANO_SIGNING_KEY"
echo "⚠️  This will cost approximately 50 ADA from the signing key"
echo ""

# Check if docker is running for socket access
if docker-compose ps >/dev/null 2>&1; then
    echo "📡 Using cardano-node via Docker socket..."
    
    # Get the container name for cardano-node
    CONTAINER_NAME=$(docker-compose ps --services | grep cardano)
    SOCKET_PATH="/ipc/node.socket"
    
    # Run publish-scripts via docker exec
    echo "🔨 Executing publish-scripts command..."
    
    docker-compose exec "$CONTAINER_NAME" \
        /opt/cardano/bin/hydra-node publish-scripts \
        --testnet-magic "$NETWORK_MAGIC" \
        --node-socket "$SOCKET_PATH" \
        --cardano-signing-key "$CARDANO_SIGNING_KEY"
        
else
    echo "❌ Docker not running. Checking for Blockfrost configuration..."
    
    # Check for Blockfrost API key
    BLOCKFROST_FILE="/Users/hoangvu/hade/no-witness-labs/sailfish-network/blockfrost.txt"
    
    if [ -f "$BLOCKFROST_FILE" ]; then
        echo "📡 Using Blockfrost API..."
        
        "$HYDRA_NODE" publish-scripts \
            --testnet-magic "$NETWORK_MAGIC" \
            --blockfrost "$BLOCKFROST_FILE" \
            --cardano-signing-key "$CARDANO_SIGNING_KEY"
    else
        echo "❌ Neither Docker nor Blockfrost available."
        echo ""
        echo "Please either:"
        echo "1. Start cardano-node with: docker-compose up cardano-node -d"
        echo "2. Create blockfrost.txt with your Blockfrost API key"
        echo "3. Provide a --node-socket path to a running cardano-node"
        echo ""
        echo "Example with custom socket:"
        echo "$HYDRA_NODE publish-scripts \\"
        echo "  --testnet-magic $NETWORK_MAGIC \\"
        echo "  --node-socket /path/to/node.socket \\"
        echo "  --cardano-signing-key $CARDANO_SIGNING_KEY"
        
        exit 1
    fi
fi

echo ""
echo "✅ Scripts published successfully!"
echo "📋 Copy the transaction ID(s) output above"
echo "🔧 Add them to docker-compose.yml as:"
echo "     --hydra-scripts-tx-id"
echo "     <TRANSACTION_ID>"