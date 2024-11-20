#!/bin/bash
# start_tunnel.sh

# Check if TEST environment variable is set
if [ "$TEST" = "true" ]; then
    echo "Test environment - not starting cloudflared tunnel"
    exit 0
else
    export TUNNEL_TOKEN=$(cat /run/secrets/cloudflared_theseus_tunnel_token)
    cloudflared tunnel run --url http://0.0.0.0:3000 theseus
fi