#!/bin/bash

PORT=8000

cleanup() {
    kill $WATCH_PID $SERVER_PID 2>/dev/null
    exit 0
}

trap cleanup SIGINT SIGTERM

bash watch.sh &
WATCH_PID=$!

python3 -m http.server -d build/html "${PORT}" &
SERVER_PID=$!

# If either process exits, kill the other and stop
wait -n 2>/dev/null
cleanup
