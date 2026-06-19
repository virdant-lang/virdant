#!/usr/bin/env bash
#
# Watch script for Sphinx documentation
# Automatically rebuilds docs when source files change
#

set -e

BUILD_LOCK="$(dirname "$0")/.build-lock"

acquire_lock() {
    if [ -f "$BUILD_LOCK" ]; then
        echo "Build already in progress (lockfile present). Skipping..."
        return 1
    fi
    touch "$BUILD_LOCK"
    return 0
}

release_lock() {
    rm -f "$BUILD_LOCK"
}

build() {
    acquire_lock || return 0
    make clean && make auto
    release_lock
}

cleanup() {
    release_lock
    exit 0
}

trap cleanup EXIT SIGINT SIGTERM

echo "Starting watch mode for Sphinx documentation..."
echo "Watching for changes in source/ directory..."
echo "Press Ctrl+C to stop"
echo ""

# Initial build
echo "Running initial build..."
build
echo ""
echo "Initial build complete. Watching for changes..."
echo ""

# Watch for changes using inotifywait (Linux) or fswatch (macOS)
if command -v inotifywait &> /dev/null; then
    # Linux: use inotify
    while true; do
        inotifywait -r -e modify,create,delete,move source/
        echo ""
        echo "Change detected! Rebuilding..."
        build
        echo ""
        echo "Build complete. Watching for changes..."
        echo ""
    done
elif command -v fswatch &> /dev/null; then
    # macOS: use fswatch
    fswatch -o source/ | while read f; do
        echo ""
        echo "Change detected! Rebuilding..."
        build
        echo ""
        echo "Build complete. Watching for changes..."
        echo ""
    done
else
    # Fallback: simple polling loop
    echo "Warning: Neither inotifywait nor fswatch found. Using polling fallback."
    echo "Install inotify-tools (Linux) or fswatch (macOS) for better performance."
    echo ""

    # Get initial checksums
    LAST_CHECKSUM=$(find source/ -type f -exec md5sum {} \; 2>/dev/null | sort | md5sum)

    while true; do
        sleep 2
        CURRENT_CHECKSUM=$(find source/ -type f -exec md5sum {} \; 2>/dev/null | sort | md5sum)

        if [ "$CURRENT_CHECKSUM" != "$LAST_CHECKSUM" ]; then
            echo ""
            echo "Change detected! Rebuilding..."
            build
            echo ""
            echo "Build complete. Watching for changes..."
            echo ""
            LAST_CHECKSUM=$CURRENT_CHECKSUM
        fi
    done
fi
