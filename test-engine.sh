#!/usr/bin/env bash
set -euo pipefail

# Port for the old version server
OLD_SERVER_PORT=18082
OLD_SERVER_URL="http://localhost:$OLD_SERVER_PORT"

# PID file for tracking the server process
PID_FILE="/tmp/hnefatafl-test-engine-server.pid"

# Default positions file
POSITIONS_FILE="${POSITIONS_FILE:-backend/engine_comparison_positions.txt}"

# Number of actors for self-play
NUM_ACTORS="${NUM_ACTORS:-20}"

cleanup() {
    echo "Cleaning up..."
    if [ -f "$PID_FILE" ]; then
        pid=$(cat "$PID_FILE")
        if kill -0 "$pid" 2>/dev/null; then
            echo "Stopping server (PID: $pid)..."
            kill "$pid" 2>/dev/null || true
            # Wait for process to exit
            for i in {1..10}; do
                if ! kill -0 "$pid" 2>/dev/null; then
                    break
                fi
                sleep 0.5
            done
            # Force kill if still running
            if kill -0 "$pid" 2>/dev/null; then
                echo "Force killing server..."
                kill -9 "$pid" 2>/dev/null || true
            fi
        fi
        rm -f "$PID_FILE"
    fi
}

trap cleanup EXIT

# Check if server is already running
if [ -f "$PID_FILE" ]; then
    pid=$(cat "$PID_FILE")
    if kill -0 "$pid" 2>/dev/null; then
        echo "Error: A test server is already running (PID: $pid)"
        echo "Stop it first or remove $PID_FILE if stale"
        exit 1
    else
        # Stale PID file
        rm -f "$PID_FILE"
    fi
fi

# Check if port is in use
if lsof -i ":$OLD_SERVER_PORT" >/dev/null 2>&1; then
    echo "Error: Port $OLD_SERVER_PORT is already in use"
    exit 1
fi

# Get current version
echo "Getting current version..."
current_version=$(nix eval '.#packages.x86_64-linux.backend."hnefatafl:exe:cli".identifier.version' --raw)
echo "Current version: $current_version"

# Compute previous version or use provided argument
if [ $# -ge 1 ]; then
    previous_version="$1"
    echo "Using provided previous version: $previous_version"
else
    # Parse version (format: x.y.z) and decrement patch component
    # This only works when patch > 0 (e.g., 0.0.2 -> 0.0.1)
    # For versions like 0.1.0, user must provide the previous version explicitly
    IFS='.' read -r major minor patch <<< "$current_version"

    if [ "$patch" -gt 0 ]; then
        previous_version="$major.$minor.$((patch - 1))"
        echo "Computed previous version: $previous_version"
    else
        echo "Error: Cannot auto-compute previous version from $current_version"
        echo "When patch is 0, the previous version could be anything (e.g., 0.0.5, 0.0.99)"
        echo "Please provide the previous version as an argument:"
        echo "  ./test-engine.sh <previous-version>"
        exit 1
    fi
fi

# Check if previous version binary exists
previous_binary="dist/$previous_version/hnefatafl"
if [ ! -x "$previous_binary" ]; then
    echo "Error: Previous version binary not found at $previous_binary"
    echo "Make sure to build it first with: ./build-release.sh"
    echo "Or specify a different version as an argument"
    exit 1
fi

# Build current version
echo "Building current version..."
nix build '.#packages.x86_64-linux.backend."hnefatafl:exe:cli"'
current_binary="result/bin/cli"

if [ ! -x "$current_binary" ]; then
    echo "Error: Failed to build current version"
    exit 1
fi

# Check positions file exists
if [ ! -f "$POSITIONS_FILE" ]; then
    echo "Error: Positions file not found: $POSITIONS_FILE"
    echo "Set POSITIONS_FILE environment variable or create test-positions.txt"
    exit 1
fi

# Start the old version server
echo "Starting old version server ($previous_version) on port $OLD_SERVER_PORT..."
# "$previous_binary" server "$OLD_SERVER_PORT" </dev/null &
"$previous_binary" server "$OLD_SERVER_PORT" &
server_pid=$!
echo "$server_pid" > "$PID_FILE"

# Wait for server to be ready
echo "Waiting for server to be ready..."
for i in {1..30}; do
    if curl -s "$OLD_SERVER_URL/health" >/dev/null 2>&1; then
        echo "Server is ready"
        break
    fi
    if ! kill -0 "$server_pid" 2>/dev/null; then
        echo "Error: Server process died"
        exit 1
    fi
    sleep 0.5
done

if ! curl -s "$OLD_SERVER_URL/health" >/dev/null 2>&1; then
    echo "Error: Server failed to start within timeout"
    exit 1
fi

# Run self-play: current version (new) vs previous version (old via server)
echo ""
echo "=========================================="
echo "Running self-play comparison"
echo "  New (local):  $current_version"
echo "  Old (remote): $previous_version at $OLD_SERVER_URL"
echo "  Positions:    $POSITIONS_FILE"
echo "  Actors:       $NUM_ACTORS"
echo "=========================================="
echo ""

"$current_binary" self-play \
    --actors "$NUM_ACTORS" \
    --old-server "$OLD_SERVER_URL" \
    "$POSITIONS_FILE"

echo ""
echo "Self-play completed successfully"
