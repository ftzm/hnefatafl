#!/usr/bin/env bash

# Script to process game batches from hnefatafl_games_transformed.json
# Usage: ./process_batch.sh [start_index] [end_index]
# Example: ./process_batch.sh 0 10    (processes games 1-10)
#          ./process_batch.sh 10 20   (processes games 11-20)
#          ./process_batch.sh 40 50   (processes games 41-50)

JSON_FILE="/home/ftzm/dev/hnefatafl_pst/hnefatafl_games_transformed.json"

if [ $# -ne 2 ]; then
    echo "Usage: $0 <start_index> <end_index>"
    echo "Example: $0 0 10     # Process games 1-10"
    echo "         $0 10 20    # Process games 11-20"
    echo "         $0 40 50    # Process games 41-50"
    exit 1
fi

START=$1
END=$2

if [ $START -ge $END ]; then
    echo "Error: start_index must be less than end_index"
    exit 1
fi

echo "=== Processing games $((START+1))-$END ==="

# Extract the specific range of games with all their data
jq -r ".games[$START:$END][] | @base64" "$JSON_FILE" | \
{
    game_num=$START
    while IFS= read -r encoded_game; do
        game_num=$((game_num + 1))

        # Decode the game data
        game_data=$(echo "$encoded_game" | base64 -d)
        moves=$(echo "$game_data" | jq -r '.moves')

        # echo "Processing game $game_num: ${moves:0:60}..."

        if cabal exec cli -- process-moves "$moves" --silent-success --allow-repetition; then
            # echo "  ✓ Success"
	    continue;
        else
            # Extract game information for failed validation
            game_id=$(echo "$game_data" | jq -r '.game_id')
            white_player=$(echo "$game_data" | jq -r '.white_player.name')
            white_location=$(echo "$game_data" | jq -r '.white_player.location // "Unknown"')
            black_player=$(echo "$game_data" | jq -r '.black_player.name')
            black_location=$(echo "$game_data" | jq -r '.black_player.location // "Unknown"')
            variant=$(echo "$game_data" | jq -r '.variant')
            winner=$(echo "$game_data" | jq -r '.winner')
            result=$(echo "$game_data" | jq -r '.result')
            start_date=$(echo "$game_data" | jq -r '.start_date')
            finish_date=$(echo "$game_data" | jq -r '.finish_date')
            duration=$(echo "$game_data" | jq -r '.duration')

            echo "=== VALIDATION FAILED: Game $game_num ==="
            echo "Game ID: $game_id"
            echo "White Player: $white_player ($white_location)"
            echo "Black Player: $black_player ($black_location)"
            echo "Variant: $variant"
            echo "Winner: $winner"
            echo "Result: $result"
            echo "Start Date: $start_date"
            echo "Finish Date: $finish_date"
            echo "Duration: $duration"
            echo "Moves: $moves"
            echo "========================================"
	    # exit 1;
        fi
        echo
    done
}
