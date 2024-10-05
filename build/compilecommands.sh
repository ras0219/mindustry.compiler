#!/bin/bash

set -euo pipefail

# Check if input is provided
if [ -t 0 ]; then
    echo "No input provided. Please pipe the output of 'make -nB' to this script."
    exit 1
fi

# Create the JSON array
echo "["

first=true

# Process each line from stdin
while IFS= read -r line; do
    # Skip empty lines
    [[ -z "$line" ]] && continue

    # Extract the command and output file
    # We assume that the last argument is the output file
    command="$line"
    output_file=$(echo "$line" | awk '{print $NF}')  # Get the last argument

    # Create the JSON entry
    if [ "$first" = true ]; then
        first=false
    else
        echo ","
    fi

    # Construct the JSON entry
    echo "  {"
    echo "    \"directory\": \"$(pwd)\","
    echo "    \"file\": \"$output_file\","
    echo "    \"command\": \"$command\""
    echo "  }"
done

# Close the JSON array
echo "]"
