#!/bin/bash

# format_code.sh
# Script to format Ada code in different projects of the repository

# Exit immediately if a command exits with a non-zero status
set -e

echo "=== Code Formatter ==="

alr exec -- gnatformat -P advent_of_code.gpr

echo "=== Code formatting complete ==="