# Advent of Code - SPARK-Verified Solutions

SPARK/Ada solutions for [Advent of Code](https://adventofcode.com/) puzzles with formal verification (Silver level - Absence of Runtime Errors).

## Overview

This project provides:
- **SPARK-verified** puzzle solutions with formal proofs of absence of runtime errors
- **Cascading Alire project structure** supporting multiple years
- **Shared utility library** (`AoC_Common`) with reusable, proven components
- **Ada 2022** standard with modern language features

## Project Structure

```
Advent_Of_Code/
├── alire.toml              # Root project manifest
├── advent_of_code.gpr      # Aggregate project (builds all)
├── src/
│   └── aoc_common/         # Shared SPARK-verified library
│       ├── aoc_common.gpr
│       └── src/
│           ├── aoc_common.ads/adb           # Core types (Coordinate, Dimensions)
│           ├── aoc_common-directions.ads/adb # Cardinal/ordinal directions
│           ├── aoc_common-grids.ads/adb     # 2D grid utilities
│           ├── aoc_common-file_io.ads/adb   # File parsing helpers
│           └── aoc_common-parsing.ads/adb   # String parsing utilities
└── 2025/
    ├── aoc_2025.gpr        # Year 2025 aggregate
    ├── day_01/             # Day 01 solution
    │   ├── alire.toml
    │   ├── aoc_2025_day_01.gpr
    │   ├── src/
    │   └── share/          # Puzzle input files
    └── day_02/             # Day 02 solution
        └── ...
```

## Requirements

- **GNAT** >= 12 (Ada 2022 support)
- **Alire** package manager
- **GNATprove** >= 15.1.0 (for SPARK verification)

## Building

### Build Everything

```bash
alr build
```

### Build a Specific Day

```bash
cd 2025/day_01
alr build
```

### Run a Solution

```bash
cd 2025/day_01
./bin/main
```

## SPARK Verification

All core algorithms are formally verified using GNATprove at Silver level (Absence of Runtime Errors):

```bash
cd 2025/day_01
alr exec -- gnatprove -P aoc_2025_day_01.gpr --mode=silver
```

### Verification Scope

- **Verified (SPARK_Mode => On):** Pure algorithmic functions, coordinate arithmetic, parsing utilities
- **Excluded (SPARK_Mode => Off):** File I/O operations (external effects cannot be formally verified)

## Adding a New Day

1. Copy an existing day scaffold:
   ```bash
   cp -r 2025/day_01 2025/day_XX
   ```

2. Rename packages in all files:
   - `AoC_2025_Day_01` → `AoC_2025_Day_XX`
   - Update `alire.toml`, `.gpr`, and source files

3. Update `2025/aoc_2025.gpr` to include the new day

4. Add puzzle input to `share/aoc_2025_day_XX/input.txt`

5. Implement `Solve_Part_1` and `Solve_Part_2`

## Adding a New Year

1. Copy the year structure:
   ```bash
   cp -r 2025 YYYY
   ```

2. Rename all packages from `2025` to `YYYY`

3. Update root `advent_of_code.gpr` to include the new year aggregate

## Completed Puzzles

### 2025

| Day | Title | Part 1 | Part 2 | SPARK |
|-----|-------|--------|--------|-------|
| 01 | [Secret Entrance](2025/day_01/) | ⭐ | ⭐ | ✅ |
| 02 | [Gift Shop](2025/day_02/) | ⭐ | ⭐ | ✅ |

## License

MIT License - Copyright (c) 2025 Heziode

## Links

- [Advent of Code](https://adventofcode.com/)
- [Alire Package Manager](https://alire.ada.dev/)
- [SPARK User's Guide](https://docs.adacore.com/spark2014-docs/html/ug/)
