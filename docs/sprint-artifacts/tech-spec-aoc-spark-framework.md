# Tech-Spec: Advent of Code SPARK Framework

**Created:** 2025-12-01
**Status:** Completed

## Overview

### Problem Statement

Advent of Code (AoC) is an annual programming challenge with daily puzzles throughout December. The goal is to create a scalable, SPARK-verified Ada framework to solve AoC puzzles with formal proof of absence of runtime errors (AoRTE - Silver level).

### Solution

Build a cascading Alire project structure supporting multiple years and days, with a shared SPARK-proven utility library (`AoC_Common`) providing reusable components for common AoC patterns (grids, directions, parsing, etc.).

### Scope

**In Scope:**
- Root aggregate project structure
- Year 2025 aggregate project
- Day 01 scaffold (template for future days)
- `AoC_Common` shared library with initial utilities
- SPARK Silver level proof configuration
- Unit test setup for shared library

**Out of Scope:**
- Actual puzzle solutions (implemented per-day as puzzles release)
- GUI or web interface
- Performance benchmarking infrastructure
- CI/CD pipeline (can be added later)

## Context for Development

### Codebase Patterns

**Ada Style Guide Compliance (from `.windsurf/rules/ada-style-guide.md`):**
- Packages: `Pascal_Case` with underscore (e.g., `AoC_Common`)
- Types: `Pascal_Case` with `_Type` suffix for private types
- Constants: `ALL_CAPS` (e.g., `MAX_GRID_SIZE`)
- Functions (Boolean): `Is_`/`Has_`/`Can_` prefix
- Functions (Non-Bool): Descriptive nouns
- Procedures: `<Verb><Object>` pattern
- Exceptions: `E_` prefix (e.g., `E_Invalid_Input`)

**SPARK Patterns:**
- All packages with `SPARK_Mode => On`
- Contract-based design with Pre/Post conditions
- Ghost code for specification where needed
- Avoid access types, tasking, and exceptions in SPARK code

**Package Header Template:**
```ada
--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: <Package_Name>
--
--  Purpose: <Clear description>
--
--  SPARK Status: Verified (Silver - AoRTE)
```

### Files to Reference

- `.windsurf/rules/ada-style-guide.md` - Naming and documentation conventions

### Technical Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Ada Standard | Ada 2022 | Latest features, best SPARK support |
| Proof Level | Silver (AoRTE) | Absence of runtime errors without full functional proof |
| Package Manager | Alire | Standard Ada ecosystem tool |
| Project Structure | Cascading aggregates | Scalable, independent day builds |
| Resource Handling | `alire-project/resources` | Clean resource file access per day |
| Grid Coordinates | Integer-based | Simple, SPARK-friendly |

## Implementation Plan

### Tasks

- [x] **Task 1:** Create root project structure
  - `alire.toml` (aggregate)
  - `advent_of_code.gpr` (aggregate project)

- [x] **Task 2:** Create `AoC_Common` shared library
  - `src/aoc_common/alire.toml`
  - `src/aoc_common/aoc_common.gpr`
  - `src/aoc_common/src/aoc_common.ads` (root package)
  - `src/aoc_common/src/aoc_common-grids.ads/adb` (2D grid utilities)
  - `src/aoc_common/src/aoc_common-directions.ads/adb` (cardinal directions)
  - `src/aoc_common/src/aoc_common-parsing.ads/adb` (input parsing helpers)

- [x] **Task 3:** Create Year 2025 aggregate project
  - `2025/alire.toml`
  - `2025/aoc_2025.gpr`

- [x] **Task 4:** Create Day 01 scaffold (template)
  - `2025/day_01/alire.toml`
  - `2025/day_01/aoc_2025_day_01.gpr`
  - `2025/day_01/src/aoc_2025_day_01.ads/adb`
  - `2025/day_01/src/main.adb`
  - `2025/day_01/resources/input.txt` (placeholder)

- [x] **Task 5:** Configure SPARK proof settings
  - Add `gnatprove` configuration to `.gpr` files
  - Verify proof passes with `alr exec -- gnatprove -P <project>.gpr --mode=silver`

- [x] **Task 6:** Set up unit tests for `AoC_Common`
  - Test project structure
  - Initial test cases for grid/direction utilities

### Acceptance Criteria

- [x] **AC 1:** Root project builds successfully with `alr build`
- [x] **AC 2:** Day 01 executable runs and outputs placeholder "Part 1: TBD / Part 2: TBD"
- [x] **AC 3:** `AoC_Common` library compiles with SPARK_Mode => On
- [x] **AC 4:** `gnatprove --mode=silver` passes on all SPARK code with no unproven VCs
- [x] **AC 5:** Project structure allows adding new years/days without modifying existing code
- [x] **AC 6:** All code follows Ada style guide conventions

## Additional Context

### Dependencies

| Dependency | Alire Command | Purpose |
|------------|---------------|---------|
| `resources` | `alr with resources` | Load puzzle input files |
| `distance` | `alr with distance` | Distance calculations (Manhattan, etc.) |
| `spark_math` | `alr with spark_math` | SPARK-proven math (Sqrt, GCD, etc.) |

### Testing Strategy

- **Unit Tests:** `AoC_Common` utilities (grid operations, parsing)
- **Integration Tests:** Day solutions against known AoC example inputs
- **Proof:** GNATprove Silver level on all SPARK-annotated code

### Notes

**Adding a New Day:**
1. Copy `day_01/` scaffold to `day_XX/`
2. Rename packages: `AoC_2025_Day_01` -> `AoC_2025_Day_XX`
3. Update `2025/aoc_2025.gpr` to include new day
4. Add puzzle input to `resources/input.txt`
5. Implement `Solve_Part_1` and `Solve_Part_2`

**Adding a New Year:**
1. Copy `2025/` structure to `<YEAR>/`
2. Rename packages accordingly
3. Update root `advent_of_code.gpr` to include new year

**SPARK Proof Command:**
```bash
alr exec -- gnatprove -P aoc_2025_day_01.gpr --mode=silver
```

**Common AoC Patterns in `AoC_Common`:**
- 2D Grid traversal with bounds checking
- Cardinal/Ordinal direction enumeration
- Coordinate arithmetic with overflow protection
- Line-by-line input parsing
- Number extraction from strings
