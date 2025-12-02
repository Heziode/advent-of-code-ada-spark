#!/bin/bash
#-------------------------------------------------------------------------------
#  Advent of Code - New Day Generator
#  Copyright (c) 2025 Heziode
#  SPDX-License-Identifier: MIT
#-------------------------------------------------------------------------------
#
#  Usage: ./scripts/new_day.sh [OPTIONS]
#
#  Options:
#    -y, --year YEAR    Year (default: current year)
#    -d, --day DAY      Day number (required, 1-25)
#    -t, --title TITLE  Puzzle title (default: "Day XX")
#    -h, --help         Show this help message
#
#  Examples:
#    ./scripts/new_day.sh -d 3
#    ./scripts/new_day.sh -y 2025 -d 3 -t "Gear Ratios"
#    ./scripts/new_day.sh --year 2024 --day 15 --title "Warehouse Woes"
#
#-------------------------------------------------------------------------------

set -e

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default values
YEAR=$(date +%Y)
DAY=""
TITLE=""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

show_help() {
    head -25 "$0" | tail -20
    exit 0
}

error() {
    echo -e "${RED}Error:${NC} $1" >&2
    exit 1
}

info() {
    echo -e "${GREEN}✓${NC} $1"
}

warn() {
    echo -e "${YELLOW}!${NC} $1"
}

#-------------------------------------------------------------------------------
# Parse arguments
#-------------------------------------------------------------------------------

while [[ $# -gt 0 ]]; do
    case $1 in
        -y|--year)
            YEAR="$2"
            shift 2
            ;;
        -d|--day)
            DAY="$2"
            shift 2
            ;;
        -t|--title)
            TITLE="$2"
            shift 2
            ;;
        -h|--help)
            show_help
            ;;
        *)
            error "Unknown option: $1"
            ;;
    esac
done

# Validate day
if [[ -z "$DAY" ]]; then
    error "Day number is required. Use -d or --day to specify."
fi

if ! [[ "$DAY" =~ ^[0-9]+$ ]] || [[ "$DAY" -lt 1 ]] || [[ "$DAY" -gt 25 ]]; then
    error "Day must be a number between 1 and 25."
fi

# Validate year
if ! [[ "$YEAR" =~ ^[0-9]{4}$ ]]; then
    error "Year must be a 4-digit number."
fi

# Format day with leading zero
DAY_PADDED=$(printf "%02d" "$DAY")

# Default title
if [[ -z "$TITLE" ]]; then
    TITLE="Day $DAY_PADDED"
fi

#-------------------------------------------------------------------------------
# Derived values
#-------------------------------------------------------------------------------

YEAR_DIR="$PROJECT_ROOT/$YEAR"
DAY_DIR="$YEAR_DIR/day_$DAY_PADDED"
PKG_NAME="AoC_${YEAR}_Day_${DAY_PADDED}"
PKG_NAME_LOWER=$(echo "$PKG_NAME" | tr '[:upper:]' '[:lower:]')
CRATE_NAME="${PKG_NAME_LOWER}"
# Alire config package uses Title_Case: Aoc_2025_Day_03_Config
CONFIG_PKG_NAME="Aoc_${YEAR}_Day_${DAY_PADDED}_Config"

echo ""
echo "Creating Advent of Code $YEAR - Day $DAY_PADDED: $TITLE"
echo "========================================================="
echo ""

#-------------------------------------------------------------------------------
# Check if day already exists
#-------------------------------------------------------------------------------

if [[ -d "$DAY_DIR" ]]; then
    error "Directory $DAY_DIR already exists!"
fi

#-------------------------------------------------------------------------------
# Create year structure if needed
#-------------------------------------------------------------------------------

if [[ ! -d "$YEAR_DIR" ]]; then
    warn "Year $YEAR does not exist. Creating year structure..."

    mkdir -p "$YEAR_DIR"

    # Create year's alire.toml
    cat > "$YEAR_DIR/alire.toml" << EOF
name = "aoc_$YEAR"
description = "Advent of Code $YEAR - SPARK-verified solutions"
version = "0.1.0-dev"
authors = ["Heziode"]
maintainers = ["Heziode <heziode@users.noreply.github.com>"]
maintainers-logins = ["Heziode"]
licenses = "MIT"
tags = ["advent-of-code", "spark", "puzzles", "$YEAR"]

[build-switches]
"*".style_checks = ["-gnatyy", "-gnatyM120", "-gnatw.X"]
"*".ada_version = ["-gnat2022"]

[[depends-on]]
gnat = ">=12"
resources = "~0.1.0"
gnatprove = "^15.1.0"
EOF
    info "Created $YEAR_DIR/alire.toml"

    # Create year's aggregate project
    cat > "$YEAR_DIR/aoc_$YEAR.gpr" << EOF
--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) $YEAR Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Project: AoC_$YEAR (Aggregate)
--
--  Purpose: Year $YEAR aggregate project

aggregate project AoC_$YEAR is

   for Project_Files use (
      "day_$DAY_PADDED/${PKG_NAME_LOWER}.gpr"
   );

end AoC_$YEAR;
EOF
    info "Created $YEAR_DIR/aoc_$YEAR.gpr"

    # Update root aggregate project
    if grep -q "\"$YEAR/aoc_$YEAR.gpr\"" "$PROJECT_ROOT/advent_of_code.gpr" 2>/dev/null; then
        info "Root project already includes year $YEAR"
    else
        warn "Remember to add \"$YEAR/aoc_$YEAR.gpr\" to $PROJECT_ROOT/advent_of_code.gpr"
    fi
else
    info "Year $YEAR already exists"
fi

#-------------------------------------------------------------------------------
# Create day directory structure
#-------------------------------------------------------------------------------

mkdir -p "$DAY_DIR/src"
mkdir -p "$DAY_DIR/share/$CRATE_NAME"
info "Created directory structure"

#-------------------------------------------------------------------------------
# Create alire.toml
#-------------------------------------------------------------------------------

cat > "$DAY_DIR/alire.toml" << EOF
name = "$CRATE_NAME"
description = "Advent of Code $YEAR Day $DAY_PADDED - SPARK-verified solution"
version = "0.1.0-dev"
authors = ["Heziode"]
maintainers = ["Heziode <heziode@users.noreply.github.com>"]
maintainers-logins = ["Heziode"]
licenses = "MIT"
tags = ["advent-of-code", "spark", "puzzles", "$YEAR", "day$DAY_PADDED"]

executables = ["main_${PKG_NAME_LOWER}"]

[build-switches]
"*".style_checks = ["-gnatyy", "-gnatyM120", "-gnatw.X"]
"*".ada_version = ["-gnat2022"]

[[depends-on]]
gnat = ">=12"
resources = "*"
gnatprove = "^15.1.0"

[[pins]]
aoc_common = { path = "../../src/aoc_common" }
EOF
info "Created alire.toml"

#-------------------------------------------------------------------------------
# Create .gpr project file
#-------------------------------------------------------------------------------

cat > "$DAY_DIR/${PKG_NAME_LOWER}.gpr" << EOF
--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) $YEAR Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Project: $PKG_NAME
--
--  Purpose: Day $DAY_PADDED puzzle solution for Advent of Code $YEAR
--
--  SPARK Status: Verified (Silver - AoRTE)

with "config/${PKG_NAME_LOWER}_config.gpr";
with "../../src/aoc_common/aoc_common.gpr";
with "resources.gpr";

project $PKG_NAME is

   for Source_Dirs use ("src", "config");
   for Object_Dir use "obj";
   for Exec_Dir use "bin";
   for Main use ("main_${PKG_NAME_LOWER}.adb");

   package Compiler is
      for Default_Switches ("Ada") use (
         "-gnat2022",       --  Ada 2022 standard
         "-gnatyy",         --  Style checks
         "-gnatyM120",      --  Max line length 120
         "-gnatw.X",        --  Warnings
         "-gnatwa",         --  All warnings
         "-gnatVa",         --  All validity checks
         "-gnata"           --  Assertions enabled
      );
   end Compiler;

   package Binder is
      for Switches ("Ada") use ("-Es");  --  Symbolic traceback
   end Binder;

   package Prove is
      for Proof_Switches ("Ada") use (
         "--mode=silver",
         "--warnings=continue",
         "--counterexamples=on"
      );
   end Prove;

   package Install is
      for Artifacts (".") use ("share");
   end Install;

end $PKG_NAME;
EOF
info "Created ${PKG_NAME_LOWER}.gpr"

#-------------------------------------------------------------------------------
# Create package specification
#-------------------------------------------------------------------------------

cat > "$DAY_DIR/src/${PKG_NAME_LOWER}.ads" << EOF
--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) $YEAR Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: $PKG_NAME
--
--  Purpose: Day $DAY_PADDED - $TITLE
--
--  TODO: Add puzzle description here
--
--  External Effects: File I/O (reading puzzle input)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Core algorithms verified (Silver - AoRTE)

pragma Ada_2022;

package $PKG_NAME with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  Result Type
   ---------------------------------------------------------------------------

   MAX_RESULT_LENGTH : constant := 100;
   subtype Result_String is String (1 .. MAX_RESULT_LENGTH);
   BLANK_RESULT : constant Result_String := [others => ' '];

   ---------------------------------------------------------------------------
   --  Puzzle Solvers
   ---------------------------------------------------------------------------

   --  Solve Part 1
   function Solve_Part_1 (Filename : String := "example.txt") return Result_String
   with SPARK_Mode => On;

   --  Solve Part 2
   function Solve_Part_2 (Filename : String := "example.txt") return Result_String
   with SPARK_Mode => On;

end $PKG_NAME;
EOF
info "Created ${PKG_NAME_LOWER}.ads"

#-------------------------------------------------------------------------------
# Create package body
#-------------------------------------------------------------------------------

cat > "$DAY_DIR/src/${PKG_NAME_LOWER}.adb" << EOF
--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) $YEAR Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: $PKG_NAME
--
--  SPARK_Mode Off justification:
--    - Resources package uses 'Address, 'Access, Unchecked_Deallocation
--    - File I/O operations cannot be formally verified

pragma Ada_2022;

with AoC_Common.File_IO;
with Resources;
with ${CONFIG_PKG_NAME};

package body $PKG_NAME with SPARK_Mode => Off is

   ---------------------------------------------------------------------------
   --  Resource Path Helper
   ---------------------------------------------------------------------------

   package Day_Resources is new Resources (${CONFIG_PKG_NAME}.Crate_Name);

   ---------------------------------------------------------------------------
   --  Result Formatting
   ---------------------------------------------------------------------------

   function Format_Result (N : Integer) return Result_String is
      Result : Result_String := BLANK_RESULT;
      Image  : constant String := Integer'Image (N);
   begin
      if Image'Length <= Result'Length then
         Result (Result'Last - Image'Length + 1 .. Result'Last) := Image;
      end if;
      return Result;
   end Format_Result;

   ---------------------------------------------------------------------------
   --  Solve Functions
   ---------------------------------------------------------------------------

   function Solve_Part_1 (Filename : String := "example.txt") return Result_String is
      pragma Unreferenced (Filename);
      Result : Result_String := BLANK_RESULT;
   begin
      --  TODO: Implement Part 1
      Result (Result_String'Last - 2 .. Result_String'Last) := "TBD";
      return Result;
   end Solve_Part_1;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String is
      pragma Unreferenced (Filename);
      Result : Result_String := BLANK_RESULT;
   begin
      --  TODO: Implement Part 2
      Result (Result_String'Last - 2 .. Result_String'Last) := "TBD";
      return Result;
   end Solve_Part_2;

end $PKG_NAME;
EOF
info "Created ${PKG_NAME_LOWER}.adb"

#-------------------------------------------------------------------------------
# Create main_${PKG_NAME_LOWER}.adb
#-------------------------------------------------------------------------------

cat > "$DAY_DIR/src/main_${PKG_NAME_LOWER}.adb" << EOF
--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) $YEAR Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Main: $PKG_NAME Runner
--
--  Purpose: Execute and display Day $DAY_PADDED puzzle solutions

pragma Ada_2022;

with Ada.Text_IO;
with $PKG_NAME;

procedure Main with SPARK_Mode => On is
   use Ada.Text_IO;

   --  Trim leading spaces from a string
   function Trim_Left (S : String) return String
   with
      Post => Trim_Left'Result'Length <= S'Length
   is
   begin
      if S'Length = 0 then
         return "";
      end if;

      for I in S'Range loop
         if S (I) /= ' ' then
            return S (I .. S'Last);
         end if;
         pragma Loop_Invariant (for all J in S'First .. I => S (J) = ' ');
      end loop;

      return "";
   end Trim_Left;

begin
   Put_Line ("Advent of Code $YEAR - Day $DAY_PADDED (example)");
   Put_Line ("======================================");
   New_Line;

   Put_Line ("Part 1: " & Trim_Left (${PKG_NAME}.Solve_Part_1));
   Put_Line ("Part 2: " & Trim_Left (${PKG_NAME}.Solve_Part_2));
   New_Line;

   Put_Line ("Advent of Code $YEAR - Day $DAY_PADDED");
   Put_Line ("============================");
   New_Line;

   Put_Line ("Part 1: " & Trim_Left (${PKG_NAME}.Solve_Part_1 ("input.txt")));
   Put_Line ("Part 2: " & Trim_Left (${PKG_NAME}.Solve_Part_2 ("input.txt")));
end Main;
EOF
info "Created main_${PKG_NAME_LOWER}.adb"

#-------------------------------------------------------------------------------
# Create input files
#-------------------------------------------------------------------------------

cat > "$DAY_DIR/share/$CRATE_NAME/example.txt" << EOF
EOF
info "Created example.txt (empty)"

cat > "$DAY_DIR/share/$CRATE_NAME/input.txt" << EOF
EOF
info "Created input.txt (empty)"

#-------------------------------------------------------------------------------
# Create README
#-------------------------------------------------------------------------------

cat > "$DAY_DIR/README.md" << EOF
# Day $DAY_PADDED: $TITLE

## Problem

TODO: Add puzzle description here.

### Input Format

TODO: Describe input format.

### Part 1

TODO: Describe Part 1.

### Part 2

TODO: Describe Part 2.

## Solution

\`\`\`bash
# Build
alr build

# Run
./bin/main_${PKG_NAME_LOWER}
\`\`\`
EOF
info "Created README.md"

#-------------------------------------------------------------------------------
# Update year's aggregate project if day not already included
#-------------------------------------------------------------------------------

YEAR_GPR="$YEAR_DIR/aoc_$YEAR.gpr"
DAY_GPR_REF="day_$DAY_PADDED/${PKG_NAME_LOWER}.gpr"

if [[ -f "$YEAR_GPR" ]]; then
    # Check if the day is already included (exact match, not in comments)
    if grep -E "^\s*\"$DAY_GPR_REF\"" "$YEAR_GPR" > /dev/null 2>&1; then
        info "Year project already includes day $DAY_PADDED"
    else
        # Find the last .gpr entry and add a comma + new entry after it
        # This handles various formatting styles

        # Create a temp file with the update
        awk -v new_entry="$DAY_GPR_REF" '
        /for Project_Files use/ { in_list = 1 }
        in_list && /\.gpr"/ && !added {
            last_gpr_line = NR
            last_gpr_content = $0
        }
        in_list && /\);/ {
            in_list = 0
            if (last_gpr_line > 0 && !added) {
                # We need to add comma to previous line and insert new entry
                add_before = NR
            }
        }
        { lines[NR] = $0 }
        END {
            for (i = 1; i <= NR; i++) {
                if (i == last_gpr_line) {
                    # Add comma if not present
                    line = lines[i]
                    if (line !~ /,$/) {
                        sub(/"[^"]*\.gpr"/, "&,", line)
                    }
                    print line
                } else if (i == add_before) {
                    # Insert new entry before closing );
                    printf "      \"%s\"\n", new_entry
                    print lines[i]
                } else {
                    print lines[i]
                }
            }
        }
        ' "$YEAR_GPR" > "$YEAR_GPR.tmp"

        mv "$YEAR_GPR.tmp" "$YEAR_GPR"
        info "Updated $YEAR_GPR to include day $DAY_PADDED"
    fi
fi

#-------------------------------------------------------------------------------
# Done!
#-------------------------------------------------------------------------------

echo ""
echo -e "${GREEN}Success!${NC} Day $DAY_PADDED scaffold created."
echo ""
echo "Next steps:"
echo "  1. cd $DAY_DIR"
echo "  2. Add puzzle input to share/$CRATE_NAME/example.txt and input.txt"
echo "  3. Implement Solve_Part_1 and Solve_Part_2 in src/${PKG_NAME_LOWER}.adb"
echo "  4. Build with: alr build"
echo "  5. Run with: ./bin/main_${PKG_NAME_LOWER}"
echo "  6. Verify with: alr exec -- gnatprove -P ${PKG_NAME_LOWER}.gpr --mode=silver"
echo ""
