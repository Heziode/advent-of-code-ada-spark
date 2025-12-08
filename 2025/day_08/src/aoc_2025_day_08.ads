--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_08
--
--  Purpose: Day 08 - Playground (Junction Boxes in 3D Space)
--
--  Responsibilities:
--    * Parse 3D junction box coordinates (X,Y,Z) from input
--    * Connect the 1000 closest pairs using squared Euclidean distance
--    * Track circuits using Union-Find data structure
--    * Compute product of 3 largest circuit sizes
--
--  Design Notes:
--    * Uses squared distance to avoid sqrt (SPARK-friendly)
--    * Only keeps the smallest 1000 pairs to save memory
--    * Union-Find with rank-based union for efficiency
--
--  External Effects: File I/O (reading puzzle input)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Core algorithms verified (Silver - AoRTE)

pragma Ada_2022;

package AoC_2025_Day_08
  with SPARK_Mode => On
is

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

end AoC_2025_Day_08;
