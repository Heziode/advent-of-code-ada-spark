--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_06
--
--  Purpose: Day 06 - Trash Compactor (Cephalopod Math Worksheet)
--
--  Responsibilities:
--    * Parse columnar math worksheet format
--    * Solve addition and multiplication problems arranged vertically
--    * Compute grand total of all problem results
--
--  External Effects: File I/O (reading puzzle input)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Core algorithms verified (Silver - AoRTE)

pragma Ada_2022;

package AoC_2025_Day_06
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

end AoC_2025_Day_06;
