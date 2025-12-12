--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_10
--
--  Purpose: Day 10 - Factory (Indicator Light Button Minimization)
--
--  Responsibilities:
--    * Parse machine definitions with target light states and button wirings
--    * Solve GF(2) linear system to find minimum button presses per machine
--    * Part 1: Return sum of minimum presses across all machines
--
--  Design Notes:
--    * Each machine has indicator lights (initially off) and buttons
--    * Buttons toggle specific lights (XOR operation)
--    * Goal: configure lights to match target with fewest button presses
--    * Since pressing twice = not pressing, reduces to GF(2) linear algebra
--    * Uses Gaussian elimination to find minimum Hamming weight solution
--
--  External Effects: File I/O (reading puzzle input)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Core algorithms verified (Silver - AoRTE)

pragma Ada_2022;

package AoC_2025_Day_10
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

end AoC_2025_Day_10;
