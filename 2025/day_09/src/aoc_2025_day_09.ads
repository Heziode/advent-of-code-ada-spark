--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_09
--
--  Purpose: Day 09 - Movie Theater (Largest Rectangle from Red Tiles)
--
--  Responsibilities:
--    * Parse 2D coordinates (X,Y) of red tiles from input
--    * Find the largest rectangle using any two red tiles as opposite corners
--    * Part 1: Return the maximum rectangle area
--
--  Design Notes:
--    * Rectangle area = |x2 - x1| * |y2 - y1| for opposite corners (x1,y1) and (x2,y2)
--    * O(n²) algorithm iterating all pairs of tiles
--    * Coordinates can be large (up to ~100,000 based on input)
--
--  External Effects: File I/O (reading puzzle input)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Core algorithms verified (Silver - AoRTE)

pragma Ada_2022;

package AoC_2025_Day_09 with SPARK_Mode => On is

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

end AoC_2025_Day_09;
