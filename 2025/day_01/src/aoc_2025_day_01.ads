--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_01
--
--  Purpose: Day 01 - Combination Lock
--
--  A safe has a dial (0-99) starting at 50. Follow rotation instructions
--  (L/R with distance) and count specific conditions related to position 0.
--
--  Part 1: Count how many times the dial lands on 0 after a rotation
--  Part 2: Count how many times we pass through 0 during all rotations
--
--  External Effects: File I/O (reading puzzle input)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Core algorithms verified (Silver - AoRTE)

pragma Ada_2022;

package AoC_2025_Day_01
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Dial Configuration
   ---------------------------------------------------------------------------

   DIAL_SIZE : constant := 100;  --  Positions 0 to 99
   DIAL_START : constant := 50;   --  Initial position

   subtype Dial_Position is Natural range 0 .. DIAL_SIZE - 1;

   --  Direction of rotation
   type Direction_Type is (Left, Right);

   ---------------------------------------------------------------------------
   --  Result Type
   ---------------------------------------------------------------------------

   MAX_RESULT_LENGTH : constant := 100;
   subtype Result_String is String (1 .. MAX_RESULT_LENGTH);
   BLANK_RESULT : constant Result_String := [others => ' '];

   ---------------------------------------------------------------------------
   --  Pure Algorithmic Functions (SPARK Verified)
   ---------------------------------------------------------------------------

   --  Apply a rotation to the dial position
   --
   --  @param Position Current dial position
   --  @param Dir Direction of rotation (Left decreases, Right increases)
   --  @param Distance Number of positions to rotate
   --  @return New dial position after rotation
   function Apply_Rotation (Position : Dial_Position; Dir : Direction_Type; Distance : Natural) return Dial_Position;

   --  Count how many times we pass through or land on 0 during a rotation
   --
   --  @param Start_Pos Starting dial position
   --  @param Dir Direction of rotation
   --  @param Distance Number of positions to rotate
   --  @return Number of times we pass through or land on 0
   function Count_Zero_Passes (Start_Pos : Dial_Position; Dir : Direction_Type; Distance : Natural) return Natural;

   ---------------------------------------------------------------------------
   --  Puzzle Solvers (File I/O required - not SPARK)
   ---------------------------------------------------------------------------
   --  Solve Part 1: Count landings on 0
   function Solve_Part_1 (Filename : String := "example.txt") return Result_String
   with SPARK_Mode => On;

   --  Solve Part 2: Count passes through 0
   function Solve_Part_2 (Filename : String := "example.txt") return Result_String
   with SPARK_Mode => On;

end AoC_2025_Day_01;
