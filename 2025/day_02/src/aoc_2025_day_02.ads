--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_02
--
--  Purpose: Day 02 - Gift Shop
--
--  Find invalid product IDs in ranges. An invalid ID is one whose decimal
--  representation is a sequence of digits repeated exactly twice.
--  Examples: 55 ("5" twice), 1212 ("12" twice), 123123 ("123" twice)
--
--  Part 1: Sum all invalid IDs found in the given ranges
--
--  External Effects: File I/O (reading puzzle input)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Core algorithms verified (Silver - AoRTE)

pragma Ada_2022;

package AoC_2025_Day_02
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum number of digits we support (fits in Long_Long_Integer)
   --  Long_Long_Integer'Last is 9_223_372_036_854_775_807 (19 digits)
   --  We use 18 digits to have safe margin for arithmetic
   MAX_DIGITS : constant := 18;

   --  Maximum value we can represent
   MAX_VALUE : constant := 10 ** MAX_DIGITS - 1;

   subtype Product_ID is Long_Long_Integer range 0 .. MAX_VALUE;

   ---------------------------------------------------------------------------
   --  Result Type
   ---------------------------------------------------------------------------

   MAX_RESULT_LENGTH : constant := 100;
   subtype Result_String is String (1 .. MAX_RESULT_LENGTH);
   BLANK_RESULT : constant Result_String := [others => ' '];

   ---------------------------------------------------------------------------
   --  Pure Algorithmic Functions (SPARK Verified)
   ---------------------------------------------------------------------------

   --  Check if a number has an even number of digits
   --
   --  @param N The number to check (must be positive)
   --  @return True if digit count is even
   function Has_Even_Digit_Count (N : Product_ID) return Boolean
   with Pre => N > 0;

   --  Get the number of digits in a positive number
   --
   --  @param N The number to count digits for
   --  @return Number of digits (1 to MAX_DIGITS)
   function Digit_Count (N : Product_ID) return Positive
   with Pre => N > 0, Post => Digit_Count'Result in 1 .. MAX_DIGITS;

   --  Check if a number is a "double pattern" - its digit sequence repeated twice
   --
   --  A double pattern number's string representation consists of the same
   --  sequence of digits appearing exactly twice consecutively.
   --  Examples: 11, 55, 1212, 123123, 1234512345
   --
   --  @param N The number to check
   --  @return True if N is a double pattern
   function Is_Double_Pattern (N : Product_ID) return Boolean;

   --  Find the next double pattern >= Start that is <= End_Val
   --
   --  @param Start Minimum value to search from
   --  @param End_Val Maximum value to search up to
   --  @return Next double pattern, or 0 if none found
   function Next_Double_Pattern (Start : Product_ID; End_Val : Product_ID) return Product_ID
   with
     Pre => Start <= End_Val,
     Post =>
       Next_Double_Pattern'Result = 0
       or else (Next_Double_Pattern'Result >= Start
                and then Next_Double_Pattern'Result <= End_Val
                and then Is_Double_Pattern (Next_Double_Pattern'Result));

   ---------------------------------------------------------------------------
   --  Puzzle Solvers (File I/O required)
   ---------------------------------------------------------------------------

   --  Solve Part 1: Sum all invalid IDs in ranges
   function Solve_Part_1 (Filename : String := "example.txt") return Result_String
   with SPARK_Mode => On;

   --  Solve Part 2: Placeholder
   function Solve_Part_2 (Filename : String := "example.txt") return Result_String
   with SPARK_Mode => On;

end AoC_2025_Day_02;
