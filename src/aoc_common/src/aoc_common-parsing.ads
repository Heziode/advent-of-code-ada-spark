--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_Common.Parsing
--
--  Purpose: Input parsing utilities for Advent of Code puzzles
--
--  Responsibilities:
--    * Parse integers from strings safely
--    * Extract numbers from mixed text
--    * Provide character classification helpers
--
--  External Effects: None (pure package)
--  Thread Safety: Thread-safe (no mutable state)
--  SPARK Status: Not verified (parsing complexity)
--
--  Note: Parsing functions involve complex loop invariants and bounded string
--  indexing that require significant proof effort. For AoC puzzles, runtime
--  correctness via testing is acceptable for this utility code.

package AoC_Common.Parsing is

   --  Maximum length of input lines we handle
   MAX_LINE_LENGTH : constant := 10_000;

   subtype Line_Length is Natural range 0 .. MAX_LINE_LENGTH;
   subtype Line_Index is Positive range 1 .. MAX_LINE_LENGTH;

   --  Result of parsing an integer
   type Parse_Result is record
      Value   : Integer := 0;
      Success : Boolean := False;
      Next    : Natural := 0;  --  Index after the parsed number (0 if failed)
   end record;

   --  Check if a character is a digit
   --
   --  @param C The character to check
   --  @return True if C is '0' .. '9'
   function Is_Digit (C : Character) return Boolean
   is (C >= '0' and then C <= '9')
   with Inline;

   --  Check if a character is whitespace
   --
   --  @param C The character to check
   --  @return True if C is space, tab, CR, or LF
   function Is_Whitespace (C : Character) return Boolean
   is (C = ' ' or else C = ASCII.HT or else C = ASCII.CR or else C = ASCII.LF)
   with Inline;

   --  Convert a digit character to its integer value
   --
   --  @param C A digit character ('0' .. '9')
   --  @return Integer value 0 .. 9
   function Digit_Value (C : Character) return Natural
   with Pre => Is_Digit (C), Post => Digit_Value'Result <= 9;

   --  Parse an integer starting at a given position
   --
   --  Handles optional leading minus sign. Stops at first non-digit.
   --  Returns Success = False if no valid integer found at position.
   --
   --  @param S The string to parse
   --  @param Start Starting index in the string
   --  @return Parse result with value, success flag, and next index
   function Parse_Integer (S : String; Start : Positive) return Parse_Result
   with Pre => S'Length > 0 and then Start >= S'First and then Start <= S'Last;

   --  Find and parse the next integer in a string
   --
   --  Skips non-digit characters (except minus before digit) to find the next number.
   --
   --  @param S The string to search
   --  @param Start Starting index for the search
   --  @return Parse result with value, success flag, and next index
   function Find_Next_Integer
     (S : String; Start : Positive) return Parse_Result
   with Pre => S'Length > 0 and then Start >= S'First and then Start <= S'Last;

   --  Skip whitespace and return index of next non-whitespace character
   --
   --  @param S The string to scan
   --  @param Start Starting index
   --  @return Index of first non-whitespace, or S'Last + 1 if all whitespace
   function Skip_Whitespace (S : String; Start : Positive) return Natural
   with
     Pre => S'Length > 0 and then Start >= S'First and then Start <= S'Last,
     Post =>
       Skip_Whitespace'Result >= Start
       and then Skip_Whitespace'Result <= S'Last + 1;

end AoC_Common.Parsing;
