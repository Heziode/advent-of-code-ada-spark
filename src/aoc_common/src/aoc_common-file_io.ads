--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_Common.File_IO
--
--  Purpose: File I/O utilities for Advent of Code puzzles
--
--  Responsibilities:
--    * Provide line-by-line file processing with SPARK contracts
--    * Offer SPARK-proven string utilities (index searching, digit parsing)
--    * Enable safe number parsing with overflow protection
--
--  Design Notes:
--    * File I/O operations have SPARK_Mode Off due to external effects
--    * Pure string manipulation functions are fully SPARK-verified
--
--  External Effects: File I/O (reading puzzle input files)
--  Thread Safety: Not applicable (single-threaded I/O)
--  SPARK Status: Mixed (I/O excluded, string ops verified)

pragma Ada_2022;

package AoC_Common.File_IO with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  Parse Result Types
   ---------------------------------------------------------------------------

   --  Result of parsing an integer from string
   type Parse_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Integer;
         when False => null;
      end case;
   end record;

   --  Result of parsing a natural number from string
   type Natural_Parse_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Natural;
         when False => null;
      end case;
   end record;

   ---------------------------------------------------------------------------
   --  String Searching Functions (SPARK Verified)
   ---------------------------------------------------------------------------

   --  Find first occurrence of a character in a string
   --
   --  @param Source The string to search
   --  @param Pattern The character to find
   --  @return Index of Pattern in Source, or 0 if not found
   function Find_First
      (Source  : String;
       Pattern : Character) return Natural
   with
      Post => Find_First'Result = 0 or else
              (Find_First'Result in Source'Range and then
               Source (Find_First'Result) = Pattern);

   --  Find last occurrence of a character in a string
   --
   --  @param Source The string to search
   --  @param Pattern The character to find
   --  @return Index of last Pattern in Source, or 0 if not found
   function Find_Last
      (Source  : String;
       Pattern : Character) return Natural
   with
      Post => Find_Last'Result = 0 or else
              (Find_Last'Result in Source'Range and then
               Source (Find_Last'Result) = Pattern);

   ---------------------------------------------------------------------------
   --  Character Classification (SPARK Verified)
   ---------------------------------------------------------------------------

   --  Check if character is a decimal digit
   --
   --  @param C The character to test
   --  @return True if C is '0' .. '9'
   function Is_Digit (C : Character) return Boolean is
      (C in '0' .. '9')
   with Inline;

   --  Check if character is whitespace (space, tab, CR, LF)
   --
   --  @param C The character to test
   --  @return True if C is whitespace
   function Is_Whitespace (C : Character) return Boolean is
      (C = ' ' or else C = ASCII.HT or else C = ASCII.CR or else C = ASCII.LF)
   with Inline;

   ---------------------------------------------------------------------------
   --  Digit Conversion (SPARK Verified)
   ---------------------------------------------------------------------------

   --  Convert a digit character to its numeric value
   --
   --  @param C A digit character ('0' .. '9')
   --  @return The numeric value (0 .. 9)
   function Digit_Value (C : Character) return Natural
   with
      Pre  => Is_Digit (C),
      Post => Digit_Value'Result in 0 .. 9;

   ---------------------------------------------------------------------------
   --  Number Parsing (SPARK Verified)
   ---------------------------------------------------------------------------

   --  Parse a natural number from string with overflow protection
   --
   --  @param Input The string containing the number
   --  @return Parse result with Value if valid, or invalid result
   function Parse_Natural (Input : String) return Natural_Parse_Result
   with
      Post => (if Parse_Natural'Result.Valid then
                  Parse_Natural'Result.Value >= 0);

   --  Parse an integer (possibly negative) from string
   --
   --  @param Input The string containing the number
   --  @return Parse result with Value if valid, or invalid result
   function Parse_Integer (Input : String) return Parse_Result;

   ---------------------------------------------------------------------------
   --  String Trimming (SPARK Verified)
   ---------------------------------------------------------------------------

   --  Find the index of first non-whitespace character
   --
   --  @param S The string to examine
   --  @return Index of first non-whitespace, or 0 if all whitespace
   function First_Non_Whitespace (S : String) return Natural
   with
      Pre  => S'Length > 0,
      Post => First_Non_Whitespace'Result = 0 or else
              First_Non_Whitespace'Result in S'Range;

   --  Find the index of last non-whitespace character
   --
   --  @param S The string to examine
   --  @return Index of last non-whitespace, or 0 if all whitespace
   function Last_Non_Whitespace (S : String) return Natural
   with
      Pre  => S'Length > 0,
      Post => Last_Non_Whitespace'Result = 0 or else
              Last_Non_Whitespace'Result in S'Range;

   ---------------------------------------------------------------------------
   --  File Processing (Non-SPARK - External I/O)
   ---------------------------------------------------------------------------

   type Data_Context is tagged record
      Part : Puzzle_Part;
   end record;

   --  Generic procedure for line-by-line file processing
   --
   --  @generic Data_Type The user-defined data structure for results
   --  @generic Max_Line_Length Maximum expected line length (default 4096)
   --  @generic Invariant Function that must remain true throughout processing
   --  @generic Process_Line Procedure to handle each line
   generic
      type Data_Type is new Data_Context with private;
      Max_Line_Length : Positive := 4096;
      with function Invariant (Data : Data_Type) return Boolean;
      with procedure Process_Line
         (Line : String;
          Data : in out Data_Type);
   procedure For_Each_Line
      (Filename : String;
       Success  : out Boolean;
       Data     : in out Data_Type)
   with
      SPARK_Mode => On;

end AoC_Common.File_IO;
