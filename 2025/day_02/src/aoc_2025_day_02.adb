--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_02
--
--  Algorithm:
--    A "double pattern" is a number whose decimal representation is a sequence
--    repeated twice. For example, 1212 = "12" & "12".
--
--    For efficiency, instead of checking every number in a range, we generate
--    double patterns directly. A double pattern with 2n digits is formed by
--    taking an n-digit number and repeating it.
--
--    For a range [start, end], we enumerate all possible half-lengths (1 to 9
--    digits for our MAX_DIGITS=18), compute the double pattern from each
--    possible half-value, and sum those that fall within the range.
--
--  SPARK_Mode Off justification:
--    - Resources package uses 'Address, 'Access, Unchecked_Deallocation
--    - File I/O operations cannot be formally verified

pragma Ada_2022;

with AoC_Common.File_IO;
with Resources;
with Aoc_2025_Day_02_Config;

package body AoC_2025_Day_02
  with SPARK_Mode => Off
is

   ---------------------------------------------------------------------------
   --  Resource Path Helper
   ---------------------------------------------------------------------------

   package Day_02_Resources is new Resources (Aoc_2025_Day_02_Config.Crate_Name);

   ---------------------------------------------------------------------------
   --  Power of 10 Lookup (for performance)
   ---------------------------------------------------------------------------

   --  We only need powers up to 10^9 for half-patterns (max 9 digits for 18-digit doubles)
   type Power_10_Array is array (0 .. MAX_DIGITS / 2) of Long_Long_Integer;

   Powers_Of_10 : constant Power_10_Array :=
     [0 => 1,
      1 => 10,
      2 => 100,
      3 => 1_000,
      4 => 10_000,
      5 => 100_000,
      6 => 1_000_000,
      7 => 10_000_000,
      8 => 100_000_000,
      9 => 1_000_000_000];

   ---------------------------------------------------------------------------
   --  Digit_Count Implementation
   ---------------------------------------------------------------------------

   function Digit_Count (N : Product_ID) return Positive is
      Count : Positive := 1;
      Value : Product_ID := N;
   begin
      while Value >= 10 loop
         Value := Value / 10;
         Count := Count + 1;
      end loop;
      return Count;
   end Digit_Count;

   ---------------------------------------------------------------------------
   --  Has_Even_Digit_Count Implementation
   ---------------------------------------------------------------------------

   function Has_Even_Digit_Count (N : Product_ID) return Boolean is
   begin
      return Digit_Count (N) mod 2 = 0;
   end Has_Even_Digit_Count;

   ---------------------------------------------------------------------------
   --  Is_Double_Pattern Implementation
   ---------------------------------------------------------------------------

   function Is_Double_Pattern (N : Product_ID) return Boolean is
      Num_Digits  : Positive;
      Half_Digits : Positive;
      Divisor     : Product_ID;
      First_Half  : Product_ID;
      Second_Half : Product_ID;
   begin
      if N < 11 then
         --  Single digit numbers can't be double patterns (need at least 2 digits)
         return False;
      end if;

      Num_Digits := Digit_Count (N);

      --  Must have even number of digits
      if Num_Digits mod 2 /= 0 then
         return False;
      end if;

      Half_Digits := Num_Digits / 2;
      Divisor := Powers_Of_10 (Half_Digits);

      First_Half := N / Divisor;
      Second_Half := N mod Divisor;

      --  The second half must have exactly Half_Digits digits
      --  (no leading zeros allowed, but 0 = 00...0 is special)
      --  Actually, since we require no leading zeros in input, if N has
      --  2*k digits, then first_half has k digits (no leading zero),
      --  and for it to be a double pattern, second_half must equal first_half
      --  which also has k digits. The only issue is if second_half has
      --  fewer digits (leading zeros), which means first_half would need
      --  leading zeros too, making them unequal.

      return First_Half = Second_Half;
   end Is_Double_Pattern;

   ---------------------------------------------------------------------------
   --  Make_Double_Pattern: Create double pattern from half-value
   ---------------------------------------------------------------------------

   function Make_Double_Pattern (Half_Value : Product_ID; Half_Digits : Positive) return Product_ID is
   begin
      return Half_Value * Powers_Of_10 (Half_Digits) + Half_Value;
   end Make_Double_Pattern;

   ---------------------------------------------------------------------------
   --  Next_Double_Pattern Implementation
   ---------------------------------------------------------------------------

   function Next_Double_Pattern (Start : Product_ID; End_Val : Product_ID) return Product_ID is
   begin
      --  Iterate through possible half-digit counts (1 to MAX_DIGITS/2)
      for Half_Digits in 1 .. MAX_DIGITS / 2 loop
         declare
            --  Range of half-values for this digit count
            Min_Half : constant Product_ID := (if Half_Digits = 1 then 1 else Powers_Of_10 (Half_Digits - 1));
            Max_Half : constant Product_ID := Powers_Of_10 (Half_Digits) - 1;

            --  Find the range of half-values that produce doubles in [Start, End_Val]
            Min_Double : constant Product_ID := Make_Double_Pattern (Min_Half, Half_Digits);
            Max_Double : constant Product_ID := Make_Double_Pattern (Max_Half, Half_Digits);
         begin
            --  Skip if all doubles for this digit count are outside range
            if Max_Double < Start then
               goto Continue;
            end if;

            if Min_Double > End_Val then
               --  All remaining digit counts will produce even larger doubles
               return 0;
            end if;

            --  Find first half-value whose double >= Start
            for Half in Min_Half .. Max_Half loop
               declare
                  Double_Val : constant Product_ID := Make_Double_Pattern (Half, Half_Digits);
               begin
                  if Double_Val >= Start and then Double_Val <= End_Val then
                     return Double_Val;
                  end if;

                  if Double_Val > End_Val then
                     exit;  --  No more in this digit count

                  end if;
               end;
            end loop;
         end;

         <<Continue>>
      end loop;

      return 0;
   end Next_Double_Pattern;

   ---------------------------------------------------------------------------
   --  Sum_Double_Patterns_In_Range: Efficient enumeration (Part 1)
   ---------------------------------------------------------------------------

   function Sum_Double_Patterns_In_Range (Range_Start : Product_ID; Range_End : Product_ID) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      if Range_Start > Range_End then
         return 0;
      end if;

      --  Iterate through possible half-digit counts
      for Half_Digits in 1 .. MAX_DIGITS / 2 loop
         declare
            Min_Half : constant Product_ID := (if Half_Digits = 1 then 1 else Powers_Of_10 (Half_Digits - 1));
            Max_Half : constant Product_ID := Powers_Of_10 (Half_Digits) - 1;

            Min_Double : constant Product_ID := Make_Double_Pattern (Min_Half, Half_Digits);
            Max_Double : constant Product_ID := Make_Double_Pattern (Max_Half, Half_Digits);

            --  Actual range of half-values that produce doubles in our range
            Actual_Min_Half : Product_ID;
            Actual_Max_Half : Product_ID;
         begin
            --  Skip if all doubles for this digit count are outside range
            if Max_Double < Range_Start or else Min_Double > Range_End then
               goto Continue;
            end if;

            --  Find the actual min half-value whose double >= Range_Start
            --  double = half * 10^k + half = half * (10^k + 1)
            --  So half >= Range_Start / (10^k + 1)
            declare
               Multiplier : constant Product_ID := Powers_Of_10 (Half_Digits) + 1;
            begin
               Actual_Min_Half := (Range_Start + Multiplier - 1) / Multiplier;  --  Ceiling division
               if Actual_Min_Half < Min_Half then
                  Actual_Min_Half := Min_Half;
               end if;

               --  Find the actual max half-value whose double <= Range_End
               Actual_Max_Half := Range_End / Multiplier;  --  Floor division
               if Actual_Max_Half > Max_Half then
                  Actual_Max_Half := Max_Half;
               end if;

               --  Sum all doubles in this range
               if Actual_Min_Half <= Actual_Max_Half then
                  --  Sum of arithmetic sequence: half * (10^k + 1) for half in [a, b]
                  --  = (10^k + 1) * Sum(half for half in [a, b])
                  --  = (10^k + 1) * (a + a+1 + ... + b)
                  --  = (10^k + 1) * (b - a + 1) * (a + b) / 2
                  declare
                     Count      : constant Long_Long_Integer :=
                       Long_Long_Integer (Actual_Max_Half - Actual_Min_Half + 1);
                     Sum_Halves : constant Long_Long_Integer :=
                       Count * Long_Long_Integer (Actual_Min_Half + Actual_Max_Half) / 2;
                  begin
                     Total := Total + Long_Long_Integer (Multiplier) * Sum_Halves;
                  end;
               end if;
            end;
         end;

         <<Continue>>
      end loop;

      return Total;
   end Sum_Double_Patterns_In_Range;

   ---------------------------------------------------------------------------
   --  Part 2: Check if a number is ANY repeated pattern (k >= 2 repetitions)
   ---------------------------------------------------------------------------

   function Is_Repeated_Pattern (N : Product_ID) return Boolean is
      Num_Digits : Positive;
   begin
      if N < 11 then
         return False;  --  Need at least 2 digits

      end if;

      Num_Digits := Digit_Count (N);

      --  Try each possible base length that divides Num_Digits
      --  Base length B means K = Num_Digits / B repetitions (K >= 2)
      for Base_Len in 1 .. Num_Digits / 2 loop
         if Num_Digits mod Base_Len = 0 then
            declare
               K             : constant Positive := Num_Digits / Base_Len;  --  Number of repetitions
               Divisor       : Long_Long_Integer := 1;
               Base_Pattern  : Product_ID;
               Reconstructed : Long_Long_Integer;
               Multiplier    : Long_Long_Integer;
            begin
               --  Calculate 10^Base_Len
               for I in 1 .. Base_Len loop
                  Divisor := Divisor * 10;
               end loop;

               --  Extract the base pattern (first Base_Len digits)
               --  We need to get N / 10^(Num_Digits - Base_Len)
               declare
                  Shift : Long_Long_Integer := 1;
               begin
                  for I in 1 .. Num_Digits - Base_Len loop
                     Shift := Shift * 10;
                  end loop;
                  Base_Pattern := Product_ID (Long_Long_Integer (N) / Shift);
               end;

               --  Reconstruct: base * (10^(B*(K-1)) + 10^(B*(K-2)) + ... + 1)
               --  = base * ((10^(B*K) - 1) / (10^B - 1))
               --  Or just build it iteratively
               Reconstructed := 0;
               Multiplier := 1;
               for I in 1 .. K loop
                  Reconstructed := Reconstructed + Long_Long_Integer (Base_Pattern) * Multiplier;
                  if I < K then
                     Multiplier := Multiplier * Divisor;
                  end if;
               end loop;

               if Reconstructed = Long_Long_Integer (N) then
                  return True;
               end if;
            end;
         end if;
      end loop;

      return False;
   end Is_Repeated_Pattern;

   ---------------------------------------------------------------------------
   --  Part 2: Sum all repeated patterns in range
   --  Strategy: For each base length B, enumerate patterns with PRIMITIVE base
   --  (i.e., the base itself is not a repeated pattern)
   ---------------------------------------------------------------------------

   function Is_Primitive_Base (Base : Product_ID; Base_Len : Positive) return Boolean is
   begin
      --  A base is primitive if it's not itself a repeated pattern
      --  For single-digit bases, they are always primitive
      if Base_Len = 1 then
         return True;
      end if;

      --  Check if this base can be decomposed into smaller repetitions
      for Sub_Len in 1 .. Base_Len / 2 loop
         if Base_Len mod Sub_Len = 0 then
            declare
               K             : constant Positive := Base_Len / Sub_Len;
               Divisor       : Long_Long_Integer := 1;
               Sub_Pattern   : Product_ID;
               Reconstructed : Long_Long_Integer;
               Multiplier    : Long_Long_Integer;
               Shift         : Long_Long_Integer := 1;
            begin
               for I in 1 .. Sub_Len loop
                  Divisor := Divisor * 10;
               end loop;

               for I in 1 .. Base_Len - Sub_Len loop
                  Shift := Shift * 10;
               end loop;
               Sub_Pattern := Product_ID (Long_Long_Integer (Base) / Shift);

               Reconstructed := 0;
               Multiplier := 1;
               for I in 1 .. K loop
                  Reconstructed := Reconstructed + Long_Long_Integer (Sub_Pattern) * Multiplier;
                  if I < K then
                     Multiplier := Multiplier * Divisor;
                  end if;
               end loop;

               if Reconstructed = Long_Long_Integer (Base) then
                  return False;  --  Base is itself a repeated pattern

               end if;
            end;
         end if;
      end loop;

      return True;
   end Is_Primitive_Base;

   function Make_Repeated_Pattern (Base : Product_ID; Base_Len : Positive; K : Positive) return Long_Long_Integer is
      Result     : Long_Long_Integer := 0;
      Multiplier : Long_Long_Integer := 1;
      Divisor    : Long_Long_Integer := 1;
   begin
      for I in 1 .. Base_Len loop
         Divisor := Divisor * 10;
      end loop;

      for I in 1 .. K loop
         Result := Result + Long_Long_Integer (Base) * Multiplier;
         if I < K then
            Multiplier := Multiplier * Divisor;
         end if;
      end loop;

      return Result;
   end Make_Repeated_Pattern;

   function Sum_Repeated_Patterns_In_Range (Range_Start : Product_ID; Range_End : Product_ID) return Long_Long_Integer
   is
      Total      : Long_Long_Integer := 0;
      End_Digits : Positive;
   begin
      if Range_Start > Range_End then
         return 0;
      end if;

      if Range_End < 11 then
         return 0;  --  No repeated patterns below 11

      end if;

      End_Digits := Digit_Count (Range_End);

      --  For each base length (1 to 9 for 18-digit max)
      for Base_Len in 1 .. MAX_DIGITS / 2 loop
         declare
            Min_Base : constant Product_ID := (if Base_Len = 1 then 1 else Product_ID (Powers_Of_10 (Base_Len - 1)));
            Max_Base : constant Product_ID := Product_ID (Powers_Of_10 (Base_Len) - 1);
         begin
            --  For each number of repetitions K >= 2
            for K in 2 .. MAX_DIGITS / Base_Len loop
               declare
                  Total_Digits : constant Positive := Base_Len * K;
               begin
                  --  Skip if total digits exceed our range
                  if Total_Digits > End_Digits + 1 then
                     exit;  --  No need to try more K values

                  end if;

                  --  Enumerate all bases
                  for Base in Min_Base .. Max_Base loop
                     --  Only count if base is primitive (not itself a repeated pattern)
                     if Is_Primitive_Base (Base, Base_Len) then
                        declare
                           Pattern : constant Long_Long_Integer := Make_Repeated_Pattern (Base, Base_Len, K);
                        begin
                           if Pattern >= Long_Long_Integer (Range_Start)
                             and then Pattern <= Long_Long_Integer (Range_End)
                           then
                              Total := Total + Pattern;
                           end if;
                        end;
                     end if;
                  end loop;
               end;
            end loop;
         end;
      end loop;

      return Total;
   end Sum_Repeated_Patterns_In_Range;

   ---------------------------------------------------------------------------
   --  Input Parsing: Parse comma-separated ranges on a single line
   ---------------------------------------------------------------------------

   --  Parse a Long_Long_Integer from a string (needed for large product IDs)
   function Parse_Product_ID (S : String) return Product_ID is
      use AoC_Common.File_IO;
      Result    : Long_Long_Integer := 0;
      Start_Idx : Positive := S'First;
   begin
      --  Skip leading whitespace
      while Start_Idx <= S'Last and then Is_Whitespace (S (Start_Idx)) loop
         Start_Idx := Start_Idx + 1;
      end loop;

      if Start_Idx > S'Last then
         return 0;
      end if;

      --  Parse digits
      for I in Start_Idx .. S'Last loop
         if Is_Digit (S (I)) then
            Result := Result * 10 + Long_Long_Integer (Digit_Value (S (I)));
         else
            exit;
         end if;
      end loop;

      return Product_ID (Result);
   end Parse_Product_ID;

   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Total_Sum : Long_Long_Integer := 0;
   end record;

   pragma Warnings (Off, "not dispatching");
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Total_Sum >= 0);
   pragma Warnings (On, "not dispatching");

   procedure Process_Input_Line (Line : String; Data : in out Solver_State) is
      use AoC_Common.File_IO;

      Pos             : Positive := Line'First;
      Range_Start_Idx : Positive;
      Dash_Idx        : Natural;
      Range_End_Idx   : Natural;

      Range_Start : Product_ID;
      Range_End   : Product_ID;
   begin
      --  Parse format: "start1-end1,start2-end2,..."
      while Pos <= Line'Last loop
         --  Skip any leading whitespace or trailing garbage
         while Pos <= Line'Last and then (Line (Pos) = ' ' or else Line (Pos) = ASCII.LF or else Line (Pos) = ASCII.CR)
         loop
            Pos := Pos + 1;
         end loop;

         if Pos > Line'Last then
            exit;
         end if;

         --  Find the range: "start-end"
         Range_Start_Idx := Pos;

         --  Find the dash separator
         Dash_Idx := 0;
         while Pos <= Line'Last and then Line (Pos) /= ',' loop
            if Line (Pos) = '-' and then Dash_Idx = 0 then
               Dash_Idx := Pos;
            end if;
            Pos := Pos + 1;
         end loop;

         Range_End_Idx := Pos - 1;

         --  Skip trailing whitespace from range
         while Range_End_Idx >= Range_Start_Idx
           and then (Line (Range_End_Idx) = ' '
                     or else Line (Range_End_Idx) = ASCII.LF
                     or else Line (Range_End_Idx) = ASCII.CR)
         loop
            Range_End_Idx := Range_End_Idx - 1;
         end loop;

         if Dash_Idx > Range_Start_Idx and then Dash_Idx < Range_End_Idx then
            --  Parse start and end values using Long_Long_Integer parser
            Range_Start := Parse_Product_ID (Line (Range_Start_Idx .. Dash_Idx - 1));
            Range_End := Parse_Product_ID (Line (Dash_Idx + 1 .. Range_End_Idx));

            --  Sum patterns based on part
            case Data.Part is
               when AoC_Common.Part_1 =>
                  Data.Total_Sum := Data.Total_Sum + Sum_Double_Patterns_In_Range (Range_Start, Range_End);

               when AoC_Common.Part_2 =>
                  Data.Total_Sum := Data.Total_Sum + Sum_Repeated_Patterns_In_Range (Range_Start, Range_End);
            end case;
         end if;

         --  Skip the comma
         if Pos <= Line'Last and then Line (Pos) = ',' then
            Pos := Pos + 1;
         end if;
      end loop;
   end Process_Input_Line;

   procedure Process_File is new
     AoC_Common.File_IO.For_Each_Line
       (Data_Type       => Solver_State,
        Max_Line_Length => 65536,  --  Input might be one very long line
        Invariant       => State_Invariant,
        Process_Line    => Process_Input_Line);

   ---------------------------------------------------------------------------
   --  Result Formatting
   ---------------------------------------------------------------------------

   function Format_Result (N : Long_Long_Integer) return Result_String is
      Result : Result_String := BLANK_RESULT;
      Image  : constant String := Long_Long_Integer'Image (N);
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
      State   : Solver_State := (Total_Sum => 0, Part => AoC_Common.Part_1);
      Success : Boolean;
   begin
      Process_File (Filename => Day_02_Resources.Resource_Path & Filename, Success => Success, Data => State);

      if Success then
         return Format_Result (State.Total_Sum);
      else
         declare
            Error_Result : Result_String := BLANK_RESULT;
         begin
            Error_Result (Result_String'Last - 4 .. Result_String'Last) := "ERROR";
            return Error_Result;
         end;
      end if;
   end Solve_Part_1;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String is
      State   : Solver_State := (Total_Sum => 0, Part => AoC_Common.Part_2);
      Success : Boolean;
   begin
      Process_File (Filename => Day_02_Resources.Resource_Path & Filename, Success => Success, Data => State);

      if Success then
         return Format_Result (State.Total_Sum);
      else
         declare
            Error_Result : Result_String := BLANK_RESULT;
         begin
            Error_Result (Result_String'Last - 4 .. Result_String'Last) := "ERROR";
            return Error_Result;
         end;
      end if;
   end Solve_Part_2;

end AoC_2025_Day_02;
