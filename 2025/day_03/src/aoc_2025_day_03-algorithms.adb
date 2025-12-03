--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

with AoC_Common; use AoC_Common;
with AoC_Common.Parsing; use AoC_Common.Parsing;

package body AoC_2025_Day_03.Algorithms with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  Part 1 Implementation
   ---------------------------------------------------------------------------

   function Maximum_Joltage_From_Bank (Bank : String) return Natural is
      subtype Bank_Index is Positive range 1 .. MAX_BANK_LENGTH;
      subtype Digit_Range is Natural range 0 .. 9;
      type Max_Suffix_Array is array (Bank_Index range <>) of Digit_Range;

      Bank_Length  : constant Natural := Bank'Length;
      Max_Suffix   : Max_Suffix_Array (1 .. Bank_Length) := [others => 0];
      Current_Max  : Digit_Range := 0;
      Best_Joltage : Natural := 0;
      First_Digit  : Digit_Range;
      Joltage      : Natural;
   begin
      if Bank_Length < 2 then
         return 0;
      end if;

      --  Build max-suffix array (max digit from position i+1 to end)
      --  We iterate from right to left
      for I in reverse 1 .. Bank_Length loop
         pragma Loop_Invariant (Bank_Length <= MAX_BANK_LENGTH);
         pragma Loop_Invariant (I in 1 .. Bank_Length);

         if I < Bank_Length then
            Max_Suffix (I) := Current_Max;
         end if;

         --  Update current max with digit at position I
         declare
            Char_At_I : constant Character := Bank (Bank'First + (I - 1));
         begin
            if Is_Digit (Char_At_I) then
               declare
                  Digit_At_I : constant Digit_Range := Digit_Value (Char_At_I);
               begin
                  if Digit_At_I > Current_Max then
                     Current_Max := Digit_At_I;
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Find best joltage: for each first position, compute digit * 10 + max_suffix
      for I in 1 .. Bank_Length - 1 loop
         pragma Loop_Invariant (Bank_Length <= MAX_BANK_LENGTH);
         pragma Loop_Invariant (I in 1 .. Bank_Length - 1);

         declare
            Char_At_I : constant Character := Bank (Bank'First + (I - 1));
         begin
            if Is_Digit (Char_At_I) and then Max_Suffix (I) > 0 then
               First_Digit := Digit_Value (Char_At_I);
               Joltage := First_Digit * 10 + Max_Suffix (I);
               if Joltage > Best_Joltage then
                  Best_Joltage := Joltage;
               end if;
            end if;
         end;
      end loop;

      return Best_Joltage;
   end Maximum_Joltage_From_Bank;

   ---------------------------------------------------------------------------
   --  Part 2 Implementation
   ---------------------------------------------------------------------------

   function Maximum_Joltage_12_From_Bank (Bank : String) return Long_Long_Integer is
      subtype Digit_Range is Natural range 0 .. 9;

      Bank_Length    : constant Natural := Bank'Length;
      Result_Joltage : Long_Long_Integer := 0;
      Current_Pos    : Positive := 1;
      Max_End_Pos    : Positive;
      Best_Digit     : Digit_Range;
      Best_Pos       : Positive;
   begin
      --  Need at least 12 characters
      if Bank_Length < BATTERIES_TO_SELECT then
         return 0;
      end if;

      --  Greedy selection of 12 digits
      for Pick in 1 .. BATTERIES_TO_SELECT loop
         pragma Loop_Invariant (Current_Pos >= 1);
         pragma Loop_Invariant (Current_Pos <= Bank_Length - (BATTERIES_TO_SELECT - Pick) + 1);
         pragma Loop_Invariant (Result_Joltage >= 0);
         pragma Loop_Invariant (Bank_Length <= MAX_BANK_LENGTH);

         --  Valid range: [Current_Pos, Bank_Length - (BATTERIES_TO_SELECT - Pick)]
         --  We need to leave enough positions for remaining picks
         Max_End_Pos := Bank_Length - (BATTERIES_TO_SELECT - Pick);

         --  Find the largest digit in the valid range
         Best_Digit := 0;
         Best_Pos := Current_Pos;

         for Pos in Current_Pos .. Max_End_Pos loop
            pragma Loop_Invariant (Pos in Current_Pos .. Max_End_Pos);
            pragma Loop_Invariant (Best_Pos in Current_Pos .. Max_End_Pos);
            pragma Loop_Invariant (Best_Pos <= Pos); -- Optimization hint

            declare
               Char_At_Pos : constant Character := Bank (Bank'First + (Pos - 1));
            begin
               if Is_Digit (Char_At_Pos) then
                  declare
                     Digit_At_Pos : constant Digit_Range := Digit_Value (Char_At_Pos);
                  begin
                     if Digit_At_Pos > Best_Digit then
                        Best_Digit := Digit_At_Pos;
                        Best_Pos := Pos;
                     end if;
                  end;
               end if;
            end;
         end loop;

         --  Check for potential overflow (though unlikely with 12 digits)
         if Result_Joltage > Long_Long_Integer'Last / 10 - 10 then
            return Result_Joltage;
         end if;

         --  Add the selected digit to the result
         Result_Joltage := Result_Joltage * 10 + Long_Long_Integer (Best_Digit);

         --  Move to the position after the selected digit
         Current_Pos := Best_Pos + 1;
      end loop;

      return Result_Joltage;
   end Maximum_Joltage_12_From_Bank;

end AoC_2025_Day_03.Algorithms;
