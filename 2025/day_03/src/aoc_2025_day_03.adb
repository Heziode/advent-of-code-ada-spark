--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

pragma Ada_2022;

with AoC_Common;
with AoC_Common.File_IO;
with Resources;
with Aoc_2025_Day_03_Config;
with AoC_2025_Day_03.Algorithms;

package body AoC_2025_Day_03
  with SPARK_Mode => Off
is

   use AoC_Common;
   use AoC_Common.File_IO;
   use AoC_2025_Day_03.Algorithms;

   package Day_Resources is new Resources (Aoc_2025_Day_03_Config.Crate_Name);

   function Format_Result (N : Natural) return Result_String is
      Result : Result_String := BLANK_RESULT;
      Image  : constant String := Natural'Image (N);
   begin
      if Image'Length <= Result'Length then
         Result (Result'Last - Image'Length + 1 .. Result'Last) := Image;
      end if;
      return Result;
   end Format_Result;

   function Format_Result_Long (N : Long_Long_Integer) return Result_String is
      Result : Result_String := BLANK_RESULT;
      Image  : constant String := Long_Long_Integer'Image (N);
   begin
      if Image'Length <= Result'Length then
         Result (Result'Last - Image'Length + 1 .. Result'Last) := Image;
      end if;
      return Result;
   end Format_Result_Long;

   type Part_1_Context is new Data_Context with record
      Total_Joltage : Natural := 0;
   end record;

   function Part_1_Invariant (Data : Part_1_Context) return Boolean
   is (Data.Part = Part_1);

   procedure Process_Part_1_Line (Line : String; Data : in out Part_1_Context) with SPARK_Mode => Off is
      Bank_Joltage : Natural;
   begin
      if Line'Length >= 2 and then Line'Length <= MAX_BANK_LENGTH then
         Bank_Joltage := Maximum_Joltage_From_Bank (Line);
         if Data.Total_Joltage <= Natural'Last - Bank_Joltage then
            Data.Total_Joltage := Data.Total_Joltage + Bank_Joltage;
         end if;
      end if;
   end Process_Part_1_Line;

   procedure Process_Part_1_File is new
     For_Each_Line
       (Data_Type       => Part_1_Context,
        Max_Line_Length => MAX_BANK_LENGTH,
        Invariant       => Part_1_Invariant,
        Process_Line    => Process_Part_1_Line);

   type Part_2_Context is new Data_Context with record
      Total_Joltage : Long_Long_Integer := 0;
   end record;

   function Part_2_Invariant (Data : Part_2_Context) return Boolean
   is (Data.Part = Part_2);

   procedure Process_Part_2_Line (Line : String; Data : in out Part_2_Context) is
      Bank_Joltage : Long_Long_Integer;
   begin
      if Line'Length >= BATTERIES_TO_SELECT and then Line'Length <= MAX_BANK_LENGTH then
         Bank_Joltage := Maximum_Joltage_12_From_Bank (Line);
         if Data.Total_Joltage <= Long_Long_Integer'Last - Bank_Joltage then
            Data.Total_Joltage := Data.Total_Joltage + Bank_Joltage;
         end if;
      end if;
   end Process_Part_2_Line;

   procedure Process_Part_2_File is new
     For_Each_Line
       (Data_Type       => Part_2_Context,
        Max_Line_Length => MAX_BANK_LENGTH,
        Invariant       => Part_2_Invariant,
        Process_Line    => Process_Part_2_Line);

   function Solve_Part_1 (Filename : String := "example.txt") return Result_String is
      Context   : Part_1_Context := (Part => Part_1, Total_Joltage => 0);
      Success   : Boolean;
      Full_Path : constant String := Day_Resources.Resource_Path & "/" & Filename;
   begin
      Process_Part_1_File (Full_Path, Success, Context);
      if Success then
         return Format_Result (Context.Total_Joltage);
      else
         return BLANK_RESULT;
      end if;
   end Solve_Part_1;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String is
      Context   : Part_2_Context := (Part => Part_2, Total_Joltage => 0);
      Success   : Boolean;
      Full_Path : constant String := Day_Resources.Resource_Path & "/" & Filename;
   begin
      Process_Part_2_File (Full_Path, Success, Context);
      if Success then
         return Format_Result_Long (Context.Total_Joltage);
      else
         return BLANK_RESULT;
      end if;
   end Solve_Part_2;

end AoC_2025_Day_03;
