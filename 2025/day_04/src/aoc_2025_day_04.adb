--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_04
--
--  SPARK_Mode Off justification:
--    - Resources package uses 'Address, 'Access, Unchecked_Deallocation
--    - File I/O operations cannot be formally verified

pragma Ada_2022;

with AoC_Common;
with AoC_Common.File_IO;
with Resources;
with Aoc_2025_Day_04_Config;

package body AoC_2025_Day_04
  with SPARK_Mode => On
is

   use AoC_Common;
   use AoC_Common.File_IO;

   ---------------------------------------------------------------------------
   --  Resource Path Helper
   ---------------------------------------------------------------------------

   package Ress
     with SPARK_Mode => Off
   is
      package Day_Resources is new Resources (Aoc_2025_Day_04_Config.Crate_Name);
   end Ress;

   use Ress;

   MAX_LINE_LENGTH : constant := 200;
   MAX_ROWS : constant := 200;
   type Storage_Element is (None, Empty, Rolls_Of_Papper, Unlocked_Rolls_Of_Papper);

   subtype Row_Index is Natural range 1 .. MAX_ROWS;
   subtype Column_Index is Natural range 1 .. MAX_LINE_LENGTH;
   type Input_Array is array (Row_Index, Column_Index) of Storage_Element;

   type Part_1_Context is new Data_Context with record
      Input        : Input_Array := [others => [others => None]];
      Current_Line : Row_Index := Row_Index'First;
      Max_Columns  : Column_Index := Column_Index'First;
   end record;

   ---------------------------------------------------------------------------
   --  Forward Declarations
   ---------------------------------------------------------------------------

   function Format_Result (N : Integer) return Result_String;

   procedure Process_Part_1_Line (Line : String; Data : in out Part_1_Context)
   with SPARK_Mode => Off;

   function Count_Adjacent_Rolls_Of_Papers
     (Context : Part_1_Context; Current_Line : Row_Index; Current_Column : Column_Index) return Natural;

   procedure Process_Unlocking
     (Context : in out Part_1_Context; Result : in out Natural; State_Changed : out Boolean; Max_Row : Row_Index);

   procedure Clear_Unlocked_Rolls (Context : in out Part_1_Context; Max_Row : Row_Index);

   procedure Load_Input_File (Filename : String; Context : in out Part_1_Context; Success : out Boolean)
   with SPARK_Mode => Off;

   procedure Solve_Part_1_Core (Context : in out Part_1_Context; Result : out Natural);

   procedure Solve_Part_2_Core (Context : in out Part_1_Context; Result : out Natural);

   ---------------------------------------------------------------------------
   --  Result Formatting
   ---------------------------------------------------------------------------

   function Format_Result (N : Integer) return Result_String is
      Result : Result_String := BLANK_RESULT;
      Image  : constant String := Integer'Image (N);
   begin
      if Image'Length <= Result'Length then
         Result (Result'Last - Image'Length + 1 .. Result'Last) := Image;
      end if;
      return Result;
   end Format_Result;

   procedure Process_Part_1_Line (Line : String; Data : in out Part_1_Context) with SPARK_Mode => Off is
   begin
      if Data.Current_Line = 1 then
         Data.Max_Columns := Line'Length;
      end if;

      for Index in 0 .. Line'Length - 1 loop
         case Line (Line'First + Index) is
            when '.' =>
               Data.Input (Data.Current_Line, Data.Input'First(2) + Index) := Empty;

            when '@' =>
               Data.Input (Data.Current_Line, Data.Input'First(2) + Index) := Rolls_Of_Papper;

            when others =>
               null;
         end case;
      end loop;

      if Data.Current_Line /= Row_Index'Last then
         Data.Current_Line := Data.Current_Line + 1;
      end if;
   end Process_Part_1_Line;

   function Part_1_Invariant (Data : Part_1_Context) return Boolean
   is (Data.Part = Part_1);

   procedure Process_Part_1_File is new
     For_Each_Line
       (Data_Type       => Part_1_Context,
        Max_Line_Length => MAX_LINE_LENGTH,
        Invariant       => Part_1_Invariant,
        Process_Line    => Process_Part_1_Line);

   ---------------------------------------------------------------------------
   --  Solve Functions
   ---------------------------------------------------------------------------

   function Count_Adjacent_Rolls_Of_Papers
     (Context : Part_1_Context; Current_Line : Row_Index; Current_Column : Column_Index) return Natural
   is
      Result : Natural := 0;
   begin
      for Row
        in Row_Index'Max (Current_Line - 1, Row_Index'First) .. Row_Index'Min (Current_Line + 1, Context.Current_Line)
      loop
         for Column
           in Column_Index'Max (Current_Column - 1, Column_Index'First)
           .. Column_Index'Min (Current_Column + 1, Context.Max_Columns)
         loop
            if ((Row /= Current_Line or Column /= Current_Column)
                and then Context.Input (Row, Column) in Rolls_Of_Papper .. Unlocked_Rolls_Of_Papper)
            then
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Count_Adjacent_Rolls_Of_Papers;

   ---------------------------------------------------------------------------
   --  Helper Procedures
   ---------------------------------------------------------------------------

   procedure Process_Unlocking
     (Context : in out Part_1_Context; Result : in out Natural; State_Changed : out Boolean; Max_Row : Row_Index) is
   begin
      State_Changed := False;
      for Row in Row_Index'First .. Max_Row loop
         for Column in Column_Index'First .. Context.Max_Columns loop
            if Context.Input (Row, Column) = Rolls_Of_Papper then
               if Count_Adjacent_Rolls_Of_Papers (Context, Row, Column) < 4 then
                  Context.Input (Row, Column) := Unlocked_Rolls_Of_Papper;
                  Result := Result + 1;
                  State_Changed := True;
               end if;
            end if;
         end loop;
      end loop;
   end Process_Unlocking;

   procedure Clear_Unlocked_Rolls (Context : in out Part_1_Context; Max_Row : Row_Index) is
   begin
      for Row in Row_Index'First .. Max_Row loop
         for Column in Column_Index'First .. Context.Max_Columns loop
            if Context.Input (Row, Column) = Unlocked_Rolls_Of_Papper then
               Context.Input (Row, Column) := Empty;
            end if;
         end loop;
      end loop;
   end Clear_Unlocked_Rolls;

   procedure Load_Input_File (Filename : String; Context : in out Part_1_Context; Success : out Boolean)
   with SPARK_Mode => Off
   is
      Full_Path : constant String := Day_Resources.Resource_Path & "/" & Filename;
   begin
      Process_Part_1_File (Full_Path, Success, Context);
   end Load_Input_File;

   procedure Solve_Part_1_Core (Context : in out Part_1_Context; Result : out Natural) is
      State_Changed : Boolean;
   begin
      Result := 0;
      Process_Unlocking (Context, Result, State_Changed, Context.Current_Line - 1);
   end Solve_Part_1_Core;

   function Solve_Part_1 (Filename : String := "example.txt") return Result_String with SPARK_Mode => Off is
      Context : Part_1_Context :=
        (Part => Part_1, Input => [others => [others => None]], Current_Line => 1, Max_Columns => 1);
      Success : Boolean;
      Result  : Natural;
   begin
      Load_Input_File (Filename, Context, Success);

      if not Success then
         return BLANK_RESULT;
      end if;

      Solve_Part_1_Core (Context, Result);
      return Format_Result (Result);
   end Solve_Part_1;

   procedure Solve_Part_2_Core (Context : in out Part_1_Context; Result : out Natural) with SPARK_Mode => On is
      State_Changed : Boolean := True;
   begin
      Result := 0;
      while State_Changed loop
         Process_Unlocking (Context, Result, State_Changed, Context.Current_Line);
         Clear_Unlocked_Rolls (Context, Context.Current_Line);
      end loop;
   end Solve_Part_2_Core;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String with SPARK_Mode => Off is
      Context : Part_1_Context :=
        (Part => Part_1, Input => [others => [others => None]], Current_Line => 1, Max_Columns => 1);
      Success : Boolean;
      Result  : Natural;
   begin
      Load_Input_File (Filename, Context, Success);

      if not Success then
         return BLANK_RESULT;
      end if;

      Solve_Part_2_Core (Context, Result);
      return Format_Result (Result);
   end Solve_Part_2;

end AoC_2025_Day_04;
