--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_05
--
--  Purpose: Day 05 - Cafeteria (Fresh Ingredient Checker)
--
--  Algorithm: Delegates to AoC_2025_Day_05.Solver for core logic.
--
--  SPARK_Mode Off justification:
--    - File I/O operations cannot be formally verified at this level.
--    - Core logic is fully verified in the Solver child package.

pragma Ada_2022;

with Ada.Text_IO;
with Resources;
with Aoc_2025_Day_05_Config;
with AoC_2025_Day_05.Solver;
with AoC_Common.File_IO;

package body AoC_2025_Day_05 with SPARK_Mode => Off is

   package Day_Resources is new Resources (Aoc_2025_Day_05_Config.Crate_Name);

   ---------------------------------------------------------------------------
   --  Instantiation of File Processor
   ---------------------------------------------------------------------------

   procedure Process_All_Lines is new AoC_Common.File_IO.For_Each_Line
     (Data_Type       => Solver.Solver_State,
      Max_Line_Length => 1024,
      Invariant       => Solver.State_Invariant,
      Process_Line    => Solver.Process_Line);

   ---------------------------------------------------------------------------
   --  Solvers
   ---------------------------------------------------------------------------

   function Solve_Part_1 (Filename : String := "example.txt") return Result_String is
      Full_Path : constant String := Day_Resources.Resource_Path & "/" & Filename;
      State     : Solver.Solver_State;
      Success   : Boolean;
   begin
      Solver.Initialize (State);
      
      Process_All_Lines (Full_Path, Success, State);

      if not Success or else State.Error_Encountered then
         Ada.Text_IO.Put_Line ("Error: Failed to process input file or limits exceeded.");
         return BLANK_RESULT;
      end if;

      declare
         Result : constant Long_Long_Integer := Solver.Solve_Part_1 (State);
         Image  : constant String := Long_Long_Integer'Image (Result);
         Formatted : Result_String := BLANK_RESULT;
      begin
         if Image'Length <= Formatted'Length then
            Formatted (Formatted'Last - Image'Length + 1 .. Formatted'Last) := Image;
         end if;
         return Formatted;
      end;
   end Solve_Part_1;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String is
      Full_Path : constant String := Day_Resources.Resource_Path & "/" & Filename;
      State     : Solver.Solver_State;
      Success   : Boolean;
   begin
      Solver.Initialize (State);
      
      Process_All_Lines (Full_Path, Success, State);

      if not Success or else State.Error_Encountered then
         Ada.Text_IO.Put_Line ("Error: Failed to process input file or limits exceeded.");
         return BLANK_RESULT;
      end if;

      declare
         Result : constant Long_Long_Integer := Solver.Solve_Part_2 (State);
         Image  : constant String := Long_Long_Integer'Image (Result);
         Formatted : Result_String := BLANK_RESULT;
      begin
         if Image'Length <= Formatted'Length then
            Formatted (Formatted'Last - Image'Length + 1 .. Formatted'Last) := Image;
         end if;
         return Formatted;
      end;
   end Solve_Part_2;

end AoC_2025_Day_05;
