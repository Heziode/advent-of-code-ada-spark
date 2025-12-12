--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_12
--
--  SPARK_Mode Off justification:
--    - Resources package uses 'Address, 'Access, Unchecked_Deallocation
--    - File I/O operations cannot be formally verified

pragma Ada_2022;

with AoC_Common.File_IO;
with AoC_2025_Day_12.Solver;
with Resources;
with Aoc_2025_Day_12_Config;

package body AoC_2025_Day_12 with SPARK_Mode => Off is

   ---------------------------------------------------------------------------
   --  Resource Path Helper
   ---------------------------------------------------------------------------

   package Day_Resources is new Resources (Aoc_2025_Day_12_Config.Crate_Name);

   ---------------------------------------------------------------------------
   --  Result Formatting
   ---------------------------------------------------------------------------

   function Format_Result (N : Natural) return Result_String is
      Result : Result_String := BLANK_RESULT;
      Image  : constant String := Natural'Image (N);
   begin
      if Image'Length <= Result'Length then
         Result (Result'Last - Image'Length + 1 .. Result'Last) := Image;
      end if;
      return Result;
   end Format_Result;

   ---------------------------------------------------------------------------
   --  File Processing
   ---------------------------------------------------------------------------

   function State_Invariant (State : Solver.Solver_State) return Boolean
   is (Solver.State_Invariant (State));

   procedure Process_Line_Wrapper (Line : String; State : in out Solver.Solver_State)
     renames Solver.Process_Line;

   procedure Read_Input is new AoC_Common.File_IO.For_Each_Line
     (Data_Type       => Solver.Solver_State,
      Max_Line_Length => 256,
      Invariant       => State_Invariant,
      Process_Line    => Process_Line_Wrapper);

   ---------------------------------------------------------------------------
   --  Solve Functions
   ---------------------------------------------------------------------------

   function Solve_Part_1 (Filename : String := "example.txt") return Result_String is
      State   : Solver.Solver_State;
      Success : Boolean;
   begin
      Solver.Initialize (State);
      Read_Input (Day_Resources.Resource_Path & "/" & Filename, Success, State);

      if not Success then
         declare
            Result : Result_String := BLANK_RESULT;
         begin
            Result (Result_String'Last - 4 .. Result_String'Last) := "ERROR";
            return Result;
         end;
      end if;

      Solver.Solve_Puzzle (State);
      return Format_Result (Solver.Result_Part_1 (State));
   end Solve_Part_1;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String is
      State   : Solver.Solver_State;
      Success : Boolean;
   begin
      Solver.Initialize (State);
      Read_Input (Day_Resources.Resource_Path & "/" & Filename, Success, State);

      if not Success then
         declare
            Result : Result_String := BLANK_RESULT;
         begin
            Result (Result_String'Last - 4 .. Result_String'Last) := "ERROR";
            return Result;
         end;
      end if;

      Solver.Solve_Puzzle (State);
      return Format_Result (Solver.Result_Part_2 (State));
   end Solve_Part_2;

end AoC_2025_Day_12;
