--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_09
--
--  SPARK_Mode Off justification:
--    - Resources package uses 'Address, 'Access, Unchecked_Deallocation
--    - File I/O operations cannot be formally verified

pragma Ada_2022;

with AoC_Common.File_IO;
with AoC_2025_Day_09.Solver;
with Resources;
with Aoc_2025_Day_09_Config;

package body AoC_2025_Day_09
  with SPARK_Mode => Off
is

   ---------------------------------------------------------------------------
   --  Resource Path Helper
   ---------------------------------------------------------------------------

   package Day_Resources is new Resources (Aoc_2025_Day_09_Config.Crate_Name);

   ---------------------------------------------------------------------------
   --  File Processing Instantiation
   ---------------------------------------------------------------------------

   function Solver_Invariant (Data : Solver.Solver_State) return Boolean
   is (Solver.State_Invariant (Data));

   procedure Process_Solver_Line (Line : String; Data : in out Solver.Solver_State) renames Solver.Process_Line;

   procedure Read_Input is new
     AoC_Common.File_IO.For_Each_Line
       (Data_Type       => Solver.Solver_State,
        Max_Line_Length => 256,
        Invariant       => Solver_Invariant,
        Process_Line    => Process_Solver_Line);

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
      State    : Solver.Solver_State;
      Success  : Boolean;
      Filepath : constant String := Day_Resources.Resource_Path & "/" & Filename;
   begin
      Solver.Initialize (State);
      State.Part := AoC_Common.Part_1;

      Read_Input (Filepath, Success, State);

      if not Success or else State.Error_Encountered then
         return BLANK_RESULT;
      end if;

      Solver.Solve_Puzzle (State);

      return Format_Result (Solver.Solve_Part_1 (State));
   end Solve_Part_1;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String is
      State    : Solver.Solver_State;
      Success  : Boolean;
      Filepath : constant String := Day_Resources.Resource_Path & "/" & Filename;
   begin
      Solver.Initialize (State);
      State.Part := AoC_Common.Part_2;

      Read_Input (Filepath, Success, State);

      if not Success or else State.Error_Encountered then
         return BLANK_RESULT;
      end if;

      Solver.Solve_Puzzle (State);

      return Format_Result (Solver.Solve_Part_2 (State));
   end Solve_Part_2;

end AoC_2025_Day_09;
