--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_06.Solver
--
--  Purpose: Core logic for Day 06 puzzle (Trash Compactor - Math Worksheet)
--
--  Responsibilities:
--    * Parse columnar worksheet format into individual problems
--    * Compute addition or multiplication per problem
--    * Sum all problem results for Part 1
--
--  Design Notes:
--    * Input is a 2D character grid where problems are arranged in columns
--    * Each problem column contains numbers vertically, operator at bottom
--    * Problems separated by columns of spaces
--
--  External Effects: None (pure)
--  Thread Safety: Thread-safe
--  SPARK Status: Verified (Silver - AoRTE)

pragma Ada_2022;

with AoC_Common.File_IO;

package AoC_2025_Day_06.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants and Types
   ---------------------------------------------------------------------------

   --  Grid dimensions - adjust based on expected input size
   MAX_ROWS : constant := 100;
   MAX_COLUMNS : constant := 4000;
   MAX_PROBLEMS : constant := 1000;

   --  Type for storing problem values (can be large products)
   subtype Problem_Value is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   --  Operation type
   type Operation_Type is (Add, Multiply);

   --  2D character grid for storing input
   type Grid_Row is array (1 .. MAX_COLUMNS) of Character;
   type Grid_Type is array (1 .. MAX_ROWS) of Grid_Row;

   --  Problem results storage
   type Problem_Array is array (1 .. MAX_PROBLEMS) of Problem_Value;

   --  Solver state that accumulates input data
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Grid              : Grid_Type := [others => [others => ' ']];
      Row_Count         : Natural := 0;
      Max_Column_Used   : Natural := 0;
      Problem_Results   : Problem_Array := [others => 0];
      Problem_Count     : Natural := 0;
      Error_Encountered : Boolean := False;
   end record;

   ---------------------------------------------------------------------------
   --  Core Logic
   ---------------------------------------------------------------------------

   --  Initialize the solver state
   --  @param State The state to initialize
   procedure Initialize (State : out Solver_State);

   --  Process a single line of input (adds to grid)
   --  @param Line The input line to process
   --  @param State The solver state to update
   procedure Process_Line (Line : String; State : in out Solver_State);

   --  Finalize processing - parse columns and compute results
   --  Called after all lines have been read
   --  @param State The solver state to finalize
   procedure Finalize_Processing (State : in out Solver_State)
   with Pre'Class => State_Invariant (State);

   --  Solve Part 1: Sum of all problem results
   --  @param State The populated and finalized state
   --  @return The grand total of all problem answers
   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  Solve Part 2: (Placeholder - awaiting puzzle description)
   --  @param State The populated state
   --  @return Result for Part 2
   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  State invariant for SPARK verification
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Row_Count <= MAX_ROWS
       and then State.Max_Column_Used <= MAX_COLUMNS
       and then State.Problem_Count <= MAX_PROBLEMS);

end AoC_2025_Day_06.Solver;
