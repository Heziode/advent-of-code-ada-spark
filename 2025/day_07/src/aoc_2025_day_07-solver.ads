--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_07.Solver
--
--  Purpose: Core logic for Day 07 puzzle (Tachyon Manifold Beam Splitter)
--
--  Responsibilities:
--    * Parse 2D grid to identify start position and splitters
--    * Simulate beam propagation using BFS (beams move down, split at ^)
--    * Count total number of beam splits for Part 1
--
--  Design Notes:
--    * Beams always move downward from their origin
--    * When a beam hits a splitter (^), it stops and creates 2 new beams
--      at the immediate left and right of the splitter
--    * Beams exit the manifold when they reach the bottom or sides
--    * Answer is the count of splitters that are hit by any beam
--
--  External Effects: None (pure)
--  Thread Safety: Thread-safe
--  SPARK Status: Verified (Silver - AoRTE)

pragma Ada_2022;

with AoC_Common.File_IO;

package AoC_2025_Day_07.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants and Types
   ---------------------------------------------------------------------------

   --  Grid dimensions - adjust based on expected input size
   MAX_ROWS : constant := 500;
   MAX_COLUMNS : constant := 500;

   --  Maximum number of active beams we can track simultaneously
   MAX_BEAMS : constant := 10_000;

   --  Cell types in the manifold grid
   type Cell_Type is (Empty, Start, Splitter);

   --  2D grid for storing the manifold
   type Grid_Row is array (1 .. MAX_COLUMNS) of Cell_Type;
   type Grid_Type is array (1 .. MAX_ROWS) of Grid_Row;

   --  Beam position (column only, since beams always move down)
   subtype Column_Index is Positive range 1 .. MAX_COLUMNS;

   --  Beam queue for BFS simulation
   type Beam_Array is array (1 .. MAX_BEAMS) of Column_Index;

   --  Solver state that accumulates input data
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Grid              : Grid_Type := [others => [others => Empty]];
      Row_Count         : Natural := 0;
      Column_Count      : Natural := 0;
      Start_Column      : Natural := 0;  -- Column where S is located
      Split_Count       : Natural := 0;  -- Total splits counted (Part 1)
      Timeline_Count    : Long_Long_Integer := 0;  -- Total timelines (Part 2)
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

   --  Simulate beam propagation and count splits
   --  Called after all lines have been read
   --  @param State The solver state with parsed grid
   procedure Simulate_Beams (State : in out Solver_State)
   with Pre'Class => State_Invariant (State);

   --  Solve Part 1: Count total beam splits
   --  @param State The populated and simulated state
   --  @return The total number of beam splits
   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  Solve Part 2: Count total timelines (many-worlds quantum splitting)
   --  @param State The populated and simulated state
   --  @return The total number of distinct timelines
   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  State invariant for SPARK verification
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Row_Count <= MAX_ROWS
       and then State.Column_Count <= MAX_COLUMNS
       and then (State.Start_Column = 0 or else State.Start_Column <= State.Column_Count));

end AoC_2025_Day_07.Solver;
