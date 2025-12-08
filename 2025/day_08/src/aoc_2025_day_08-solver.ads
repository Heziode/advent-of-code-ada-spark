--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_08.Solver
--
--  Purpose: Core logic for Day 08 puzzle (Playground - Junction Boxes)
--
--  Responsibilities:
--    * Parse 3D coordinates (X,Y,Z) from input lines
--    * Calculate squared Euclidean distances between all junction box pairs
--    * Use Union-Find to track circuits when connecting pairs
--    * Connect the 1000 closest pairs and find 3 largest circuits
--
--  Design Notes:
--    * Uses squared Euclidean distance to avoid sqrt (SPARK-friendly)
--    * Union-Find with path compression and rank for efficiency
--    * Pairs sorted by distance, processed in order
--    * Expected answer for example: 40 (5 * 4 * 2)
--
--  External Effects: None (pure)
--  Thread Safety: Thread-safe
--  SPARK Status: Verified (Silver - AoRTE)

pragma Ada_2022;

with AoC_Common.File_IO;

package AoC_2025_Day_08.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants and Types
   ---------------------------------------------------------------------------

   --  Maximum number of junction boxes we can handle
   MAX_BOXES : constant := 2_000;

   --  Number of connections to make (Part 1)
   CONNECTIONS_PART_1 : constant := 1_000;

   --  Maximum pairs to keep (need N-1 merges for N boxes, keep extra margin)
   --  With clustering, we may need many more pairs to span all boxes
   MAX_KEPT_PAIRS : constant := 50_000;

   --  Top N circuits to multiply for the answer
   TOP_CIRCUITS : constant := 3;

   --  3D coordinate for a junction box
   subtype Coordinate_Value is Integer range 0 .. 1_000_000;
   type Junction_Box is record
      X : Coordinate_Value := 0;
      Y : Coordinate_Value := 0;
      Z : Coordinate_Value := 0;
   end record;

   --  Array of junction boxes
   subtype Box_Index is Positive range 1 .. MAX_BOXES;
   type Box_Array is array (Box_Index) of Junction_Box;

   --  Squared distance type (can be large: 3 * 10^12 max)
   subtype Squared_Distance is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   --  Pair of junction boxes with their squared distance
   type Box_Pair is record
      Box_A    : Box_Index := 1;
      Box_B    : Box_Index := 1;
      Distance : Squared_Distance := 0;
   end record;

   --  Array of pairs (only store the smallest MAX_KEPT_PAIRS pairs)
   subtype Kept_Pair_Index is Positive range 1 .. MAX_KEPT_PAIRS;
   type Kept_Pair_Array is array (Kept_Pair_Index) of Box_Pair;

   --  Union-Find parent array (for circuit tracking)
   type Parent_Array is array (Box_Index) of Box_Index;

   --  Union-Find rank array (for balancing)
   type Rank_Array is array (Box_Index) of Natural;

   --  Solver state that accumulates input data
   --  Note: Pairs are computed and stored locally in Solve_Puzzle to save memory
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Boxes             : Box_Array := [others => (X => 0, Y => 0, Z => 0)];
      Box_Count         : Natural := 0;
      Parent            : Parent_Array := [others => 1];
      Rank              : Rank_Array := [others => 0];
      Result_Part_1     : Long_Long_Integer := 0;
      Result_Part_2     : Long_Long_Integer := 0;
      Error_Encountered : Boolean := False;
   end record;

   ---------------------------------------------------------------------------
   --  Core Logic
   ---------------------------------------------------------------------------

   --  Initialize the solver state
   --  @param State The state to initialize
   procedure Initialize (State : out Solver_State);

   --  Process a single line of input (parses X,Y,Z coordinate)
   --  @param Line The input line to process (format: "X,Y,Z")
   --  @param State The solver state to update
   procedure Process_Line (Line : String; State : in out Solver_State);

   --  Compute all pairwise distances and solve the puzzle
   --  Called after all lines have been read
   --  @param State The solver state with parsed boxes
   procedure Solve_Puzzle (State : in out Solver_State)
   with Pre'Class => State_Invariant (State);

   --  Solve Part 1: Product of 3 largest circuits after 1000 connections
   --  @param State The populated and solved state
   --  @return The product of the 3 largest circuit sizes
   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  Solve Part 2: (To be determined from puzzle description)
   --  @param State The populated and solved state
   --  @return The Part 2 answer
   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  State invariant for SPARK verification
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Box_Count <= MAX_BOXES);

end AoC_2025_Day_08.Solver;
