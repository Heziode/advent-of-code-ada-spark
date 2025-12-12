--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_11.Solver
--
--  Purpose: Day 11 - Reactor path counting solver
--
--  Responsibilities:
--    * Parse device connection graph from input
--    * Count all paths from "you" to "out" using memoized DFS
--
--  Design Notes:
--    The puzzle involves counting ALL distinct paths in a directed graph.
--    We use memoization to efficiently count paths from each node to "out".
--
--  External Effects: None (pure computation)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Verified (Silver - AoRTE)

pragma Ada_2022;

with AoC_Common;
with AoC_Common.File_IO;

package AoC_2025_Day_11.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   MAX_NODES : constant := 1000;
   MAX_EDGES : constant := 25;
   MAX_NAME_LENGTH : constant := 10;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Node_Index is Natural range 0 .. MAX_NODES - 1;
   subtype Node_Name is String (1 .. MAX_NAME_LENGTH);

   BLANK_NAME : constant Node_Name := [others => ' '];

   --  Adjacency list entry for a single node
   type Edge_Array is array (1 .. MAX_EDGES) of Node_Index;

   type Node_Entry is record
      Name       : Node_Name := BLANK_NAME;
      Edges      : Edge_Array := [others => 0];
      Edge_Count : Natural := 0;
      Path_Count : Long_Long_Integer := -1;  --  -1 = not computed
   end record;

   type Graph_Array is array (Node_Index) of Node_Entry;

   --  Solver state that extends Data_Context for file processing
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Nodes             : Graph_Array;
      Node_Count        : Natural := 0;
      Start_Node        : Node_Index := 0;  --  "you" for Part 1
      End_Node          : Node_Index := 0;  --  "out"
      Svr_Node          : Node_Index := 0;  --  "svr" for Part 2
      Dac_Node          : Node_Index := 0;  --  "dac" required for Part 2
      Fft_Node          : Node_Index := 0;  --  "fft" required for Part 2
      Error_Encountered : Boolean := False;
      Result_Part_1     : Long_Long_Integer := 0;
      Result_Part_2     : Long_Long_Integer := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Solver Interface
   ---------------------------------------------------------------------------

   --  Check state invariant
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Node_Count <= MAX_NODES);

   --  Initialize solver state
   procedure Initialize (State : out Solver_State);

   --  Process a single input line
   procedure Process_Line (Line : String; State : in out Solver_State);

   --  Solve the puzzle after all lines are processed
   procedure Solve_Puzzle (State : in out Solver_State);

   --  Get Part 1 result
   function Result_Part_1 (State : Solver_State) return Long_Long_Integer
   is (State.Result_Part_1);

   --  Get Part 2 result
   function Result_Part_2 (State : Solver_State) return Long_Long_Integer
   is (State.Result_Part_2);

end AoC_2025_Day_11.Solver;
