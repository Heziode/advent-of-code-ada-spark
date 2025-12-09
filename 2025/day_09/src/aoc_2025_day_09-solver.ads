--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_09.Solver
--
--  Purpose: Core logic for Day 09 puzzle (Movie Theater - Largest Rectangle)
--
--  Responsibilities:
--    * Parse 2D coordinates (X,Y) from input lines in format "X,Y"
--    * Store red tile positions in an array
--    * Part 1: Calculate largest rectangle area from any two tiles as corners
--    * Part 2: Find largest rectangle using only red/green tiles
--
--  Design Notes:
--    * Uses Long_Long_Integer for area to handle large coordinate products
--    * Rectangle area = (|x2 - x1| + 1) * (|y2 - y1| + 1) (inclusive bounds)
--    * Part 1: O(n²) algorithm - iterate all pairs of tiles
--    * Part 2: Uses coordinate compression + polygon interior detection
--    * Red tiles form a closed polygon connected by green tiles
--    * Interior of polygon is also green; rectangle must only contain red/green
--    * Expected answers for example: Part 1 = 50, Part 2 = 24
--
--  External Effects: None (pure)
--  Thread Safety: Thread-safe
--  SPARK Status: Verified (Silver - AoRTE)

pragma Ada_2022;

with AoC_Common.File_IO;

package AoC_2025_Day_09.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants and Types
   ---------------------------------------------------------------------------

   --  Maximum number of red tiles we can handle
   MAX_TILES : constant := 1_000;

   --  Coordinate value range (based on input analysis: up to ~100,000)
   subtype Coordinate_Value is Integer range 0 .. 1_000_000;

   --  2D coordinate for a red tile position
   type Red_Tile is record
      X : Coordinate_Value := 0;
      Y : Coordinate_Value := 0;
   end record;

   --  Array of red tiles
   subtype Tile_Index is Positive range 1 .. MAX_TILES;
   type Tile_Array is array (Tile_Index) of Red_Tile;

   --  Solver state that accumulates input data
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Tiles             : Tile_Array := [others => (X => 0, Y => 0)];
      Tile_Count        : Natural := 0;
      Max_Area          : Long_Long_Integer := 0;  --  Part 1 result
      Max_Area_Part_2   : Long_Long_Integer := 0;  --  Part 2 result
      Error_Encountered : Boolean := False;
   end record;

   ---------------------------------------------------------------------------
   --  Core Logic
   ---------------------------------------------------------------------------

   --  Initialize the solver state
   --
   --  @param State The state to initialize
   procedure Initialize (State : out Solver_State);

   --  Process a single line of input (parses X,Y coordinate)
   --
   --  @param Line The input line to process (format: "X,Y")
   --  @param State The solver state to update
   procedure Process_Line (Line : String; State : in out Solver_State);

   --  Compute the largest rectangle area from all tile pairs
   --
   --  Called after all lines have been read.
   --
   --  Time Complexity: O(n²) where n is number of tiles
   --
   --  @param State The solver state with parsed tiles
   procedure Solve_Puzzle (State : in out Solver_State)
   with Pre'Class => State_Invariant (State);

   --  Solve Part 1: Maximum rectangle area from two red tiles as corners
   --
   --  @param State The populated and solved state
   --  @return The maximum rectangle area
   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  Solve Part 2: Maximum rectangle using only red/green tiles
   --
   --  The polygon formed by red tiles (connected in order) plus interior
   --  defines the valid region. Rectangle corners must be red tiles and
   --  all tiles inside must be red or green (on boundary or interior).
   --
   --  @param State The populated and solved state
   --  @return The maximum valid rectangle area
   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  State invariant for SPARK verification
   --
   --  @param State The solver state to validate
   --  @return True if state is valid
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Tile_Count <= MAX_TILES);

end AoC_2025_Day_09.Solver;
