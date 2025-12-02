--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_Common
--
--  Purpose: Root package for shared Advent of Code utilities
--
--  Responsibilities:
--    * Define common types used across all puzzles
--    * Provide coordinate and dimension types for grid-based puzzles
--    * Export reusable constants for puzzle constraints
--
--  External Effects: None (pure package)
--  Thread Safety: Thread-safe (no mutable state)
--  SPARK Status: Verified (Silver - AoRTE)

pragma SPARK_Mode (On);

package AoC_Common is

   --  Puzzle Part Identifier
   type Puzzle_Part is (Part_1, Part_2);


   --  Maximum grid dimensions for AoC puzzles (typically under 1000x1000)
   MAX_GRID_SIZE : constant := 10_000;

   --  Coordinate types for 2D grid operations with overflow protection
   subtype Coordinate_Range is Integer range -MAX_GRID_SIZE .. MAX_GRID_SIZE;
   subtype Dimension_Range is Positive range 1 .. MAX_GRID_SIZE;

   --  Standard coordinate pair for 2D puzzles
   type Coordinate is record
      Row : Coordinate_Range := 0;
      Col : Coordinate_Range := 0;
   end record;

   --  Grid dimensions
   type Dimensions is record
      Rows : Dimension_Range := 1;
      Cols : Dimension_Range := 1;
   end record;

   --  Origin constant for convenience
   ORIGIN : constant Coordinate := (Row => 0, Col => 0);

   --  Check if a coordinate is within given dimensions (1-based indexing)
   --
   --  @param Pos The coordinate to check
   --  @param Dims The bounding dimensions
   --  @return True if Pos is within [1..Dims.Rows] x [1..Dims.Cols]
   function Is_Within_Bounds
     (Pos : Coordinate; Dims : Dimensions) return Boolean
   is (Pos.Row >= 1
       and then Pos.Row <= Dims.Rows
       and then Pos.Col >= 1
       and then Pos.Col <= Dims.Cols)
   with Inline;

   --  Calculate Manhattan distance between two coordinates
   --
   --  The Manhattan distance is the sum of absolute differences:
   --  |x1 - x2| + |y1 - y2|
   --
   --  @param From Starting coordinate
   --  @param To Ending coordinate
   --  @return Manhattan distance (always non-negative)
   function Manhattan_Distance (From, To : Coordinate) return Natural
   with Post => Manhattan_Distance'Result <= 4 * MAX_GRID_SIZE;

end AoC_Common;
