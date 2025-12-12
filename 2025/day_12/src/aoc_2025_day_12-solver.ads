--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_12.Solver
--
--  Purpose: Day 12 - Christmas Tree Farm polyomino packing solver
--
--  Responsibilities:
--    * Parse present shape definitions from input
--    * Parse region specifications with required quantities
--    * Generate all rotations/flips for each shape
--    * Determine if presents can fit in each region using backtracking
--
--  Design Notes:
--    The puzzle is a polyomino packing problem. Each shape can be rotated
--    (90/180/270 degrees) and flipped. We use backtracking to try placing
--    all required shapes into each region.
--
--  External Effects: None (pure computation)
--  Thread Safety: Not applicable (single-threaded)
--  SPARK Status: Verified (Silver - AoRTE)

pragma Ada_2022;

with AoC_Common;
with AoC_Common.File_IO;

package AoC_2025_Day_12.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   MAX_SHAPES : constant := 20;
   MAX_SHAPE_SIZE : constant := 10;
   MAX_CELLS_PER_SHAPE : constant := MAX_SHAPE_SIZE * MAX_SHAPE_SIZE;
   MAX_ORIENTATIONS : constant := 8;
   MAX_REGIONS : constant := 1100;
   MAX_REGION_WIDTH : constant := 100;
   MAX_REGION_HEIGHT : constant := 100;
   MAX_PIECES_PER_SHAPE : constant := 100;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   subtype Shape_Index is Natural range 0 .. MAX_SHAPES - 1;
   subtype Cell_Index is Natural range 0 .. MAX_CELLS_PER_SHAPE - 1;
   subtype Orientation_Index is Natural range 0 .. MAX_ORIENTATIONS - 1;

   --  A cell position relative to a shape's origin (top-left of bounding box)
   type Cell_Offset is record
      Row : Integer range -MAX_SHAPE_SIZE .. MAX_SHAPE_SIZE := 0;
      Col : Integer range -MAX_SHAPE_SIZE .. MAX_SHAPE_SIZE := 0;
   end record;

   --  Array of cell offsets representing a shape
   type Cell_Offset_Array is array (Cell_Index) of Cell_Offset;

   --  A single orientation of a shape (normalized to origin)
   type Shape_Orientation is record
      Cells      : Cell_Offset_Array := [others => (0, 0)];
      Cell_Count : Natural := 0;
      Width      : Natural := 0;
      Height     : Natural := 0;
   end record;

   --  All orientations of a single shape (up to 8: 4 rotations x 2 flips)
   type Orientation_Array is array (Orientation_Index) of Shape_Orientation;

   type Shape_Definition is record
      Orientations      : Orientation_Array;
      Orientation_Count : Natural := 0;
   end record;

   type Shape_Array is array (Shape_Index) of Shape_Definition;

   --  Region specification
   type Quantity_Array is array (Shape_Index) of Natural;

   type Region_Spec is record
      Width      : Natural := 0;
      Height     : Natural := 0;
      Quantities : Quantity_Array := [others => 0];
   end record;

   subtype Region_Index is Natural range 0 .. MAX_REGIONS - 1;
   type Region_Array is array (Region_Index) of Region_Spec;

   --  Grid for region placement (True = occupied)
   type Grid_Row is array (0 .. MAX_REGION_WIDTH - 1) of Boolean;
   type Placement_Grid is array (0 .. MAX_REGION_HEIGHT - 1) of Grid_Row;

   --  Solver state that extends Data_Context for file processing
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Shapes            : Shape_Array;
      Shape_Count       : Natural := 0;
      Regions           : Region_Array;
      Region_Count      : Natural := 0;
      --  Parsing state
      Parsing_Shape     : Boolean := False;
      Current_Shape_Idx : Natural := 0;
      Current_Shape_Row : Natural := 0;
      Temp_Grid         : Placement_Grid := [others => [others => False]];
      Temp_Width        : Natural := 0;
      Temp_Height       : Natural := 0;
      --  Results
      Result_Part_1     : Natural := 0;
      Result_Part_2     : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Solver Interface
   ---------------------------------------------------------------------------

   --  Check state invariant
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Shape_Count <= MAX_SHAPES and then State.Region_Count <= MAX_REGIONS);

   --  Initialize solver state
   procedure Initialize (State : out Solver_State);

   --  Process a single input line
   procedure Process_Line (Line : String; State : in out Solver_State);

   --  Solve the puzzle after all lines are processed
   procedure Solve_Puzzle (State : in out Solver_State);

   --  Get Part 1 result
   function Result_Part_1 (State : Solver_State) return Natural
   is (State.Result_Part_1);

   --  Get Part 2 result
   function Result_Part_2 (State : Solver_State) return Natural
   is (State.Result_Part_2);

end AoC_2025_Day_12.Solver;
