--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_Common.Grids
--
--  Purpose: Generic 2D grid utilities for grid-based AoC puzzles
--
--  Responsibilities:
--    * Provide bounded 2D grid container
--    * Support safe element access with bounds checking
--    * Enable grid traversal and neighbor enumeration
--
--  External Effects: None (pure package)
--  Thread Safety: Thread-safe (no mutable state in package)
--  SPARK Status: Verified (Silver - AoRTE)

pragma SPARK_Mode (On);

generic
   type Element_Type is private;
   Default_Value : Element_Type;
package AoC_Common.Grids is

   --  Maximum dimensions for grids (can be specialized per instance)
   MAX_ROWS : constant := 1_000;
   MAX_COLS : constant := 1_000;

   subtype Row_Range is Positive range 1 .. MAX_ROWS;
   subtype Col_Range is Positive range 1 .. MAX_COLS;

   --  2D Grid type with fixed maximum bounds
   type Grid_Type
     (Rows : Row_Range;
      Cols : Col_Range)
   is
     private;

   --  Create a grid filled with the default value
   --
   --  @param Rows Number of rows
   --  @param Cols Number of columns
   --  @return A new grid with all elements set to Default_Value
   function Create (Rows : Row_Range; Cols : Col_Range) return Grid_Type
   with
     Post =>
       Get_Rows (Create'Result) = Rows
       and then Get_Cols (Create'Result) = Cols;

   --  Get the number of rows in the grid
   function Get_Rows (G : Grid_Type) return Row_Range
   with Inline;

   --  Get the number of columns in the grid
   function Get_Cols (G : Grid_Type) return Col_Range
   with Inline;

   --  Get an element at a position
   --
   --  @param G The grid
   --  @param Row Row index (1-based)
   --  @param Col Column index (1-based)
   --  @return The element at (Row, Col)
   function Get
     (G : Grid_Type; Row : Positive; Col : Positive) return Element_Type
   with Pre => Row <= G.Rows and then Col <= G.Cols;

   --  Get an element using a Coordinate
   --
   --  @param G The grid
   --  @param Pos The coordinate (Row, Col)
   --  @return The element at Pos
   function Get (G : Grid_Type; Pos : Coordinate) return Element_Type
   with
     Pre =>
       Pos.Row >= 1
       and then Pos.Row <= G.Rows
       and then Pos.Col >= 1
       and then Pos.Col <= G.Cols;

   --  Set an element at a position
   --
   --  @param G The grid (modified)
   --  @param Row Row index (1-based)
   --  @param Col Column index (1-based)
   --  @param Value The value to set
   procedure Set
     (G     : in out Grid_Type;
      Row   : Positive;
      Col   : Positive;
      Value : Element_Type)
   with Pre => Row <= G.Rows and then Col <= G.Cols;

   --  Set an element using a Coordinate
   --
   --  @param G The grid (modified)
   --  @param Pos The coordinate (Row, Col)
   --  @param Value The value to set
   procedure Set (G : in out Grid_Type; Pos : Coordinate; Value : Element_Type)
   with
     Pre =>
       Pos.Row >= 1
       and then Pos.Row <= G.Rows
       and then Pos.Col >= 1
       and then Pos.Col <= G.Cols;

   --  Check if a coordinate is within the grid bounds
   --
   --  @param G The grid
   --  @param Pos The coordinate to check
   --  @return True if Pos is within the grid
   function Is_Valid_Position (G : Grid_Type; Pos : Coordinate) return Boolean
   is (Pos.Row >= 1
       and then Pos.Row <= G.Rows
       and then Pos.Col >= 1
       and then Pos.Col <= G.Cols)
   with Inline;

private

   type Grid_Array is
     array (Row_Range range <>, Col_Range range <>) of Element_Type;

   type Grid_Type
     (Rows : Row_Range;
      Cols : Col_Range)
   is record
      Data : Grid_Array (1 .. Rows, 1 .. Cols);
   end record;

end AoC_Common.Grids;
