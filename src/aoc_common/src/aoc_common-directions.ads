--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_Common.Directions
--
--  Purpose: Cardinal and ordinal direction utilities for grid navigation
--
--  Responsibilities:
--    * Define direction enumerations (N, S, E, W and diagonals)
--    * Provide direction-to-offset conversion
--    * Support direction rotation and reversal
--
--  External Effects: None (pure package)
--  Thread Safety: Thread-safe (no mutable state)
--  SPARK Status: Verified (Silver - AoRTE)

pragma SPARK_Mode (On);

package AoC_Common.Directions is

   --  Cardinal directions (4-way movement)
   type Cardinal_Direction is (North, East, South, West);

   --  All eight directions (8-way movement)
   type Direction is
     (North,
      North_East,
      East,
      South_East,
      South,
      South_West,
      West,
      North_West);

   --  Movement offset for a single step in a direction
   type Direction_Offset is record
      Delta_Row : Integer range -1 .. 1;
      Delta_Col : Integer range -1 .. 1;
   end record;

   --  Get the offset for moving one step in a direction
   --
   --  Convention: North decreases row, East increases column
   --  (standard screen/matrix coordinates)
   --
   --  @param Dir The direction to move
   --  @return The row/column delta for one step
   function To_Offset (Dir : Direction) return Direction_Offset
   with Inline;

   --  Get the offset for a cardinal direction
   --
   --  @param Dir The cardinal direction
   --  @return The row/column delta for one step
   function To_Offset (Dir : Cardinal_Direction) return Direction_Offset
   with Inline;

   --  Rotate a cardinal direction 90 degrees clockwise
   --
   --  @param Dir The starting direction
   --  @return The direction after rotating right
   function Rotate_Clockwise
     (Dir : Cardinal_Direction) return Cardinal_Direction
   with Inline;

   --  Rotate a cardinal direction 90 degrees counter-clockwise
   --
   --  @param Dir The starting direction
   --  @return The direction after rotating left
   function Rotate_Counter_Clockwise
     (Dir : Cardinal_Direction) return Cardinal_Direction
   with Inline;

   --  Get the opposite direction
   --
   --  @param Dir The direction
   --  @return The reverse direction (180 degree turn)
   function Opposite (Dir : Cardinal_Direction) return Cardinal_Direction
   with Inline;

   --  Move a coordinate one step in a direction
   --
   --  @param Pos The starting position
   --  @param Dir The direction to move
   --  @return New coordinate after moving (may be out of bounds)
   function Move (Pos : Coordinate; Dir : Cardinal_Direction) return Coordinate
   with
     Pre =>
       Pos.Row > Coordinate_Range'First
       and then Pos.Row < Coordinate_Range'Last
       and then Pos.Col > Coordinate_Range'First
       and then Pos.Col < Coordinate_Range'Last;

   --  Move a coordinate one step in any direction (8-way)
   --
   --  @param Pos The starting position
   --  @param Dir The direction to move
   --  @return New coordinate after moving (may be out of bounds)
   function Move (Pos : Coordinate; Dir : Direction) return Coordinate
   with
     Pre =>
       Pos.Row > Coordinate_Range'First
       and then Pos.Row < Coordinate_Range'Last
       and then Pos.Col > Coordinate_Range'First
       and then Pos.Col < Coordinate_Range'Last;

end AoC_Common.Directions;
