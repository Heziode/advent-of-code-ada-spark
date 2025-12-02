--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_Common.Directions

pragma SPARK_Mode (On);

package body AoC_Common.Directions is

   --  Lookup table for direction offsets (8-way)
   Direction_Offsets : constant array (Direction) of Direction_Offset :=
      [North      => (Delta_Row => -1, Delta_Col =>  0),
       North_East => (Delta_Row => -1, Delta_Col =>  1),
       East       => (Delta_Row =>  0, Delta_Col =>  1),
       South_East => (Delta_Row =>  1, Delta_Col =>  1),
       South      => (Delta_Row =>  1, Delta_Col =>  0),
       South_West => (Delta_Row =>  1, Delta_Col => -1),
       West       => (Delta_Row =>  0, Delta_Col => -1),
       North_West => (Delta_Row => -1, Delta_Col => -1)];

   --  Lookup table for cardinal direction offsets
   Cardinal_Offsets : constant array (Cardinal_Direction) of Direction_Offset :=
      [North => (Delta_Row => -1, Delta_Col =>  0),
       East  => (Delta_Row =>  0, Delta_Col =>  1),
       South => (Delta_Row =>  1, Delta_Col =>  0),
       West  => (Delta_Row =>  0, Delta_Col => -1)];

   function To_Offset (Dir : Direction) return Direction_Offset is
   begin
      return Direction_Offsets (Dir);
   end To_Offset;

   function To_Offset (Dir : Cardinal_Direction) return Direction_Offset is
   begin
      return Cardinal_Offsets (Dir);
   end To_Offset;

   function Rotate_Clockwise (Dir : Cardinal_Direction) return Cardinal_Direction is
   begin
      case Dir is
         when North => return East;
         when East  => return South;
         when South => return West;
         when West  => return North;
      end case;
   end Rotate_Clockwise;

   function Rotate_Counter_Clockwise (Dir : Cardinal_Direction) return Cardinal_Direction is
   begin
      case Dir is
         when North => return West;
         when West  => return South;
         when South => return East;
         when East  => return North;
      end case;
   end Rotate_Counter_Clockwise;

   function Opposite (Dir : Cardinal_Direction) return Cardinal_Direction is
   begin
      case Dir is
         when North => return South;
         when South => return North;
         when East  => return West;
         when West  => return East;
      end case;
   end Opposite;

   function Move
      (Pos : Coordinate;
       Dir : Cardinal_Direction) return Coordinate
   is
      Offset : constant Direction_Offset := Cardinal_Offsets (Dir);
   begin
      return (Row => Pos.Row + Offset.Delta_Row,
              Col => Pos.Col + Offset.Delta_Col);
   end Move;

   function Move
      (Pos : Coordinate;
       Dir : Direction) return Coordinate
   is
      Offset : constant Direction_Offset := Direction_Offsets (Dir);
   begin
      return (Row => Pos.Row + Offset.Delta_Row,
              Col => Pos.Col + Offset.Delta_Col);
   end Move;

end AoC_Common.Directions;
