--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_Common.Grids

pragma SPARK_Mode (On);

package body AoC_Common.Grids is

   function Create
      (Rows : Row_Range;
       Cols : Col_Range) return Grid_Type
   is
   begin
      return (Rows => Rows,
              Cols => Cols,
              Data => [others => [others => Default_Value]]);
   end Create;

   function Get_Rows (G : Grid_Type) return Row_Range is
   begin
      return G.Rows;
   end Get_Rows;

   function Get_Cols (G : Grid_Type) return Col_Range is
   begin
      return G.Cols;
   end Get_Cols;

   function Get
      (G   : Grid_Type;
       Row : Positive;
       Col : Positive) return Element_Type
   is
   begin
      return G.Data (Row, Col);
   end Get;

   function Get
      (G   : Grid_Type;
       Pos : Coordinate) return Element_Type
   is
   begin
      return G.Data (Pos.Row, Pos.Col);
   end Get;

   procedure Set
      (G     : in out Grid_Type;
       Row   : Positive;
       Col   : Positive;
       Value : Element_Type)
   is
   begin
      G.Data (Row, Col) := Value;
   end Set;

   procedure Set
      (G     : in out Grid_Type;
       Pos   : Coordinate;
       Value : Element_Type)
   is
   begin
      G.Data (Pos.Row, Pos.Col) := Value;
   end Set;

end AoC_Common.Grids;
