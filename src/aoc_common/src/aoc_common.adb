--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_Common

pragma SPARK_Mode (On);

package body AoC_Common is

   function Manhattan_Distance (From, To : Coordinate) return Natural is
   begin
      return abs (From.Row - To.Row) + abs (From.Col - To.Col);
   end Manhattan_Distance;

end AoC_Common;
