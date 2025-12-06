--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Main: AoC_2025_Day_06 Runner
--
--  Purpose: Execute and display Day 06 puzzle solutions

pragma Ada_2022;

with Ada.Text_IO;
with AoC_2025_Day_06;

procedure Main_aoc_2025_day_06 with SPARK_Mode => On is
   use Ada.Text_IO;

   --  Trim leading spaces from a string
   function Trim_Left (S : String) return String with Post => Trim_Left'Result'Length <= S'Length is
   begin
      if S'Length = 0 then
         return "";
      end if;

      for I in S'Range loop
         if S (I) /= ' ' then
            return S (I .. S'Last);
         end if;
         pragma Loop_Invariant (for all J in S'First .. I => S (J) = ' ');
      end loop;

      return "";
   end Trim_Left;

begin
   Put_Line ("Advent of Code 2025 - Day 06 (example)");
   Put_Line ("======================================");
   New_Line;

   Put_Line ("Part 1: " & Trim_Left (AoC_2025_Day_06.Solve_Part_1));
   Put_Line ("Part 2: " & Trim_Left (AoC_2025_Day_06.Solve_Part_2));
   New_Line;

   Put_Line ("Advent of Code 2025 - Day 06");
   Put_Line ("============================");
   New_Line;

   Put_Line ("Part 1: " & Trim_Left (AoC_2025_Day_06.Solve_Part_1 ("input.txt")));
   Put_Line ("Part 2: " & Trim_Left (AoC_2025_Day_06.Solve_Part_2 ("input.txt")));
end Main_aoc_2025_day_06;
