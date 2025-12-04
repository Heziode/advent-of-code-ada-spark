--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Main: AoC_2025_Day_04 Runner
--
--  Purpose: Execute and display Day 04 puzzle solutions

pragma Ada_2022;

with Ada.Text_IO;
with AoC_2025_Day_04;

procedure Main_Aoc_2025_Day_04 with SPARK_Mode => Off is
   use Ada.Text_IO;

   --  Trim leading spaces from a string
   function Trim_Left (S : String) return String
   with Post => Trim_Left'Result'Length <= S'Length;

   function Trim_Left (S : String) return String is
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
   Put_Line ("Advent of Code 2025 - Day 04 (example)");
   Put_Line ("======================================");
   New_Line;

   Put_Line ("Part 1: " & Trim_Left (AoC_2025_Day_04.Solve_Part_1));
   Put_Line ("Part 2: " & Trim_Left (AoC_2025_Day_04.Solve_Part_2));
   New_Line;

   Put_Line ("Advent of Code 2025 - Day 04");
   Put_Line ("============================");
   New_Line;

   Put_Line ("Part 1: " & Trim_Left (AoC_2025_Day_04.Solve_Part_1 ("input.txt")));
   Put_Line ("Part 2: " & Trim_Left (AoC_2025_Day_04.Solve_Part_2 ("input.txt")));
end Main_Aoc_2025_Day_04;
