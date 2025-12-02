--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Test Runner: AoC_Common Unit Tests
--
--  Purpose: Execute all unit tests for the AoC_Common library

with Ada.Text_IO;
with AoC_Common;
with AoC_Common.Directions;
with AoC_Common.Parsing;

procedure Test_Runner is
   use Ada.Text_IO;

   package Common renames AoC_Common;
   package Dir renames AoC_Common.Directions;
   package Parse renames AoC_Common.Parsing;

   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String);

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      if Condition then
         Put_Line ("[PASS] " & Test_Name);
         Tests_Passed := Tests_Passed + 1;
      else
         Put_Line ("[FAIL] " & Test_Name);
         Tests_Failed := Tests_Failed + 1;
      end if;
   end Assert;

begin
   Put_Line ("AoC_Common Unit Tests");
   Put_Line ("=====================");
   New_Line;

   --  Test: Coordinate and Distance
   Put_Line ("-- Coordinate Tests --");

   Assert (Common.Manhattan_Distance (Common.ORIGIN, Common.ORIGIN) = 0,
           "Manhattan distance from origin to origin is 0");

   Assert (Common.Manhattan_Distance ((Row => 0, Col => 0), (Row => 3, Col => 4)) = 7,
           "Manhattan distance (0,0) to (3,4) is 7");

   Assert (Common.Manhattan_Distance ((Row => -2, Col => 3), (Row => 2, Col => -1)) = 8,
           "Manhattan distance (-2,3) to (2,-1) is 8");

   declare
      Dims : constant Common.Dimensions := (Rows => 5, Cols => 10);
   begin
      Assert (Common.Is_Within_Bounds ((Row => 1, Col => 1), Dims),
              "Is_Within_Bounds (1,1) in 5x10 grid");

      Assert (Common.Is_Within_Bounds ((Row => 5, Col => 10), Dims),
              "Is_Within_Bounds (5,10) in 5x10 grid");

      Assert (not Common.Is_Within_Bounds ((Row => 0, Col => 1), Dims),
              "Is_Within_Bounds (0,1) NOT in 5x10 grid");

      Assert (not Common.Is_Within_Bounds ((Row => 6, Col => 1), Dims),
              "Is_Within_Bounds (6,1) NOT in 5x10 grid");
   end;

   New_Line;
   Put_Line ("-- Direction Tests --");

   declare
      use Dir;
      N_Offset : constant Direction_Offset := To_Offset (Cardinal_Direction'(North));
      E_Offset : constant Direction_Offset := To_Offset (Cardinal_Direction'(East));
   begin
      Assert (N_Offset.Delta_Row = -1 and N_Offset.Delta_Col = 0,
              "North offset is (-1, 0)");

      Assert (E_Offset.Delta_Row = 0 and E_Offset.Delta_Col = 1,
              "East offset is (0, 1)");

      Assert (Rotate_Clockwise (Cardinal_Direction'(North)) = Cardinal_Direction'(East),
              "Rotate North clockwise = East");

      Assert (Rotate_Clockwise (Cardinal_Direction'(East)) = Cardinal_Direction'(South),
              "Rotate East clockwise = South");

      Assert (Rotate_Counter_Clockwise (Cardinal_Direction'(North)) = Cardinal_Direction'(West),
              "Rotate North counter-clockwise = West");

      Assert (Opposite (Cardinal_Direction'(North)) = Cardinal_Direction'(South),
              "Opposite of North = South");

      Assert (Opposite (Cardinal_Direction'(East)) = Cardinal_Direction'(West),
              "Opposite of East = West");
   end;

   declare
      use Dir;
      Start_Pos : constant Common.Coordinate := (Row => 5, Col => 5);
      New_Pos   : Common.Coordinate;
   begin
      New_Pos := Move (Start_Pos, Cardinal_Direction'(North));
      Assert (New_Pos.Row = 4 and New_Pos.Col = 5,
              "Move North from (5,5) = (4,5)");

      New_Pos := Move (Start_Pos, Direction'(South_East));
      Assert (New_Pos.Row = 6 and New_Pos.Col = 6,
              "Move South_East from (5,5) = (6,6)");
   end;

   New_Line;
   Put_Line ("-- Parsing Tests --");

   Assert (Parse.Is_Digit ('5'), "Is_Digit('5') = True");
   Assert (not Parse.Is_Digit ('a'), "Is_Digit('a') = False");
   Assert (Parse.Is_Whitespace (' '), "Is_Whitespace(' ') = True");
   Assert (not Parse.Is_Whitespace ('x'), "Is_Whitespace('x') = False");
   Assert (Parse.Digit_Value ('7') = 7, "Digit_Value('7') = 7");

   declare
      use Parse;
      Result : Parse_Result;
   begin
      Result := Parse_Integer ("123abc", 1);
      Assert (Result.Success and Result.Value = 123 and Result.Next = 4,
              "Parse_Integer '123abc' = 123, next=4");

      Result := Parse_Integer ("-42xyz", 1);
      Assert (Result.Success and Result.Value = -42 and Result.Next = 4,
              "Parse_Integer '-42xyz' = -42, next=4");

      Result := Parse_Integer ("abc123", 1);
      Assert (not Result.Success,
              "Parse_Integer 'abc123' at 1 fails (no digit at start)");

      Result := Find_Next_Integer ("value: 999 end", 1);
      Assert (Result.Success and Result.Value = 999,
              "Find_Next_Integer 'value: 999 end' = 999");

      Result := Find_Next_Integer ("x=-5,y=10", 1);
      Assert (Result.Success and Result.Value = -5,
              "Find_Next_Integer 'x=-5,y=10' finds -5 first");
   end;

   declare
      Idx : Natural;
   begin
      Idx := Parse.Skip_Whitespace ("   hello", 1);
      Assert (Idx = 4, "Skip_Whitespace '   hello' from 1 = 4");

      Idx := Parse.Skip_Whitespace ("hello", 1);
      Assert (Idx = 1, "Skip_Whitespace 'hello' from 1 = 1 (no whitespace)");
   end;

   --  Summary
   New_Line;
   Put_Line ("=====================");
   Put_Line ("Tests Passed:" & Natural'Image (Tests_Passed));
   Put_Line ("Tests Failed:" & Natural'Image (Tests_Failed));

   if Tests_Failed > 0 then
      Put_Line ("SOME TESTS FAILED!");
   else
      Put_Line ("ALL TESTS PASSED!");
   end if;

end Test_Runner;
