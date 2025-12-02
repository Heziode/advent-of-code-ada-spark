--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_01
--
--  Structure:
--    - Pure algorithmic functions (Apply_Rotation, Count_Zero_Passes)
--      are SPARK-verified in the specification
--    - File I/O operations use non-SPARK features (Resources package,
--      Ada.Text_IO) and have SPARK_Mode Off
--
--  SPARK_Mode Off justification for body:
--    1. Resources package (Alire) uses 'Address, 'Access, and
--       Unchecked_Deallocation internally to locate resource files
--    2. AoC_Common.File_IO.For_Each_Line uses Ada.Text_IO for file I/O
--  These are external I/O operations that cannot be formally verified
--  but are necessary to load and process puzzle input files.
--  The core algorithms (Apply_Rotation, Count_Zero_Passes) are still
--  verified through their specification contracts.

pragma Ada_2022;

with AoC_Common.File_IO;
with Resources;
with Aoc_2025_Day_01_Config;

package body AoC_2025_Day_01
  with SPARK_Mode => Off
is

   ---------------------------------------------------------------------------
   --  Resource Path Helper
   ---------------------------------------------------------------------------

   package Day_01_Resources is new Resources (Aoc_2025_Day_01_Config.Crate_Name);

   ---------------------------------------------------------------------------
   --  Apply_Rotation Implementation
   ---------------------------------------------------------------------------

   function Apply_Rotation (Position : Dial_Position; Dir : Direction_Type; Distance : Natural) return Dial_Position is
      --  Reduce distance to single rotation (0 to 99)
      Reduced : constant Natural := Distance mod DIAL_SIZE;
      Result  : Integer;
   begin
      case Dir is
         when Left =>
            Result := Integer (Position) - Reduced;

         when Right =>
            Result := Integer (Position) + Reduced;
      end case;

      --  Normalize to 0 .. 99 range
      Result := Result mod DIAL_SIZE;

      --  Handle negative modulo (Ada mod can return negative)
      if Result < 0 then
         Result := Result + DIAL_SIZE;
      end if;

      return Dial_Position (Result);
   end Apply_Rotation;

   ---------------------------------------------------------------------------
   --  Count_Zero_Passes Implementation
   ---------------------------------------------------------------------------

   function Count_Zero_Passes (Start_Pos : Dial_Position; Dir : Direction_Type; Distance : Natural) return Natural is
   begin
      if Distance = 0 then
         return 0;
      end if;

      case Dir is
         when Left =>
            --  Moving left (decreasing): we hit 0 when cumulative clicks
            --  mod 100 = Start_Pos (i.e., after Start_Pos, Start_Pos + 100, ... clicks)
            --  Number of such N in [1..Distance] where N mod 100 = Start_Pos
            if Start_Pos = 0 then
               --  Hit 0 at N = 100, 200, 300, ...
               return Distance / DIAL_SIZE;
            elsif Start_Pos <= Distance then
               --  Hit 0 at N = Start_Pos, Start_Pos + 100, ...
               return (Distance - Start_Pos) / DIAL_SIZE + 1;
            else
               return 0;
            end if;

         when Right =>
            --  Moving right (increasing): we hit 0 when (Start_Pos + N) mod 100 = 0
            --  i.e., N mod 100 = (100 - Start_Pos) mod 100
            declare
               First_Zero : constant Natural := (if Start_Pos = 0 then DIAL_SIZE else DIAL_SIZE - Start_Pos);
            begin
               if Distance >= First_Zero then
                  return (Distance - First_Zero) / DIAL_SIZE + 1;
               else
                  return 0;
               end if;
            end;
      end case;
   end Count_Zero_Passes;

   ---------------------------------------------------------------------------
   --  File I/O and Solving
   ---------------------------------------------------------------------------

   --  State for processing input lines
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Position   : Dial_Position := DIAL_START;
      Zero_Count : Natural := 0;
   end record;

   pragma Warnings (Off, "not dispatching");
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Position'Valid);
   pragma Warnings (On, "not dispatching");

   procedure Process_Input_Line (Line : String; Data : in out Solver_State) is
      use AoC_Common;
      use AoC_Common.File_IO;

      Dir       : Direction_Type;
      Distance  : Natural;
      Parse_Res : Natural_Parse_Result;
   begin
      if Line'Length < 2 then
         return;  --  Skip empty or too short lines

      end if;

      --  Parse direction
      case Line (Line'First) is
         when 'L' =>
            Dir := Left;

         when 'R' =>
            Dir := Right;

         when others =>
            return;  --  Invalid direction
      end case;

      --  Parse distance (skip the first character)
      Parse_Res := Parse_Natural (Line (Line'First + 1 .. Line'Last));
      if not Parse_Res.Valid then
         return;  --  Invalid number

      end if;
      Distance := Parse_Res.Value;

      --  Process based on part
      case Data.Part is
         when Part_1 =>
            --  Apply rotation
            Data.Position := Apply_Rotation (Data.Position, Dir, Distance);
            --  Count landing on 0
            if Data.Position = 0 then
               Data.Zero_Count := Data.Zero_Count + 1;
            end if;

         when Part_2 =>
            --  Count passes through 0
            Data.Zero_Count := Data.Zero_Count + Count_Zero_Passes (Data.Position, Dir, Distance);
            --  Update position
            Data.Position := Apply_Rotation (Data.Position, Dir, Distance);
      end case;
   end Process_Input_Line;

   procedure Process_File is new
     AoC_Common.File_IO.For_Each_Line
       (Data_Type       => Solver_State,
        Max_Line_Length => 256,
        Invariant       => State_Invariant,
        Process_Line    => Process_Input_Line);

   function Format_Result (N : Natural) return Result_String is
      Result : Result_String := BLANK_RESULT;
      Image  : constant String := Natural'Image (N);
   begin
      if Image'Length <= Result'Length then
         Result (Result'Last - Image'Length + 1 .. Result'Last) := Image;
      end if;
      return Result;
   end Format_Result;

   function Solve_Part_1 (Filename : String := "example.txt") return Result_String is
      State   : Solver_State := (Position => DIAL_START, Zero_Count => 0, Part => AoC_Common.Part_1);
      Success : Boolean;
   begin
      Process_File (Filename => Day_01_Resources.Resource_Path & Filename, Success => Success, Data => State);

      if Success then
         return Format_Result (State.Zero_Count);
      else
         declare
            Error_Result : Result_String := BLANK_RESULT;
         begin
            Error_Result (Result_String'Last - 4 .. Result_String'Last) := "ERROR";
            return Error_Result;
         end;
      end if;
   end Solve_Part_1;

   function Solve_Part_2 (Filename : String := "example.txt") return Result_String is
      State   : Solver_State := (Position => DIAL_START, Zero_Count => 0, Part => AoC_Common.Part_2);
      Success : Boolean;
   begin
      Process_File (Filename => Day_01_Resources.Resource_Path & Filename, Success => Success, Data => State);

      if Success then
         return Format_Result (State.Zero_Count);
      else
         declare
            Error_Result : Result_String := BLANK_RESULT;
         begin
            Error_Result (Result_String'Last - 4 .. Result_String'Last) := "ERROR";
            return Error_Result;
         end;
      end if;
   end Solve_Part_2;

end AoC_2025_Day_01;
