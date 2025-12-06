--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_05.Solver
--
--  Purpose: Core logic for Day 05 puzzle (Fresh Ingredient Checker)
--
--  Responsibilities:
--    * Parse ingredient ranges and IDs
--    * Check individual ingredients against ranges (Part 1)
--    * Merge ranges and count total coverage (Part 2)
--
--  External Effects: None (pure)
--  Thread Safety: Thread-safe
--  SPARK Status: Verified (Silver - AoRTE)

with AoC_Common.File_IO;

package AoC_2025_Day_05.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants and Types
   ---------------------------------------------------------------------------

   --  Use Long_Long_Integer to ensure we cover large IDs
   subtype Ingredient_ID is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   MAX_RANGES : constant := 1000;
   MAX_INGREDIENTS : constant := 5000;

   type Fresh_Range is record
      Min : Ingredient_ID := 0;
      Max : Ingredient_ID := 0;
   end record;

   type Range_Array is array (1 .. MAX_RANGES) of Fresh_Range;
   type Ingredient_Array is array (1 .. MAX_INGREDIENTS) of Ingredient_ID;

   type Parsing_State_Type is (Parsing_Ranges, Parsing_Ingredients);

   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Ranges            : Range_Array := [others => (0, 0)];
      Range_Count       : Natural := 0;
      Ingredients       : Ingredient_Array := [others => 0];
      Ingredient_Count  : Natural := 0;
      Current_State     : Parsing_State_Type := Parsing_Ranges;
      Error_Encountered : Boolean := False;
   end record;

   ---------------------------------------------------------------------------
   --  Core Logic
   ---------------------------------------------------------------------------

   --  Initialize the state
   procedure Initialize (State : out Solver_State);

   --  Process a single line of input
   --  @param Line The input line
   --  @param State The solver state to update
   procedure Process_Line (Line : String; State : in out Solver_State);

   --  Solve Part 1: Count how many ingredients are "fresh"
   --  @param State The populated state
   --  @return The count of fresh ingredients
   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   --  Solve Part 2: Count total unique IDs covered by ranges
   --  @param State The populated state
   --  @return The total count
   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer
   with Pre'Class => State_Invariant (State);

   ---------------------------------------------------------------------------
   --  Internal Helpers (Public for testing/proof visibility if needed)
   ---------------------------------------------------------------------------

   function Is_Fresh (ID : Ingredient_ID; Ranges : Range_Array; Count : Natural) return Boolean;

   --  Invariant for the state (optional, but good for SPARK)
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Range_Count <= MAX_RANGES and then State.Ingredient_Count <= MAX_INGREDIENTS);

private

   procedure Sort_Ranges (Ranges : in out Range_Array; Count : Natural)
   with Pre => Count <= MAX_RANGES;

end AoC_2025_Day_05.Solver;
