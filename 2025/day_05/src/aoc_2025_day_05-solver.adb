--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

with AoC_Common.File_IO;
with AoC_Common;

package body AoC_2025_Day_05.Solver with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  Local Helpers
   ---------------------------------------------------------------------------

   --  Parse a Long_Long_Integer from a substring
   procedure Parse_Long
     (S       : String;
      Start   : Positive;
      Result  : out Ingredient_ID;
      Success : out Boolean;
      Next    : out Positive);

   procedure Parse_Long
     (S       : String;
      Start   : Positive;
      Result  : out Ingredient_ID;
      Success : out Boolean;
      Next    : out Positive)
   is
      Val : Ingredient_ID := 0;
      Idx : Positive := Start;
   begin
      Success := False;
      Next := Start;
      Result := 0; -- Ensure initialization

      --  Skip leading spaces (though typical inputs might not have them, it's safe)
      while Idx <= S'Last and then S (Idx) = ' ' loop
         Idx := Idx + 1;
      end loop;


      if Idx > S'Last or else S (Idx) not in '0' .. '9' then
         return;
      end if;

      while Idx <= S'Last and then S (Idx) in '0' .. '9' loop
         declare
            Digit : constant Ingredient_ID := Ingredient_ID (Character'Pos (S (Idx)) - Character'Pos ('0'));
         begin
            --  Check for overflow (basic check)
            if Val > Ingredient_ID'Last / 10 then
               return; -- Overflow
            end if;
            Val := Val * 10 + Digit;
         end;
         Idx := Idx + 1;
      end loop;

      Result := Val;
      Success := True;
      Next := Idx;
   end Parse_Long;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State) is
   begin
      State := (Part => AoC_Common.Part_1, -- Default, unused
                Ranges => [others => (0, 0)],
                Range_Count => 0,
                Ingredients => [others => 0],
                Ingredient_Count => 0,
                Current_State => Parsing_Ranges,
                Error_Encountered => False);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Line Processing
   ---------------------------------------------------------------------------

   procedure Process_Line (Line : String; State : in out Solver_State) is
      Min_Val, Max_Val : Ingredient_ID;
      Success_1, Success_2 : Boolean;
      Next_Idx : Positive;

      function Is_Blank (S : String) return Boolean is
      begin
         for I in S'Range loop
            if S (I) /= ' ' and then S (I) /= ASCII.CR and then S (I) /= ASCII.HT then
               return False;
            end if;
         end loop;
         return True;
      end Is_Blank;
   begin
      if State.Error_Encountered then
         return;
      end if;

      --  Empty line switches state
      if Is_Blank (Line) then
         State.Current_State := Parsing_Ingredients;
         return;
      end if;

      case State.Current_State is
         when Parsing_Ranges =>
            --  Format: Min-Max
            if State.Range_Count >= MAX_RANGES then
               State.Error_Encountered := True;
               return;
            end if;

            Parse_Long (Line, Line'First, Min_Val, Success_1, Next_Idx);

            if Success_1 and then Next_Idx <= Line'Last and then Line (Next_Idx) = '-' then
               Parse_Long (Line, Next_Idx + 1, Max_Val, Success_2, Next_Idx);

               if Success_2 then
                  State.Range_Count := State.Range_Count + 1;
                  State.Ranges (State.Range_Count) := (Min => Min_Val, Max => Max_Val);
               end if;
            end if;

         when Parsing_Ingredients =>
            --  Format: N
            if State.Ingredient_Count >= MAX_INGREDIENTS then
               State.Error_Encountered := True;
               return;
            end if;

            Parse_Long (Line, Line'First, Min_Val, Success_1, Next_Idx);
            if Success_1 then
               State.Ingredient_Count := State.Ingredient_Count + 1;
               State.Ingredients (State.Ingredient_Count) := Min_Val;
            end if;
      end case;
   end Process_Line;

   ---------------------------------------------------------------------------
   --  Internal Logic
   ---------------------------------------------------------------------------

   function Is_Fresh (ID : Ingredient_ID; Ranges : Range_Array; Count : Natural) return Boolean is
   begin
      for I in 1 .. Count loop
         if ID >= Ranges (I).Min and then ID <= Ranges (I).Max then
            return True;
         end if;
      end loop;
      return False;
   end Is_Fresh;

   procedure Sort_Ranges (Ranges : in out Range_Array; Count : Natural) is
      Temp : Fresh_Range;
      J : Natural;
   begin
      if Count < 2 then
         return;
      end if;

      --  Insertion sort
      for I in 2 .. Count loop
         Temp := Ranges (I);
         J := I - 1;
         
         while J > 0 and then Ranges (J).Min > Temp.Min loop
            pragma Loop_Variant (Decreases => J);
            pragma Loop_Invariant (J < I);
            
            Ranges (J + 1) := Ranges (J);
            J := J - 1;
         end loop;
         Ranges (J + 1) := Temp;
      end loop;
   end Sort_Ranges;

   ---------------------------------------------------------------------------
   --  Solvers
   ---------------------------------------------------------------------------

   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer is
      Count : Long_Long_Integer := 0;
   begin
      if State.Error_Encountered then
         return -1;
      end if;

      for I in 1 .. State.Ingredient_Count loop
         pragma Loop_Invariant (Count <= Long_Long_Integer (I - 1));
         
         if Is_Fresh (State.Ingredients (I), State.Ranges, State.Range_Count) then
            Count := Count + 1;
         end if;
      end loop;

      return Count;
   end Solve_Part_1;

   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer is
      Local_Ranges : Range_Array := State.Ranges;
      Count        : constant Natural := State.Range_Count;
      Total        : Long_Long_Integer := 0;
      Current      : Fresh_Range;
      Merged_Count : Natural := 0;
      
      Range_Size : Long_Long_Integer;
   begin
      if State.Error_Encountered or else Count = 0 then
         return 0;
      end if;

      Sort_Ranges (Local_Ranges, Count);

      Current := Local_Ranges (1);

      for I in 2 .. Count loop
         pragma Loop_Invariant (Merged_Count < I);
         pragma Loop_Invariant (Total >= 0);
         
         --  Check for overlap or adjacency (e.g., 1-5 and 6-10 can merge to 1-10)
         --  We must check safely to avoid overflow of Current.Max + 1
         if Local_Ranges (I).Min <= Current.Max
            or else (Current.Max < Long_Long_Integer'Last and then Local_Ranges (I).Min = Current.Max + 1)
         then
            if Local_Ranges (I).Max > Current.Max then
               Current.Max := Local_Ranges (I).Max;
            end if;
         else
            --  End of current range block
            Range_Size := Current.Max - Current.Min;
            
            --  Check for overflow of Range_Len = Range_Size + 1
            if Range_Size = Long_Long_Integer'Last then
               return -1;
            end if;
            
            --  Check for overflow of Total
            if Long_Long_Integer'Last - Total < (Range_Size + 1) then
               return -1;
            end if;

            Total := Total + (Range_Size + 1);
            Merged_Count := Merged_Count + 1;
            Current := Local_Ranges (I);
         end if;
      end loop;

      --  Process the last range
      Range_Size := Current.Max - Current.Min;
      
      if Range_Size = Long_Long_Integer'Last then
         return -1;
      end if;
      
      if Long_Long_Integer'Last - Total < (Range_Size + 1) then
         return -1;
      end if;

      Total := Total + (Range_Size + 1);

      return Total;
   end Solve_Part_2;

end AoC_2025_Day_05.Solver;
