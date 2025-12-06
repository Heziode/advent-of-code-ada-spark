--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_06.Solver
--
--  Algorithm:
--    1. Read input lines into a 2D character grid
--    2. Identify problem columns (groups of non-space columns separated by
--       all-space columns)
--    3. For each problem: extract numbers from rows, get operator from last row
--    4. Compute result (sum or product) for each problem
--    5. Return grand total of all problem results

pragma Ada_2022;

with AoC_Common;

package body AoC_2025_Day_06.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Local Helper Functions
   ---------------------------------------------------------------------------

   --  Check if a column is entirely spaces (separator column)
   function Is_Separator_Column (Grid : Grid_Type; Column : Positive; Row_Count : Natural) return Boolean
   with Pre => Column <= MAX_COLUMNS and then Row_Count <= MAX_ROWS
   is
   begin
      for Row in 1 .. Row_Count loop
         pragma Loop_Invariant (Row <= Row_Count);
         if Grid (Row) (Column) /= ' ' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Separator_Column;

   --  Parse a number from a string slice (handles leading/trailing spaces)
   procedure Parse_Number_From_Slice (S : String; Value : out Problem_Value; Success : out Boolean)
   with Pre => S'Length > 0 and then S'First >= 1 and then S'Last <= Integer'Last - 1 and then S'Last < Positive'Last
   is
      Start_Idx : Natural := 0;
      End_Idx   : Natural := 0;
      Accum     : Problem_Value := 0;
   begin
      Value := 0;
      Success := False;

      --  Find first digit
      for I in S'Range loop
         pragma Loop_Invariant (I in S'Range);
         pragma Loop_Invariant (Start_Idx = 0);
         if S (I) in '0' .. '9' then
            Start_Idx := I;
            exit;
         end if;
      end loop;

      if Start_Idx = 0 then
         return;  --  No digits found

      end if;

      --  Find last consecutive digit starting from Start_Idx
      End_Idx := Start_Idx;
      for I in Start_Idx .. S'Last loop
         pragma Loop_Invariant (I in S'Range);
         pragma Loop_Invariant (End_Idx in S'Range);
         pragma Loop_Invariant (End_Idx >= Start_Idx);
         if S (I) in '0' .. '9' then
            End_Idx := I;
         else
            exit;
         end if;
      end loop;

      --  Parse the number - we verified Start_Idx..End_Idx are all digits above
      for I in Start_Idx .. End_Idx loop
         pragma Loop_Invariant (Accum >= 0);
         pragma Loop_Invariant (I in S'Range);

         --  Check that current character is a digit (defensive)
         if S (I) not in '0' .. '9' then
            return;  --  Shouldn't happen with valid input

         end if;

         declare
            Char_Val : constant Natural := Character'Pos (S (I)) - Character'Pos ('0');
            Digit    : constant Problem_Value := Problem_Value (Char_Val);
         begin
            --  Overflow check
            if Accum > (Problem_Value'Last - Digit) / 10 then
               return;  --  Would overflow

            end if;
            Accum := Accum * 10 + Digit;
         end;
      end loop;

      Value := Accum;
      Success := True;
   end Parse_Number_From_Slice;

   --  Extract a column range as a string from the grid
   function Extract_Column_Slice (Grid : Grid_Type; Row : Positive; Col_Start, Col_End : Positive) return String
   with
     Pre =>
       Row <= MAX_ROWS
       and then Col_Start >= 1
       and then Col_Start <= MAX_COLUMNS
       and then Col_End <= MAX_COLUMNS
       and then Col_Start <= Col_End
       and then Col_End - Col_Start < Integer'Last - 1
   is
      Len    : constant Positive := Col_End - Col_Start + 1;
      Result : String (1 .. Len) := [others => ' '];
   begin
      for I in 0 .. Len - 1 loop
         pragma Loop_Invariant (I >= 0 and then I < Len);
         pragma Loop_Invariant (Col_Start + I <= Col_End);
         Result (I + 1) := Grid (Row) (Col_Start + I);
      end loop;
      return Result;
   end Extract_Column_Slice;

   --  Find the operator in the last row within a column range
   function Find_Operator (Grid : Grid_Type; Last_Row : Positive; Col_Start, Col_End : Positive) return Operation_Type
   with
     Pre =>
       Last_Row <= MAX_ROWS
       and then Col_Start <= MAX_COLUMNS
       and then Col_End <= MAX_COLUMNS
       and then Col_Start <= Col_End
   is
   begin
      for Col in Col_Start .. Col_End loop
         pragma Loop_Invariant (Col <= Col_End);
         if Grid (Last_Row) (Col) = '*' then
            return Multiply;
         elsif Grid (Last_Row) (Col) = '+' then
            return Add;
         end if;
      end loop;
      --  Default to Add if no operator found (shouldn't happen with valid input)
      return Add;
   end Find_Operator;

   --  Compute a single problem given column range
   procedure Compute_Problem
     (Grid               : Grid_Type;
      Row_Count          : Positive;
      Col_Start, Col_End : Positive;
      Result             : out Problem_Value;
      Success            : out Boolean)
   with
     Pre =>
       Row_Count >= 2
       and then Row_Count <= MAX_ROWS
       and then Col_Start <= MAX_COLUMNS
       and then Col_End <= MAX_COLUMNS
       and then Col_Start <= Col_End
   is
      Op          : Operation_Type;
      Accumulator : Problem_Value;
      Num_Value   : Problem_Value;
      Parse_OK    : Boolean;
      First_Num   : Boolean := True;
   begin
      Result := 0;
      Success := False;

      --  Get the operator from the last row
      Op := Find_Operator (Grid, Row_Count, Col_Start, Col_End);

      --  Initialize accumulator based on operation
      case Op is
         when Add =>
            Accumulator := 0;

         when Multiply =>
            Accumulator := 1;
      end case;

      --  Process all rows except the last (which contains the operator)
      for Row in 1 .. Row_Count - 1 loop
         pragma Loop_Invariant (Row <= Row_Count - 1);
         pragma Loop_Invariant (Accumulator >= 0);

         declare
            Slice : constant String := Extract_Column_Slice (Grid, Row, Col_Start, Col_End);
         begin
            --  Slice is guaranteed non-empty since Col_Start <= Col_End
            if Slice'Length > 0 and then Slice'Last < Positive'Last then
               Parse_Number_From_Slice (Slice, Num_Value, Parse_OK);
            else
               Parse_OK := False;
               Num_Value := 0;
            end if;

            if Parse_OK then
               First_Num := False;
               case Op is
                  when Add =>
                     --  Overflow check for addition
                     if Accumulator > Problem_Value'Last - Num_Value then
                        return;  --  Overflow

                     end if;
                     Accumulator := Accumulator + Num_Value;

                  when Multiply =>
                     --  Overflow check for multiplication
                     if Num_Value > 0 and then Accumulator > Problem_Value'Last / Num_Value then
                        return;  --  Overflow

                     end if;
                     Accumulator := Accumulator * Num_Value;
               end case;
            end if;
         end;
      end loop;

      if First_Num then
         --  No numbers found in this problem column
         return;
      end if;

      Result := Accumulator;
      Success := True;
   end Compute_Problem;

   --  Read a number vertically from a single column (top = most significant)
   procedure Parse_Vertical_Number
     (Grid : Grid_Type; Column : Positive; Row_Count : Positive; Value : out Problem_Value; Success : out Boolean)
   with Pre => Column <= MAX_COLUMNS and then Row_Count >= 2 and then Row_Count <= MAX_ROWS
   is
      Accum       : Problem_Value := 0;
      Found_Digit : Boolean := False;
   begin
      Value := 0;
      Success := False;

      --  Read from top to bottom (rows 1 to Row_Count-1, excluding operator row)
      for Row in 1 .. Row_Count - 1 loop
         pragma Loop_Invariant (Row <= Row_Count - 1);
         pragma Loop_Invariant (Accum >= 0);

         declare
            C : constant Character := Grid (Row) (Column);
         begin
            if C in '0' .. '9' then
               Found_Digit := True;
               declare
                  Digit : constant Problem_Value := Problem_Value (Character'Pos (C) - Character'Pos ('0'));
               begin
                  --  Overflow check
                  if Accum > (Problem_Value'Last - Digit) / 10 then
                     return;  --  Overflow

                  end if;
                  Accum := Accum * 10 + Digit;
               end;
            elsif Found_Digit then
               --  Non-digit after digits - stop (shouldn't happen with valid input)
               exit;
            end if;
            --  Leading spaces are ignored
         end;
      end loop;

      if not Found_Digit then
         return;  --  No digits in this column

      end if;

      Value := Accum;
      Success := True;
   end Parse_Vertical_Number;

   --  Compute a single problem for Part 2 (vertical number reading)
   procedure Compute_Problem_Part2
     (Grid               : Grid_Type;
      Row_Count          : Positive;
      Col_Start, Col_End : Positive;
      Result             : out Problem_Value;
      Success            : out Boolean)
   with
     Pre =>
       Row_Count >= 2
       and then Row_Count <= MAX_ROWS
       and then Col_Start >= 1
       and then Col_Start <= MAX_COLUMNS
       and then Col_End <= MAX_COLUMNS
       and then Col_Start <= Col_End
   is
      Op          : Operation_Type;
      Accumulator : Problem_Value;
      Num_Value   : Problem_Value;
      Parse_OK    : Boolean;
      First_Num   : Boolean := True;
   begin
      Result := 0;
      Success := False;

      --  Get the operator from the last row
      Op := Find_Operator (Grid, Row_Count, Col_Start, Col_End);

      --  Initialize accumulator based on operation
      case Op is
         when Add =>
            Accumulator := 0;

         when Multiply =>
            Accumulator := 1;
      end case;

      --  Process each column within the problem range
      --  Each column forms a single vertical number
      for Col in Col_Start .. Col_End loop
         pragma Loop_Invariant (Col <= Col_End);
         pragma Loop_Invariant (Accumulator >= 0);

         Parse_Vertical_Number (Grid, Col, Row_Count, Num_Value, Parse_OK);

         if Parse_OK then
            First_Num := False;
            case Op is
               when Add =>
                  --  Overflow check for addition
                  if Accumulator > Problem_Value'Last - Num_Value then
                     return;  --  Overflow

                  end if;
                  Accumulator := Accumulator + Num_Value;

               when Multiply =>
                  --  Overflow check for multiplication
                  if Num_Value > 0 and then Accumulator > Problem_Value'Last / Num_Value then
                     return;  --  Overflow

                  end if;
                  Accumulator := Accumulator * Num_Value;
            end case;
         end if;
      end loop;

      if First_Num then
         --  No numbers found in this problem
         return;
      end if;

      Result := Accumulator;
      Success := True;
   end Compute_Problem_Part2;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State) is
   begin
      State :=
        (Part              => AoC_Common.Part_1,
         Grid              => [others => [others => ' ']],
         Row_Count         => 0,
         Max_Column_Used   => 0,
         Problem_Results   => [others => 0],
         Problem_Count     => 0,
         Error_Encountered => False);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Line Processing
   ---------------------------------------------------------------------------

   procedure Process_Line (Line : String; State : in out Solver_State) is
   begin
      if State.Error_Encountered then
         return;
      end if;

      --  Check row limit
      if State.Row_Count >= MAX_ROWS then
         State.Error_Encountered := True;
         return;
      end if;

      State.Row_Count := State.Row_Count + 1;

      --  Copy line into grid (up to MAX_COLUMNS)
      --  Guard against potential overflow in index calculation
      if Line'First > Integer'Last - MAX_COLUMNS then
         State.Error_Encountered := True;
         return;
      end if;

      declare
         Copy_Len : constant Natural := Natural'Min (Line'Length, MAX_COLUMNS);
      begin
         for I in 1 .. Copy_Len loop
            pragma Loop_Invariant (I >= 1 and then I <= Copy_Len);
            pragma Loop_Invariant (State.Row_Count <= MAX_ROWS);
            pragma Loop_Invariant (Line'First + I - 1 <= Line'Last);
            State.Grid (State.Row_Count) (I) := Line (Line'First + I - 1);
         end loop;

         --  Update max column used
         if Copy_Len > State.Max_Column_Used then
            State.Max_Column_Used := Copy_Len;
         end if;
      end;
   end Process_Line;

   ---------------------------------------------------------------------------
   --  Finalize Processing - Parse columns and compute results
   ---------------------------------------------------------------------------

   procedure Finalize_Processing (State : in out Solver_State) is
      Col           : Natural := 1;
      Problem_Start : Natural := 0;
      In_Problem    : Boolean := False;
   begin
      if State.Error_Encountered or else State.Row_Count < 2 then
         return;
      end if;

      --  Scan columns to identify problems
      while Col <= State.Max_Column_Used loop
         pragma Loop_Invariant (Col >= 1);
         pragma Loop_Invariant (Col <= State.Max_Column_Used);
         pragma Loop_Invariant (State.Problem_Count <= MAX_PROBLEMS);
         pragma Loop_Invariant (State.Row_Count >= 2);
         pragma Loop_Invariant (State.Row_Count <= MAX_ROWS);
         pragma Loop_Invariant (State.Max_Column_Used <= MAX_COLUMNS);
         pragma Loop_Invariant (if In_Problem then Problem_Start >= 1 and then Problem_Start <= State.Max_Column_Used);

         declare
            Is_Sep : constant Boolean := Is_Separator_Column (State.Grid, Col, State.Row_Count);
         begin
            if not Is_Sep and then not In_Problem then
               --  Start of a new problem
               In_Problem := True;
               Problem_Start := Col;

            elsif Is_Sep and then In_Problem then
               --  End of current problem
               In_Problem := False;

               --  Compute this problem if we have room and Col > 1
               if State.Problem_Count < MAX_PROBLEMS
                 and then Problem_Start >= 1
                 and then Problem_Start <= MAX_COLUMNS
                 and then Col > 1
                 and then Col - 1 >= Problem_Start
               then
                  declare
                     Result  : Problem_Value;
                     Success : Boolean;
                  begin
                     Compute_Problem (State.Grid, State.Row_Count, Problem_Start, Col - 1, Result, Success);

                     if Success then
                        State.Problem_Count := State.Problem_Count + 1;
                        State.Problem_Results (State.Problem_Count) := Result;
                     end if;
                  end;
               end if;
            end if;
         end;

         exit when Col = State.Max_Column_Used;
         Col := Col + 1;
      end loop;

      --  Handle last problem if we ended while still in a problem
      if In_Problem
        and then State.Problem_Count < MAX_PROBLEMS
        and then Problem_Start >= 1
        and then Problem_Start <= MAX_COLUMNS
        and then State.Max_Column_Used >= Problem_Start
      then
         declare
            Result  : Problem_Value;
            Success : Boolean;
         begin
            Compute_Problem (State.Grid, State.Row_Count, Problem_Start, State.Max_Column_Used, Result, Success);

            if Success then
               State.Problem_Count := State.Problem_Count + 1;
               State.Problem_Results (State.Problem_Count) := Result;
            end if;
         end;
      end if;
   end Finalize_Processing;

   ---------------------------------------------------------------------------
   --  Solvers
   ---------------------------------------------------------------------------

   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer is
      Total : Long_Long_Integer := 0;
   begin
      if State.Error_Encountered then
         return -1;
      end if;

      for I in 1 .. State.Problem_Count loop
         pragma Loop_Invariant (I <= State.Problem_Count);
         pragma Loop_Invariant (Total >= 0);

         --  Overflow check
         if Total > Long_Long_Integer'Last - State.Problem_Results (I) then
            return -1;  --  Overflow

         end if;

         Total := Total + State.Problem_Results (I);
      end loop;

      return Total;
   end Solve_Part_1;

   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer is
      Total         : Long_Long_Integer := 0;
      Col           : Natural := 1;
      Problem_Start : Natural := 0;
      In_Problem    : Boolean := False;
   begin
      if State.Error_Encountered or else State.Row_Count < 2 then
         return -1;
      end if;

      --  Scan columns to identify problems (same as Part 1)
      while Col <= State.Max_Column_Used loop
         pragma Loop_Invariant (Col >= 1);
         pragma Loop_Invariant (Col <= State.Max_Column_Used);
         pragma Loop_Invariant (Total >= 0);
         pragma Loop_Invariant (State.Row_Count >= 2);
         pragma Loop_Invariant (State.Row_Count <= MAX_ROWS);
         pragma Loop_Invariant (State.Max_Column_Used <= MAX_COLUMNS);
         pragma Loop_Invariant (if In_Problem then Problem_Start >= 1 and then Problem_Start <= State.Max_Column_Used);

         declare
            Is_Sep : constant Boolean := Is_Separator_Column (State.Grid, Col, State.Row_Count);
         begin
            if not Is_Sep and then not In_Problem then
               --  Start of a new problem
               In_Problem := True;
               Problem_Start := Col;

            elsif Is_Sep and then In_Problem then
               --  End of current problem - compute with Part 2 logic
               In_Problem := False;

               if Problem_Start >= 1
                 and then Problem_Start <= MAX_COLUMNS
                 and then Col > 1
                 and then Col - 1 >= Problem_Start
               then
                  declare
                     Result  : Problem_Value;
                     Success : Boolean;
                  begin
                     Compute_Problem_Part2 (State.Grid, State.Row_Count, Problem_Start, Col - 1, Result, Success);

                     if Success then
                        --  Overflow check
                        if Total > Long_Long_Integer'Last - Result then
                           return -1;
                        end if;
                        Total := Total + Result;
                     end if;
                  end;
               end if;
            end if;
         end;

         exit when Col = State.Max_Column_Used;
         Col := Col + 1;
      end loop;

      --  Handle last problem if we ended while still in a problem
      if In_Problem
        and then Problem_Start >= 1
        and then Problem_Start <= MAX_COLUMNS
        and then State.Max_Column_Used >= Problem_Start
      then
         declare
            Result  : Problem_Value;
            Success : Boolean;
         begin
            Compute_Problem_Part2 (State.Grid, State.Row_Count, Problem_Start, State.Max_Column_Used, Result, Success);

            if Success then
               if Total > Long_Long_Integer'Last - Result then
                  return -1;
               end if;
               Total := Total + Result;
            end if;
         end;
      end if;

      return Total;
   end Solve_Part_2;

end AoC_2025_Day_06.Solver;
