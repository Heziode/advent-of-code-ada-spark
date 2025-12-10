--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_10.Solver
--
--  Implementation of solvers for button press minimization.
--
--  Algorithms:
--    Part 1: BFS over light states (bitset representation)
--           - Each button toggles specific lights (XOR)
--           - Find minimum presses to reach target state
--           - O(2^n × m) where n = lights, m = buttons
--
--    Part 2: Integer Gaussian Elimination with free variable enumeration
--           - Each button increments specific counters (+1)
--           - Find minimum sum of non-negative integer presses
--           - O(n³ + 2^k) where k = number of free variables
--
pragma Ada_2022;

package body AoC_2025_Day_10.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum BFS queue size (2^20 for 20 lights)
   MAX_QUEUE_SIZE : constant := 2 ** 16;

   --  Maximum search depth for free variable enumeration
   --  Note: Needs to be large enough to find valid solutions with large button presses
   MAX_FREE_VAR_SEARCH : constant := 2_000;

   ---------------------------------------------------------------------------
   --  Bitset Types for Efficient State Representation
   ---------------------------------------------------------------------------

   --  A light state represented as a bitmask (up to 32 lights)
   subtype Light_Bitset is Natural;

   --  Convert Light_Vector to bitset
   function To_Bitset
     (Vector      : Light_Vector;
      Light_Count : Natural) return Light_Bitset
   with
     Pre => Light_Count <= MAX_LIGHTS
   is
      Result : Light_Bitset := 0;
   begin
      for I in 0 .. Light_Count - 1 loop
         if Vector (I) = 1 then
            Result := Result + 2 ** I;
         end if;
      end loop;
      return Result;
   end To_Bitset;

   --  Convert Button_Mask to bitset
   function Mask_To_Bitset
     (Mask        : Button_Mask;
      Light_Count : Natural) return Light_Bitset
   with
     Pre => Light_Count <= MAX_LIGHTS
   is
      Result : Light_Bitset := 0;
   begin
      for I in 0 .. Light_Count - 1 loop
         if Mask (I) = 1 then
            Result := Result + 2 ** I;
         end if;
      end loop;
      return Result;
   end Mask_To_Bitset;

   --  XOR two bitsets (toggle operation)
   function Xor_Bitset (A, B : Light_Bitset) return Light_Bitset
   is
      --  Implement XOR using arithmetic for SPARK compatibility
      --  XOR(a,b) = (a + b) - 2 * AND(a,b)
      --  We use the fact that for single bits: a XOR b = (a + b) mod 2
      Result : Light_Bitset := 0;
      Bit_A, Bit_B : Natural;
      Power_Of_2 : Natural := 1;
      Temp_A : Natural := A;
      Temp_B : Natural := B;
   begin
      for I in 0 .. MAX_LIGHTS - 1 loop
         Bit_A := Temp_A mod 2;
         Bit_B := Temp_B mod 2;
         if Bit_A /= Bit_B then
            Result := Result + Power_Of_2;
         end if;
         Temp_A := Temp_A / 2;
         Temp_B := Temp_B / 2;
         exit when Temp_A = 0 and then Temp_B = 0;
         if Power_Of_2 <= Natural'Last / 2 then
            Power_Of_2 := Power_Of_2 * 2;
         else
            exit;
         end if;
      end loop;
      return Result;
   end Xor_Bitset;

   ---------------------------------------------------------------------------
   --  Parsing Helpers
   ---------------------------------------------------------------------------

   --  Check if character is a digit
   function Is_Digit (C : Character) return Boolean
   is (C in '0' .. '9')
   with Inline;

   --  Parse a natural number starting at position Idx, advance Idx past it
   procedure Parse_Natural_At
     (Line    : String;
      Idx     : in out Positive;
      Value   : out Natural;
      Success : out Boolean)
   with
     Pre  => Idx in Line'Range,
     Post => Idx <= Line'Last + 1
   is
      Result : Natural := 0;
   begin
      Success := False;
      Value := 0;

      --  Skip leading whitespace
      while Idx <= Line'Last and then Line (Idx) = ' ' loop
         Idx := Idx + 1;
      end loop;

      if Idx > Line'Last or else not Is_Digit (Line (Idx)) then
         return;
      end if;

      while Idx <= Line'Last and then Is_Digit (Line (Idx)) loop
         declare
            Digit : constant Natural := Character'Pos (Line (Idx))
                                        - Character'Pos ('0');
         begin
            --  Overflow check
            if Result > (Natural'Last - Digit) / 10 then
               return;
            end if;
            Result := Result * 10 + Digit;
         end;
         Idx := Idx + 1;
      end loop;

      Value := Result;
      Success := True;
   end Parse_Natural_At;

   --  Parse target light state from [.##.] format
   procedure Parse_Target
     (Line         : String;
      Start_Idx    : Positive;
      Target       : out Light_Vector;
      Light_Count  : out Natural;
      End_Idx      : out Natural;
      Success      : out Boolean)
   with
     Pre => Start_Idx in Line'Range
   is
      Idx : Natural := Start_Idx;
   begin
      Target := [others => 0];
      Light_Count := 0;
      End_Idx := Start_Idx;
      Success := False;

      --  Find opening bracket
      while Idx <= Line'Last and then Line (Idx) /= '[' loop
         Idx := Idx + 1;
      end loop;

      if Idx > Line'Last then
         return;
      end if;

      Idx := Idx + 1;  --  Skip '['

      --  Parse light states until ']'
      while Idx <= Line'Last and then Line (Idx) /= ']' loop
         if Light_Count >= MAX_LIGHTS then
            return;  --  Too many lights
         end if;

         case Line (Idx) is
            when '.' =>
               Target (Light_Count) := 0;  --  Light off
               Light_Count := Light_Count + 1;

            when '#' =>
               Target (Light_Count) := 1;  --  Light on
               Light_Count := Light_Count + 1;

            when others =>
               return;  --  Invalid character
         end case;

         Idx := Idx + 1;
      end loop;

      if Idx > Line'Last or else Light_Count = 0 then
         return;
      end if;

      End_Idx := Idx + 1;  --  Position after ']'
      Success := True;
   end Parse_Target;

   --  Parse a single button definition (comma-separated light indices)
   procedure Parse_Button
     (Line         : String;
      Start_Idx    : Positive;
      Light_Count  : Natural;
      Mask         : out Button_Mask;
      End_Idx      : out Natural;
      Success      : out Boolean)
   with
     Pre => Start_Idx in Line'Range and then Light_Count <= MAX_LIGHTS
   is
      Idx        : Natural := Start_Idx;
      Light_Idx  : Natural;
      Parse_Ok   : Boolean;
   begin
      Mask := [others => 0];
      End_Idx := Start_Idx;
      Success := False;

      --  Find opening parenthesis
      while Idx <= Line'Last and then Line (Idx) /= '(' loop
         Idx := Idx + 1;
      end loop;

      if Idx > Line'Last then
         return;
      end if;

      Idx := Idx + 1;  --  Skip '('

      --  Parse comma-separated light indices until ')'
      loop
         --  Skip whitespace and commas
         while Idx <= Line'Last
           and then (Line (Idx) = ' ' or else Line (Idx) = ',')
         loop
            Idx := Idx + 1;
         end loop;

         if Idx > Line'Last then
            return;  --  Unclosed parenthesis
         end if;

         if Line (Idx) = ')' then
            End_Idx := Idx + 1;
            Success := True;
            return;
         end if;

         --  Parse light index
         Parse_Natural_At (Line, Idx, Light_Idx, Parse_Ok);
         if not Parse_Ok then
            return;
         end if;

         --  Set bit in mask if valid
         if Light_Idx < Light_Count then
            Mask (Light_Idx) := 1;
         end if;
      end loop;
   end Parse_Button;

   --  Parse all buttons from line, stopping at '{' (joltage section)
   procedure Parse_Buttons
     (Line         : String;
      Start_Idx    : Positive;
      Light_Count  : Natural;
      Buttons      : out Button_Array;
      Button_Count : out Natural;
      Success      : out Boolean)
   with
     Pre => Start_Idx <= Line'Last + 1 and then Light_Count <= MAX_LIGHTS
   is
      Idx          : Natural := Start_Idx;
      End_Idx      : Natural;
      Parse_Ok     : Boolean;
   begin
      Buttons := [others => [others => 0]];
      Button_Count := 0;
      Success := True;

      while Idx <= Line'Last loop
         --  Stop at joltage section
         if Line (Idx) = '{' then
            return;
         end if;

         --  Look for next button '('
         if Line (Idx) = '(' then
            if Button_Count >= MAX_BUTTONS then
               Success := False;
               return;
            end if;

            Parse_Button (Line, Idx, Light_Count,
                          Buttons (Button_Count), End_Idx, Parse_Ok);
            if not Parse_Ok then
               Success := False;
               return;
            end if;

            Button_Count := Button_Count + 1;
            Idx := End_Idx;
         else
            Idx := Idx + 1;
         end if;
      end loop;
   end Parse_Buttons;

   --  Parse joltage requirements from {3,5,4,7} format
   procedure Parse_Joltage
     (Line           : String;
      Start_Idx      : Positive;
      Counter_Count  : Natural;
      Joltage        : out Joltage_Array;
      Success        : out Boolean)
   with
     Pre => Start_Idx <= Line'Last + 1 and then Counter_Count <= MAX_LIGHTS
   is
      Idx           : Natural := Start_Idx;
      Counter_Idx   : Natural := 0;
      Value         : Natural;
      Parse_Ok      : Boolean;
   begin
      Joltage := [others => 0];
      Success := False;

      --  Find opening brace
      while Idx <= Line'Last and then Line (Idx) /= '{' loop
         Idx := Idx + 1;
      end loop;

      if Idx > Line'Last then
         return;
      end if;

      Idx := Idx + 1;  --  Skip '{'

      --  Parse comma-separated joltage values until '}'
      loop
         --  Skip whitespace and commas
         while Idx <= Line'Last
           and then (Line (Idx) = ' ' or else Line (Idx) = ',')
         loop
            Idx := Idx + 1;
         end loop;

         if Idx > Line'Last then
            return;  --  Unclosed brace
         end if;

         if Line (Idx) = '}' then
            Success := Counter_Idx = Counter_Count;
            return;
         end if;

         --  Parse joltage value
         Parse_Natural_At (Line, Idx, Value, Parse_Ok);
         if not Parse_Ok then
            return;
         end if;

         --  Store value if within bounds
         if Counter_Idx < Counter_Count then
            if Value <= MAX_JOLTAGE then
               Joltage (Counter_Idx) := Value;
            else
               return;  --  Value too large
            end if;
            Counter_Idx := Counter_Idx + 1;
         end if;
      end loop;
   end Parse_Joltage;

   --  Parse a complete machine definition from a line
   procedure Parse_Machine
     (Line    : String;
      Mach    : out Machine;
      Success : out Boolean)
   is
      End_Idx     : Natural;
      Parse_Ok    : Boolean;
   begin
      Mach := (Light_Count   => 0,
               Button_Count  => 0,
               Target        => [others => 0],
               Joltage       => [others => 0],
               Buttons       => [others => [others => 0]]);
      Success := False;

      if Line'Length = 0 then
         return;
      end if;

      --  Parse target light state
      Parse_Target (Line, Line'First, Mach.Target, Mach.Light_Count,
                    End_Idx, Parse_Ok);
      if not Parse_Ok or else End_Idx > Line'Last then
         return;
      end if;

      --  Parse buttons
      Parse_Buttons (Line, End_Idx, Mach.Light_Count,
                     Mach.Buttons, Mach.Button_Count, Parse_Ok);
      if not Parse_Ok or else Mach.Button_Count = 0 then
         return;
      end if;

      --  Parse joltage requirements (for Part 2) - required!
      Parse_Joltage (Line, End_Idx, Mach.Light_Count, Mach.Joltage, Parse_Ok);
      if not Parse_Ok then
         Success := False;
         return;
      end if;

      Success := True;
   end Parse_Machine;

   ---------------------------------------------------------------------------
   --  Part 1 Solver: BFS over Light States
   ---------------------------------------------------------------------------

   --  BFS queue entry
   type BFS_Entry is record
      State  : Light_Bitset;
      Presses : Natural;
   end record;

   --  Simple hash set for visited states (using array for SPARK compatibility)
   --  We use a fixed-size array with linear probing
   HASH_SIZE : constant := 65_521;  --  Prime near 2^16
   type Visited_Set is array (0 .. HASH_SIZE - 1) of Natural;
   UNVISITED : constant Natural := Natural'Last;

   function Hash_State (State : Light_Bitset) return Natural
   is (State mod HASH_SIZE);

   procedure Mark_Visited
     (Visited : in out Visited_Set;
      State   : Light_Bitset;
      Already : out Boolean)
   is
      H : Natural := Hash_State (State);
      Probes : Natural := 0;
   begin
      Already := False;
      while Probes < HASH_SIZE loop
         if Visited (H) = State then
            Already := True;
            return;
         elsif Visited (H) = UNVISITED then
            Visited (H) := State;
            return;
         end if;
         H := (H + 1) mod HASH_SIZE;
         Probes := Probes + 1;
      end loop;
      --  Table full - shouldn't happen with proper sizing
   end Mark_Visited;

   --  Solve Part 1 using BFS
   function Solve_Part_1_BFS (Mach : Machine) return Natural
   is
      --  Queue for BFS
      Queue : array (0 .. MAX_QUEUE_SIZE - 1) of BFS_Entry :=
        [others => (State => 0, Presses => 0)];
      Queue_Head : Natural := 0;
      Queue_Tail : Natural := 0;

      --  Visited states
      Visited : Visited_Set := [others => UNVISITED];

      --  Goal state
      Goal : constant Light_Bitset := To_Bitset (Mach.Target, Mach.Light_Count);

      --  Button masks as bitsets
      Button_Masks : array (Button_Index) of Light_Bitset :=
        [others => 0];

      Current : BFS_Entry;
      Next_State : Light_Bitset;
      Already_Visited : Boolean;
   begin
      if Mach.Light_Count = 0 or else Mach.Button_Count = 0 then
         return 0;
      end if;

      --  Precompute button masks
      for I in 0 .. Mach.Button_Count - 1 loop
         Button_Masks (I) := Mask_To_Bitset (Mach.Buttons (I), Mach.Light_Count);
      end loop;

      --  Start with all lights off (state = 0)
      Queue (0) := (State => 0, Presses => 0);
      Queue_Tail := 1;
      Mark_Visited (Visited, 0, Already_Visited);

      --  BFS
      while Queue_Head < Queue_Tail loop
         Current := Queue (Queue_Head);
         Queue_Head := Queue_Head + 1;

         --  Check if we reached the goal
         if Current.State = Goal then
            return Current.Presses;
         end if;

         --  Try pressing each button
         for Button_Idx in 0 .. Mach.Button_Count - 1 loop
            Next_State := Xor_Bitset (Current.State, Button_Masks (Button_Idx));
            Mark_Visited (Visited, Next_State, Already_Visited);

            if not Already_Visited then
               if Queue_Tail < MAX_QUEUE_SIZE then
                  Queue (Queue_Tail) := (State => Next_State,
                                         Presses => Current.Presses + 1);
                  Queue_Tail := Queue_Tail + 1;
               end if;
            end if;
         end loop;
      end loop;

      --  No solution found
      return Natural'Last;
   end Solve_Part_1_BFS;

   ---------------------------------------------------------------------------
   --  Part 2 Solver: Integer Gaussian Elimination
   ---------------------------------------------------------------------------

   --  Extended matrix for Gaussian elimination: [Buttons | Target]
   --  Row index = counter index, Column index = button index + 1 (last col = target)
   subtype Matrix_Row_Index is Natural range 0 .. MAX_LIGHTS - 1;
   subtype Matrix_Col_Index is Natural range 0 .. MAX_BUTTONS;

   type Integer_Matrix is array (Matrix_Row_Index, Matrix_Col_Index) of Integer;

   --  Column permutation to track original button order after pivoting
   type Column_Permutation is array (Button_Index) of Natural;

   --  Build augmented matrix from machine definition for Part 2
   procedure Build_Matrix_Integer
     (Mach   : Machine;
      Matrix : out Integer_Matrix)
   is
   begin
      Matrix := [others => [others => 0]];

      --  Fill button columns (transposed: row = counter, col = button)
      for Button_Idx in 0 .. Mach.Button_Count - 1 loop
         for Counter_Idx in 0 .. Mach.Light_Count - 1 loop
            Matrix (Counter_Idx, Button_Idx) :=
              Integer (Mach.Buttons (Button_Idx) (Counter_Idx));
         end loop;
      end loop;

      --  Fill target column (last column = joltage requirements)
      for Counter_Idx in 0 .. Mach.Light_Count - 1 loop
         Matrix (Counter_Idx, Mach.Button_Count) :=
           Integer (Mach.Joltage (Counter_Idx));
      end loop;
   end Build_Matrix_Integer;

   --  Swap two rows
   procedure Swap_Rows_Integer
     (Matrix : in out Integer_Matrix;
      Row1   : Matrix_Row_Index;
      Row2   : Matrix_Row_Index;
      Cols   : Natural)
   is
      Temp : Integer;
   begin
      if Row1 = Row2 then
         return;
      end if;
      for Col in 0 .. Cols - 1 loop
         Temp := Matrix (Row1, Col);
         Matrix (Row1, Col) := Matrix (Row2, Col);
         Matrix (Row2, Col) := Temp;
      end loop;
   end Swap_Rows_Integer;

   --  Swap two columns and update permutation
   procedure Swap_Columns_Integer
     (Matrix : in out Integer_Matrix;
      Col1   : Natural;
      Col2   : Natural;
      Rows   : Natural;
      Perm   : in out Column_Permutation)
   is
      Temp_Val  : Integer;
      Temp_Perm : Natural;
   begin
      if Col1 = Col2 then
         return;
      end if;

      for Row in 0 .. Rows - 1 loop
         Temp_Val := Matrix (Row, Col1);
         Matrix (Row, Col1) := Matrix (Row, Col2);
         Matrix (Row, Col2) := Temp_Val;
      end loop;

      Temp_Perm := Perm (Col1);
      Perm (Col1) := Perm (Col2);
      Perm (Col2) := Temp_Perm;
   end Swap_Columns_Integer;

   --  Compute GCD (needed for row reduction)
   function GCD (A, B : Natural) return Natural is
      X : Natural := A;
      Y : Natural := B;
      T : Natural;
   begin
      while Y /= 0 loop
         T := Y;
         Y := X mod Y;
         X := T;
      end loop;
      return X;
   end GCD;

   --  Integer-preserving row elimination
   --  Eliminates Matrix(Target, Pivot_Col) using row Pivot
   --  Result: Matrix(Target, *) scaled to eliminate the pivot column
   procedure Integer_Eliminate
     (Matrix    : in out Integer_Matrix;
      Target    : Matrix_Row_Index;
      Pivot     : Matrix_Row_Index;
      Pivot_Col : Natural;
      Cols      : Natural)
   is
      Pivot_Val  : constant Integer := Matrix (Pivot, Pivot_Col);
      Target_Val : constant Integer := Matrix (Target, Pivot_Col);
      G : Natural;
      Scale_P, Scale_T : Integer;
   begin
      if Target_Val = 0 then
         return;  --  Already eliminated
      end if;

      --  Use GCD to minimize coefficient growth
      G := GCD (Natural (abs Pivot_Val), Natural (abs Target_Val));
      Scale_P := Target_Val / Integer (G);
      Scale_T := Pivot_Val / Integer (G);

      --  Target_Row := Target_Row * Scale_T - Pivot_Row * Scale_P
      for Col in 0 .. Cols loop
         Matrix (Target, Col) := Matrix (Target, Col) * Scale_T -
                                  Matrix (Pivot, Col) * Scale_P;
      end loop;
   end Integer_Eliminate;

   --  Gaussian elimination over integers with column pivoting
   --  Returns Reduced Row Echelon Form (RREF)
   procedure Gaussian_Elimination_Integer
     (Matrix : in out Integer_Matrix;
      Rows   : Natural;
      Cols   : Natural;
      Perm   : out Column_Permutation;
      Rank   : out Natural)
   is
      Pivot_Row : Natural := 0;
      Pivot_Col : Natural := 0;
      Found     : Boolean;
      Best_Row, Best_Col : Natural;
   begin
      Rank := 0;
      Perm := [others => 0];
      for I in 0 .. MAX_BUTTONS - 1 loop
         Perm (I) := I;
      end loop;

      while Pivot_Row < Rows and then Pivot_Col < Cols loop
         Found := False;
         Best_Row := Pivot_Row;
         Best_Col := Pivot_Col;

         --  Find non-zero pivot (prefer current column)
         for Row in Pivot_Row .. Rows - 1 loop
            if Matrix (Row, Pivot_Col) /= 0 then
               Best_Row := Row;
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            --  Search other columns
            Search_Cols : for Col in Pivot_Col + 1 .. Cols - 1 loop
               for Row in Pivot_Row .. Rows - 1 loop
                  if Matrix (Row, Col) /= 0 then
                     Best_Row := Row;
                     Best_Col := Col;
                     Found := True;
                     exit Search_Cols;
                  end if;
               end loop;
            end loop Search_Cols;

            if Found then
               Swap_Columns_Integer (Matrix, Pivot_Col, Best_Col, Rows, Perm);
            end if;
         end if;

         if Found then
            Swap_Rows_Integer (Matrix, Pivot_Row, Best_Row, Cols + 1);

            --  Eliminate all other rows
            for Row in 0 .. Rows - 1 loop
               if Row /= Pivot_Row then
                  Integer_Eliminate (Matrix, Row, Pivot_Row, Pivot_Col, Cols);
               end if;
            end loop;

            Rank := Rank + 1;
            Pivot_Row := Pivot_Row + 1;
         end if;

         Pivot_Col := Pivot_Col + 1;
      end loop;
   end Gaussian_Elimination_Integer;

   --  Check if a rational value is a non-negative integer
   function Is_Valid_Rational (Numerator, Denominator : Integer) return Boolean
   is
   begin
      if Denominator = 0 then
         return False;
      end if;
      if Numerator mod Denominator /= 0 then
         return False;  --  Not an integer
      end if;
      if (Numerator >= 0) /= (Denominator >= 0) then
         --  Different signs => negative result
         return Numerator = 0;
      end if;
      return True;  --  Non-negative integer
   end Is_Valid_Rational;

   --  Solution array type for Part 2
   type Button_Press_Array is array (Button_Index) of Natural;

   --  Try to compute a solution given free variable values
   --  Returns True if valid, and populates Solution
   procedure Try_Compute_Solution
     (Matrix       : Integer_Matrix;
      Rank         : Natural;
      Free_Count   : Natural;
      Button_Count : Natural;
      Free_Values  : Column_Permutation;
      Solution     : out Button_Press_Array;
      Is_Valid     : out Boolean)
   is
      Numerator, Denominator : Integer;
   begin
      Solution := [others => 0];
      Is_Valid := True;

      --  Set free variables
      for F in 0 .. Free_Count - 1 loop
         Solution (Rank + F) := Free_Values (F);
      end loop;

      --  Compute pivot variables by back-substitution
      for Row in 0 .. Rank - 1 loop
         Denominator := Matrix (Row, Row);
         Numerator := Matrix (Row, Button_Count);

         for F in 0 .. Free_Count - 1 loop
            Numerator := Numerator -
              Matrix (Row, Rank + F) * Integer (Free_Values (F));
         end loop;

         if not Is_Valid_Rational (Numerator, Denominator) then
            Is_Valid := False;
            return;
         end if;

         Solution (Row) := Natural (Numerator / Denominator);
      end loop;
   end Try_Compute_Solution;

   --  Compute sum of solution
   function Compute_Solution_Sum
     (Solution     : Button_Press_Array;
      Button_Count : Natural) return Natural
   is
      Sum : Natural := 0;
   begin
      for I in 0 .. Button_Count - 1 loop
         Sum := Sum + Solution (I);
      end loop;
      return Sum;
   end Compute_Solution_Sum;

   --  Compute upper bound for a free variable based on constraints
   --  Returns an upper bound such that x_free <= bound keeps all pivots non-negative
   --
   --  Only uses constraints where this is the LAST free variable with non-zero coeff,
   --  because other free variables (not yet assigned) could compensate.
   --
   function Compute_Free_Var_Upper_Bound
     (Matrix       : Integer_Matrix;
      Rank         : Natural;
      Free_Idx     : Natural;
      Free_Count   : Natural;
      Button_Count : Natural;
      Current_Free : Column_Permutation) return Natural
   is
      Upper_Bound : Integer := MAX_FREE_VAR_SEARCH;
   begin
      for Row in 0 .. Rank - 1 loop
         declare
            Coeff : constant Integer := Matrix (Row, Rank + Free_Idx);
            Pivot_Coeff : constant Integer := Matrix (Row, Row);
            Has_Later_Free : Boolean := False;
         begin
            --  Skip if coefficient is zero (no constraint from this row)
            if Coeff = 0 then
               null;
            else
               --  Check if any later free variable has non-zero coefficient
               --  If so, we can't bound this variable from this row
               for F in Free_Idx + 1 .. Free_Count - 1 loop
                  if Matrix (Row, Rank + F) /= 0 then
                     Has_Later_Free := True;
                     exit;
                  end if;
               end loop;

               if not Has_Later_Free then
                  --  This is the only remaining free variable in this row
                  --  Compute adjusted RHS by subtracting already-assigned free vars
                  declare
                     Adjusted_RHS : Integer := Matrix (Row, Button_Count);
                  begin
                     for F in 0 .. Free_Idx - 1 loop
                        Adjusted_RHS := Adjusted_RHS - Matrix (Row, Rank + F) *
                          Integer (Current_Free (F));
                     end loop;

                     --  Case 1: Pivot > 0, Coeff > 0 => x_free <= Adjusted_RHS / Coeff
                     if Pivot_Coeff > 0 and then Coeff > 0 then
                        declare
                           Bound : constant Integer := Adjusted_RHS / Coeff;
                        begin
                           if Bound >= 0 and then Bound < Upper_Bound then
                              Upper_Bound := Bound;
                           elsif Bound < 0 then
                              --  Constraint is impossible, no valid x_free
                              Upper_Bound := -1;
                              exit;  --  Early exit, no valid solution
                           end if;
                        end;

                     --  Case 2: Pivot < 0, Coeff < 0 => x_free <= Adjusted_RHS / Coeff
                     elsif Pivot_Coeff < 0 and then Coeff < 0 then
                        declare
                           Bound : constant Integer := Adjusted_RHS / Coeff;
                        begin
                           if Bound >= 0 and then Bound < Upper_Bound then
                              Upper_Bound := Bound;
                           elsif Bound < 0 then
                              Upper_Bound := -1;
                              exit;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;

      --  Clamp to valid range
      if Upper_Bound < 0 then
         return 0;
      elsif Upper_Bound > MAX_FREE_VAR_SEARCH then
         return MAX_FREE_VAR_SEARCH;
      else
         return Natural (Upper_Bound);
      end if;
   end Compute_Free_Var_Upper_Bound;

   --  Iterative search over free variables using depth-first approach
   --  Returns minimum sum, or Natural'Last if no valid solution
   function Search_Free_Variables_Iterative
     (Matrix       : Integer_Matrix;
      Rank         : Natural;
      Free_Count   : Natural;
      Button_Count : Natural) return Natural
   is
      --  Stack for DFS (stores current free variable values)
      Free_Values  : Column_Permutation := [others => 0];
      Current_Idx  : Natural := 0;
      Best_Sum     : Natural := Natural'Last;
      Solution     : Button_Press_Array;
      Is_Valid     : Boolean;
      Upper_Bound  : Natural;
      Current_Sum  : Natural := 0;
   begin
      if Free_Count = 0 then
         --  No free variables - try the unique solution
         Try_Compute_Solution (Matrix, Rank, 0, Button_Count, Free_Values,
                               Solution, Is_Valid);
         if Is_Valid then
            return Compute_Solution_Sum (Solution, Button_Count);
         else
            return Natural'Last;
         end if;
      end if;

      --  DFS over free variables
      loop
         if Current_Idx = Free_Count then
            --  All free variables assigned - evaluate solution
            Try_Compute_Solution (Matrix, Rank, Free_Count, Button_Count,
                                  Free_Values, Solution, Is_Valid);
            if Is_Valid then
               declare
                  Total : constant Natural :=
                    Compute_Solution_Sum (Solution, Button_Count);
               begin
                  if Total < Best_Sum then
                     Best_Sum := Total;
                  end if;
               end;
            end if;

            --  Backtrack
            if Current_Idx = 0 then
               exit;
            end if;
            Current_Idx := Current_Idx - 1;
            Current_Sum := Current_Sum - Free_Values (Current_Idx);
            Free_Values (Current_Idx) := Free_Values (Current_Idx) + 1;
         else
            --  Compute upper bound for current free variable
            Upper_Bound := Compute_Free_Var_Upper_Bound
              (Matrix, Rank, Current_Idx, Free_Count, Button_Count, Free_Values);

            --  Pruning: skip if current sum >= best
            if Current_Sum >= Best_Sum then
               --  Backtrack
               if Current_Idx = 0 then
                  exit;
               end if;
               Current_Idx := Current_Idx - 1;
               Current_Sum := Current_Sum - Free_Values (Current_Idx);
               Free_Values (Current_Idx) := Free_Values (Current_Idx) + 1;
            elsif Free_Values (Current_Idx) > Upper_Bound then
               --  Exhausted values for this variable, backtrack
               Free_Values (Current_Idx) := 0;
               if Current_Idx = 0 then
                  exit;
               end if;
               Current_Idx := Current_Idx - 1;
               Current_Sum := Current_Sum - Free_Values (Current_Idx);
               Free_Values (Current_Idx) := Free_Values (Current_Idx) + 1;
            else
               --  Move to next free variable
               Current_Sum := Current_Sum + Free_Values (Current_Idx);
               Current_Idx := Current_Idx + 1;
            end if;
         end if;
      end loop;

      return Best_Sum;
   end Search_Free_Variables_Iterative;

   --  Solve Part 2 for a single machine
   function Solve_Part_2_Integer (Mach : Machine) return Natural
   is
      Matrix     : Integer_Matrix;
      Perm       : Column_Permutation;
      Rank       : Natural;
      Free_Count : Natural;
   begin
      if Mach.Light_Count = 0 or else Mach.Button_Count = 0 then
         return 0;
      end if;

      Build_Matrix_Integer (Mach, Matrix);
      Gaussian_Elimination_Integer (Matrix, Mach.Light_Count, Mach.Button_Count,
                                    Perm, Rank);

      --  Check for inconsistency: non-zero in RHS of zero rows
      for Row in Rank .. Mach.Light_Count - 1 loop
         if Matrix (Row, Mach.Button_Count) /= 0 then
            return Natural'Last;  --  No solution
         end if;
      end loop;

      Free_Count := Mach.Button_Count - Rank;

      declare
         Result : constant Natural := Search_Free_Variables_Iterative
           (Matrix, Rank, Free_Count, Mach.Button_Count);
      begin
         return Result;
      end;
   end Solve_Part_2_Integer;

   ---------------------------------------------------------------------------
   --  Public Interface Implementation
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State)
   is
   begin
      State := (Part                 => AoC_Common.Part_1,
                Machine_Count        => 0,
                Total_Presses        => 0,
                Total_Presses_Part_2 => 0,
                Error_Encountered    => False);
   end Initialize;

   procedure Process_Line (Line : String; State : in out Solver_State)
   is
      Mach           : Machine;
      Parse_Ok       : Boolean;
      Min_Presses_P1 : Natural;
      Min_Presses_P2 : Natural;
   begin
      --  Skip empty lines
      if Line'Length = 0 then
         return;
      end if;

      --  Check if it's a valid machine line (starts with '[')
      declare
         Found_Bracket : Boolean := False;
      begin
         for I in Line'Range loop
            if Line (I) = '[' then
               Found_Bracket := True;
               exit;
            elsif Line (I) /= ' ' then
               return;  --  Non-whitespace before '[' - skip line
            end if;
         end loop;

         if not Found_Bracket then
            return;
         end if;
      end;

      --  Parse the machine definition
      Parse_Machine (Line, Mach, Parse_Ok);

      if not Parse_Ok then
         State.Error_Encountered := True;
         return;
      end if;

      --  Solve Part 1: BFS for minimum light toggle presses
      Min_Presses_P1 := Solve_Part_1_BFS (Mach);

      if Min_Presses_P1 = Natural'Last then
         State.Error_Encountered := True;
         return;
      end if;

      --  Accumulate Part 1 result
      if State.Total_Presses <= Natural'Last - Min_Presses_P1 then
         State.Total_Presses := State.Total_Presses + Min_Presses_P1;
      else
         State.Error_Encountered := True;
      end if;

      --  Solve Part 2: Integer linear system for joltage counters
      Min_Presses_P2 := Solve_Part_2_Integer (Mach);

      if Min_Presses_P2 /= Natural'Last then
         if State.Total_Presses_Part_2 <= Natural'Last - Min_Presses_P2 then
            State.Total_Presses_Part_2 :=
              State.Total_Presses_Part_2 + Min_Presses_P2;
         else
            State.Error_Encountered := True;
         end if;
      end if;

      State.Machine_Count := State.Machine_Count + 1;
   end Process_Line;

   procedure Solve_Puzzle (State : in out Solver_State)
   is
   begin
      --  Processing is done line-by-line, nothing additional needed here
      null;
   end Solve_Puzzle;

   function Result_Part_1 (State : Solver_State) return Natural
   is
   begin
      return State.Total_Presses;
   end Result_Part_1;

   function Result_Part_2 (State : Solver_State) return Natural
   is
   begin
      return State.Total_Presses_Part_2;
   end Result_Part_2;

end AoC_2025_Day_10.Solver;
