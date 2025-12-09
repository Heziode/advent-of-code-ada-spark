--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_08.Solver
--
--  Algorithm:
--    1. Parse input lines to extract 3D coordinates (X,Y,Z)
--    2. Calculate squared Euclidean distance for all pairs
--    3. Sort pairs by distance (ascending)
--    4. Use Union-Find to process connections
--    5. After 1000 connections, find 3 largest circuits
--    6. Return product of 3 largest circuit sizes

pragma Ada_2022;

with AoC_Common;

package body AoC_2025_Day_08.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Calculate squared Euclidean distance between two boxes
   function Squared_Euclidean_Distance (A : Junction_Box; B : Junction_Box) return Squared_Distance is
      DX : constant Long_Long_Integer := Long_Long_Integer (A.X) - Long_Long_Integer (B.X);
      DY : constant Long_Long_Integer := Long_Long_Integer (A.Y) - Long_Long_Integer (B.Y);
      DZ : constant Long_Long_Integer := Long_Long_Integer (A.Z) - Long_Long_Integer (B.Z);
   begin
      return DX * DX + DY * DY + DZ * DZ;
   end Squared_Euclidean_Distance;

   --  Parse a coordinate value from string at given position
   --  Returns the value and the position after the number
   procedure Parse_Number
     (Line : String; Start_Pos : Positive; Value : out Coordinate_Value; End_Pos : out Natural; Success : out Boolean)
   with Pre => Start_Pos in Line'Range
   is
      Pos    : Positive := Start_Pos;
      Result : Natural := 0;
   begin
      Value := 0;
      End_Pos := Start_Pos;
      Success := False;

      --  Skip leading whitespace
      while Pos <= Line'Last and then Line (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      --  Check we have a digit
      if Pos > Line'Last or else Line (Pos) not in '0' .. '9' then
         return;
      end if;

      --  Parse digits
      while Pos <= Line'Last and then Line (Pos) in '0' .. '9' loop
         declare
            Digit : constant Natural := Character'Pos (Line (Pos)) - Character'Pos ('0');
         begin
            --  Check for overflow
            if Result > (Coordinate_Value'Last - Digit) / 10 then
               return;
            end if;
            Result := Result * 10 + Digit;
            Pos := Pos + 1;
         end;
      end loop;

      Value := Result;
      End_Pos := Pos;
      Success := True;
   end Parse_Number;

   ---------------------------------------------------------------------------
   --  Union-Find Operations
   ---------------------------------------------------------------------------

   --  Find the root of a box's circuit (pure function, no path compression)
   function Find_Root (Parent : Parent_Array; Box : Box_Index; Count : Natural) return Box_Index is
      Current : Box_Index := Box;
   begin
      if Count = 0 or else Box > Count then
         return Box;
      end if;

      --  Follow parent chain to find root
      while Current <= Count and then Parent (Current) /= Current loop
         Current := Parent (Current);
      end loop;

      return Current;
   end Find_Root;

   --  Union two circuits by rank (procedure with in out parameters)
   procedure Union_Circuits
     (Parent     : in out Parent_Array;
      Rank       : in out Rank_Array;
      Box_A      : Box_Index;
      Box_B      : Box_Index;
      Count      : Natural;
      Was_Merged : out Boolean)
   is
      Root_A : Box_Index;
      Root_B : Box_Index;
   begin
      Was_Merged := False;

      if Count = 0 or else Box_A > Count or else Box_B > Count then
         return;
      end if;

      Root_A := Find_Root (Parent, Box_A, Count);
      Root_B := Find_Root (Parent, Box_B, Count);

      --  Already in the same circuit
      if Root_A = Root_B then
         return;
      end if;

      --  Union by rank
      if Rank (Root_A) < Rank (Root_B) then
         Parent (Root_A) := Root_B;
      elsif Rank (Root_A) > Rank (Root_B) then
         Parent (Root_B) := Root_A;
      else
         Parent (Root_B) := Root_A;
         if Rank (Root_A) < Natural'Last then
            Rank (Root_A) := Rank (Root_A) + 1;
         end if;
      end if;

      Was_Merged := True;
   end Union_Circuits;

   ---------------------------------------------------------------------------
   --  Pair Management (keep only smallest N pairs)
   ---------------------------------------------------------------------------

   --  Insert a pair into a sorted array, keeping only the smallest N pairs
   --  The array is kept sorted in ascending order by distance
   procedure Insert_Pair_If_Smaller (Pairs : in out Kept_Pair_Array; Count : in out Natural; New_Pair : Box_Pair) is
   begin
      --  If array is not full, just insert in sorted position
      if Count < MAX_KEPT_PAIRS then
         Count := Count + 1;

         --  Find insertion point (binary search would be better, but insertion is fine)
         declare
            Insert_Pos : Positive := Count;
         begin
            --  Find where to insert (from the end)
            while Insert_Pos > 1 and then Pairs (Insert_Pos - 1).Distance > New_Pair.Distance loop
               Pairs (Insert_Pos) := Pairs (Insert_Pos - 1);
               Insert_Pos := Insert_Pos - 1;
            end loop;
            Pairs (Insert_Pos) := New_Pair;
         end;

         --  If array is full, only insert if smaller than the largest (last) element
      elsif New_Pair.Distance < Pairs (Count).Distance then
         --  Find insertion point
         declare
            Insert_Pos : Positive := Count;
         begin
            while Insert_Pos > 1 and then Pairs (Insert_Pos - 1).Distance > New_Pair.Distance loop
               Pairs (Insert_Pos) := Pairs (Insert_Pos - 1);
               Insert_Pos := Insert_Pos - 1;
            end loop;
            Pairs (Insert_Pos) := New_Pair;
         end;
      end if;
   end Insert_Pair_If_Smaller;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State) is
   begin
      State :=
        (Part              => AoC_Common.Part_1,
         Boxes             => [others => (X => 0, Y => 0, Z => 0)],
         Box_Count         => 0,
         Parent            => [for I in Box_Index => I],
         -- Each box is its own parent
         Rank              => [others => 0],
         Result_Part_1     => 0,
         Result_Part_2     => 0,
         Error_Encountered => False);
   end Initialize;

   ---------------------------------------------------------------------------
   --  Line Processing
   ---------------------------------------------------------------------------

   procedure Process_Line (Line : String; State : in out Solver_State) is
      X_Value, Y_Value, Z_Value : Coordinate_Value := 0;
      Pos                       : Positive;
      End_Pos                   : Natural;
      Success                   : Boolean;
   begin
      if State.Error_Encountered then
         return;
      end if;

      --  Check box limit
      if State.Box_Count >= MAX_BOXES then
         State.Error_Encountered := True;
         return;
      end if;

      --  Skip empty lines
      if Line'Length = 0 then
         return;
      end if;

      Pos := Line'First;

      --  Parse X coordinate
      Parse_Number (Line, Pos, X_Value, End_Pos, Success);
      if not Success then
         State.Error_Encountered := True;
         return;
      end if;

      --  Skip comma
      Pos := End_Pos;
      if Pos > Line'Last or else Line (Pos) /= ',' then
         State.Error_Encountered := True;
         return;
      end if;
      Pos := Pos + 1;

      --  Parse Y coordinate
      if Pos > Line'Last then
         State.Error_Encountered := True;
         return;
      end if;
      Parse_Number (Line, Pos, Y_Value, End_Pos, Success);
      if not Success then
         State.Error_Encountered := True;
         return;
      end if;

      --  Skip comma
      Pos := End_Pos;
      if Pos > Line'Last or else Line (Pos) /= ',' then
         State.Error_Encountered := True;
         return;
      end if;
      Pos := Pos + 1;

      --  Parse Z coordinate
      if Pos > Line'Last then
         State.Error_Encountered := True;
         return;
      end if;
      Parse_Number (Line, Pos, Z_Value, End_Pos, Success);
      if not Success then
         State.Error_Encountered := True;
         return;
      end if;

      --  Add box
      State.Box_Count := State.Box_Count + 1;
      State.Boxes (State.Box_Count) := (X => X_Value, Y => Y_Value, Z => Z_Value);
   end Process_Line;

   ---------------------------------------------------------------------------
   --  Helper: Compute Part 1 result (product of top 3 circuit sizes)
   ---------------------------------------------------------------------------

   function Compute_Part_1_Result (Parent : Parent_Array; Box_Count : Natural) return Long_Long_Integer
   with Pre => Box_Count >= 1 and then Box_Count <= MAX_BOXES
   is
      subtype Valid_Box_Index is Positive range 1 .. Box_Count;
      type Size_Array is array (Box_Index) of Natural;
      Circuit_Sizes : Size_Array := [others => 0];
      Top_Sizes     : array (1 .. TOP_CIRCUITS) of Long_Long_Integer := [others => 0];
   begin
      --  Count how many boxes belong to each root
      for I in Valid_Box_Index loop
         declare
            Root : constant Box_Index := Find_Root (Parent, I, Box_Count);
         begin
            if Circuit_Sizes (Root) < Natural'Last then
               Circuit_Sizes (Root) := Circuit_Sizes (Root) + 1;
            end if;
         end;
      end loop;

      --  Find the TOP_CIRCUITS largest sizes
      for I in Valid_Box_Index loop
         declare
            Size : constant Long_Long_Integer := Long_Long_Integer (Circuit_Sizes (I));
         begin
            if Size > Top_Sizes (TOP_CIRCUITS) then
               for K in 1 .. TOP_CIRCUITS loop
                  if Size > Top_Sizes (K) then
                     --  Shift smaller values down
                     for L in reverse K + 1 .. TOP_CIRCUITS loop
                        Top_Sizes (L) := Top_Sizes (L - 1);
                     end loop;
                     Top_Sizes (K) := Size;
                     exit;
                  end if;
               end loop;
            end if;
         end;
      end loop;

      --  Calculate product of top 3 sizes (bounded by Box_Count^3)
      return Top_Sizes (1) * Top_Sizes (2) * Top_Sizes (3);
   end Compute_Part_1_Result;

   ---------------------------------------------------------------------------
   --  Puzzle Solving
   ---------------------------------------------------------------------------

   procedure Solve_Puzzle (State : in out Solver_State) is
      --  Local storage for the smallest pairs (memory efficient)
      Smallest_Pairs : Kept_Pair_Array := [others => (1, 1, Squared_Distance'Last)];
      Pair_Count     : Natural := 0;
      Was_Merged     : Boolean;

      --  Track circuit count and Part 2 answer
      Circuit_Count    : Natural;
      Connections_Made : Natural := 0;
      Part_1_Computed  : Boolean := False;
      Last_Merge_X1    : Coordinate_Value := 0;
      Last_Merge_X2    : Coordinate_Value := 0;
   begin
      if State.Error_Encountered or else State.Box_Count < 2 then
         return;
      end if;

      --  Step 1: Generate all pairs and keep only the smallest MAX_KEPT_PAIRS
      for I in 1 .. State.Box_Count - 1 loop
         pragma Loop_Invariant (I >= 1 and then I < State.Box_Count);
         pragma Loop_Invariant (Pair_Count <= MAX_KEPT_PAIRS);

         for J in I + 1 .. State.Box_Count loop
            pragma Loop_Invariant (J > I and then J <= State.Box_Count);
            pragma Loop_Invariant (Pair_Count <= MAX_KEPT_PAIRS);

            declare
               New_Pair : constant Box_Pair :=
                 (Box_A => I, Box_B => J, Distance => Squared_Euclidean_Distance (State.Boxes (I), State.Boxes (J)));
            begin
               Insert_Pair_If_Smaller (Smallest_Pairs, Pair_Count, New_Pair);
            end;
         end loop;
      end loop;

      --  Step 2: Initialize Union-Find (each box is its own circuit)
      Circuit_Count := State.Box_Count;
      for I in 1 .. State.Box_Count loop
         pragma Loop_Invariant (I >= 1 and then I <= State.Box_Count);
         State.Parent (I) := I;
         State.Rank (I) := 0;
      end loop;

      --  Step 3: Connect pairs in order (already sorted by Insert_Pair_If_Smaller)
      --  Track Part 1 (after 1000 connections) and Part 2 (when all connected)
      for P in 1 .. Pair_Count loop
         pragma Loop_Invariant (P >= 1 and then P <= Pair_Count);
         pragma Loop_Invariant (Connections_Made < P);
         pragma Loop_Invariant (Connections_Made < MAX_KEPT_PAIRS);
         pragma Loop_Invariant (State.Box_Count >= 2 and then State.Box_Count <= MAX_BOXES);
         pragma Loop_Invariant (Circuit_Count >= 1 and then Circuit_Count <= State.Box_Count);

         --  Union the two boxes
         Union_Circuits
           (State.Parent, State.Rank, Smallest_Pairs (P).Box_A, Smallest_Pairs (P).Box_B, State.Box_Count, Was_Merged);

         if Was_Merged then
            --  Track last merge for Part 2
            Last_Merge_X1 := State.Boxes (Smallest_Pairs (P).Box_A).X;
            Last_Merge_X2 := State.Boxes (Smallest_Pairs (P).Box_B).X;

            if Circuit_Count > 1 then
               Circuit_Count := Circuit_Count - 1;
            end if;
         end if;

         Connections_Made := Connections_Made + 1;

         --  Part 1: After 1000 connections, compute circuit sizes
         if Connections_Made = CONNECTIONS_PART_1 and then not Part_1_Computed then
            Part_1_Computed := True;
            State.Result_Part_1 := Compute_Part_1_Result (State.Parent, State.Box_Count);
         end if;

         --  Part 2: Stop when all boxes are in one circuit
         exit when Circuit_Count = 1;
      end loop;

      --  If Part 1 wasn't computed (fewer than 1000 pairs), compute now
      if not Part_1_Computed then
         State.Result_Part_1 := Compute_Part_1_Result (State.Parent, State.Box_Count);
      end if;

      --  Part 2: Product of X coordinates of last merged boxes
      State.Result_Part_2 := Long_Long_Integer (Last_Merge_X1) * Long_Long_Integer (Last_Merge_X2);
   end Solve_Puzzle;

   ---------------------------------------------------------------------------
   --  Solvers
   ---------------------------------------------------------------------------

   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer is
   begin
      if State.Error_Encountered then
         return -1;
      end if;

      return State.Result_Part_1;
   end Solve_Part_1;

   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer is
   begin
      if State.Error_Encountered then
         return -1;
      end if;

      return State.Result_Part_2;
   end Solve_Part_2;

end AoC_2025_Day_08.Solver;
