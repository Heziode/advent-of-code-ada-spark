--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_11.Solver
--
--  Implementation of the path counting algorithm using memoized DFS.

pragma Ada_2022;

package body AoC_2025_Day_11.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Pad a string to Node_Name length
   function To_Node_Name (S : String) return Node_Name is
      Result : Node_Name := BLANK_NAME;
   begin
      if S'Length > 0 and then S'Length <= MAX_NAME_LENGTH then
         Result (1 .. S'Length) := S;
      end if;
      return Result;
   end To_Node_Name;

   --  Check if two node names are equal (ignoring trailing spaces)
   function Is_Names_Equal (A, B : Node_Name) return Boolean is
      A_Len : Natural := 0;
      B_Len : Natural := 0;
   begin
      --  Find actual length of A
      for I in reverse A'Range loop
         if A (I) /= ' ' then
            A_Len := I;
            exit;
         end if;
      end loop;

      --  Find actual length of B
      for I in reverse B'Range loop
         if B (I) /= ' ' then
            B_Len := I;
            exit;
         end if;
      end loop;

      if A_Len /= B_Len then
         return False;
      end if;

      if A_Len = 0 then
         return True;
      end if;

      return A (1 .. A_Len) = B (1 .. B_Len);
   end Is_Names_Equal;

   --  Find or create a node by name, returns the node index
   procedure Find_Or_Create_Node
     (State : in out Solver_State; Name : Node_Name; Index : out Node_Index; Found : out Boolean)
   with Pre => State.Node_Count < MAX_NODES
   is
   begin
      --  Search for existing node
      for I in 0 .. State.Node_Count - 1 loop
         if Is_Names_Equal (State.Nodes (I).Name, Name) then
            Index := I;
            Found := True;
            return;
         end if;
         pragma Loop_Invariant (I < State.Node_Count);
      end loop;

      --  Create new node
      Index := State.Node_Count;
      State.Nodes (Index).Name := Name;
      State.Nodes (Index).Edge_Count := 0;
      State.Nodes (Index).Path_Count := -1;
      State.Node_Count := State.Node_Count + 1;
      Found := False;
   end Find_Or_Create_Node;

   --  Parse a single word from a string starting at position Pos
   procedure Parse_Word (Line : String; Pos : in out Natural; Word : out Node_Name; Has_Word : out Boolean) is
      Start_Pos : Natural;
      End_Pos   : Natural;
   begin
      Word := BLANK_NAME;
      Has_Word := False;

      if Pos > Line'Last then
         return;
      end if;

      --  Skip whitespace
      while Pos <= Line'Last and then Line (Pos) = ' ' loop
         if Pos < Line'Last then
            Pos := Pos + 1;
         else
            Pos := Pos + 1;
            exit;
         end if;
         pragma Loop_Invariant (Pos >= Line'First);
      end loop;

      if Pos > Line'Last then
         return;
      end if;

      Start_Pos := Pos;

      --  Find end of word
      while Pos <= Line'Last and then Line (Pos) /= ' ' and then Line (Pos) /= ':' loop
         if Pos < Line'Last then
            Pos := Pos + 1;
         else
            Pos := Pos + 1;
            exit;
         end if;
         pragma Loop_Invariant (Pos >= Line'First);
      end loop;

      End_Pos := Pos - 1;

      if End_Pos >= Start_Pos then
         declare
            Word_Len : constant Natural := End_Pos - Start_Pos + 1;
         begin
            if Word_Len <= MAX_NAME_LENGTH then
               Word (1 .. Word_Len) := Line (Start_Pos .. End_Pos);
               Has_Word := True;
            end if;
         end;
      end if;
   end Parse_Word;

   --  Skip the colon separator
   procedure Skip_Colon (Line : String; Pos : in out Natural) is
   begin
      while Pos <= Line'Last and then Line (Pos) = ' ' loop
         if Pos < Line'Last then
            Pos := Pos + 1;
         else
            Pos := Pos + 1;
            exit;
         end if;
         pragma Loop_Invariant (Pos >= Line'First);
      end loop;

      if Pos <= Line'Last and then Line (Pos) = ':' then
         Pos := Pos + 1;
      end if;
   end Skip_Colon;

   --  Count paths from a node to the end node using memoization
   procedure Count_Paths_From (State : in out Solver_State; Node_Idx : Node_Index; Result : out Long_Long_Integer) is
      Child_Count : Long_Long_Integer;
      Total       : Long_Long_Integer := 0;
   begin
      --  Base case: reached the end
      if Node_Idx = State.End_Node then
         Result := 1;
         return;
      end if;

      --  Check memoization
      if State.Nodes (Node_Idx).Path_Count >= 0 then
         Result := State.Nodes (Node_Idx).Path_Count;
         return;
      end if;

      --  No outgoing edges means no path to end
      if State.Nodes (Node_Idx).Edge_Count = 0 then
         State.Nodes (Node_Idx).Path_Count := 0;
         Result := 0;
         return;
      end if;

      --  Sum paths through all children
      declare
         Edge_Count : constant Natural := State.Nodes (Node_Idx).Edge_Count;
      begin
         for I in 1 .. Edge_Count loop
            pragma Loop_Invariant (Total >= 0);
            pragma Loop_Invariant (I <= Edge_Count);
            if I in State.Nodes (Node_Idx).Edges'Range then
               declare
                  Child_Idx : constant Node_Index := State.Nodes (Node_Idx).Edges (I);
               begin
                  if Child_Idx < State.Node_Count then
                     Count_Paths_From (State, Child_Idx, Child_Count);
                     if Child_Count >= 0 and then Total <= Long_Long_Integer'Last - Child_Count then
                        Total := Total + Child_Count;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end;

      State.Nodes (Node_Idx).Path_Count := Total;
      Result := Total;
   end Count_Paths_From;

   --  Count paths with state tracking (4 states: visited dac/fft combinations)
   --  State encoding: 0=none, 1=dac only, 2=fft only, 3=both
   type Visit_State is mod 4;
   type State_Memo_Array is array (Node_Index, Visit_State) of Long_Long_Integer;

   procedure Count_Paths_With_State
     (State     : Solver_State;
      Memo      : in out State_Memo_Array;
      Node_Idx  : Node_Index;
      Vis_State : Visit_State;
      Result    : out Long_Long_Integer)
   is
      New_State   : Visit_State := Vis_State;
      Child_Count : Long_Long_Integer;
      Total       : Long_Long_Integer := 0;
   begin
      --  Update state based on current node
      if Node_Idx = State.Dac_Node then
         New_State := New_State or 1;
      end if;
      if Node_Idx = State.Fft_Node then
         New_State := New_State or 2;
      end if;

      --  Base case: reached the end
      if Node_Idx = State.End_Node then
         if New_State = 3 then
            Result := 1;
         else
            Result := 0;
         end if;
         return;
      end if;

      --  Check memoization
      if Memo (Node_Idx, New_State) >= 0 then
         Result := Memo (Node_Idx, New_State);
         return;
      end if;

      --  No outgoing edges
      if State.Nodes (Node_Idx).Edge_Count = 0 then
         Memo (Node_Idx, New_State) := 0;
         Result := 0;
         return;
      end if;

      --  Sum paths through all children
      declare
         Edge_Count : constant Natural := State.Nodes (Node_Idx).Edge_Count;
      begin
         for I in 1 .. Edge_Count loop
            pragma Loop_Invariant (Total >= 0);
            pragma Loop_Invariant (I <= Edge_Count);
            if I in State.Nodes (Node_Idx).Edges'Range then
               declare
                  Child_Idx : constant Node_Index := State.Nodes (Node_Idx).Edges (I);
               begin
                  if Child_Idx < State.Node_Count then
                     Count_Paths_With_State (State, Memo, Child_Idx, New_State, Child_Count);
                     if Child_Count >= 0 and then Total <= Long_Long_Integer'Last - Child_Count then
                        Total := Total + Child_Count;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end;

      Memo (Node_Idx, New_State) := Total;
      Result := Total;
   end Count_Paths_With_State;

   ---------------------------------------------------------------------------
   --  Public Interface
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State) is
   begin
      State :=
        (Part              => AoC_Common.Part_1,
         Nodes             => [others => (BLANK_NAME, [others => 0], 0, -1)],
         Node_Count        => 0,
         Start_Node        => 0,
         End_Node          => 0,
         Svr_Node          => 0,
         Dac_Node          => 0,
         Fft_Node          => 0,
         Error_Encountered => False,
         Result_Part_1     => 0,
         Result_Part_2     => 0);
   end Initialize;

   procedure Process_Line (Line : String; State : in out Solver_State) with SPARK_Mode => Off is
      Source_Name : Node_Name;
      Target_Name : Node_Name;
      Source_Idx  : Node_Index;
      Target_Idx  : Node_Index;
      Has_Word    : Boolean;
      Found       : Boolean;
      Pos         : Natural;
      You_Name    : constant Node_Name := To_Node_Name ("you");
      Out_Name    : constant Node_Name := To_Node_Name ("out");
      Svr_Name    : constant Node_Name := To_Node_Name ("svr");
      Dac_Name    : constant Node_Name := To_Node_Name ("dac");
      Fft_Name    : constant Node_Name := To_Node_Name ("fft");

      procedure Check_Special_Node (Name : Node_Name; Idx : Node_Index) is
      begin
         if Is_Names_Equal (Name, You_Name) then
            State.Start_Node := Idx;
         elsif Is_Names_Equal (Name, Out_Name) then
            State.End_Node := Idx;
         elsif Is_Names_Equal (Name, Svr_Name) then
            State.Svr_Node := Idx;
         elsif Is_Names_Equal (Name, Dac_Name) then
            State.Dac_Node := Idx;
         elsif Is_Names_Equal (Name, Fft_Name) then
            State.Fft_Node := Idx;
         end if;
      end Check_Special_Node;
   begin
      if State.Error_Encountered or else Line'Length = 0 then
         return;
      end if;

      if State.Node_Count >= MAX_NODES - MAX_EDGES - 1 then
         State.Error_Encountered := True;
         return;
      end if;

      Pos := Line'First;

      --  Parse source node name
      Parse_Word (Line, Pos, Source_Name, Has_Word);

      if not Has_Word then
         return;
      end if;

      Find_Or_Create_Node (State, Source_Name, Source_Idx, Found);
      Check_Special_Node (Source_Name, Source_Idx);

      --  Skip colon
      Skip_Colon (Line, Pos);

      --  Parse target nodes
      loop
         exit when Pos > Line'Last;
         exit when State.Node_Count >= MAX_NODES - 1;
         exit when State.Nodes (Source_Idx).Edge_Count >= MAX_EDGES;

         Parse_Word (Line, Pos, Target_Name, Has_Word);
         exit when not Has_Word;

         Find_Or_Create_Node (State, Target_Name, Target_Idx, Found);
         Check_Special_Node (Target_Name, Target_Idx);

         --  Add edge
         State.Nodes (Source_Idx).Edge_Count := State.Nodes (Source_Idx).Edge_Count + 1;
         State.Nodes (Source_Idx).Edges (State.Nodes (Source_Idx).Edge_Count) := Target_Idx;
      end loop;
   end Process_Line;

   procedure Solve_Puzzle (State : in out Solver_State) is
      Path_Count : Long_Long_Integer;
      Memo       : State_Memo_Array := [others => [others => -1]];
   begin
      if State.Node_Count = 0 then
         State.Result_Part_1 := 0;
         State.Result_Part_2 := 0;
         return;
      end if;

      --  Part 1: Count all paths from "you" to "out"
      Count_Paths_From (State, State.Start_Node, Path_Count);
      State.Result_Part_1 := Path_Count;

      --  Part 2: Count paths from "svr" to "out" visiting both "dac" and "fft"
      Count_Paths_With_State (State, Memo, State.Svr_Node, 0, Path_Count);
      State.Result_Part_2 := Path_Count;
   end Solve_Puzzle;

end AoC_2025_Day_11.Solver;
