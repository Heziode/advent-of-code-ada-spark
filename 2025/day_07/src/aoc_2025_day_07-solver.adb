--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_07.Solver
--
--  Algorithm:
--    1. Parse input into a 2D grid, identifying S (start) and ^ (splitters)
--    2. Use BFS to simulate beams: start from S going down
--    3. When a beam hits a splitter, increment split count and spawn 2 beams
--    4. Continue until all beams exit the grid
--    5. Return total split count

pragma Ada_2022;

with AoC_Common;

package body AoC_2025_Day_07.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State) is
   begin
      State :=
        (Part              => AoC_Common.Part_1,
         Grid              => [others => [others => Empty]],
         Row_Count         => 0,
         Column_Count      => 0,
         Start_Column      => 0,
         Split_Count       => 0,
         Timeline_Count    => 0,
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

      --  Update column count (assume all rows have same width)
      if State.Row_Count = 1 then
         if Line'Length > MAX_COLUMNS then
            State.Column_Count := MAX_COLUMNS;
         else
            State.Column_Count := Line'Length;
         end if;
      end if;

      --  Parse each character in the line
      --  Guard against potential overflow in index calculation
      if Line'First > Integer'Last - MAX_COLUMNS then
         State.Error_Encountered := True;
         return;
      end if;

      declare
         Parse_Len : constant Natural := Natural'Min (Line'Length, MAX_COLUMNS);
      begin
         for I in 1 .. Parse_Len loop
            pragma Loop_Invariant (I >= 1 and then I <= Parse_Len);
            pragma Loop_Invariant (State.Row_Count <= MAX_ROWS);
            pragma Loop_Invariant (Line'First <= Integer'Last - MAX_COLUMNS);
            pragma Loop_Invariant (Line'First + I - 1 <= Line'Last);

            declare
               Idx : constant Integer := Line'First + I - 1;
               C   : constant Character := Line (Idx);
            begin
               case C is
                  when 'S' =>
                     State.Grid (State.Row_Count) (I) := Start;
                     State.Start_Column := I;

                  when '^' =>
                     State.Grid (State.Row_Count) (I) := Splitter;

                  when others =>
                     State.Grid (State.Row_Count) (I) := Empty;
               end case;
            end;
         end loop;
      end;
   end Process_Line;

   ---------------------------------------------------------------------------
   --  Beam Simulation
   ---------------------------------------------------------------------------

   procedure Simulate_Beams (State : in out Solver_State) is
      --  Part 1: Track which columns have at least one active beam (boolean)
      type Column_Has_Beam is array (1 .. MAX_COLUMNS) of Boolean;

      --  Part 2: Track timeline counts per column (quantum many-worlds)
      type Timeline_Count_Array is array (1 .. MAX_COLUMNS) of Long_Long_Integer;

      Current_Beams     : Column_Has_Beam := [others => False];
      Next_Beams        : Column_Has_Beam;
      Current_Timelines : Timeline_Count_Array := [others => 0];
      Next_Timelines    : Timeline_Count_Array;
      Total_Splits      : Natural := 0;
      Total_Timelines   : Long_Long_Integer := 0;
   begin
      if State.Error_Encountered
        or else State.Row_Count = 0
        or else State.Column_Count = 0
        or else State.Start_Column = 0
      then
         return;
      end if;

      --  Start with one beam at the start column (row 1 is where S is)
      --  Beams will start propagating from row 2 (below S)
      --  First, find which row S is on
      declare
         Start_Row : Natural := 0;
      begin
         --  Find the row containing S
         for R in 1 .. State.Row_Count loop
            pragma Loop_Invariant (R <= State.Row_Count);
            pragma Loop_Invariant (Start_Row = 0);

            for C in 1 .. State.Column_Count loop
               pragma Loop_Invariant (C <= State.Column_Count);
               if State.Grid (R) (C) = Start then
                  Start_Row := R;
                  exit;
               end if;
            end loop;

            exit when Start_Row > 0;
         end loop;

         if Start_Row = 0 or else Start_Row >= State.Row_Count then
            --  No start found or S is on last row
            return;
         end if;

         --  Initialize: one beam/timeline starting at Start_Column
         Current_Beams (State.Start_Column) := True;
         Current_Timelines (State.Start_Column) := 1;

         --  Process each row from Start_Row + 1 to the end
         for Row in Start_Row + 1 .. State.Row_Count loop
            pragma Loop_Invariant (Row > Start_Row and then Row <= State.Row_Count);
            pragma Loop_Invariant (Total_Splits <= MAX_ROWS * MAX_COLUMNS);

            --  Reset next row arrays
            Next_Beams := [others => False];
            Next_Timelines := [others => 0];

            --  Process each column with active beams
            for Col in 1 .. State.Column_Count loop
               pragma Loop_Invariant (Col >= 1 and then Col <= MAX_COLUMNS);
               pragma Loop_Invariant (Total_Splits <= MAX_ROWS * MAX_COLUMNS);

               if Current_Beams (Col) and then Current_Timelines (Col) >= 0 then
                  --  There is a beam in this column going down
                  declare
                     Timeline_Cnt : constant Long_Long_Integer := Current_Timelines (Col);
                  begin
                     pragma Assert (Timeline_Cnt >= 0);

                     case State.Grid (Row) (Col) is
                        when Empty | Start =>
                           --  Beam passes through, continue to next row
                           Next_Beams (Col) := True;
                           --  Timelines continue unchanged
                           if Next_Timelines (Col) >= 0
                             and then Next_Timelines (Col) <= Long_Long_Integer'Last - Timeline_Cnt
                           then
                              Next_Timelines (Col) := Next_Timelines (Col) + Timeline_Cnt;
                           end if;

                        when Splitter =>
                           --  Part 1: Beam hits splitter, count one split
                           if Total_Splits < MAX_ROWS * MAX_COLUMNS then
                              Total_Splits := Total_Splits + 1;
                           end if;

                           --  Part 2: Each timeline splits into 2 (left and right)
                           --  Spawn beam/timelines to the left (Col - 1)
                           if Col > 1 then
                              Next_Beams (Col - 1) := True;
                              if Next_Timelines (Col - 1) >= 0
                                and then Next_Timelines (Col - 1) <= Long_Long_Integer'Last - Timeline_Cnt
                              then
                                 Next_Timelines (Col - 1) := Next_Timelines (Col - 1) + Timeline_Cnt;
                              end if;
                           else
                              --  Exits left edge: add to total timelines
                              if Total_Timelines <= Long_Long_Integer'Last - Timeline_Cnt then
                                 Total_Timelines := Total_Timelines + Timeline_Cnt;
                              end if;
                           end if;

                           --  Spawn beam/timelines to the right (Col + 1)
                           if Col < State.Column_Count then
                              Next_Beams (Col + 1) := True;
                              if Next_Timelines (Col + 1) >= 0
                                and then Next_Timelines (Col + 1) <= Long_Long_Integer'Last - Timeline_Cnt
                              then
                                 Next_Timelines (Col + 1) := Next_Timelines (Col + 1) + Timeline_Cnt;
                              end if;
                           else
                              --  Exits right edge: add to total timelines
                              if Total_Timelines <= Long_Long_Integer'Last - Timeline_Cnt then
                                 Total_Timelines := Total_Timelines + Timeline_Cnt;
                              end if;
                           end if;
                     end case;
                  end;
               end if;
            end loop;

            --  Move to next row
            Current_Beams := Next_Beams;
            Current_Timelines := Next_Timelines;
         end loop;

         --  Count timelines that exited at the bottom (still active after last row)
         for Col in 1 .. State.Column_Count loop
            pragma Loop_Invariant (Col >= 1);
            pragma Loop_Invariant (State.Column_Count <= MAX_COLUMNS);

            declare
               Exit_Count : constant Long_Long_Integer := Current_Timelines (Col);
            begin
               if Current_Beams (Col)
                 and then Exit_Count > 0
                 and then Total_Timelines >= 0
                 and then Total_Timelines <= Long_Long_Integer'Last - Exit_Count
               then
                  Total_Timelines := Total_Timelines + Exit_Count;
               end if;
            end;
         end loop;
      end;

      State.Split_Count := Total_Splits;
      State.Timeline_Count := Total_Timelines;
   end Simulate_Beams;

   ---------------------------------------------------------------------------
   --  Solvers
   ---------------------------------------------------------------------------

   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer is
   begin
      if State.Error_Encountered then
         return -1;
      end if;

      return Long_Long_Integer (State.Split_Count);
   end Solve_Part_1;

   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer is
   begin
      if State.Error_Encountered then
         return -1;
      end if;

      return State.Timeline_Count;
   end Solve_Part_2;

end AoC_2025_Day_07.Solver;
