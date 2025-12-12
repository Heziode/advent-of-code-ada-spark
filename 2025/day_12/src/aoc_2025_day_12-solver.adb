--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_12.Solver
--
--  Implementation of the polyomino packing solver using backtracking.

pragma Ada_2022;

package body AoC_2025_Day_12.Solver with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Check if a character is a digit
   function Is_Digit (C : Character) return Boolean
   is (C in '0' .. '9');

   --  Convert digit character to value
   function Digit_Value (C : Character) return Natural
   is (Character'Pos (C) - Character'Pos ('0'))
   with Pre => Is_Digit (C);

   --  Parse a natural number from string starting at position Pos
   procedure Parse_Natural_At
     (S       : String;
      Pos     : in out Natural;
      Value   : out Natural;
      Success : out Boolean)
   with
     Pre => Pos <= S'Last + 1
   is
      Result : Natural := 0;
      Started : Boolean := False;
   begin
      Value := 0;
      Success := False;

      --  Skip whitespace
      while Pos <= S'Last and then S (Pos) = ' ' loop
         Pos := Pos + 1;
      end loop;

      --  Parse digits
      while Pos <= S'Last and then Is_Digit (S (Pos)) loop
         Started := True;
         if Result <= (Natural'Last - Digit_Value (S (Pos))) / 10 then
            Result := Result * 10 + Digit_Value (S (Pos));
         end if;
         Pos := Pos + 1;
      end loop;

      if Started then
         Value := Result;
         Success := True;
      end if;
   end Parse_Natural_At;

   --  Normalize a shape orientation to have minimum row/col at 0
   procedure Normalize_Orientation (Orient : in out Shape_Orientation) is
      Min_Row : Integer := Integer'Last;
      Min_Col : Integer := Integer'Last;
      Max_Row : Integer := Integer'First;
      Max_Col : Integer := Integer'First;
   begin
      if Orient.Cell_Count = 0 then
         Orient.Width := 0;
         Orient.Height := 0;
         return;
      end if;

      --  Find bounds
      for I in 0 .. Orient.Cell_Count - 1 loop
         pragma Loop_Invariant (I < Orient.Cell_Count);
         if Orient.Cells (I).Row < Min_Row then
            Min_Row := Orient.Cells (I).Row;
         end if;
         if Orient.Cells (I).Row > Max_Row then
            Max_Row := Orient.Cells (I).Row;
         end if;
         if Orient.Cells (I).Col < Min_Col then
            Min_Col := Orient.Cells (I).Col;
         end if;
         if Orient.Cells (I).Col > Max_Col then
            Max_Col := Orient.Cells (I).Col;
         end if;
      end loop;

      --  Shift to origin
      for I in 0 .. Orient.Cell_Count - 1 loop
         pragma Loop_Invariant (I < Orient.Cell_Count);
         Orient.Cells (I).Row := Orient.Cells (I).Row - Min_Row;
         Orient.Cells (I).Col := Orient.Cells (I).Col - Min_Col;
      end loop;

      Orient.Width := Max_Col - Min_Col + 1;
      Orient.Height := Max_Row - Min_Row + 1;
   end Normalize_Orientation;

   --  Rotate shape 90 degrees clockwise: (r, c) -> (c, -r)
   function Rotate_90 (Orient : Shape_Orientation) return Shape_Orientation is
      Result : Shape_Orientation;
   begin
      Result.Cell_Count := Orient.Cell_Count;
      for I in 0 .. Orient.Cell_Count - 1 loop
         pragma Loop_Invariant (I < Orient.Cell_Count);
         pragma Loop_Invariant (Result.Cell_Count = Orient.Cell_Count);
         Result.Cells (I).Row := Orient.Cells (I).Col;
         Result.Cells (I).Col := -Orient.Cells (I).Row;
      end loop;
      Normalize_Orientation (Result);
      return Result;
   end Rotate_90;

   --  Flip shape horizontally: (r, c) -> (r, -c)
   function Flip_Horizontal (Orient : Shape_Orientation) return Shape_Orientation is
      Result : Shape_Orientation;
   begin
      Result.Cell_Count := Orient.Cell_Count;
      for I in 0 .. Orient.Cell_Count - 1 loop
         pragma Loop_Invariant (I < Orient.Cell_Count);
         pragma Loop_Invariant (Result.Cell_Count = Orient.Cell_Count);
         Result.Cells (I).Row := Orient.Cells (I).Row;
         Result.Cells (I).Col := -Orient.Cells (I).Col;
      end loop;
      Normalize_Orientation (Result);
      return Result;
   end Flip_Horizontal;

   --  Check if two orientations are equivalent (same cells after sorting)
   function Is_Orientations_Equal (A, B : Shape_Orientation) return Boolean is
   begin
      if A.Cell_Count /= B.Cell_Count then
         return False;
      end if;
      if A.Width /= B.Width or else A.Height /= B.Height then
         return False;
      end if;

      --  Check if all cells in A are in B
      for I in 0 .. A.Cell_Count - 1 loop
         pragma Loop_Invariant (I < A.Cell_Count);
         declare
            Found : Boolean := False;
         begin
            for J in 0 .. B.Cell_Count - 1 loop
               pragma Loop_Invariant (J < B.Cell_Count);
               if A.Cells (I).Row = B.Cells (J).Row
                 and then A.Cells (I).Col = B.Cells (J).Col
               then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Is_Orientations_Equal;

   --  Generate all unique orientations for a shape
   procedure Generate_Orientations (Shape : in out Shape_Definition) is
      Base     : constant Shape_Orientation := Shape.Orientations (0);
      Current  : Shape_Orientation;
      Flipped  : Shape_Orientation;
      Is_Unique : Boolean;
   begin
      Shape.Orientation_Count := 1;

      --  Generate 4 rotations of base shape
      Current := Base;
      for Rot in 1 .. 3 loop
         pragma Loop_Invariant (Shape.Orientation_Count <= 4);
         Current := Rotate_90 (Current);
         Is_Unique := True;
         for I in 0 .. Shape.Orientation_Count - 1 loop
            pragma Loop_Invariant (I < Shape.Orientation_Count);
            if Is_Orientations_Equal (Current, Shape.Orientations (I)) then
               Is_Unique := False;
               exit;
            end if;
         end loop;
         if Is_Unique and then Shape.Orientation_Count < MAX_ORIENTATIONS then
            Shape.Orientations (Shape.Orientation_Count) := Current;
            Shape.Orientation_Count := Shape.Orientation_Count + 1;
         end if;
      end loop;

      --  Generate flipped version and its 4 rotations
      Flipped := Flip_Horizontal (Base);
      Current := Flipped;

      for Rot in 0 .. 3 loop
         pragma Loop_Invariant (Shape.Orientation_Count <= MAX_ORIENTATIONS);
         if Rot > 0 then
            Current := Rotate_90 (Current);
         end if;
         Is_Unique := True;
         for I in 0 .. Shape.Orientation_Count - 1 loop
            pragma Loop_Invariant (I < Shape.Orientation_Count);
            if Is_Orientations_Equal (Current, Shape.Orientations (I)) then
               Is_Unique := False;
               exit;
            end if;
         end loop;
         if Is_Unique and then Shape.Orientation_Count < MAX_ORIENTATIONS then
            Shape.Orientations (Shape.Orientation_Count) := Current;
            Shape.Orientation_Count := Shape.Orientation_Count + 1;
         end if;
      end loop;
   end Generate_Orientations;

   --  Convert temp grid to shape orientation
   procedure Grid_To_Orientation
     (Grid   : Placement_Grid;
      Width  : Natural;
      Height : Natural;
      Orient : out Shape_Orientation)
   is
      Count : Natural := 0;
   begin
      Orient := (others => <>);

      for R in 0 .. Height - 1 loop
         pragma Loop_Invariant (Count <= R * MAX_SHAPE_SIZE + MAX_SHAPE_SIZE);
         exit when R >= MAX_SHAPE_SIZE;
         for C in 0 .. Width - 1 loop
            pragma Loop_Invariant (Count <= R * MAX_SHAPE_SIZE + C + 1);
            exit when C >= MAX_SHAPE_SIZE;
            if Grid (R) (C) and then Count < MAX_CELLS_PER_SHAPE then
               Orient.Cells (Count) := (Row => R, Col => C);
               Count := Count + 1;
            end if;
         end loop;
      end loop;

      Orient.Cell_Count := Count;
      Orient.Width := Width;
      Orient.Height := Height;
      Normalize_Orientation (Orient);
   end Grid_To_Orientation;

   --  Finalize current shape being parsed
   procedure Finalize_Shape (State : in out Solver_State) is
      Orient : Shape_Orientation;
   begin
      if not State.Parsing_Shape then
         return;
      end if;

      if State.Current_Shape_Idx < MAX_SHAPES
        and then State.Temp_Height > 0
        and then State.Temp_Width > 0
      then
         Grid_To_Orientation
           (State.Temp_Grid, State.Temp_Width, State.Temp_Height, Orient);

         if Orient.Cell_Count > 0 then
            State.Shapes (State.Current_Shape_Idx).Orientations (0) := Orient;
            State.Shapes (State.Current_Shape_Idx).Orientation_Count := 1;
            Generate_Orientations (State.Shapes (State.Current_Shape_Idx));

            if State.Current_Shape_Idx >= State.Shape_Count then
               State.Shape_Count := State.Current_Shape_Idx + 1;
            end if;
         end if;
      end if;

      State.Parsing_Shape := False;
      State.Temp_Grid := [others => [others => False]];
      State.Temp_Width := 0;
      State.Temp_Height := 0;
      State.Current_Shape_Row := 0;
   end Finalize_Shape;

   --  Parse shape definition line (e.g., "###" or "##.")
   procedure Parse_Shape_Line (Line : String; State : in out Solver_State) is
      Row : constant Natural := State.Current_Shape_Row;
   begin
      if Row >= MAX_SHAPE_SIZE or else Line'Length > MAX_SHAPE_SIZE then
         return;
      end if;

      for I in Line'Range loop
         pragma Loop_Invariant (I >= Line'First);
         declare
            Col : constant Natural := I - Line'First;
         begin
            exit when Col >= MAX_SHAPE_SIZE;
            if Line (I) = '#' then
               State.Temp_Grid (Row) (Col) := True;
            end if;
            if Col + 1 > State.Temp_Width then
               State.Temp_Width := Col + 1;
            end if;
         end;
      end loop;

      State.Current_Shape_Row := Row + 1;
      if State.Current_Shape_Row > State.Temp_Height then
         State.Temp_Height := State.Current_Shape_Row;
      end if;
   end Parse_Shape_Line;

   --  Parse region specification line (e.g., "4x4: 0 0 0 0 2 0")
   procedure Parse_Region_Line (Line : String; State : in out Solver_State) is
      Pos       : Natural := Line'First;
      Width_Val : Natural;
      Height_Val : Natural;
      Quantity  : Natural;
      Success   : Boolean;
      X_Pos     : Natural := 0;
      Colon_Pos : Natural := 0;
      Shape_Idx : Natural := 0;
   begin
      if State.Region_Count >= MAX_REGIONS then
         return;
      end if;

      --  Find 'x' position for dimensions
      for I in Line'Range loop
         if Line (I) = 'x' or else Line (I) = 'X' then
            X_Pos := I;
            exit;
         end if;
      end loop;

      if X_Pos = 0 then
         return;
      end if;

      --  Find ':' position
      for I in X_Pos .. Line'Last loop
         if Line (I) = ':' then
            Colon_Pos := I;
            exit;
         end if;
      end loop;

      if Colon_Pos = 0 then
         return;
      end if;

      --  Parse width
      Pos := Line'First;
      Parse_Natural_At (Line (Line'First .. X_Pos - 1), Pos, Width_Val, Success);
      if not Success or else Width_Val = 0 or else Width_Val > MAX_REGION_WIDTH then
         return;
      end if;

      --  Parse height
      Pos := X_Pos + 1;
      Parse_Natural_At (Line (X_Pos + 1 .. Colon_Pos - 1), Pos, Height_Val, Success);
      if not Success or else Height_Val = 0 or else Height_Val > MAX_REGION_HEIGHT then
         return;
      end if;

      --  Parse quantities
      State.Regions (State.Region_Count).Width := Width_Val;
      State.Regions (State.Region_Count).Height := Height_Val;
      State.Regions (State.Region_Count).Quantities := [others => 0];

      Pos := Colon_Pos + 1;
      Shape_Idx := 0;
      while Pos <= Line'Last and then Shape_Idx < MAX_SHAPES loop
         pragma Loop_Invariant (Shape_Idx < MAX_SHAPES);
         Parse_Natural_At (Line, Pos, Quantity, Success);
         exit when not Success;
         if Quantity <= MAX_PIECES_PER_SHAPE then
            State.Regions (State.Region_Count).Quantities (Shape_Idx) := Quantity;
         end if;
         Shape_Idx := Shape_Idx + 1;
      end loop;

      State.Region_Count := State.Region_Count + 1;
   end Parse_Region_Line;

   ---------------------------------------------------------------------------
   --  Backtracking Solver
   ---------------------------------------------------------------------------

   --  Check if we can place a shape orientation at position (Row, Col)
   function Can_Place
     (Grid   : Placement_Grid;
      Orient : Shape_Orientation;
      Row    : Natural;
      Col    : Natural;
      Width  : Natural;
      Height : Natural) return Boolean
   is
   begin
      for I in 0 .. Orient.Cell_Count - 1 loop
         pragma Loop_Invariant (I < Orient.Cell_Count);
         declare
            Cell_Row : constant Integer := Row + Orient.Cells (I).Row;
            Cell_Col : constant Integer := Col + Orient.Cells (I).Col;
         begin
            if Cell_Row < 0 or else Cell_Row >= Height
              or else Cell_Col < 0 or else Cell_Col >= Width
            then
               return False;
            end if;
            if Grid (Cell_Row) (Cell_Col) then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Can_Place;

   --  Place a shape on the grid
   procedure Place_Shape
     (Grid   : in out Placement_Grid;
      Orient : Shape_Orientation;
      Row    : Natural;
      Col    : Natural)
   is
   begin
      for I in 0 .. Orient.Cell_Count - 1 loop
         pragma Loop_Invariant (I < Orient.Cell_Count);
         declare
            Cell_Row : constant Natural := Row + Orient.Cells (I).Row;
            Cell_Col : constant Natural := Col + Orient.Cells (I).Col;
         begin
            if Cell_Row < MAX_REGION_HEIGHT and then Cell_Col < MAX_REGION_WIDTH then
               Grid (Cell_Row) (Cell_Col) := True;
            end if;
         end;
      end loop;
   end Place_Shape;

   --  Remove a shape from the grid
   procedure Remove_Shape
     (Grid   : in out Placement_Grid;
      Orient : Shape_Orientation;
      Row    : Natural;
      Col    : Natural)
   is
   begin
      for I in 0 .. Orient.Cell_Count - 1 loop
         pragma Loop_Invariant (I < Orient.Cell_Count);
         declare
            Cell_Row : constant Natural := Row + Orient.Cells (I).Row;
            Cell_Col : constant Natural := Col + Orient.Cells (I).Col;
         begin
            if Cell_Row < MAX_REGION_HEIGHT and then Cell_Col < MAX_REGION_WIDTH then
               Grid (Cell_Row) (Cell_Col) := False;
            end if;
         end;
      end loop;
   end Remove_Shape;

   --  Build list of pieces to place (shape index, count)
   type Piece_Entry is record
      Shape_Idx : Shape_Index := 0;
      Remaining : Natural := 0;
   end record;

   MAX_PIECE_TYPES : constant := MAX_SHAPES;
   type Piece_List is array (0 .. MAX_PIECE_TYPES - 1) of Piece_Entry;

   --  Check if all pieces are placed
   function Is_All_Placed (Pieces : Piece_List) return Boolean is
   begin
      for I in Pieces'Range loop
         if Pieces (I).Remaining > 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_All_Placed;

   --  Recursive backtracking solver - try each piece type
   procedure Solve_Backtrack
     (Shapes  : Shape_Array;
      Grid    : in out Placement_Grid;
      Pieces  : in out Piece_List;
      Width   : Natural;
      Height  : Natural;
      Success : out Boolean)
   is
      Recursive_Success : Boolean;
   begin
      Success := False;

      --  All pieces placed successfully
      if Is_All_Placed (Pieces) then
         Success := True;
         return;
      end if;

      --  Try each piece type that has remaining pieces
      for Piece_Idx in Pieces'Range loop
         if Pieces (Piece_Idx).Remaining > 0 then
            declare
               Shape_Idx : constant Shape_Index := Pieces (Piece_Idx).Shape_Idx;
               Shape     : Shape_Definition renames Shapes (Shape_Idx);
            begin
               --  Try each orientation
               for Orient_Idx in 0 .. Shape.Orientation_Count - 1 loop
                  pragma Loop_Invariant (Orient_Idx < Shape.Orientation_Count);
                  declare
                     Orient : Shape_Orientation renames
                       Shape.Orientations (Orient_Idx);
                  begin
                     --  Try each position
                     for Row in 0 .. Height - 1 loop
                        pragma Loop_Invariant (Row < Height);
                        exit when Row + Orient.Height > Height;
                        for Col in 0 .. Width - 1 loop
                           pragma Loop_Invariant (Col < Width);
                           exit when Col + Orient.Width > Width;

                           if Can_Place (Grid, Orient, Row, Col, Width, Height)
                           then
                              Place_Shape (Grid, Orient, Row, Col);
                              Pieces (Piece_Idx).Remaining :=
                                Pieces (Piece_Idx).Remaining - 1;

                              Solve_Backtrack
                                (Shapes, Grid, Pieces, Width, Height,
                                 Recursive_Success);
                              if Recursive_Success then
                                 Success := True;
                                 return;
                              end if;

                              --  Backtrack
                              Remove_Shape (Grid, Orient, Row, Col);
                              Pieces (Piece_Idx).Remaining :=
                                Pieces (Piece_Idx).Remaining + 1;
                           end if;
                        end loop;
                     end loop;
                  end;
               end loop;
            end;

            --  Important: only try the first piece type with remaining > 0
            --  to avoid redundant exploration (all orderings covered by recursion)
            exit;
         end if;
      end loop;
   end Solve_Backtrack;

   --  Check if a region can fit all its pieces
   function Can_Fit_Region
     (Shapes : Shape_Array;
      Region : Region_Spec) return Boolean
   is
      Total_Cells  : Natural := 0;
      Region_Area  : constant Natural := Region.Width * Region.Height;
      Grid         : Placement_Grid := [others => [others => False]];
      Pieces       : Piece_List := [others => (0, 0)];
      Piece_Count  : Natural := 0;
      Fit_Success  : Boolean;
   begin
      --  Calculate total cells needed and build piece list
      for I in Region.Quantities'Range loop
         if Region.Quantities (I) > 0 then
            declare
               Shape_Cells : constant Natural :=
                 Shapes (I).Orientations (0).Cell_Count;
            begin
               if Total_Cells <= Natural'Last - Region.Quantities (I) * Shape_Cells
               then
                  Total_Cells := Total_Cells + Region.Quantities (I) * Shape_Cells;
               end if;

               if Piece_Count < MAX_PIECE_TYPES then
                  Pieces (Piece_Count) := (Shape_Idx => I,
                                           Remaining => Region.Quantities (I));
                  Piece_Count := Piece_Count + 1;
               end if;
            end;
         end if;
      end loop;

      --  Quick check: if total cells > area, can't possibly fit
      if Total_Cells > Region_Area then
         return False;
      end if;

      --  No pieces to place = trivially fits
      if Piece_Count = 0 then
         return True;
      end if;

      --  Use backtracking to verify actual placement
      Solve_Backtrack
        (Shapes, Grid, Pieces, Region.Width, Region.Height, Fit_Success);
      return Fit_Success;
   end Can_Fit_Region;

   ---------------------------------------------------------------------------
   --  Public Interface
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State) is
   begin
      State := (Part              => AoC_Common.Part_1,
                Shapes            => [others => (others => <>)],
                Shape_Count       => 0,
                Regions           => [others => (others => <>)],
                Region_Count      => 0,
                Parsing_Shape     => False,
                Current_Shape_Idx => 0,
                Current_Shape_Row => 0,
                Temp_Grid         => [others => [others => False]],
                Temp_Width        => 0,
                Temp_Height       => 0,
                Result_Part_1     => 0,
                Result_Part_2     => 0);
   end Initialize;

   procedure Process_Line (Line : String; State : in out Solver_State) is
      Pos     : Natural;
      Idx_Val : Natural;
      Success : Boolean;
   begin
      if Line'Length = 0 then
         --  Empty line: finalize current shape if parsing
         Finalize_Shape (State);
         return;
      end if;

      --  Check for shape definition start (e.g., "0:")
      Pos := Line'First;
      Parse_Natural_At (Line, Pos, Idx_Val, Success);

      if Success and then Pos <= Line'Last and then Line (Pos) = ':' then
         --  Finalize previous shape first
         Finalize_Shape (State);

         --  Start new shape
         if Idx_Val < MAX_SHAPES then
            State.Parsing_Shape := True;
            State.Current_Shape_Idx := Idx_Val;
            State.Current_Shape_Row := 0;
            State.Temp_Grid := [others => [others => False]];
            State.Temp_Width := 0;
            State.Temp_Height := 0;
         end if;
         return;
      end if;

      --  Check for region specification (contains 'x' and ':')
      declare
         Has_X     : Boolean := False;
         Has_Colon : Boolean := False;
      begin
         for I in Line'Range loop
            if Line (I) = 'x' or else Line (I) = 'X' then
               Has_X := True;
            elsif Line (I) = ':' then
               Has_Colon := True;
            end if;
         end loop;

         if Has_X and then Has_Colon then
            Finalize_Shape (State);
            Parse_Region_Line (Line, State);
            return;
         end if;
      end;

      --  Otherwise it's a shape pattern line
      if State.Parsing_Shape then
         Parse_Shape_Line (Line, State);
      end if;
   end Process_Line;

   procedure Solve_Puzzle (State : in out Solver_State) is
      Fit_Count : Natural := 0;
   begin
      --  Finalize any remaining shape
      Finalize_Shape (State);

      --  Part 1: Count regions that can fit all their pieces
      for I in 0 .. State.Region_Count - 1 loop
         pragma Loop_Invariant (I < State.Region_Count);
         pragma Loop_Invariant (Fit_Count <= I);
         if Can_Fit_Region (State.Shapes, State.Regions (I)) then
            Fit_Count := Fit_Count + 1;
         end if;
      end loop;

      State.Result_Part_1 := Fit_Count;
      State.Result_Part_2 := 0;  --  Part 2 TBD
   end Solve_Puzzle;

end AoC_2025_Day_12.Solver;
