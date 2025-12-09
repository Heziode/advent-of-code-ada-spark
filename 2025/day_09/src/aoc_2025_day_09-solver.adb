--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_2025_Day_09.Solver
--
--  Implementation of the largest rectangle algorithm for Day 09.
--  Part 1: Simple O(n²) brute force over all tile pairs
--  Part 2: Coordinate compression + polygon interior detection

pragma Ada_2022;

with Ada.Unchecked_Deallocation;

package body AoC_2025_Day_09.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Part 2: Coordinate Compression Types
   ---------------------------------------------------------------------------

   --  Maximum unique coordinates (same as MAX_TILES - can't have more unique
   --  coordinates than tiles)
   MAX_COORDS : constant := MAX_TILES;

   subtype Coord_Index is Natural range 0 .. MAX_COORDS;
   type Coord_Array is array (1 .. MAX_COORDS) of Coordinate_Value;

   --  Compressed grid for Part 2 (using cell-based approach)
   --  Grid marks which coordinate intersections are valid (inside polygon)
   subtype Grid_Index is Natural range 0 .. MAX_COORDS;
   type Grid_Row is array (Grid_Index) of Boolean with Pack;
   type Validity_Grid is array (Grid_Index) of Grid_Row;

   --  2D prefix sum grid for O(1) rectangle queries
   type Prefix_Sum_Grid is array (Grid_Index, Grid_Index) of Natural;

   --  Access types for heap allocation (grids too large for stack)
   type Validity_Grid_Ptr is access Validity_Grid;
   type Prefix_Sum_Grid_Ptr is access Prefix_Sum_Grid;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Parse a coordinate value from a substring
   --
   --  @param S The string containing the number
   --  @param Value Output: the parsed value
   --  @param Success Output: True if parsing succeeded
   procedure Parse_Coordinate (S : String; Value : out Coordinate_Value; Success : out Boolean)
   with Post => (if Success then Value in Coordinate_Value)
   is
      Result : Long_Long_Integer := 0;
   begin
      Value := 0;
      Success := False;

      if S'Length = 0 then
         return;
      end if;

      for I in S'Range loop
         if S (I) in '0' .. '9' then
            declare
               Digit : constant Long_Long_Integer := Character'Pos (S (I)) - Character'Pos ('0');
            begin
               --  Check for overflow before multiplication
               if Result > (Long_Long_Integer (Coordinate_Value'Last) - Digit) / 10 then
                  return;  --  Would overflow

               end if;
               Result := Result * 10 + Digit;
            end;
         else
            return;  --  Invalid character
         end if;
         pragma Loop_Invariant (Result >= 0);
         pragma Loop_Invariant (Result <= Long_Long_Integer (Coordinate_Value'Last));
      end loop;

      if Result <= Long_Long_Integer (Coordinate_Value'Last) then
         Value := Coordinate_Value (Result);
         Success := True;
      end if;
   end Parse_Coordinate;

   --  Calculate rectangle area from two opposite corners
   --
   --  The area includes all tiles within the rectangle (inclusive bounds).
   --  For corners (x1,y1) and (x2,y2), area = (|x2-x1|+1) * (|y2-y1|+1)
   --
   --  @param Tile_A First corner tile
   --  @param Tile_B Second corner tile (opposite)
   --  @return The rectangle area (width * height) counting all tiles inside
   function Calculate_Rectangle_Area (Tile_A : Red_Tile; Tile_B : Red_Tile) return Long_Long_Integer is
      Width  : Long_Long_Integer;
      Height : Long_Long_Integer;
   begin
      --  Calculate dimensions (inclusive: add 1 to count tiles, not gaps)
      if Tile_A.X >= Tile_B.X then
         Width := Long_Long_Integer (Tile_A.X - Tile_B.X) + 1;
      else
         Width := Long_Long_Integer (Tile_B.X - Tile_A.X) + 1;
      end if;

      if Tile_A.Y >= Tile_B.Y then
         Height := Long_Long_Integer (Tile_A.Y - Tile_B.Y) + 1;
      else
         Height := Long_Long_Integer (Tile_B.Y - Tile_A.Y) + 1;
      end if;

      return Width * Height;
   end Calculate_Rectangle_Area;

   ---------------------------------------------------------------------------
   --  Part 2: Coordinate Compression and Polygon Interior
   ---------------------------------------------------------------------------

   --  Simple insertion sort for coordinate array
   procedure Sort_Coordinates (Coords : in out Coord_Array; Count : Natural) is
      Key : Coordinate_Value;
      J   : Natural;
   begin
      if Count <= 1 then
         return;
      end if;

      for I in 2 .. Count loop
         Key := Coords (I);
         J := I - 1;

         while J >= 1 and then Coords (J) > Key loop
            Coords (J + 1) := Coords (J);
            J := J - 1;
         end loop;

         Coords (J + 1) := Key;

         pragma Loop_Invariant (I <= Count);
      end loop;
   end Sort_Coordinates;

   --  Remove duplicates from sorted coordinate array
   procedure Remove_Duplicates (Coords : in out Coord_Array; Count : in out Natural) is
      Write_Pos : Natural := 1;
   begin
      if Count <= 1 then
         return;
      end if;

      for I in 2 .. Count loop
         if Coords (I) /= Coords (Write_Pos) then
            Write_Pos := Write_Pos + 1;
            Coords (Write_Pos) := Coords (I);
         end if;
         pragma Loop_Invariant (Write_Pos <= I);
         pragma Loop_Invariant (Write_Pos >= 1);
      end loop;

      Count := Write_Pos;
   end Remove_Duplicates;

   --  Binary search for coordinate index
   function Find_Coord_Index (Coords : Coord_Array; Count : Natural; Value : Coordinate_Value) return Natural is
      Low, High, Mid : Natural;
   begin
      if Count = 0 then
         return 0;
      end if;

      Low := 1;
      High := Count;

      while Low <= High loop
         Mid := Low + (High - Low) / 2;

         if Coords (Mid) = Value then
            return Mid;
         elsif Coords (Mid) < Value then
            Low := Mid + 1;
         else
            if Mid = 1 then
               return 0;
            end if;
            High := Mid - 1;
         end if;

         pragma Loop_Invariant (Low >= 1);
         pragma Loop_Invariant (High <= Count);
      end loop;

      return 0;  --  Not found
   end Find_Coord_Index;

   --  Check if a point is inside the polygon using ray casting
   --  Counts crossings of horizontal ray going right from point
   function Is_Point_Inside_Polygon
     (Point_X : Coordinate_Value; Point_Y : Coordinate_Value; Tiles : Tile_Array; Tile_Count : Natural) return Boolean
   is
      Crossings      : Natural := 0;
      X1, Y1, X2, Y2 : Coordinate_Value;
      Next_Idx       : Natural;
   begin
      if Tile_Count < 3 then
         return False;
      end if;

      --  For each edge of the polygon
      for I in 1 .. Tile_Count loop
         Next_Idx := (if I = Tile_Count then 1 else I + 1);

         X1 := Tiles (I).X;
         Y1 := Tiles (I).Y;
         X2 := Tiles (Next_Idx).X;
         Y2 := Tiles (Next_Idx).Y;

         --  Check if this is a vertical edge that our horizontal ray might cross
         if X1 = X2 then
            --  Vertical edge at X = X1
            declare
               Min_Y : constant Coordinate_Value := (if Y1 < Y2 then Y1 else Y2);
               Max_Y : constant Coordinate_Value := (if Y1 > Y2 then Y1 else Y2);
            begin
               --  Ray crosses if edge is to the right and spans our Y
               if X1 > Point_X and then Point_Y > Min_Y and then Point_Y <= Max_Y then
                  Crossings := Crossings + 1;
               end if;
            end;
         end if;

         pragma Loop_Invariant (I <= Tile_Count);
      end loop;

      --  Odd number of crossings = inside
      return (Crossings mod 2) = 1;
   end Is_Point_Inside_Polygon;

   --  Check if a point is on the polygon boundary
   function Is_Point_On_Boundary
     (Point_X : Coordinate_Value; Point_Y : Coordinate_Value; Tiles : Tile_Array; Tile_Count : Natural) return Boolean
   is
      X1, Y1, X2, Y2 : Coordinate_Value;
      Next_Idx       : Natural;
   begin
      if Tile_Count < 2 then
         return False;
      end if;

      for I in 1 .. Tile_Count loop
         Next_Idx := (if I = Tile_Count then 1 else I + 1);

         X1 := Tiles (I).X;
         Y1 := Tiles (I).Y;
         X2 := Tiles (Next_Idx).X;
         Y2 := Tiles (Next_Idx).Y;

         --  Check if point is on this edge
         if X1 = X2 then
            --  Vertical edge
            declare
               Min_Y : constant Coordinate_Value := (if Y1 < Y2 then Y1 else Y2);
               Max_Y : constant Coordinate_Value := (if Y1 > Y2 then Y1 else Y2);
            begin
               if Point_X = X1 and then Point_Y >= Min_Y and then Point_Y <= Max_Y then
                  return True;
               end if;
            end;
         elsif Y1 = Y2 then
            --  Horizontal edge
            declare
               Min_X : constant Coordinate_Value := (if X1 < X2 then X1 else X2);
               Max_X : constant Coordinate_Value := (if X1 > X2 then X1 else X2);
            begin
               if Point_Y = Y1 and then Point_X >= Min_X and then Point_X <= Max_X then
                  return True;
               end if;
            end;
         end if;

         pragma Loop_Invariant (I <= Tile_Count);
      end loop;

      return False;
   end Is_Point_On_Boundary;

   --  Check if a point is valid (red/green) for Part 2
   function Is_Point_Valid
     (Point_X : Coordinate_Value; Point_Y : Coordinate_Value; Tiles : Tile_Array; Tile_Count : Natural) return Boolean
   is
   begin
      return
        Is_Point_On_Boundary (Point_X, Point_Y, Tiles, Tile_Count)
        or else Is_Point_Inside_Polygon (Point_X, Point_Y, Tiles, Tile_Count);
   end Is_Point_Valid;

   --  Build validity grid: precompute which coordinate intersections are valid
   procedure Build_Validity_Grid
     (Tiles      : Tile_Array;
      Tile_Count : Natural;
      X_Coords   : Coord_Array;
      X_Count    : Natural;
      Y_Coords   : Coord_Array;
      Y_Count    : Natural;
      Grid       : out Validity_Grid) is
   begin
      --  Initialize grid to False
      Grid := [others => [others => False]];

      --  Mark each coordinate intersection as valid or not
      for XI in 1 .. X_Count loop
         for YI in 1 .. Y_Count loop
            Grid (XI) (YI) := Is_Point_Valid (X_Coords (XI), Y_Coords (YI), Tiles, Tile_Count);
         end loop;
      end loop;
   end Build_Validity_Grid;

   --  Build 2D prefix sum of invalid cells for O(1) rectangle queries
   --  Prefix(i,j) = count of invalid cells in rectangle (1,1) to (i,j)
   procedure Build_Invalid_Prefix_Sum
     (Grid : Validity_Grid; X_Count : Natural; Y_Count : Natural; Prefix : out Prefix_Sum_Grid)
   is
      Invalid_Count : Natural;
   begin
      Prefix := [others => [others => 0]];

      for XI in 1 .. X_Count loop
         for YI in 1 .. Y_Count loop
            --  Current cell invalid count (1 if invalid, 0 if valid)
            Invalid_Count := (if Grid (XI) (YI) then 0 else 1);

            --  2D prefix sum formula
            Prefix (XI, YI) := Invalid_Count;

            if XI > 1 then
               Prefix (XI, YI) := Prefix (XI, YI) + Prefix (XI - 1, YI);
            end if;

            if YI > 1 then
               Prefix (XI, YI) := Prefix (XI, YI) + Prefix (XI, YI - 1);
            end if;

            if XI > 1 and then YI > 1 then
               --  Subtract double-counted region
               Prefix (XI, YI) := Prefix (XI, YI) - Prefix (XI - 1, YI - 1);
            end if;
         end loop;
      end loop;
   end Build_Invalid_Prefix_Sum;

   --  Query invalid count in rectangle using prefix sums (O(1))
   function Query_Invalid_Count (Prefix : Prefix_Sum_Grid; X1, X2, Y1, Y2 : Natural) return Natural is
      Result : Integer;
   begin
      Result := Prefix (X2, Y2);

      if X1 > 1 then
         Result := Result - Prefix (X1 - 1, Y2);
      end if;

      if Y1 > 1 then
         Result := Result - Prefix (X2, Y1 - 1);
      end if;

      if X1 > 1 and then Y1 > 1 then
         Result := Result + Prefix (X1 - 1, Y1 - 1);
      end if;

      return Natural'Max (0, Result);
   end Query_Invalid_Count;

   ---------------------------------------------------------------------------
   --  Public Interface Implementation
   ---------------------------------------------------------------------------

   procedure Initialize (State : out Solver_State) is
   begin
      State.Part := AoC_Common.Part_1;
      State.Tiles := [others => (X => 0, Y => 0)];
      State.Tile_Count := 0;
      State.Max_Area := 0;
      State.Max_Area_Part_2 := 0;
      State.Error_Encountered := False;
   end Initialize;

   procedure Process_Line (Line : String; State : in out Solver_State) is
      Comma_Pos : Natural := 0;
      X_Value   : Coordinate_Value;
      Y_Value   : Coordinate_Value;
      X_Success : Boolean;
      Y_Success : Boolean;
   begin
      --  Find comma separator
      for I in Line'Range loop
         if Line (I) = ',' then
            Comma_Pos := I;
            exit;
         end if;
      end loop;

      --  Validate format
      if Comma_Pos = 0 or else Comma_Pos = Line'First or else Comma_Pos = Line'Last then
         State.Error_Encountered := True;
         return;
      end if;

      --  Parse X coordinate (before comma)
      Parse_Coordinate (Line (Line'First .. Comma_Pos - 1), X_Value, X_Success);
      if not X_Success then
         State.Error_Encountered := True;
         return;
      end if;

      --  Parse Y coordinate (after comma)
      Parse_Coordinate (Line (Comma_Pos + 1 .. Line'Last), Y_Value, Y_Success);
      if not Y_Success then
         State.Error_Encountered := True;
         return;
      end if;

      --  Store tile if we have space
      if State.Tile_Count < MAX_TILES then
         State.Tile_Count := State.Tile_Count + 1;
         State.Tiles (State.Tile_Count) := (X => X_Value, Y => Y_Value);
      else
         State.Error_Encountered := True;
      end if;
   end Process_Line;

   procedure Solve_Puzzle (State : in out Solver_State) is
      Current_Area : Long_Long_Integer;

      --  Part 2: Coordinate compression arrays
      X_Coords : Coord_Array := [others => 0];
      Y_Coords : Coord_Array := [others => 0];
      X_Count  : Natural := 0;
      Y_Count  : Natural := 0;

      --  Part 2: Pre-computed grids (heap-allocated to avoid stack overflow)
      Valid_Grid_Heap : Validity_Grid_Ptr;
      Prefix_Heap     : Prefix_Sum_Grid_Ptr;

      --  Part 2: Grid indices for rectangle bounds
      XI1, XI2, YI1, YI2 : Natural;

      --  Deallocation procedures
      procedure Free_Validity_Grid is new Ada.Unchecked_Deallocation (Validity_Grid, Validity_Grid_Ptr);
      procedure Free_Prefix_Sum is new Ada.Unchecked_Deallocation (Prefix_Sum_Grid, Prefix_Sum_Grid_Ptr);
   begin
      State.Max_Area := 0;
      State.Max_Area_Part_2 := 0;

      if State.Tile_Count < 2 then
         return;
      end if;

      --  =========================================================
      --  Part 1: Find maximum rectangle area (any two tiles)
      --  =========================================================
      for I in 1 .. State.Tile_Count - 1 loop
         pragma Loop_Invariant (State.Max_Area >= 0);
         pragma Loop_Invariant (I <= State.Tile_Count - 1);

         for J in I + 1 .. State.Tile_Count loop
            pragma Loop_Invariant (State.Max_Area >= 0);
            pragma Loop_Invariant (J <= State.Tile_Count);

            Current_Area := Calculate_Rectangle_Area (State.Tiles (I), State.Tiles (J));

            if Current_Area > State.Max_Area then
               State.Max_Area := Current_Area;
            end if;
         end loop;
      end loop;

      --  =========================================================
      --  Part 2: Find maximum rectangle using only red/green tiles
      --  =========================================================

      --  Allocate grids on heap
      Valid_Grid_Heap := new Validity_Grid;
      Prefix_Heap := new Prefix_Sum_Grid;

      --  Step 1: Build compressed coordinate arrays
      for I in 1 .. State.Tile_Count loop
         X_Count := X_Count + 1;
         X_Coords (X_Count) := State.Tiles (I).X;

         Y_Count := Y_Count + 1;
         Y_Coords (Y_Count) := State.Tiles (I).Y;

         pragma Loop_Invariant (X_Count = I);
         pragma Loop_Invariant (Y_Count = I);
         pragma Loop_Invariant (X_Count <= MAX_COORDS);
         pragma Loop_Invariant (Y_Count <= MAX_COORDS);
      end loop;

      --  Sort and deduplicate coordinates
      Sort_Coordinates (X_Coords, X_Count);
      Remove_Duplicates (X_Coords, X_Count);
      Sort_Coordinates (Y_Coords, Y_Count);
      Remove_Duplicates (Y_Coords, Y_Count);

      --  Step 2: Pre-compute validity grid (O(m² × n) but done ONCE)
      Build_Validity_Grid (State.Tiles, State.Tile_Count, X_Coords, X_Count, Y_Coords, Y_Count, Valid_Grid_Heap.all);

      --  Step 3: Build prefix sum for O(1) rectangle queries
      Build_Invalid_Prefix_Sum (Valid_Grid_Heap.all, X_Count, Y_Count, Prefix_Heap.all);

      --  Step 4: Check each pair of tiles for valid rectangle (O(n²))
      for I in 1 .. State.Tile_Count - 1 loop
         pragma Loop_Invariant (State.Max_Area_Part_2 >= 0);
         pragma Loop_Invariant (I <= State.Tile_Count - 1);

         for J in I + 1 .. State.Tile_Count loop
            pragma Loop_Invariant (State.Max_Area_Part_2 >= 0);
            pragma Loop_Invariant (J <= State.Tile_Count);

            --  Find grid indices for rectangle bounds
            XI1 := Find_Coord_Index (X_Coords, X_Count, State.Tiles (I).X);
            XI2 := Find_Coord_Index (X_Coords, X_Count, State.Tiles (J).X);
            YI1 := Find_Coord_Index (Y_Coords, Y_Count, State.Tiles (I).Y);
            YI2 := Find_Coord_Index (Y_Coords, Y_Count, State.Tiles (J).Y);

            --  Ensure indices are ordered (min, max)
            if XI1 > XI2 then
               declare
                  Tmp : constant Natural := XI1;
               begin
                  XI1 := XI2;
                  XI2 := Tmp;
               end;
            end if;

            if YI1 > YI2 then
               declare
                  Tmp : constant Natural := YI1;
               begin
                  YI1 := YI2;
                  YI2 := Tmp;
               end;
            end if;

            --  Check if rectangle is valid using O(1) prefix sum query
            if XI1 > 0 and then XI2 > 0 and then YI1 > 0 and then YI2 > 0 then
               if Query_Invalid_Count (Prefix_Heap.all, XI1, XI2, YI1, YI2) = 0 then
                  Current_Area := Calculate_Rectangle_Area (State.Tiles (I), State.Tiles (J));

                  if Current_Area > State.Max_Area_Part_2 then
                     State.Max_Area_Part_2 := Current_Area;
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      --  Free heap memory
      Free_Validity_Grid (Valid_Grid_Heap);
      Free_Prefix_Sum (Prefix_Heap);
   end Solve_Puzzle;

   function Solve_Part_1 (State : Solver_State) return Long_Long_Integer is
   begin
      return State.Max_Area;
   end Solve_Part_1;

   function Solve_Part_2 (State : Solver_State) return Long_Long_Integer is
   begin
      return State.Max_Area_Part_2;
   end Solve_Part_2;

end AoC_2025_Day_09.Solver;
