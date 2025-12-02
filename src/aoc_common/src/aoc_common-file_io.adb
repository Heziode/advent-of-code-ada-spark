--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_Common.File_IO

pragma Ada_2022;

with Ada.Text_IO;

package body AoC_Common.File_IO with SPARK_Mode => On is

   ---------------------------------------------------------------------------
   --  String Searching Functions
   ---------------------------------------------------------------------------

   function Find_First
      (Source  : String;
       Pattern : Character) return Natural
   is
   begin
      for I in Source'Range loop
         if Source (I) = Pattern then
            return I;
         end if;
         pragma Loop_Invariant
            (for all J in Source'First .. I => Source (J) /= Pattern);
      end loop;
      return 0;
   end Find_First;

   function Find_Last
      (Source  : String;
       Pattern : Character) return Natural
   is
   begin
      for I in reverse Source'Range loop
         if Source (I) = Pattern then
            return I;
         end if;
         pragma Loop_Invariant
            (for all J in I .. Source'Last => Source (J) /= Pattern);
      end loop;
      return 0;
   end Find_Last;

   ---------------------------------------------------------------------------
   --  Digit Conversion
   ---------------------------------------------------------------------------

   function Digit_Value (C : Character) return Natural is
   begin
      return Character'Pos (C) - Character'Pos ('0');
   end Digit_Value;

   ---------------------------------------------------------------------------
   --  Number Parsing
   ---------------------------------------------------------------------------

   function Parse_Natural (Input : String) return Natural_Parse_Result is
      Result : Natural := 0;
      Digit  : Natural;
   begin
      if Input'Length = 0 then
         return (Valid => False);
      end if;

      for I in Input'Range loop
         if not Is_Digit (Input (I)) then
            return (Valid => False);
         end if;

         Digit := Digit_Value (Input (I));

         --  Overflow check for multiplication
         if Result > Natural'Last / 10 then
            return (Valid => False);
         end if;

         Result := Result * 10;

         --  Overflow check for addition
         if Result > Natural'Last - Digit then
            return (Valid => False);
         end if;

         Result := Result + Digit;
      end loop;

      return (Valid => True, Value => Result);
   end Parse_Natural;

   function Parse_Integer (Input : String) return Parse_Result is
      Is_Negative : Boolean := False;
      Start_Idx   : Positive;
      Abs_Result  : Natural;
      Nat_Result  : Natural_Parse_Result;
   begin
      if Input'Length = 0 then
         return (Valid => False);
      end if;

      --  Check for leading sign
      if Input (Input'First) = '-' then
         Is_Negative := True;
         if Input'Length = 1 then
            return (Valid => False);  --  Just "-" is invalid
         end if;
         Start_Idx := Input'First + 1;
      elsif Input (Input'First) = '+' then
         if Input'Length = 1 then
            return (Valid => False);  --  Just "+" is invalid
         end if;
         Start_Idx := Input'First + 1;
      else
         Start_Idx := Input'First;
      end if;

      --  Parse the absolute value
      Nat_Result := Parse_Natural (Input (Start_Idx .. Input'Last));

      if not Nat_Result.Valid then
         return (Valid => False);
      end if;

      Abs_Result := Nat_Result.Value;

      --  Convert to Integer
      --  Note: On this platform Natural'Last = Integer'Last, so any valid
      --  Natural fits in Integer. For negative, we negate.
      if Is_Negative then
         return (Valid => True, Value => -Integer (Abs_Result));
      else
         return (Valid => True, Value => Integer (Abs_Result));
      end if;
   end Parse_Integer;

   ---------------------------------------------------------------------------
   --  String Trimming
   ---------------------------------------------------------------------------

   function First_Non_Whitespace (S : String) return Natural is
   begin
      for I in S'Range loop
         if not Is_Whitespace (S (I)) then
            return I;
         end if;
         pragma Loop_Invariant
            (for all J in S'First .. I => Is_Whitespace (S (J)));
      end loop;
      return 0;
   end First_Non_Whitespace;

   function Last_Non_Whitespace (S : String) return Natural is
   begin
      for I in reverse S'Range loop
         if not Is_Whitespace (S (I)) then
            return I;
         end if;
         pragma Loop_Invariant
            (for all J in I .. S'Last => Is_Whitespace (S (J)));
      end loop;
      return 0;
   end Last_Non_Whitespace;

   ---------------------------------------------------------------------------
   --  File Processing
   ---------------------------------------------------------------------------

   procedure For_Each_Line
      (Filename : String;
       Success  : out Boolean;
       Data     : in out Data_Type)
   is
      pragma SPARK_Mode (Off);

      use Ada.Text_IO;

      File   : File_Type;
      Buffer : String (1 .. Max_Line_Length);
      Last   : Natural;
   begin
      Success := False;

      begin
         Open (File, In_File, Filename);
      exception
         when Name_Error =>
            Put_Line (Standard_Error, "Error: File not found: " & Filename);
            return;
         when Use_Error =>
            Put_Line (Standard_Error, "Error: Cannot open file: " & Filename);
            return;
      end;

      begin
         while not End_Of_File (File) loop
            Get_Line (File, Buffer, Last);
            if Last >= Buffer'First then
               Process_Line (Buffer (Buffer'First .. Last), Data);
            end if;
         end loop;
         Success := True;
      exception
         when End_Error =>
            Success := True;  --  Normal end of file
         when others =>
            null;  --  Keep Success = False
      end;

      if Is_Open (File) then
         Close (File);
      end if;
   end For_Each_Line;

end AoC_Common.File_IO;
