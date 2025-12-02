--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package Body: AoC_Common.Parsing

package body AoC_Common.Parsing is

   function Digit_Value (C : Character) return Natural is
   begin
      return Character'Pos (C) - Character'Pos ('0');
   end Digit_Value;

   function Parse_Integer
      (S     : String;
       Start : Positive) return Parse_Result
   is
      Idx      : Natural := Start;
      Negative : Boolean := False;
      Value    : Integer := 0;
      Found    : Boolean := False;
   begin
      --  Check for leading minus
      if S (Idx) = '-' then
         Negative := True;
         if Idx < S'Last then
            Idx := Idx + 1;
         else
            return (Value => 0, Success => False, Next => 0);
         end if;
      end if;

      --  Parse digits
      while Idx <= S'Last and then Is_Digit (S (Idx)) loop
         Found := True;
         declare
            Digit : constant Natural := Digit_Value (S (Idx));
         begin
            --  Overflow protection
            if Value > (Integer'Last - Digit) / 10 then
               return (Value => 0, Success => False, Next => 0);
            end if;
            Value := Value * 10 + Digit;
         end;
         if Idx < S'Last then
            Idx := Idx + 1;
         else
            Idx := Idx + 1;
            exit;
         end if;
      end loop;

      if not Found then
         return (Value => 0, Success => False, Next => 0);
      end if;

      if Negative then
         Value := -Value;
      end if;

      return (Value => Value, Success => True, Next => Idx);
   end Parse_Integer;

   function Find_Next_Integer
      (S     : String;
       Start : Positive) return Parse_Result
   is
      Idx : Natural := Start;
   begin
      --  Skip until we find a digit or a minus followed by a digit
      while Idx <= S'Last loop
         if Is_Digit (S (Idx)) then
            return Parse_Integer (S, Idx);
         elsif S (Idx) = '-' and then Idx < S'Last and then Is_Digit (S (Idx + 1)) then
            return Parse_Integer (S, Idx);
         end if;
         Idx := Idx + 1;
      end loop;

      return (Value => 0, Success => False, Next => 0);
   end Find_Next_Integer;

   function Skip_Whitespace
      (S     : String;
       Start : Positive) return Natural
   is
      Idx : Natural := Start;
   begin
      while Idx <= S'Last and then Is_Whitespace (S (Idx)) loop
         Idx := Idx + 1;
      end loop;
      return Idx;
   end Skip_Whitespace;

end AoC_Common.Parsing;
