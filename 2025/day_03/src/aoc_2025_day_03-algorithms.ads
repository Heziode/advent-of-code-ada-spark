--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_03.Algorithms
--
--  Purpose: Pure algorithmic solutions for Day 03 puzzle
--
--  Responsibilities:
--    * Calculate maximum joltage from a battery bank line (Part 1)
--    * Calculate maximum 12-digit joltage using greedy selection (Part 2)
--
--  External Effects: None
--  Thread Safety: Thread-safe (pure functions)
--  SPARK Status: Verified (Silver - AoRTE)

package AoC_2025_Day_03.Algorithms with SPARK_Mode => On is

   --  Maximum line length for battery banks
   MAX_BANK_LENGTH : constant := 1000;

   --  Calculate maximum joltage from a battery bank line
   --
   --  Algorithm:
   --    For each position i, the best result is digit[i] * 10 + max(digit[j])
   --    where j > i. We precompute max-suffix to achieve O(n) complexity.
   --
   --  Time Complexity: O(N) where N is the length of the Bank string
   --  Side Effects: None
   --
   --  **Preconditions:**
   --    - Bank length must be <= MAX_BANK_LENGTH
   --
   --  @param Bank String of digit characters ('1'..'9')
   --  @return Maximum two-digit joltage achievable, or 0 if invalid
   function Maximum_Joltage_From_Bank (Bank : String) return Natural
     with Pre => Bank'Length <= MAX_BANK_LENGTH;

   --  Number of batteries to select for Part 2
   BATTERIES_TO_SELECT : constant := 12;

   --  Calculate maximum 12-digit joltage from a battery bank using greedy selection
   --
   --  Algorithm:
   --    Greedily select 12 digits by always picking the largest digit
   --    in the valid range [current_pos, N - remaining_picks] for each position.
   --
   --  Time Complexity: O(K * N) where K is BATTERIES_TO_SELECT and N is Bank length
   --  Side Effects: None
   --
   --  **Preconditions:**
   --    - Bank length must be <= MAX_BANK_LENGTH
   --
   --  @param Bank String of digit characters ('1'..'9')
   --  @return Maximum 12-digit joltage achievable, or 0 if bank too short
   function Maximum_Joltage_12_From_Bank (Bank : String) return Long_Long_Integer
     with Pre => Bank'Length <= MAX_BANK_LENGTH;

end AoC_2025_Day_03.Algorithms;
