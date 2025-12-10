--------------------------------------------------------------------------------
--  Advent of Code - SPARK-verified puzzle solutions
--  Copyright (c) 2025 Heziode
--  SPDX-License-Identifier: MIT
--------------------------------------------------------------------------------

--  Package: AoC_2025_Day_10.Solver
--
--  Purpose: Core logic for Day 10 puzzle (Factory - Indicator Light Buttons)
--
--  Responsibilities:
--    * Parse machine definitions: target light state, button wiring, joltage
--    * Part 1: Solve GF(2) linear system for minimum light toggle presses
--    * Part 2: Solve integer linear system for minimum joltage counter presses
--
--  Design Notes:
--    * Part 1: Toggle lights (XOR) - each button pressed 0 or 1 times
--      - Solve Ax = t (mod 2) for minimum Hamming weight x
--      - Uses Gaussian elimination over GF(2)
--    * Part 2: Increment counters (addition) - buttons pressed >= 0 times
--      - Solve Ax = t over non-negative integers for minimum sum(x)
--      - Uses iterative deepening search with pruning
--    * Max 20 counters and 20 buttons per machine
--
--  External Effects: None (pure)
--  Thread Safety: Thread-safe
--  SPARK Status: Verified (Silver - AoRTE)

pragma Ada_2022;

with AoC_Common.File_IO;

package AoC_2025_Day_10.Solver
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants and Types
   ---------------------------------------------------------------------------

   --  Maximum number of indicator lights per machine
   MAX_LIGHTS : constant := 20;

   --  Maximum number of buttons per machine
   MAX_BUTTONS : constant := 20;

   --  Maximum number of machines in input
   MAX_MACHINES : constant := 1_000;

   --  Light/Button indices (0-based as per puzzle)
   subtype Light_Index is Natural range 0 .. MAX_LIGHTS - 1;
   subtype Button_Index is Natural range 0 .. MAX_BUTTONS - 1;

   --  GF(2) bit value (0 or 1)
   subtype Bit is Natural range 0 .. 1;

   --  Light state vector: each element is 0 (off) or 1 (on)
   type Light_Vector is array (Light_Index) of Bit;

   --  Button mask: which lights a button toggles (1 = toggles, 0 = no effect)
   type Button_Mask is array (Light_Index) of Bit;

   --  Button collection for a machine
   type Button_Array is array (Button_Index) of Button_Mask;

   --  Solution vector: which buttons to press (0 or 1)
   type Button_Solution is array (Button_Index) of Bit;

   --  Maximum joltage value per counter
   MAX_JOLTAGE : constant := 100_000;
   subtype Joltage_Value is Natural range 0 .. MAX_JOLTAGE;

   --  Joltage requirements array (target counter values)
   type Joltage_Array is array (Light_Index) of Joltage_Value;

   --  Machine definition parsed from input
   type Machine is record
      Light_Count   : Natural := 0;                    --  Number of lights/counters
      Button_Count  : Natural := 0;                    --  Number of buttons
      Target        : Light_Vector := [others => 0];   --  Target light state (Part 1)
      Joltage       : Joltage_Array := [others => 0];  --  Target joltage (Part 2)
      Buttons       : Button_Array := [others => [others => 0]];  --  Button masks
   end record;

   --  Solver state that accumulates input data and results
   type Solver_State is new AoC_Common.File_IO.Data_Context with record
      Machine_Count        : Natural := 0;
      Total_Presses        : Natural := 0;   --  Part 1 result
      Total_Presses_Part_2 : Natural := 0;   --  Part 2 result (if applicable)
      Error_Encountered    : Boolean := False;
   end record;

   ---------------------------------------------------------------------------
   --  Core Logic
   ---------------------------------------------------------------------------

   --  Initialize the solver state
   --
   --  @param State The state to initialize
   procedure Initialize (State : out Solver_State);

   --  Process a single line of input (one machine definition)
   --
   --  Format: [.##.] (0,1) (2) (0,2,3) {ignored}
   --
   --  @param Line The input line to process
   --  @param State The solver state to update
   procedure Process_Line (Line : String; State : in out Solver_State);

   --  Solve the puzzle (Part 1: sum of minimum presses for all machines)
   --
   --  Called after all lines have been read.
   --
   --  @param State The solver state with parsed machines
   procedure Solve_Puzzle (State : in out Solver_State)
   with Pre'Class => State_Invariant (State);

   --  Get Part 1 result: total minimum button presses
   --
   --  @param State The populated and solved state
   --  @return The total minimum presses
   function Result_Part_1 (State : Solver_State) return Natural
   with Pre'Class => State_Invariant (State);

   --  Get Part 2 result: total minimum joltage counter presses
   --
   --  @param State The populated and solved state
   --  @return The Part 2 result
   function Result_Part_2 (State : Solver_State) return Natural
   with Pre'Class => State_Invariant (State);

   --  State invariant for SPARK verification
   --
   --  @param State The solver state to validate
   --  @return True if state is valid
   function State_Invariant (State : Solver_State) return Boolean
   is (State.Machine_Count <= MAX_MACHINES);

end AoC_2025_Day_10.Solver;
