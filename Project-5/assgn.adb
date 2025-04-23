with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Assgn is
   -- Random generator setup for binary values
   package Random_Bit is new Ada.Numerics.Discrete_Random(BINARY_NUMBER);
   Generator : Random_Bit.Generator;

   -- Initialize array with random values
   procedure Init_Array (Arr: in out BINARY_ARRAY) is
   begin
      Random_Bit.Reset(Generator); 
      for Index in Arr'Range loop
         Arr(Index) := Random_Bit.Random(Generator);
      end loop;
   end Init_Array;

   -- Display binary array
   procedure Print_Bin_Arr (Arr : in BINARY_ARRAY) is
   begin
      for Index in Arr'Range loop
         Ada.Integer_Text_IO.Put(Arr(Index), 1);
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Bin_Arr;

   -- Reverse binary array
   procedure Reverse_Bin_Arr (Arr : in out BINARY_ARRAY) is
      Placeholder : BINARY_NUMBER;
   begin
      for Index in 1 .. Arr'Length/2 loop
         Placeholder := Arr(Index);
         Arr(Index) := Arr(Arr'Last - Index + 1);
         Arr(Arr'Last - Index + 1) := Placeholder;
      end loop;
   end Reverse_Bin_Arr;

   -- Convert int to Binary Array
   function Int_To_Bin(Num : in INTEGER) return BINARY_ARRAY is
      Output : BINARY_ARRAY := (others => 0);
      Working : INTEGER := Num;
   begin
      for Index in reverse Output'Range loop
         Output(Index) := Working mod 2;
         Working := Working / 2;
         exit when Working = 0;
      end loop;
      
      return Output;
   end Int_To_Bin;

   -- Convert Binary Array to int
   function Bin_To_Int (Arr : in BINARY_ARRAY) return INTEGER is
      Output : INTEGER := 0;
      Weight : INTEGER := 1;
   begin
      for Index in reverse Arr'Range loop
         Output := Output + (Arr(Index) * Weight);
         Weight := Weight * 2;
      end loop;
      
      return Output;
   end Bin_To_Int;

   -- Helper function for binary addition
   function Binary_Addition(Left, Right : in BINARY_ARRAY) return BINARY_ARRAY is
      Output : BINARY_ARRAY := (others => 0);
      Carry : BINARY_NUMBER := 0;
      Position_Sum : INTEGER;
   begin
      for Index in reverse Output'Range loop
         Position_Sum := Left(Index) + Right(Index) + Carry;
         
         case Position_Sum is
            when 0 =>
               Output(Index) := 0;
               Carry := 0;
            when 1 =>
               Output(Index) := 1;
               Carry := 0;
            when 2 =>
               Output(Index) := 0;
               Carry := 1;
            when 3 =>
               Output(Index) := 1;
               Carry := 1;
            when others =>
               null; 
         end case;
      end loop;
      
      return Output;
   end Binary_Addition;

   -- Helper function for binary sub
   function Binary_Subtraction(Left, Right : in BINARY_ARRAY) return BINARY_ARRAY is
      Output : BINARY_ARRAY := (others => 0);
      Borrow : BINARY_NUMBER := 0;
      Position_Diff : INTEGER;
   begin
      for Index in reverse Output'Range loop
         Position_Diff := Left(Index) - Right(Index) - Borrow;
         
         case Position_Diff is
            when 0 =>
               Output(Index) := 0;
               Borrow := 0;
            when 1 =>
               Output(Index) := 1;
               Borrow := 0;
            when -1 =>
               Output(Index) := 1;
               Borrow := 1;
            when -2 =>
               Output(Index) := 0;
               Borrow := 1;
            when others =>
               null; 
         end case;
      end loop;
      
      return Output;
   end Binary_Subtraction;

   -- Overloaded + operator for binary arrays
   function "+" (Left, Right : in BINARY_ARRAY) return BINARY_ARRAY is
   begin
      return Binary_Addition(Left, Right);
   end "+";

   -- Overloaded + operator for int and binary array
   function "+" (Left_Int : in INTEGER;
                 Right : in BINARY_ARRAY) return BINARY_ARRAY is
   begin
      return Binary_Addition(Int_To_Bin(Left_Int), Right);
   end "+";

   -- Overloaded - operator for binary arrays
   function "-" (Left, Right : in BINARY_ARRAY) return BINARY_ARRAY is
   begin
      return Binary_Subtraction(Left, Right);
   end "-";

   -- Overloaded - operator for int and binary array
   function "-" (Left_Int : in Integer;
                 Right : in BINARY_ARRAY) return BINARY_ARRAY is
   begin
      return Binary_Subtraction(Int_To_Bin(Left_Int), Right);
   end "-";

end Assgn;
