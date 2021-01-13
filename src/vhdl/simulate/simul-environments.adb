--  Naive values for interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with System;
with Ada.Unchecked_Conversion;
with GNAT.Debug_Utilities;

with Simple_IO;
with Name_Table;
with Vhdl.Utils; use Vhdl.Utils;
with Simul.Debugger; use Simul.Debugger;

package body Simul.Environments is

   -- Functions for iir_value_literal
   function Is_Equal (Left, Right: Iir_Value_Literal_Acc) return Boolean is
   begin
      if Left.Kind /= Right.Kind then
         raise Internal_Error;
      end if;
      case Left.Kind is
         when Iir_Value_B1 =>
            return Left.B1 = Right.B1;
         when Iir_Value_E8 =>
            return Left.E8 = Right.E8;
         when Iir_Value_E32 =>
            return Left.E32 = Right.E32;
         when Iir_Value_I64 =>
            return Left.I64 = Right.I64;
         when Iir_Value_F64 =>
            return Left.F64 = Right.F64;
         when Iir_Value_Access =>
            return Left.Val_Access = Right.Val_Access;
         when Iir_Value_File =>
            raise Internal_Error;
         when Iir_Value_Array =>
            if Left.Bounds.Nbr_Dims /= Right.Bounds.Nbr_Dims then
               raise Internal_Error;
            end if;
            for I in Left.Bounds.D'Range loop
               if Left.Bounds.D (I).Length /= Right.Bounds.D (I).Length then
                  return False;
               end if;
            end loop;
            for I in Left.Val_Array.V'Range loop
               if not Is_Equal (Left.Val_Array.V (I),
                                Right.Val_Array.V (I)) then
                  return False;
               end if;
            end loop;
            return True;
         when Iir_Value_Record =>
            if Left.Val_Record.Len /= Right.Val_Record.Len then
               raise Constraint_Error;
            end if;
            for I in Left.Val_Record.V'Range loop
               if not Is_Equal (Left.Val_Record.V (I),
                                Right.Val_Record.V (I)) then
                  return False;
               end if;
            end loop;
            return True;
         when Iir_Value_Range =>
            if Left.Dir /= Right.Dir then
               return False;
            end if;
            if not Is_Equal (Left.Left, Right.Left) then
               return False;
            end if;
            if not Is_Equal (Left.Right, Right.Right) then
               return False;
            end if;
            return True;
         when Iir_Value_Signal
           | Iir_Value_Protected
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Is_Equal;

   function Compare_Value (Left, Right : Iir_Value_Literal_Acc)
                           return Order is
   begin
      if Left.Kind /= Right.Kind then
         raise Constraint_Error;
      end if;
      case Left.Kind is
         when Iir_Value_B1 =>
            if Left.B1 < Right.B1 then
               return Less;
            elsif Left.B1 = Right.B1 then
               return Equal;
            else
               return Greater;
            end if;
         when Iir_Value_E8 =>
            if Left.E8 < Right.E8 then
               return Less;
            elsif Left.E8 = Right.E8 then
               return Equal;
            else
               return Greater;
            end if;
         when Iir_Value_E32 =>
            if Left.E32 < Right.E32 then
               return Less;
            elsif Left.E32 = Right.E32 then
               return Equal;
            else
               return Greater;
            end if;
         when Iir_Value_I64 =>
            if Left.I64 < Right.I64 then
               return Less;
            elsif Left.I64 = Right.I64 then
               return Equal;
            else
               return Greater;
            end if;
         when Iir_Value_F64 =>
            if Left.F64 < Right.F64 then
               return Less;
            elsif Left.F64 = Right.F64 then
               return Equal;
            elsif Left.F64 > Right.F64 then
               return Greater;
            else
               raise Constraint_Error;
            end if;
         when Iir_Value_Array =>
            --  LRM93 7.2.2
            --  For discrete array types, the relation < (less than) is defined
            --  such as the left operand is less than the right operand if
            --  and only if:
            --  *  the left operand is a null array and the right operand is
            --     a non-null array; otherwise
            --  *  both operands are non-null arrays, and one of the following
            --     conditions is satisfied:
            --     -  the leftmost element of the left operand is less than
            --        that of the right; or
            --     -  the leftmost element of the left operand is equal to
            --        that of the right, and the tail of the left operand is
            --        less than that of the right (the tail consists of the
            --        remaining elements to the rights of the leftmost element
            --        and can be null)
            --  The relation <= (less than or equal) for discrete array types
            --  is defined to be the inclusive disjunction of the results of
            --  the < and = operators for the same two operands.
            --  The relation > (greater than) and >= (greater than of equal)
            --  are defined to be the complements of the <= and < operators
            --  respectively for the same two operands.
            if Left.Bounds.Nbr_Dims /= 1 or Right.Bounds.Nbr_Dims /= 1 then
               raise Internal_Error;
            end if;
            for I in 1 .. Iir_Index32'Min (Left.Bounds.D (1).Length,
                                           Right.Bounds.D (1).Length)
            loop
               case Compare_Value (Left.Val_Array.V (I),
                                   Right.Val_Array.V (I)) is
                  when Less =>
                     return Less;
                  when Greater =>
                     return Greater;
                  when Equal =>
                     null;
               end case;
            end loop;
            if Left.Bounds.D (1).Length < Right.Bounds.D (1).Length then
               return Less;
            elsif Left.Bounds.D (1).Length = Right.Bounds.D (1).Length then
               return Equal;
            else
               return Greater;
            end if;
         when Iir_Value_Signal
           | Iir_Value_Access
           | Iir_Value_Range
           | Iir_Value_Record
           | Iir_Value_File
           | Iir_Value_Protected
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Compare_Value;

   function Is_Null_Range (Arange : Iir_Value_Literal_Acc) return Boolean
   is
      Cmp : Order;
   begin
      Cmp := Compare_Value (Arange.Left, Arange.Right);
      case Arange.Dir is
         when Dir_To =>
            return Cmp = Greater;
         when Dir_Downto =>
            return Cmp = Less;
      end case;
   end Is_Null_Range;

   procedure Increment (Val : Iir_Value_Literal_Acc) is
   begin
      case Val.Kind is
         when Iir_Value_B1 =>
            if Val.B1 = False then
               Val.B1 := True;
            else
               raise Constraint_Error;
            end if;
         when Iir_Value_E8 =>
            Val.E8 := Val.E8 + 1;
         when Iir_Value_E32 =>
            Val.E32 := Val.E32 + 1;
         when Iir_Value_I64 =>
            Val.I64 := Val.I64 + 1;
         when Iir_Value_F64
           | Iir_Value_Array
           | Iir_Value_Record
           | Iir_Value_Range
           | Iir_Value_File
           | Iir_Value_Access
           | Iir_Value_Signal
           | Iir_Value_Protected
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Increment;

   procedure Store (Dest : Iir_Value_Literal_Acc; Src : Iir_Value_Literal_Acc)
   is
   begin
      if Dest.Kind /= Src.Kind then
         raise Constraint_Error;
      end if;
      case Dest.Kind is
         when Iir_Value_Array =>
            if Dest.Val_Array.Len /= Src.Val_Array.Len then
               raise Constraint_Error;
            end if;
            for I in Dest.Val_Array.V'Range loop
               Store (Dest.Val_Array.V (I), Src.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            if Dest.Val_Record.Len /= Src.Val_Record.Len then
               raise Constraint_Error;
            end if;
            for I in Dest.Val_Record.V'Range loop
               Store (Dest.Val_Record.V (I), Src.Val_Record.V (I));
            end loop;
         when Iir_Value_B1 =>
            Dest.B1 := Src.B1;
         when Iir_Value_E8 =>
            Dest.E8 := Src.E8;
         when Iir_Value_E32 =>
            Dest.E32 := Src.E32;
         when Iir_Value_I64 =>
            Dest.I64 := Src.I64;
         when Iir_Value_F64 =>
            Dest.F64 := Src.F64;
         when Iir_Value_Access =>
            Dest.Val_Access := Src.Val_Access;
         when Iir_Value_File =>
            Dest.File := Src.File;
         when Iir_Value_Protected =>
            Dest.Prot := Src.Prot;
         when Iir_Value_Signal =>
            pragma Assert (Dest.Sig = null);
            Dest.Sig := Src.Sig;
         when Iir_Value_Range
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Store;

   procedure Check_Bounds (Dest : Iir_Value_Literal_Acc;
                           Src : Iir_Value_Literal_Acc;
                           Loc : Iir)
   is
   begin
      case Dest.Kind is
         when Iir_Value_Array =>
            if Src.Kind /= Iir_Value_Array then
               raise Internal_Error;
            end if;
            if Dest.Val_Array.Len /= Src.Val_Array.Len then
               Error_Msg_Constraint (Loc);
            end if;
            if Dest.Val_Array.Len /= 0 then
               Check_Bounds (Dest.Val_Array.V (1), Src.Val_Array.V (1), Loc);
            end if;
         when Iir_Value_Record =>
            if Src.Kind /= Iir_Value_Record then
               raise Internal_Error;
            end if;
            if Dest.Val_Record.Len /= Src.Val_Record.Len then
               raise Internal_Error;
            end if;
            for I in Dest.Val_Record.V'Range loop
               Check_Bounds (Dest.Val_Record.V (I), Src.Val_Record.V (I), Loc);
            end loop;
         when Iir_Value_Access
           | Iir_Value_File =>
            if Src.Kind /= Dest.Kind then
               raise Internal_Error;
            end if;
         when Iir_Value_Scalars
           | Iir_Value_Signal =>
            return;
         when Iir_Value_Range
           | Iir_Value_Protected
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Check_Bounds;

   function To_Iir_Value_Literal_Acc is new Ada.Unchecked_Conversion
     (System.Address, Iir_Value_Literal_Acc);
   function To_Value_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Array_Acc);
   function To_Value_Bounds_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Bounds_Array_Acc);

   Last_Sig_Id : Signal_Index_Type := 0;

   function Get_Last_Signal_Index return Signal_Index_Type is
   begin
      return Last_Sig_Id;
   end Get_Last_Signal_Index;

   function Create_Signal_Value (Sig : Ghdl_Signal_Ptr)
                                return Iir_Value_Literal_Acc
   is
      subtype Signal_Value is Iir_Value_Literal (Iir_Value_Signal);
      function Alloc is new Alloc_On_Pool_Addr (Signal_Value);
   begin
      Last_Sig_Id := Last_Sig_Id + 1;
      return To_Iir_Value_Literal_Acc
        (Alloc (Global_Pool'Access,
                (Kind => Iir_Value_Signal,
                 Sig => Sig, Sig_Id => Last_Sig_Id)));
   end Create_Signal_Value;

   function Create_Terminal_Value (Terminal : Terminal_Index_Type)
                                  return Iir_Value_Literal_Acc
   is
      subtype Terminal_Value is Iir_Value_Literal (Iir_Value_Terminal);
      function Alloc is new Alloc_On_Pool_Addr (Terminal_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Global_Pool'Access,
                (Kind => Iir_Value_Terminal, Terminal => Terminal)));
   end Create_Terminal_Value;

   function Create_Quantity_Value (Quantity : Quantity_Index_Type)
                                  return Iir_Value_Literal_Acc
   is
      subtype Quantity_Value is Iir_Value_Literal (Iir_Value_Quantity);
      function Alloc is new Alloc_On_Pool_Addr (Quantity_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Global_Pool'Access,
                (Kind => Iir_Value_Quantity, Quantity => Quantity)));
   end Create_Quantity_Value;

   function Create_Instance_Value (Inst : Block_Instance_Acc)
                                  return Iir_Value_Literal_Acc
   is
      subtype Instance_Value is Iir_Value_Literal (Iir_Value_Instance);
      function Alloc is new Alloc_On_Pool_Addr (Instance_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Global_Pool'Access,
                (Kind => Iir_Value_Instance, Instance => Inst)));
   end Create_Instance_Value;

   function Create_Protected_Value (Prot : Protected_Index_Type)
                                   return Iir_Value_Literal_Acc
   is
      subtype Protected_Value is Iir_Value_Literal (Iir_Value_Protected);
      function Alloc is new Alloc_On_Pool_Addr (Protected_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Global_Pool'Access,
                (Kind => Iir_Value_Protected, Prot => Prot)));
   end Create_Protected_Value;

   function Create_B1_Value (Val : Ghdl_B1) return Iir_Value_Literal_Acc
   is
      subtype B1_Value is Iir_Value_Literal (Iir_Value_B1);
      function Alloc is new Alloc_On_Pool_Addr (B1_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Current_Pool, (Kind => Iir_Value_B1, B1 => Val)));
   end Create_B1_Value;

   function Create_E8_Value (Val : Ghdl_E8) return Iir_Value_Literal_Acc
   is
      subtype E8_Value is Iir_Value_Literal (Iir_Value_E8);
      function Alloc is new Alloc_On_Pool_Addr (E8_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Current_Pool, (Kind => Iir_Value_E8, E8 => Val)));
   end Create_E8_Value;

   function Create_E32_Value (Val : Ghdl_E32) return Iir_Value_Literal_Acc
   is
      subtype E32_Value is Iir_Value_Literal (Iir_Value_E32);
      function Alloc is new Alloc_On_Pool_Addr (E32_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Current_Pool, (Kind => Iir_Value_E32, E32 => Val)));
   end Create_E32_Value;

   function Create_I64_Value (Val : Ghdl_I64) return Iir_Value_Literal_Acc
   is
      subtype I64_Value is Iir_Value_Literal (Iir_Value_I64);
      function Alloc is new Alloc_On_Pool_Addr (I64_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Current_Pool, (Kind => Iir_Value_I64, I64 => Val)));
   end Create_I64_Value;

   function Create_F64_Value (Val : Ghdl_F64) return Iir_Value_Literal_Acc
   is
      subtype F64_Value is Iir_Value_Literal (Iir_Value_F64);
      function Alloc is new Alloc_On_Pool_Addr (F64_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Current_Pool, (Kind => Iir_Value_F64, F64 => Val)));
   end Create_F64_Value;

   function Create_Access_Value (Val : Iir_Value_Literal_Acc)
                                return Iir_Value_Literal_Acc
   is
      subtype Access_Value is Iir_Value_Literal (Iir_Value_Access);
      function Alloc is new Alloc_On_Pool_Addr (Access_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Current_Pool,
                (Kind => Iir_Value_Access, Val_Access => Val)));
   end Create_Access_Value;

   function Create_Range_Value (Left, Right : Iir_Value_Literal_Acc;
                                Dir : Direction_Type;
                                Length : Iir_Index32)
                               return Iir_Value_Literal_Acc
   is
      subtype Range_Value is Iir_Value_Literal (Iir_Value_Range);
      function Alloc is new Alloc_On_Pool_Addr (Range_Value);
   begin
      return To_Iir_Value_Literal_Acc (Alloc (Current_Pool,
                                              (Kind => Iir_Value_Range,
                                               Left => Left,
                                               Right => Right,
                                               Dir => Dir,
                                               Length => Length)));
   end Create_Range_Value;

   function Create_File_Value (Val : Grt.Files.Ghdl_File_Index)
                              return Iir_Value_Literal_Acc
   is
      subtype File_Value is Iir_Value_Literal (Iir_Value_File);
      function Alloc is new Alloc_On_Pool_Addr (File_Value);
   begin
      return To_Iir_Value_Literal_Acc
        (Alloc (Current_Pool,
                (Kind => Iir_Value_File, File => Val)));
   end Create_File_Value;

   --  Create a range_value of life LIFE.
   function Create_Range_Value (Left, Right : Iir_Value_Literal_Acc;
                                Dir : Direction_Type)
                               return Iir_Value_Literal_Acc
   is
      Low, High : Iir_Value_Literal_Acc;
      Len : Iir_Index32;
   begin
      case Dir is
         when Dir_To =>
            Low := Left;
            High := Right;
         when Dir_Downto =>
            Low := Right;
            High := Left;
      end case;

      case Iir_Value_Scalars (Low.Kind) is
         when Iir_Value_B1 =>
            if High.B1 >= Low.B1 then
               Len := Ghdl_B1'Pos (High.B1) - Ghdl_B1'Pos (Low.B1) + 1;
            else
               Len := 0;
            end if;
         when Iir_Value_E32 =>
            if High.E32 >= Low.E32 then
               Len := Iir_Index32 (High.E32 - Low.E32 + 1);
            else
               Len := 0;
            end if;
         when Iir_Value_E8 =>
            if High.E8 >= Low.E8 then
               Len := Ghdl_E8'Pos (High.E8) - Ghdl_E8'Pos (Low.E8) + 1;
            else
               Len := 0;
            end if;
         when Iir_Value_I64 =>
            declare
               L : Ghdl_I64;
            begin
               if High.I64 = Ghdl_I64'Last and Low.I64 = Ghdl_I64'First
               then
                  --  Prevent overflow
                  Len := Iir_Index32'Last;
               else
                  L := High.I64 - Low.I64;
                  if L >= Ghdl_I64 (Iir_Index32'Last) then
                     --  Prevent overflow
                     Len := Iir_Index32'Last;
                  else
                     L := L + 1;
                     if L < 0 then
                        --  null range.
                        Len := 0;
                     else
                        Len := Iir_Index32 (L);
                     end if;
                  end if;
               end if;
            end;
         when Iir_Value_F64 =>
            Len := 0;
      end case;
      return Create_Range_Value (Left, Right, Dir, Len);
   end Create_Range_Value;

   function Create_Array_Value (Dim : Iir_Index32;
                                Pool : Areapool_Acc := Current_Pool)
                               return Iir_Value_Literal_Acc
   is
      subtype Array_Value is Iir_Value_Literal (Iir_Value_Array);
      function Alloc_Array is new Alloc_On_Pool_Addr (Array_Value);
      subtype Dim_Type is Value_Bounds_Array (Dim);
      function Alloc_Bounds is new Alloc_On_Pool_Addr (Dim_Type);
      Res : Iir_Value_Literal_Acc;
   begin
      Res := To_Iir_Value_Literal_Acc
        (Alloc_Array (Pool,
                      (Kind => Iir_Value_Array,
                       Bounds => null, Val_Array => null)));

      Res.Bounds := To_Value_Bounds_Array_Acc
        (Alloc_Bounds (Pool, Dim_Type'(Nbr_Dims => Dim,
                                       D => (others => null))));

      return Res;
   end Create_Array_Value;

   function Create_Value_Array (Len : Iir_Index32;
                                Pool : Areapool_Acc := Current_Pool)
                               return Value_Array_Acc
   is
      use System;
      subtype Data_Type is Value_Array (Len);
      Res : Address;
   begin
      --  Manually allocate the array to handle large arrays without
      --  creating a large temporary value.
      Allocate
        (Pool.all, Res, Data_Type'Size / Storage_Unit, Data_Type'Alignment);

      declare
         --  Discard the warnings for no pragma Import as we really want
         --  to use the default initialization.
         pragma Warnings (Off);
         Addr1 : constant Address := Res;
         Init : Data_Type;
         for Init'Address use Addr1;
         pragma Warnings (On);
      begin
         null;
      end;

      return To_Value_Array_Acc (Res);
   end Create_Value_Array;

   procedure Create_Array_Data (Arr : Iir_Value_Literal_Acc;
                                Len : Iir_Index32;
                                Pool : Areapool_Acc := Current_Pool) is
   begin
      Arr.Val_Array := Create_Value_Array (Len, Pool);
   end Create_Array_Data;

   function Create_Array_Value (Length: Iir_Index32;
                                Dim : Iir_Index32;
                                Pool : Areapool_Acc := Current_Pool)
                               return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      Res := Create_Array_Value (Dim, Pool);
      Create_Array_Data (Res, Length, Pool);
      return Res;
   end Create_Array_Value;

   function Create_Record_Value
     (Nbr : Iir_Index32; Pool : Areapool_Acc := Current_Pool)
     return Iir_Value_Literal_Acc
   is
      subtype Record_Value is Iir_Value_Literal (Iir_Value_Record);
      function Alloc_Record is new Alloc_On_Pool_Addr (Record_Value);
      subtype Data_Type is Value_Array (Nbr);
      function Alloc_Data is new Alloc_On_Pool_Addr (Data_Type);
      Res : Iir_Value_Literal_Acc;
   begin
      Res := To_Iir_Value_Literal_Acc
        (Alloc_Record (Pool, (Kind => Iir_Value_Record, Val_Record => null)));

      Res.Val_Record := To_Value_Array_Acc
        (Alloc_Data (Pool, Data_Type'(Len => Nbr, V => (others => null))));

      return Res;
   end Create_Record_Value;

   -- Create a copy of SRC with a specified life.
   function Copy (Src: in Iir_Value_Literal_Acc)
                  return Iir_Value_Literal_Acc
   is
      Res: Iir_Value_Literal_Acc;
   begin
      case Src.Kind is
         when Iir_Value_B1 =>
            return Create_B1_Value (Src.B1);
         when Iir_Value_E32 =>
            return Create_E32_Value (Src.E32);
         when Iir_Value_E8 =>
            return Create_E8_Value (Src.E8);
         when Iir_Value_I64 =>
            return Create_I64_Value (Src.I64);
         when Iir_Value_F64 =>
            return Create_F64_Value (Src.F64);
         when Iir_Value_Access =>
            return Create_Access_Value (Src.Val_Access);
         when Iir_Value_Array =>
            Res := Copy_Array_Bound (Src);
            for I in Src.Val_Array.V'Range loop
               Res.Val_Array.V (I) := Copy (Src.Val_Array.V (I));
            end loop;
            return Res;

         when Iir_Value_Range =>
            return Create_Range_Value
              (Left => Copy (Src.Left),
               Right => Copy (Src.Right),
               Dir => Src.Dir,
               Length => Src.Length);

         when Iir_Value_Record =>
            Res := Copy_Record (Src);
            for I in Src.Val_Record.V'Range loop
               Res.Val_Record.V (I) := Copy (Src.Val_Record.V (I));
            end loop;
            return Res;

         when Iir_Value_File =>
            return Create_File_Value (Src.File);
         when Iir_Value_Protected =>
            return Create_Protected_Value (Src.Prot);

         when Iir_Value_Signal =>
            pragma Assert (Src.Sig = null);
            return Create_Signal_Value (Src.Sig);

         when Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Copy;

   function Copy_Array_Bound (Src : Iir_Value_Literal_Acc)
                             return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      Res := Create_Array_Value (Src.Val_Array.Len, Src.Bounds.Nbr_Dims);
      for I in Res.Bounds.D'Range loop
         Res.Bounds.D (I) := Copy (Src.Bounds.D (I));
      end loop;
      return Res;
   end Copy_Array_Bound;

   function Copy_Record (Src : Iir_Value_Literal_Acc)
                        return Iir_Value_Literal_Acc is
   begin
      return Create_Record_Value (Src.Val_Record.Len);
   end Copy_Record;

   function Unshare (Src : Iir_Value_Literal_Acc; Pool : Areapool_Acc)
                    return Iir_Value_Literal_Acc
   is
      Prev_Pool : constant Areapool_Acc := Current_Pool;
      Res : Iir_Value_Literal_Acc;
   begin
      Current_Pool := Pool;
      Res := Copy (Src);
      Current_Pool := Prev_Pool;
      return Res;
   end Unshare;

   function Unshare_Bounds (Src : Iir_Value_Literal_Acc; Pool : Areapool_Acc)
                           return Iir_Value_Literal_Acc is
   begin
      if Src.Kind /= Iir_Value_Array then
         return Src;
      end if;
      declare
         Prev_Pool : constant Areapool_Acc := Current_Pool;
         Res : Iir_Value_Literal_Acc;
      begin
         Current_Pool := Pool;
         Res := Create_Array_Value (Src.Val_Array.Len, Src.Bounds.Nbr_Dims);
         for I in Src.Bounds.D'Range loop
            Res.Bounds.D (I) := Copy (Src.Bounds.D (I));
         end loop;
         Res.Val_Array.V := Src.Val_Array.V;
         Current_Pool := Prev_Pool;
         return Res;
      end;
   end Unshare_Bounds;

   Heap_Pool : aliased Areapool;

   function Unshare_Heap (Src : Iir_Value_Literal_Acc)
                         return Iir_Value_Literal_Acc is
   begin
      --  FIXME: this is never free.
      return Unshare (Src, Heap_Pool'Access);
   end Unshare_Heap;

   procedure Free_Heap_Value (Acc : Iir_Value_Literal_Acc) is
   begin
      null;
   end Free_Heap_Value;

   function Get_Nbr_Of_Scalars (Val : Iir_Value_Literal_Acc) return Natural is
   begin
      case Val.Kind is
         when Iir_Value_Scalars
           | Iir_Value_Access
           | Iir_Value_Signal =>
            return 1;
         when Iir_Value_Record =>
            declare
               Total : Natural := 0;
            begin
               for I in Val.Val_Record.V'Range loop
                  Total := Total + Get_Nbr_Of_Scalars (Val.Val_Record.V (I));
               end loop;
               return Total;
            end;
         when Iir_Value_Array =>
            if Val.Val_Array.Len = 0 then
               --  Nul array
               return 0;
            else
               --  At least one element.
               return Natural (Val.Val_Array.Len)
                 * Get_Nbr_Of_Scalars (Val.Val_Array.V (1));
            end if;
         when Iir_Value_File
           | Iir_Value_Range
           | Iir_Value_Protected
           | Iir_Value_Terminal
           | Iir_Value_Quantity
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Get_Nbr_Of_Scalars;

   function Get_Enum_Pos (Val : Iir_Value_Literal_Acc) return Natural is
   begin
      case Val.Kind is
         when Iir_Value_E8 =>
            return Ghdl_E8'Pos (Val.E8);
         when Iir_Value_E32 =>
            return Ghdl_E32'Pos (Val.E32);
         when Iir_Value_B1 =>
            return Ghdl_B1'Pos (Val.B1);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Enum_Pos;

   procedure Put_Indent (Indent : Natural) is
   begin
      Simple_IO.Put ((1 .. 2 * Indent => ' '));
   end Put_Indent;

   procedure Disp_Value_Tab (Value: Iir_Value_Literal_Acc;
                             Indent : Natural)
   is
      use Simple_IO;
      use GNAT.Debug_Utilities;
   begin
      Put_Indent (Indent);
      if Value = null then
         Put_Line ("*NULL*");
         return;
      end if;

      if Boolean'(True) then
         Put (Image (Value.all'Address) & ' ');
      end if;

      case Value.Kind is
         when Iir_Value_B1 =>
            Put_Line ("b1:" & Ghdl_B1'Image (Value.B1));
         when Iir_Value_E8 =>
            Put_Line ("E8:" & Ghdl_E8'Image (Value.E8));
         when Iir_Value_E32 =>
            Put_Line ("e32:" & Ghdl_E32'Image (Value.E32));
         when Iir_Value_I64 =>
            Put_Line ("i64:" & Ghdl_I64'Image (Value.I64));
         when Iir_Value_F64 =>
            Put_Line ("F64:" & Ghdl_F64'Image (Value.F64));
         when Iir_Value_Access =>
            -- FIXME.
            if Value.Val_Access = null then
               Put_Line ("access: null");
            else
               Put ("access: ");
               Put_Line (Image (Value.Val_Access.all'Address));
            end if;
         when Iir_Value_Array =>
            if Value.Val_Array = null then
               Put_Line ("array, without elements");
               return;
            else
               Put_Line ("array, length: "
                         & Iir_Index32'Image (Value.Val_Array.Len));
               declare
                  Nindent: constant Natural := Indent + 1;
               begin
                  Put_Indent (Nindent);
                  if Value.Bounds /= null then
                     Put_Line ("bounds 1 .."
                               & Iir_Index32'Image (Value.Bounds.Nbr_Dims)
                               & ':');
                     for I in Value.Bounds.D'Range loop
                        Disp_Value_Tab (Value.Bounds.D (I), Nindent);
                     end loop;
                  else
                     Put_Line ("bounds = null");
                  end if;
                  Put_Indent (Nindent);
                  Put_Line ("values 1 .."
                            & Iir_Index32'Image (Value.Val_Array.Len)
                            & ':');
                  for I in Value.Val_Array.V'Range loop
                     Disp_Value_Tab (Value.Val_Array.V (I), Nindent);
                  end loop;
               end;
            end if;

         when Iir_Value_Range =>
            Put_Line ("range:");
            Put_Indent (Indent);
            Put (" direction: ");
            Put (Direction_Type'Image (Value.Dir));
            Put (", length:");
            Put_Line (Iir_Index32'Image (Value.Length));
            if Value.Left /= null then
               Put_Indent (Indent);
               Put (" left bound: ");
               Disp_Value_Tab (Value.Left, 0);
            end if;
            if Value.Right /= null then
               Put_Indent (Indent);
               Put (" right bound: ");
               Disp_Value_Tab (Value.Right, 0);
            end if;

         when Iir_Value_Record =>
            Put_Line ("record:");
            for I in Value.Val_Record.V'Range loop
               Disp_Value_Tab (Value.Val_Record.V (I), Indent + 1);
            end loop;
         when Iir_Value_Signal =>
            Put ("signal: ");
            if Value.Sig = null then
               Put_Line ("(not created)");
            else
               Put_Line (Image (Value.Sig.all'Address));
            end if;

         when Iir_Value_File =>
            Put_Line ("file:" & Grt.Files.Ghdl_File_Index'Image (Value.File));
         when Iir_Value_Protected =>
            Put_Line ("protected");
         when Iir_Value_Quantity =>
            Put_Line ("quantity");
         when Iir_Value_Terminal =>
            Put_Line ("terminal");
         when Iir_Value_Instance =>
            Put_Line ("instance");
      end case;
   end Disp_Value_Tab;

   procedure Disp_Value (Value: Iir_Value_Literal_Acc) is
   begin
      Disp_Value_Tab (Value, 0);
   end Disp_Value;

   --  Return TRUE if VALUE has an indirect value.
   function Is_Indirect (Value : Iir_Value_Literal_Acc) return Boolean is
   begin
      case Value.Kind is
         when Iir_Value_Scalars
           | Iir_Value_Access
           | Iir_Value_File
           | Iir_Value_Protected
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            return False;
         when Iir_Value_Range =>
            return Is_Indirect (Value.Left)
              or else Is_Indirect (Value.Right);
         when Iir_Value_Array =>
            for I in Value.Val_Array.V'Range loop
               if Is_Indirect (Value.Val_Array.V (I)) then
                  return True;
               end if;
            end loop;
            return False;
         when Iir_Value_Record =>
            for I in Value.Val_Record.V'Range loop
               if Is_Indirect (Value.Val_Record.V (I)) then
                  return True;
               end if;
            end loop;
            return False;
         when Iir_Value_Signal =>
            return True;
      end case;
   end Is_Indirect;

   procedure Disp_Iir_Value_Array (Value: Iir_Value_Literal_Acc;
                                   A_Type: Iir;
                                   Dim: Iir_Index32;
                                   Off : in out Iir_Index32)
   is
      use Simple_IO;
      type Last_Enum_Type is (None, Char, Identifier);
      Last_Enum: Last_Enum_Type;
      El_Type: Iir;
      Enum_List: Iir_Flist;
      El_Id : Name_Id;
      El_Pos : Natural;
   begin
      if Dim = Value.Bounds.Nbr_Dims then
         --  Last dimension
         El_Type := Get_Base_Type (Get_Element_Subtype (A_Type));

         --  Pretty print vectors of enumerated types
         if Get_Kind (El_Type) = Iir_Kind_Enumeration_Type_Definition
           and then not Is_Indirect (Value)
         then
            Last_Enum := None;
            Enum_List := Get_Enumeration_Literal_List (El_Type);
            for I in 1 .. Value.Bounds.D (Dim).Length loop
               El_Pos := Get_Enum_Pos (Value.Val_Array.V (Off));
               Off := Off + 1;
               El_Id := Get_Identifier (Get_Nth_Element (Enum_List, El_Pos));
               if Name_Table.Is_Character (El_Id) then
                  case Last_Enum is
                     when None =>
                        Put ("""");
                     when Identifier =>
                        Put (" & """);
                     when Char =>
                        null;
                  end case;
                  Put (Name_Table.Get_Character (El_Id));
                  Last_Enum := Char;
               else
                  case Last_Enum is
                     when None =>
                        null;
                     when Identifier =>
                        Put (" & ");
                     when Char =>
                        Put (""" & ");
                  end case;
                  Put (Name_Table.Image (El_Id));
                  Last_Enum := Identifier;
               end if;
            end loop;
            case Last_Enum is
               when None =>
                  Put ("""""");  --  Simply ""
               when Identifier =>
                  null;
               when Char =>
                  Put ("""");
            end case;
         else
            Put ("(");
            for I in 1 .. Value.Bounds.D (Dim).Length loop
               if I /= 1 then
                  Put (", ");
               end if;
               Disp_Iir_Value (Value.Val_Array.V (Off), El_Type);
               Off := Off + 1;
            end loop;
            Put (")");
         end if;
      else
         Put ("(");
         for I in 1 .. Value.Bounds.D (Dim).Length loop
            if I /= 1 then
               Put (", ");
            end if;
            Disp_Iir_Value_Array (Value, A_Type, Dim + 1, Off);
         end loop;
         Put (")");
      end if;
   end Disp_Iir_Value_Array;

   procedure Disp_Iir_Value_Record
     (Value: Iir_Value_Literal_Acc; A_Type: Iir)
   is
      use Simple_IO;
      List : constant Iir_Flist :=
        Get_Elements_Declaration_List (Get_Base_Type (A_Type));
      El : Iir_Element_Declaration;
   begin
      Put ("(");
      for I in Value.Val_Record.V'Range loop
         El := Get_Nth_Element (List, Natural (I - 1));
         if I /= 1 then
            Put (", ");
         end if;
         Put (Name_Table.Image (Get_Identifier (El)));
         Put (" => ");
         Disp_Iir_Value (Value.Val_Record.V (I), Get_Type (El));
      end loop;
      Put (")");
   end Disp_Iir_Value_Record;

   procedure Disp_Iir_Value_Enum (Pos : Natural; A_Type : Iir)
   is
      Bt : constant Iir := Get_Base_Type (A_Type);
      Id : Name_Id;
   begin
      Id := Get_Identifier
        (Get_Nth_Element (Get_Enumeration_Literal_List (Bt), Pos));
      Simple_IO.Put (Name_Table.Image (Id));
   end Disp_Iir_Value_Enum;

   procedure Disp_Iir_Value (Value: Iir_Value_Literal_Acc; A_Type: Iir)
   is
      use Simple_IO;
   begin
      if Value = null then
         Put ("!NULL!");
         return;
      end if;
      case Value.Kind is
         when Iir_Value_I64 =>
            Put (Ghdl_I64'Image (Value.I64));
         when Iir_Value_F64 =>
            Put (Ghdl_F64'Image (Value.F64));
         when Iir_Value_E32 =>
            Disp_Iir_Value_Enum (Ghdl_E32'Pos (Value.E32), A_Type);
         when Iir_Value_E8 =>
            Disp_Iir_Value_Enum (Ghdl_E8'Pos (Value.E8), A_Type);
         when Iir_Value_B1 =>
            Disp_Iir_Value_Enum (Ghdl_B1'Pos (Value.B1), A_Type);
         when Iir_Value_Access =>
            if Value.Val_Access = null then
               Put ("null");
            else
               -- FIXME.
               Put ("*acc*");
            end if;
         when Iir_Value_Array =>
            declare
               Off : Iir_Index32;
            begin
               Off := 1;
               Disp_Iir_Value_Array (Value, A_Type, 1, Off);
               pragma Assert (Off = Value.Val_Array.Len + 1);
            end;
         when Iir_Value_File =>
            raise Internal_Error;
         when Iir_Value_Record =>
            Disp_Iir_Value_Record (Value, A_Type);
         when Iir_Value_Range =>
            -- FIXME.
            raise Internal_Error;
         when Iir_Value_Quantity =>
            Put ("[quantity]");
         when Iir_Value_Terminal =>
            Put ("[terminal]");
         when Iir_Value_Signal =>
            Put ("[signal]");
         when Iir_Value_Protected =>
            Put ("[protected]");
         when Iir_Value_Instance =>
            Put ("[instance]");
      end case;
   end Disp_Iir_Value;
end Simul.Environments;
