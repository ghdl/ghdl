--  Verilog associative arrays
--  Copyright (C) 2023 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Verilog.Errors; use Verilog.Errors;
with Verilog.Allocates; use Verilog.Allocates;
with Verilog.Executions;
with Verilog.Sv_Strings;

package body Verilog.Sv_Maps is
   function New_Sv_Map (Atype : Node) return Sv_Map
   is
      Idx_Type : constant Node := Get_Type_Index_Type (Atype);
      El_Type : constant Node := Get_Type_Element_Type (Atype);
   begin
      return new Sv_Map_Type'(Ref => 1,
                              El_Size => Get_Storage_Size (El_Type),
                              El_Type => El_Type,
                              Idx_Type => Idx_Type,
                              Len => 0,
                              Default => No_Data_Ptr,
                              Top => null);
   end New_Sv_Map;

   function Compare (L, R : Data_Ptr; Typ : Node) return Order_Type is
   begin
      case Get_Kind (Typ) is
         when N_String_Type =>
            declare
               use Verilog.Sv_Strings;
               Ls : constant Sv_String := To_Sv_String_Ptr (L).all;
               Rs : constant Sv_String := To_Sv_String_Ptr (R).all;
            begin
               return Compare (Ls, Rs);
            end;
         when others =>
            Error_Kind ("sv_maps.compare", Typ);
      end case;
   end Compare;

   procedure Assign (Dest : in out Data_Ptr; Val : Data_Ptr; Typ : Node)
   is
      pragma Unreferenced (Dest, Val);
   begin
      case Get_Kind (Typ) is
         when others =>
            Error_Kind ("sv_maps.assign", Typ);
      end case;
   end Assign;

   function Allocate (Val : Data_Ptr; Typ : Node) return Data_Ptr
   is
      Res : Data_Ptr;
      Ssize : Storage_Index;
   begin
      case Get_Kind (Typ) is
         when N_String_Type
           | N_Bit_Packed_Array_Cst =>
            Ssize := Get_Storage_Size (Typ);
            Res := Malloc (Ssize);
            Executions.Execute_Simple_Copy (Res, Val, Typ);
            return Res;
--         when N_Bit_Type =>
--            To_Bit_Ptr (Res'Address).all := To_Bit_Ptr (Val).all;
         when others =>
            Error_Kind ("sv_maps.allocate", Typ);
      end case;
   end Allocate;

   function Read_Value (Val : Data_Ptr; Typ : Node) return Data_Ptr is
   begin
      case Get_Kind (Typ) is
         when N_String_Type
           | N_Bit_Packed_Array_Cst =>
            return Val;
         when others =>
            Error_Kind ("sv_maps.read_value", Typ);
      end case;
   end Read_Value;

   procedure Set_Map (Map : Sv_Map; Idx : Data_Ptr; Val : Data_Ptr)
   is
      Last, N : Map_Node_Acc;
      Cmp : Order_Type;
   begin
      Last := null;
      N := Map.Top;
      while N /= null loop
         Last := N;
         Cmp := Compare (Idx, N.Idx, Map.Idx_Type);
         case Cmp is
            when Equal =>
               Assign (N.Value, Val, Map.El_Type);
               return;
            when Greater =>
               N := N.Right;
            when Less =>
               N := N.Left;
         end case;
      end loop;

      N := new Map_Node'(Left => null,
                         Right => null,
                         Parent => null,
                         Idx => Allocate (Idx, Map.Idx_Type),
                         Value => Allocate (Val, Map.El_Type));
      if Last = null then
         Map.Top := N;
      else
         case Cmp is
            when Greater =>
               pragma Assert (Last.Right = null);
               Last.Right := N;
               N.Parent := Last;
            when Less =>
               pragma Assert (Last.Left = null);
               Last.Left := N;
               N.Parent := Last;
            when Equal =>
               raise Internal_Error;
         end case;
      end if;
   end Set_Map;

   function Get_Map (Map : Sv_Map; Idx : Data_Ptr) return Data_Ptr
   is
      Last, N : Map_Node_Acc;
      Cmp : Order_Type;
   begin
      Last := null;
      N := Map.Top;
      while N /= null loop
         Last := N;
         Cmp := Compare (Idx, N.Idx, Map.Idx_Type);
         case Cmp is
            when Equal =>
               return Read_Value (N.Value, Map.El_Type);
            when Greater =>
               N := N.Right;
            when Less =>
               N := N.Left;
         end case;
      end loop;

      --  Not found.
      pragma Unreferenced (Last);
      raise Internal_Error;
   end Get_Map;

   procedure Iterator_Init (It : out Sv_Map_Iterator; Map : Sv_Map)
   is
      N : Map_Node_Acc;
   begin
      N := Map.Top;
      while N /= null loop
         exit when N.Left = null;
         N := N.Left;
      end loop;
      It := (N => N);
   end Iterator_Init;

   function Iterator_Done (It : Sv_Map_Iterator) return Boolean is
   begin
      return It.N = null;
   end Iterator_Done;

   function Iterator_Get_Index (It : Sv_Map_Iterator) return Data_Ptr is
   begin
      return It.N.Idx;
   end Iterator_Get_Index;

   procedure Iterator_Next (It : in out Sv_Map_Iterator)
   is
      N, P : Map_Node_Acc;
   begin
      --  This node has been handled.
      N := It.N;

      if N.Right /= null then
         --  Return the left-est node on the right.
         N := N.Right;
         while N.Left /= null loop
            N := N.Left;
         end loop;
         It.N := N;
         return;
      else
         loop
            P := N.Parent;
            if P = null then
               --  End.
               It.N := null;
               return;
            end if;
            if P.Left = N then
               --  Comes from the left, so return the parent.
               It.N := P;
               return;
            end if;
            --  Comes from the right, so the parent has already been handled.
            --  Continue the walk.
            N := P;
         end loop;
      end if;
   end Iterator_Next;

end Verilog.Sv_Maps;
