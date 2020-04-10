--  Values in synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements;

with Vhdl.Nodes; use Vhdl.Nodes;

package body Synth.Values is
   function To_Value_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Acc);

   function Is_Static (Val : Value_Acc) return Boolean is
   begin
      case Val.Kind is
         when Value_Memory =>
            return True;
         when Value_Net
           | Value_Wire =>
            return False;
         when Value_File =>
            return True;
         when Value_Alias =>
            return Is_Static (Val.A_Obj);
         when Value_Const =>
            return True;
      end case;
   end Is_Static;

   function Is_Static_Val (Val : Value_Acc) return Boolean is
   begin
      case Val.Kind is
         when Value_Memory =>
            return True;
         when Value_Net =>
            return False;
         when Value_Wire =>
            return Is_Static_Wire (Val.W);
         when Value_File =>
            return True;
         when Value_Const =>
            return True;
         when Value_Alias =>
            return Is_Static_Val (Val.A_Obj);
      end case;
   end Is_Static_Val;

   function Strip_Alias_Const (V : Value_Acc) return Value_Acc
   is
      Res : Value_Acc;
   begin
      Res := V;
      loop
         case Res.Kind is
            when Value_Const =>
               Res := Res.C_Val;
            when Value_Alias =>
               if Res.A_Off /= (0, 0) then
                  raise Internal_Error;
               end if;
               Res := Res.A_Obj;
            when others =>
               return Res;
         end case;
      end loop;
   end Strip_Alias_Const;

   function Strip_Alias_Const (V : Valtyp) return Valtyp is
   begin
      return (V.Typ, Strip_Alias_Const (V.Val));
   end Strip_Alias_Const;

   function Is_Equal (L, R : Memtyp) return Boolean is
   begin
      if L = R then
         return True;
      end if;

      if L.Typ.Sz /= R.Typ.Sz then
         return False;
      end if;

      --  FIXME: not correct for records, not correct for floats!
      for I in 1 .. L.Typ.Sz loop
         if L.Mem (I - 1) /= R.Mem (I - 1) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Equal;

   function Is_Equal (L, R : Valtyp) return Boolean is
   begin
      return Is_Equal (Get_Memtyp (L), Get_Memtyp (R));
   end Is_Equal;

   function Create_Value_Wire (W : Wire_Id) return Value_Acc
   is
      subtype Value_Type_Wire is Value_Type (Values.Value_Wire);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Wire);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Wire,
                                   W => W)));
   end Create_Value_Wire;

   function Create_Value_Wire (W : Wire_Id; Wtype : Type_Acc) return Valtyp
   is
      pragma Assert (Wtype /= null);
   begin
      return (Wtype, Create_Value_Wire (W));
   end Create_Value_Wire;

   function Create_Value_Net (N : Net) return Value_Acc
   is
      subtype Value_Type_Net is Value_Type (Value_Net);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Net);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool, Value_Type_Net'(Kind => Value_Net, N => N)));
   end Create_Value_Net;

   function Create_Value_Net (N : Net; Ntype : Type_Acc) return Valtyp
   is
      pragma Assert (Ntype /= null);
   begin
      return (Ntype, Create_Value_Net (N));
   end Create_Value_Net;

   function Create_Value_Memory (Vtype : Type_Acc) return Valtyp
   is
      subtype Value_Type_Memory is Value_Type (Value_Memory);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Memory);
      function To_Memory_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Memory_Ptr);
      V : Value_Acc;
      M : System.Address;
   begin
      Areapools.Allocate (Current_Pool.all, M,
                          Vtype.Sz, Size_Type (2 ** Natural (Vtype.Al)));
      V := To_Value_Acc
        (Alloc (Current_Pool, Value_Type_Memory'(Kind => Value_Memory,
                                                 Mem => To_Memory_Ptr (M))));

      return (Vtype, V);
   end Create_Value_Memory;

   function Create_Value_Memory (Mt : Memtyp) return Valtyp
   is
      subtype Value_Type_Memory is Value_Type (Value_Memory);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Memory);
      V : Value_Acc;
   begin
      V := To_Value_Acc
        (Alloc (Current_Pool, Value_Type_Memory'(Kind => Value_Memory,
                                                 Mem => Mt.Mem)));

      return (Mt.Typ, V);
   end Create_Value_Memory;

   function Create_Value_File (File : File_Index) return Value_Acc
   is
      subtype Value_Type_File is Value_Type (Value_File);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_File);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_File, File => File)));
   end Create_Value_File;

   function Create_Value_File (Vtype : Type_Acc; File : File_Index)
                              return Valtyp
   is
      pragma Assert (Vtype /= null);
   begin
      return (Vtype, Create_Value_File (File));
   end Create_Value_File;

   function Vec_Length (Typ : Type_Acc) return Iir_Index32 is
   begin
      return Iir_Index32 (Typ.Vbound.Len);
   end Vec_Length;

   function Get_Array_Flat_Length (Typ : Type_Acc) return Iir_Index32 is
   begin
      case Typ.Kind is
         when Type_Vector =>
            return Iir_Index32 (Typ.Vbound.Len);
         when Type_Array =>
            declare
               Len : Width;
            begin
               Len := 1;
               for I in Typ.Abounds.D'Range loop
                  Len := Len * Typ.Abounds.D (I).Len;
               end loop;
               return Iir_Index32 (Len);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Get_Array_Flat_Length;

   function Create_Value_Alias
     (Obj : Valtyp; Off : Value_Offsets; Typ : Type_Acc) return Valtyp
   is
      pragma Assert (Typ /= null);
      subtype Value_Type_Alias is Value_Type (Value_Alias);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Alias);
      Val : Value_Acc;
   begin
      Val := To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Alias,
                                   A_Obj => Obj.Val,
                                   A_Typ => Obj.Typ,
                                   A_Off => Off)));
      return (Typ, Val);
   end Create_Value_Alias;

   function Create_Value_Const (Val : Value_Acc; Loc : Syn_Src)
                               return Value_Acc
   is
      subtype Value_Type_Const is Value_Type (Value_Const);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Const);
   begin
      pragma Assert (Val = null or else Val.Kind /= Value_Const);
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Const,
                                   C_Val => Val,
                                   C_Loc => Loc,
                                   C_Net => No_Net)));
   end Create_Value_Const;

   function Create_Value_Const (Val : Valtyp; Loc : Syn_Src)
                               return Valtyp is
   begin
      return (Val.Typ, Create_Value_Const (Val.Val, Loc));
   end Create_Value_Const;

   procedure Strip_Const (Vt : in out Valtyp) is
   begin
      if Vt.Val.Kind = Value_Const then
         Vt.Val := Vt.Val.C_Val;
      end if;
   end Strip_Const;

   procedure Copy_Memory (Dest : Memory_Ptr; Src : Memory_Ptr; Sz : Size_Type)
   is
   begin
      for I in 1 .. Sz loop
         Dest (I - 1) := Src (I - 1);
      end loop;
   end Copy_Memory;

   procedure Write_Value (Dest : Memory_Ptr; Vt : Valtyp)
   is
      Mt : Memtyp;
   begin
      Mt := Get_Memtyp (Vt);
      Copy_Memory (Dest, Mt.Mem, Mt.Typ.Sz);
   end Write_Value;

   function Copy (Src : Valtyp) return Valtyp
   is
      Res : Valtyp;
   begin
      case Src.Val.Kind is
         when Value_Memory =>
            Res := Create_Value_Memory (Src.Typ);
            for I in 1 .. Src.Typ.Sz loop
               Res.Val.Mem (I - 1) := Src.Val.Mem (I - 1);
            end loop;
         when Value_Net =>
            Res := Create_Value_Net (Src.Val.N, Src.Typ);
         when Value_Wire =>
            Res := Create_Value_Wire (Src.Val.W, Src.Typ);
         when Value_File =>
            Res := Create_Value_File (Src.Typ, Src.Val.File);
         when Value_Const =>
            raise Internal_Error;
         when Value_Alias =>
            raise Internal_Error;
      end case;
      return Res;
   end Copy;

   function Unshare (Src : Valtyp; Pool : Areapool_Acc) return Valtyp
   is
      Prev_Pool : constant Areapool_Acc := Current_Pool;
      Res : Valtyp;
   begin
      Current_Pool := Pool;
      Res := Copy (Src);
      Current_Pool := Prev_Pool;
      return Res;
   end Unshare;

   type Ghdl_U8_Ptr is access all Ghdl_U8;
   function To_U8_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Ghdl_U8_Ptr);

   procedure Write_U8 (Mem : Memory_Ptr; Val : Ghdl_U8) is
   begin
      To_U8_Ptr (Mem).all := Val;
   end Write_U8;

   function Read_U8 (Mem : Memory_Ptr) return Ghdl_U8 is
   begin
      return To_U8_Ptr (Mem).all;
   end Read_U8;

   type Ghdl_I32_Ptr is access all Ghdl_I32;
   function To_I32_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Ghdl_I32_Ptr);

   procedure Write_I32 (Mem : Memory_Ptr; Val : Ghdl_I32) is
   begin
      To_I32_Ptr (Mem).all := Val;
   end Write_I32;

   function Read_I32 (Mem : Memory_Ptr) return Ghdl_I32 is
   begin
      return To_I32_Ptr (Mem).all;
   end Read_I32;

   type Ghdl_U32_Ptr is access all Ghdl_U32;
   function To_U32_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Ghdl_U32_Ptr);

   procedure Write_U32 (Mem : Memory_Ptr; Val : Ghdl_U32) is
   begin
      To_U32_Ptr (Mem).all := Val;
   end Write_U32;

   function Read_U32 (Mem : Memory_Ptr) return Ghdl_U32 is
   begin
      return To_U32_Ptr (Mem).all;
   end Read_U32;

   type Ghdl_I64_Ptr is access all Ghdl_I64;
   function To_I64_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Ghdl_I64_Ptr);

   procedure Write_I64 (Mem : Memory_Ptr; Val : Ghdl_I64) is
   begin
      To_I64_Ptr (Mem).all := Val;
   end Write_I64;

   function Read_I64 (Mem : Memory_Ptr) return Ghdl_I64 is
   begin
      return To_I64_Ptr (Mem).all;
   end Read_I64;

   type Fp64_Ptr is access all Fp64;
   function To_Fp64_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Fp64_Ptr);

   procedure Write_Fp64 (Mem : Memory_Ptr; Val : Fp64) is
   begin
      To_Fp64_Ptr (Mem).all := Val;
   end Write_Fp64;

   function Read_Fp64 (Mem : Memory_Ptr) return Fp64 is
   begin
      return To_Fp64_Ptr (Mem).all;
   end Read_Fp64;

   function Read_Fp64 (Mt : Memtyp) return Fp64 is
   begin
      return Read_Fp64 (Mt.Mem);
   end Read_Fp64;

   type Heap_Index_Ptr is access all Heap_Index;
   function To_Heap_Index_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Heap_Index_Ptr);

   procedure Write_Access (Mem : Memory_Ptr; Val : Heap_Index) is
   begin
      To_Heap_Index_Ptr (Mem).all := Val;
   end Write_Access;

   function Read_Access (Mem : Memory_Ptr) return Heap_Index is
   begin
      return To_Heap_Index_Ptr (Mem).all;
   end Read_Access;

   function Read_Access (Mt : Memtyp) return Heap_Index is
   begin
      return Read_Access (Mt.Mem);
   end Read_Access;

   function "+" (Base : Memory_Ptr; Off : Size_Type) return Memory_Ptr
   is
      use System.Storage_Elements;

      function To_Address is new Ada.Unchecked_Conversion
        (Memory_Ptr, System.Address);
      function To_Memory_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Memory_Ptr);
   begin
      return To_Memory_Ptr (To_Address (Base) + Storage_Offset (Off));
   end "+";

   procedure Write_Discrete (Mem : Memory_Ptr; Typ : Type_Acc; Val : Int64) is
   begin
      case Typ.Sz is
         when 1 =>
            Write_U8 (Mem, Ghdl_U8 (Val));
         when 4 =>
            Write_I32 (Mem, Ghdl_I32 (Val));
         when 8 =>
            Write_I64 (Mem, Ghdl_I64 (Val));
         when others =>
            raise Internal_Error;
      end case;
   end Write_Discrete;

   procedure Write_Discrete (Vt : Valtyp; Val : Int64) is
   begin
      Write_Discrete (Vt.Val.Mem, Vt.Typ, Val);
   end Write_Discrete;

   function Read_Discrete (Mt : Memtyp) return Int64 is
   begin
      case Mt.Typ.Sz is
         when 1 =>
            return Int64 (Read_U8 (Mt.Mem));
         when 4 =>
            return Int64 (Read_I32 (Mt.Mem));
         when 8 =>
            return Int64 (Read_I64 (Mt.Mem));
         when others =>
            raise Internal_Error;
      end case;
   end Read_Discrete;

   function Read_Discrete (Vt : Valtyp) return Int64 is
   begin
      return Read_Discrete (Get_Memtyp (Vt));
   end Read_Discrete;

   function Create_Value_Float (Val : Fp64; Vtype : Type_Acc) return Valtyp
   is
      Res : Valtyp;
      pragma Assert (Vtype /= null);
   begin
      Res := Create_Value_Memory (Vtype);
      Write_Fp64 (Res.Val.Mem, Val);
      return Res;
   end Create_Value_Float;

   function Read_Fp64 (Vt : Valtyp) return Fp64 is
   begin
      pragma Assert (Vt.Typ.Kind = Type_Float);
      pragma Assert (Vt.Typ.Sz = 8);
      return Read_Fp64 (Vt.Val.Mem);
   end Read_Fp64;

   function Read_Access (Vt : Valtyp) return Heap_Index is
   begin
      pragma Assert (Vt.Typ.Kind = Type_Access);
      return Read_Access (Vt.Val.Mem);
   end Read_Access;

   function Create_Value_Discrete (Val : Int64; Vtype : Type_Acc) return Valtyp
   is
      Res : Valtyp;
   begin
      Res := Create_Value_Memory (Vtype);
      case Vtype.Sz is
         when 1 =>
            Write_U8 (Res.Val.Mem, Ghdl_U8 (Val));
         when 4 =>
            Write_I32 (Res.Val.Mem, Ghdl_I32 (Val));
         when 8 =>
            Write_I64 (Res.Val.Mem, Ghdl_I64 (Val));
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Value_Discrete;

   function Create_Value_Uns (Val : Uns64; Vtype : Type_Acc) return Valtyp
   is
      Res : Valtyp;
   begin
      Res := Create_Value_Memory (Vtype);
      case Vtype.Sz is
         when 1 =>
            Write_U8 (Res.Val.Mem, Ghdl_U8 (Val));
         when 4 =>
            Write_U32 (Res.Val.Mem, Ghdl_U32 (Val));
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Value_Uns;

   pragma Unreferenced (Read_U32);

   function Create_Value_Int (Val : Int64; Vtype : Type_Acc) return Valtyp
   is
      Res : Valtyp;
   begin
      Res := Create_Value_Memory (Vtype);
      case Vtype.Sz is
         when 4 =>
            Write_I32 (Res.Val.Mem, Ghdl_I32 (Val));
         when 8 =>
            Write_I64 (Res.Val.Mem, Ghdl_I64 (Val));
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Value_Int;

   function Arr_Index (M : Memory_Ptr; Idx : Iir_Index32; El_Typ : Type_Acc)
                      return Memory_Ptr is
   begin
      return M + Size_Type (Idx) * El_Typ.Sz;
   end Arr_Index;

   procedure Write_Value_Default (M : Memory_Ptr; Typ : Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic =>
            --  FIXME: what about subtype ?
            Write_U8 (M, 0);
         when Type_Discrete =>
            Write_Discrete (M, Typ, Typ.Drange.Left);
         when Type_Float =>
            Write_Fp64 (M, Typ.Frange.Left);
         when Type_Vector =>
            declare
               Len : constant Iir_Index32 := Vec_Length (Typ);
               El_Typ : constant Type_Acc := Typ.Vec_El;
            begin
               for I in 1 .. Len loop
                  Write_Value_Default (Arr_Index (M, I - 1, El_Typ), El_Typ);
               end loop;
            end;
         when Type_Unbounded_Vector =>
            raise Internal_Error;
         when Type_Slice =>
            raise Internal_Error;
         when Type_Array =>
            declare
               Len : constant Iir_Index32 := Get_Array_Flat_Length (Typ);
               El_Typ : constant Type_Acc := Typ.Arr_El;
            begin
               for I in 1 .. Len loop
                  Write_Value_Default (Arr_Index (M, I - 1, El_Typ), El_Typ);
               end loop;
            end;
         when Type_Unbounded_Array =>
            raise Internal_Error;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Write_Value_Default (M + Typ.Rec.E (I).Moff, Typ.Rec.E (I).Typ);
            end loop;
         when Type_Access =>
            Write_Access (M, Null_Heap_Index);
         when Type_File =>
            raise Internal_Error;
      end case;
   end Write_Value_Default;

   function Create_Value_Default (Typ : Type_Acc) return Valtyp
   is
      Res : Valtyp;
   begin
      Res := Create_Value_Memory (Typ);
      Write_Value_Default (Res.Val.Mem, Typ);
      return Res;
   end Create_Value_Default;

   function Create_Value_Access (Val : Heap_Index; Acc_Typ : Type_Acc)
                                return Valtyp
   is
      Res : Valtyp;
   begin
      Res := Create_Value_Memory (Acc_Typ);
      Write_Access (Res.Val.Mem, Val);
      return Res;
   end Create_Value_Access;

   function Value_To_String (Val : Valtyp) return String
   is
      Str : String (1 .. Natural (Val.Typ.Abounds.D (1).Len));
   begin
      for I in Str'Range loop
         Str (Natural (I)) := Character'Val
           (Read_U8 (Val.Val.Mem + Size_Type (I - 1)));
      end loop;
      return Str;
   end Value_To_String;

   function Get_Memtyp (V : Valtyp) return Memtyp is
   begin
      case V.Val.Kind is
         when Value_Net
           | Value_Wire =>
            raise Internal_Error;
         when Value_Memory =>
            return (V.Typ, V.Val.Mem);
         when Value_Alias =>
            declare
               T : Memtyp;
            begin
               T := Get_Memtyp ((V.Typ, V.Val.A_Obj));
               return (T.Typ, T.Mem + V.Val.A_Off.Mem_Off);
            end;
         when Value_Const =>
            return Get_Memtyp ((V.Typ, V.Val.C_Val));
         when Value_File =>
            raise Internal_Error;
      end case;
   end Get_Memtyp;
end Synth.Values;
