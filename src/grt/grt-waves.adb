--  GHDL Run Time (GRT) - wave dumper (GHW) module.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with System; use System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Stdio; use Grt.Stdio;
with Grt.C; use Grt.C;
with Grt.Errors; use Grt.Errors;
with Grt.Astdio; use Grt.Astdio;
with Grt.Callbacks; use Grt.Callbacks;
with Grt.Hooks; use Grt.Hooks;
with Grt.Table;
with Grt.Avls; use Grt.Avls;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Rtis_Utils;
with Grt.Rtis_Types;
with Grt.Signals; use Grt.Signals;
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Ghw; use Grt.Ghw;
with Grt.Wave_Opt; use Grt.Wave_Opt;
with Grt.Wave_Opt.File; use Grt.Wave_Opt.File;
with Grt.Wave_Opt.Design; use Grt.Wave_Opt.Design;
with Grt.Algos; use Grt.Algos;

pragma Elaborate_All (Grt.Rtis_Utils);
pragma Elaborate_All (Grt.Table);

package body Grt.Waves is
   --  Waves filename.
   Wave_Filename : String_Access := null;
   --  Stream corresponding to the GHW filename.
   Wave_Stream : FILEs;

   --  Return TRUE if OPT is an option for wave.
   function Wave_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
   begin
      if Opt'Length < 6 or else Opt (F .. F + 5) /= "--wave" then
         return False;
      end if;
      if Opt'Length > 6 and then Opt (F + 6) = '=' then
         --  Add an extra NUL character.
         Wave_Filename := new String (1 .. Opt'Length - 7 + 1);
         Wave_Filename (1 .. Opt'Length - 7) := Opt (F + 7 .. Opt'Last);
         Wave_Filename (Wave_Filename'Last) := NUL;
         return True;
      else
         return False;
      end if;
   end Wave_Option;

   procedure Wave_Help is
   begin
      Put_Line (" --wave=FILENAME    dump signal values into a wave file");
   end Wave_Help;

   procedure Wave_Put (Str : String)
   is
      R : size_t;
      pragma Unreferenced (R);
   begin
      R := fwrite (Str'Address, Str'Length, 1, Wave_Stream);
   end Wave_Put;

   procedure Wave_Putc (C : Character)
   is
      R : int;
      pragma Unreferenced (R);
   begin
      R := fputc (Character'Pos (C), Wave_Stream);
   end Wave_Putc;

   procedure Wave_Newline is
   begin
      Wave_Putc (Nl);
   end Wave_Newline;

   procedure Wave_Put_Byte (B : Unsigned_8)
   is
      V : Unsigned_8 := B;
      R : size_t;
      pragma Unreferenced (R);
   begin
      R := fwrite (V'Address, 1, 1, Wave_Stream);
   end Wave_Put_Byte;

   procedure Wave_Put_ULEB128 (Val : Ghdl_E32)
   is
      V : Ghdl_E32;
      R : Ghdl_E32;
   begin
      V := Val;
      loop
         R := V mod 128;
         V := V / 128;
         if V = 0 then
            Wave_Put_Byte (Unsigned_8 (R));
            exit;
         else
            Wave_Put_Byte (Unsigned_8 (128 + R));
         end if;
      end loop;
   end Wave_Put_ULEB128;

   procedure Wave_Put_SLEB128 (Val : Ghdl_I32)
   is
      function To_Ghdl_U32 is new Ada.Unchecked_Conversion
        (Ghdl_I32, Ghdl_U32);
      V : Ghdl_U32 := To_Ghdl_U32 (Val);

--        function Shift_Right_Arithmetic (Value : Ghdl_U32; Amount : Natural)
--                                        return Ghdl_U32;
--        pragma Import (Intrinsic, Shift_Right_Arithmetic);
      R : Unsigned_8;
   begin
      loop
         R := Unsigned_8 (V mod 128);
         V := Shift_Right_Arithmetic (V, 7);
         if (V = 0 and (R and 16#40#) = 0) or (V = -1 and (R and 16#40#) /= 0)
         then
            Wave_Put_Byte (R);
            exit;
         else
            Wave_Put_Byte (R or 16#80#);
         end if;
      end loop;
   end Wave_Put_SLEB128;

   procedure Wave_Put_LSLEB128 (Val : Ghdl_I64)
   is
      function To_Ghdl_U64 is new Ada.Unchecked_Conversion
        (Ghdl_I64, Ghdl_U64);
      V : Ghdl_U64 := To_Ghdl_U64 (Val);

      R : Unsigned_8;
   begin
      loop
         R := Unsigned_8 (V mod 128);
         V := Shift_Right_Arithmetic (V, 7);
         if (V = 0 and (R and 16#40#) = 0) or (V = -1 and (R and 16#40#) /= 0)
         then
            Wave_Put_Byte (R);
            exit;
         else
            Wave_Put_Byte (R or 16#80#);
         end if;
      end loop;
   end Wave_Put_LSLEB128;

   procedure Wave_Put_I32 (Val : Ghdl_I32)
   is
      V : Ghdl_I32 := Val;
      R : size_t;
      pragma Unreferenced (R);
   begin
      R := fwrite (V'Address, 4, 1, Wave_Stream);
   end Wave_Put_I32;

   procedure Wave_Put_I64 (Val : Ghdl_I64)
   is
      V : Ghdl_I64 := Val;
      R : size_t;
      pragma Unreferenced (R);
   begin
      R := fwrite (V'Address, 8, 1, Wave_Stream);
   end Wave_Put_I64;

   procedure Wave_Put_F64 (F64 : Ghdl_F64)
   is
      V : Ghdl_F64 := F64;
      R : size_t;
      pragma Unreferenced (R);
   begin
      R := fwrite (V'Address, Ghdl_F64'Size / Storage_Unit, 1, Wave_Stream);
   end Wave_Put_F64;

   procedure Wave_Puts (Str : Ghdl_C_String) is
   begin
      Put (Wave_Stream, Str);
   end Wave_Puts;

   procedure Write_Value (Value : Ghdl_Value_Ptr; Mode : Mode_Type) is
   begin
      case Mode is
         when Mode_B1 =>
            Wave_Put_Byte (Ghdl_B1'Pos (Value.B1));
         when Mode_E8 =>
            Wave_Put_Byte (Ghdl_E8'Pos (Value.E8));
         when Mode_E32 =>
            Wave_Put_ULEB128 (Value.E32);
         when Mode_I32 =>
            Wave_Put_SLEB128 (Value.I32);
         when Mode_I64 =>
            Wave_Put_LSLEB128 (Value.I64);
         when Mode_F64 =>
            Wave_Put_F64 (Value.F64);
      end case;
   end Write_Value;

   subtype Section_Name is String (1 .. 4);
   type Header_Type is record
      Name : Section_Name;
      Pos : long;
   end record;

   package Section_Table is new Grt.Table
     (Table_Component_Type => Header_Type,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   --  Create a new section.
   --  Write the header in the file.
   --  Save the location for the directory.
   procedure Wave_Section (Name : Section_Name) is
   begin
      Section_Table.Append (Header_Type'(Name => Name,
                                         Pos => ftell (Wave_Stream)));
      Wave_Put (Name);
   end Wave_Section;

   procedure Wave_Write_Size_Order is
   begin
      --  Byte order, 1 byte.
      --  0: bad, 1 : little-endian, 2 : big endian.
      declare
         type Byte_Arr is array (0 .. 3) of Unsigned_8;
         function To_Byte_Arr is new Ada.Unchecked_Conversion
           (Source => Unsigned_32, Target => Byte_Arr);
         B4 : constant Byte_Arr := To_Byte_Arr (16#11_22_33_44#);
         V : Unsigned_8;
      begin
         if B4 (0) = 16#11# then
            --  Big endian.
            V := 2;
         elsif B4 (0) = 16#44# then
            --  Little endian.
            V := 1;
         else
            --  Unknown endian.
            V := 0;
         end if;
         Wave_Put_Byte (V);
      end;
      --  Word size, 1 byte.
      Wave_Put_Byte (Integer'Size / 8);
      --  File offset size, 1 byte
      Wave_Put_Byte (1);
      --  Unused, must be zero (MBZ).
      Wave_Put_Byte (0);
   end Wave_Write_Size_Order;

   procedure Wave_Write_Directory
   is
      Pos : long;
   begin
      Pos := ftell (Wave_Stream);
      Wave_Section ("DIR" & NUL);
      Wave_Write_Size_Order;
      Wave_Put_I32 (Ghdl_I32 (Section_Table.Last));
      for I in Section_Table.First .. Section_Table.Last loop
         Wave_Put (Section_Table.Table (I).Name);
         Wave_Put_I32 (Ghdl_I32 (Section_Table.Table (I).Pos));
      end loop;
      Wave_Put ("EOD" & NUL);

      Wave_Section ("TAI" & NUL);
      Wave_Write_Size_Order;
      Wave_Put_I32 (Ghdl_I32 (Pos));
   end Wave_Write_Directory;

   --  Called before elaboration.
   procedure Wave_Init
   is
      Mode : constant String := "wb" & NUL;
   begin
      if Wave_Filename = null then
         Wave_Stream := NULL_Stream;
         return;
      end if;
      if Wave_Filename.all = "-" & NUL then
         Wave_Stream := stdout;
      else
         Wave_Stream := fopen (Wave_Filename.all'Address, Mode'Address);
         if Wave_Stream = NULL_Stream then
            Error_S ("cannot open ");
            Error_E (Wave_Filename (Wave_Filename'First
                                   .. Wave_Filename'Last - 1));
            return;
         end if;
      end if;
   end Wave_Init;

   procedure Write_File_Header is
   begin
      --  Magic, 9 bytes.
      Wave_Put ("GHDLwave" & Nl);
      --  Header length.
      Wave_Put_Byte (16);
      --  Version-major, 1 byte.
      Wave_Put_Byte (0);
      --  Version-minor, 1 byte.
      Wave_Put_Byte (1);

      Wave_Write_Size_Order;

      --  TODO: add time resolution.
   end Write_File_Header;

   procedure Avhpi_Error (Err : AvhpiErrorT)
   is
      pragma Unreferenced (Err);
   begin
      Put_Line ("Waves.Avhpi_Error!");
      null;
   end Avhpi_Error;

   package Str_Table is new Grt.Table
     (Table_Component_Type => Ghdl_C_String,
      Table_Index_Type => AVL_Value,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   package Str_AVL is new Grt.Table
     (Table_Component_Type => AVL_Node,
      Table_Index_Type => AVL_Nid,
      Table_Low_Bound => AVL_Root,
      Table_Initial => 16);

   Strings_Len : Natural := 0;

   function Str_Compare (L, R : AVL_Value) return Integer
   is
      Ls, Rs : Ghdl_C_String;
   begin
      Ls := Str_Table.Table (L);
      Rs := Str_Table.Table (R);
      if L = R then
         return 0;
      end if;
      return strcmp (Ls, Rs);
   end Str_Compare;

   procedure Disp_Str_Avl (N : AVL_Nid) is
   begin
      Put (stdout, "node: ");
      Put_I32 (stdout, Ghdl_I32 (N));
      New_Line (stdout);
      Put (stdout, " left: ");
      Put_I32 (stdout, Ghdl_I32 (Str_AVL.Table (N).Left));
      New_Line (stdout);
      Put (stdout, " right: ");
      Put_I32 (stdout, Ghdl_I32 (Str_AVL.Table (N).Right));
      New_Line (stdout);
      Put (stdout, " height: ");
      Put_I32 (stdout, Str_AVL.Table (N).Height);
      New_Line (stdout);
      Put (stdout, " str: ");
      --Put (stdout, Str_AVL.Table (N).Val);
      New_Line (stdout);
   end Disp_Str_Avl;

   pragma Unreferenced (Disp_Str_Avl);

   function Create_Str_Index (Str : Ghdl_C_String) return AVL_Value
   is
      Res : AVL_Nid;
   begin
      Str_Table.Append (Str);
      Str_AVL.Append (AVL_Node'(Val => Str_Table.Last,
                                Left | Right => AVL_Nil,
                                Height => 1));
      Get_Node (AVL_Tree (Str_AVL.Table (Str_AVL.First .. Str_AVL.Last)),
                Str_Compare'Access,
                Str_AVL.Last, Res);
      if Res /= Str_AVL.Last then
         Str_AVL.Decrement_Last;
         Str_Table.Decrement_Last;
      else
         Strings_Len := Strings_Len + strlen (Str);
      end if;
      return Str_AVL.Table (Res).Val;
   end Create_Str_Index;

   pragma Unreferenced (Create_Str_Index);

   procedure Create_String_Id (Str : Ghdl_C_String)
   is
      Res : AVL_Nid;
   begin
      if Str = null then
         return;
      end if;
      Str_Table.Append (Str);
      Str_AVL.Append (AVL_Node'(Val => Str_Table.Last,
                                Left | Right => AVL_Nil,
                                Height => 1));
      Get_Node (AVL_Tree (Str_AVL.Table (Str_AVL.First .. Str_AVL.Last)),
                Str_Compare'Access,
                Str_AVL.Last, Res);
      if Res /= Str_AVL.Last then
         Str_AVL.Decrement_Last;
         Str_Table.Decrement_Last;
      else
         Strings_Len := Strings_Len + strlen (Str);
      end if;
   end Create_String_Id;

   function Get_String (Str : Ghdl_C_String) return AVL_Value
   is
      H, L, M : AVL_Value;
      Diff : Integer;
   begin
      L := Str_Table.First;
      H := Str_Table.Last;
      loop
         M := (L + H) / 2;
         Diff := strcmp (Str, Str_Table.Table (M));
         if Diff = 0 then
            return M;
         elsif Diff < 0 then
            H := M - 1;
         else
            L := M + 1;
         end if;
         exit when L > H;
      end loop;
      return 0;
   end Get_String;

   procedure Write_String_Id (Str : Ghdl_C_String) is
   begin
      if Str = null then
         Wave_Put_Byte (0);
      else
         Wave_Put_ULEB128 (Ghdl_E32 (Get_String (Str)));
      end if;
   end Write_String_Id;

   type Type_Node is record
      Type_Rti : Ghdl_Rti_Access;
      Context : Rti_Context;
   end record;

   package Types_Table is new Grt.Table
     (Table_Component_Type => Type_Node,
      Table_Index_Type => AVL_Value,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   package Types_AVL is new Grt.Table
     (Table_Component_Type => AVL_Node,
      Table_Index_Type => AVL_Nid,
      Table_Low_Bound => AVL_Root,
      Table_Initial => 16);

   function Type_Compare (L, R : AVL_Value) return Integer
   is
      function To_Ia is new
        Ada.Unchecked_Conversion (Ghdl_Rti_Access, Integer_Address);

      function "<" (L, R : Ghdl_Rti_Access) return Boolean is
      begin
         return To_Ia (L) < To_Ia (R);
      end "<";

      Ls : Type_Node renames Types_Table.Table (L);
      Rs : Type_Node renames Types_Table.Table (R);
   begin
      if Ls.Type_Rti /= Rs.Type_Rti then
         if Ls.Type_Rti < Rs.Type_Rti then
            return -1;
         else
            return 1;
         end if;
      end if;
      if Ls.Context.Block /= Rs.Context.Block then
         if Ls.Context.Block < Rs.Context.Block then
            return -1;
         else
            return +1;
         end if;
      end if;
      if Ls.Context.Base /= Rs.Context.Base then
         if Ls.Context.Base < Rs.Context.Base then
            return -1;
         else
            return +1;
         end if;
      end if;
      return 0;
   end Type_Compare;

   --  Try to find type (RTI, CTXT) in the types_AVL table.
   --  The first step is to canonicalize CTXT, so that it is the CTXT of
   --   the type (and not a sub-scope of it).
   procedure Find_Type (Rti : Ghdl_Rti_Access;
                        Ctxt : Rti_Context;
                        N_Ctxt : out Rti_Context;
                        Id : out AVL_Nid)
   is
      Depth : Ghdl_Rti_Depth;
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_E8 =>
            N_Ctxt := Null_Context;
         when Ghdl_Rtik_Port
           | Ghdl_Rtik_Signal =>
            N_Ctxt := Ctxt;
         when others =>
            --  Compute the canonical context.
            if Rti.Max_Depth < Rti.Depth then
               Internal_Error ("grt.waves.find_type");
            end if;
            Depth := Rti.Max_Depth;
            if Depth = 0 or else Ctxt.Block = null then
               N_Ctxt := Null_Context;
            else
               N_Ctxt := Ctxt;
               while N_Ctxt.Block.Depth > Depth loop
                  N_Ctxt := Get_Parent_Context (N_Ctxt);
               end loop;
            end if;
      end case;

      --  If the type is already known, return now.
      --  Otherwise, ID is set to AVL_Nil.
      Types_Table.Append (Type_Node'(Type_Rti => Rti, Context => N_Ctxt));
      Id := Find_Node
        (AVL_Tree (Types_AVL.Table (Types_AVL.First .. Types_AVL.Last)),
         Type_Compare'Access,
         Types_Table.Last);
      Types_Table.Decrement_Last;
   end Find_Type;

   procedure Write_Type_Id (Tid : AVL_Nid) is
   begin
      Wave_Put_ULEB128 (Ghdl_E32 (Types_AVL.Table (Tid).Val));
   end Write_Type_Id;

   procedure Write_Type_Id (Rti : Ghdl_Rti_Access; Ctxt : Rti_Context)
   is
      N_Ctxt : Rti_Context;
      Res : AVL_Nid;
   begin
      Find_Type (Rti, Ctxt, N_Ctxt, Res);
      if Res = AVL_Nil then
         -- raise Program_Error;
         Internal_Error ("write_type_id");
      end if;
      Write_Type_Id (Res);
   end Write_Type_Id;

   procedure Add_Type (Rti : Ghdl_Rti_Access; Ctxt : Rti_Context)
   is
      Res : AVL_Nid;
   begin
      --  Then, create the type.
      Types_Table.Append (Type_Node'(Type_Rti => Rti, Context => Ctxt));
      Types_AVL.Append (AVL_Node'(Val => Types_Table.Last,
                                  Left | Right => AVL_Nil,
                                  Height => 1));

      Get_Node
        (AVL_Tree (Types_AVL.Table (Types_AVL.First .. Types_AVL.Last)),
         Type_Compare'Access,
         Types_AVL.Last, Res);
      if Res /= Types_AVL.Last then
         --raise Program_Error;
         Internal_Error ("wave.create_type(2)");
      end if;
   end Add_Type;

   procedure Create_Type (Rti : Ghdl_Rti_Access; Ctxt : Rti_Context)
   is
      N_Ctxt : Rti_Context;
      Res : AVL_Nid;
   begin
      Find_Type (Rti, Ctxt, N_Ctxt, Res);
      if Res /= AVL_Nil then
         return;
      end if;

      --  First, create all the types it depends on.
      case Rti.Kind is
         when Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_E8 =>
            declare
               Enum : constant Ghdl_Rtin_Type_Enum_Acc :=
                 To_Ghdl_Rtin_Type_Enum_Acc (Rti);
            begin
               Create_String_Id (Enum.Name);
               for I in 1 .. Enum.Nbr loop
                  Create_String_Id (Enum.Names (I - 1));
               end loop;
            end;
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64
           | Ghdl_Rtik_Type_F64 =>
            declare
               Base : constant Ghdl_Rtin_Type_Scalar_Acc :=
                 To_Ghdl_Rtin_Type_Scalar_Acc (Rti);
            begin
               Create_String_Id (Base.Name);
            end;
         when Ghdl_Rtik_Type_P32
           | Ghdl_Rtik_Type_P64 =>
            declare
               Base : constant Ghdl_Rtin_Type_Physical_Acc :=
                 To_Ghdl_Rtin_Type_Physical_Acc (Rti);
               Unit_Name : Ghdl_C_String;
            begin
               Create_String_Id (Base.Name);
               for I in 1 .. Base.Nbr loop
                  Unit_Name :=
                    Rtis_Utils.Get_Physical_Unit_Name (Base.Units (I - 1));
                  Create_String_Id (Unit_Name);
               end loop;
            end;
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Sub : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
                 To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti);
            begin
               Create_String_Id (Sub.Name);
               Create_Type (Sub.Basetype, N_Ctxt);
            end;
         when Ghdl_Rtik_Type_Array =>
            declare
               Arr : constant Ghdl_Rtin_Type_Array_Acc :=
                 To_Ghdl_Rtin_Type_Array_Acc (Rti);
            begin
               Create_String_Id (Arr.Name);
               Create_Type (Arr.Element, N_Ctxt);
               for I in 1 .. Arr.Nbr_Dim loop
                  Create_Type (Arr.Indexes (I - 1), N_Ctxt);
               end loop;
            end;
         when Ghdl_Rtik_Type_Record
           |  Ghdl_Rtik_Type_Unbounded_Record =>
            declare
               Rec : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Rti);
               El : Ghdl_Rtin_Element_Acc;
            begin
               Create_String_Id (Rec.Name);
               for I in 1 .. Rec.Nbrel loop
                  El := To_Ghdl_Rtin_Element_Acc (Rec.Elements (I - 1));
                  Create_String_Id (El.Name);
                  Create_Type (El.Eltype, N_Ctxt);
               end loop;
            end;
         when Ghdl_Rtik_Subtype_Array
            | Ghdl_Rtik_Subtype_Record
            | Ghdl_Rtik_Subtype_Unbounded_Record
            | Ghdl_Rtik_Subtype_Unbounded_Array =>
            declare
               Arr : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
               B_Ctxt : Rti_Context;
            begin
               Create_String_Id (Arr.Name);
               if Rti_Complex_Type (Rti) then
                  B_Ctxt := Ctxt;
               else
                  B_Ctxt := N_Ctxt;
               end if;
               Create_Type (Arr.Basetype, B_Ctxt);
            end;
         when others =>
            Internal_Error ("wave.create_type");
--              Internal_Error ("wave.create_type: does not handle " &
--                             Ghdl_Rtik'Image (Rti.Kind));
      end case;

      --  Then, create the type.
      Add_Type (Rti, N_Ctxt);
   end Create_Type;

   procedure Create_Object_Type (Obj : VhpiHandleT)
   is
      Obj_Type : VhpiHandleT;
      Error : AvhpiErrorT;
      Rti : Ghdl_Rti_Access;
   begin
      --  Extract type of the signal.
      Vhpi_Handle (VhpiSubtype, Obj, Obj_Type, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;
      Rti := Avhpi_Get_Rti (Obj_Type);
      Create_Type (Rti, Avhpi_Get_Context (Obj_Type));

      --  The the signal type is an unbounded type, also put the object
      --  in the type AVL.  This is for unbounded ports.
      --  The real type will be written to the file.
      case Rti.Kind is
         when Ghdl_Rtik_Type_Array
            | Ghdl_Rtik_Subtype_Unbounded_Array
            | Ghdl_Rtik_Type_Unbounded_Record
            | Ghdl_Rtik_Subtype_Unbounded_Record =>
            Add_Type (Avhpi_Get_Rti (Obj), Avhpi_Get_Context (Obj));
         when others =>
            null;
      end case;
   end Create_Object_Type;

   procedure Write_Object_Type (Obj : VhpiHandleT)
   is
      Obj_Type : VhpiHandleT;
      Error : AvhpiErrorT;
      Rti : Ghdl_Rti_Access;
   begin
      --  Extract type of the signal.
      Vhpi_Handle (VhpiSubtype, Obj, Obj_Type, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;
      Rti := Avhpi_Get_Rti (Obj_Type);
      case Rti.Kind is
         when Ghdl_Rtik_Type_Array
            | Ghdl_Rtik_Subtype_Unbounded_Array
            | Ghdl_Rtik_Type_Unbounded_Record
            | Ghdl_Rtik_Subtype_Unbounded_Record =>
            Write_Type_Id (Avhpi_Get_Rti (Obj), Avhpi_Get_Context (Obj));
         when others =>
            Write_Type_Id (Rti, Avhpi_Get_Context (Obj_Type));
      end case;
   end Write_Object_Type;

   procedure Create_Generate_Type (Gen : VhpiHandleT)
   is
      Iterator : VhpiHandleT;
      Error : AvhpiErrorT;
   begin
      --  Extract the iterator.
      Vhpi_Handle (VhpiIterScheme, Gen, Iterator, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;
      Create_Object_Type (Iterator);
   end Create_Generate_Type;

   procedure Write_Generate_Type_And_Value (Gen : VhpiHandleT)
   is
      Iter : VhpiHandleT;
      Iter_Type : VhpiHandleT;
      Error : AvhpiErrorT;
      Addr : Address;
      Mode : Mode_Type;
      Rti : Ghdl_Rti_Access;
   begin
      --  Extract the iterator.
      Vhpi_Handle (VhpiIterScheme, Gen, Iter, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;
      Write_Object_Type (Iter);

      Vhpi_Handle (VhpiSubtype, Iter, Iter_Type, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;
      Rti := Avhpi_Get_Rti (Iter_Type);
      Addr := Avhpi_Get_Address (Iter);

      case Get_Base_Type (Rti).Kind is
         when Ghdl_Rtik_Type_B1 =>
            Mode := Mode_B1;
         when Ghdl_Rtik_Type_E8 =>
            Mode := Mode_E8;
         when Ghdl_Rtik_Type_E32 =>
            Mode := Mode_E32;
         when Ghdl_Rtik_Type_I32 =>
            Mode := Mode_I32;
         when Ghdl_Rtik_Type_I64 =>
            Mode := Mode_I64;
         when Ghdl_Rtik_Type_F64 =>
            Mode := Mode_F64;
         when others =>
            Internal_Error ("bad iterator type");
      end case;
      Write_Value (To_Ghdl_Value_Ptr (Addr), Mode);
   end Write_Generate_Type_And_Value;

   type Step_Type is (Step_Name, Step_Hierarchy);

   Nbr_Scopes : Natural := 0;
   Nbr_Scope_Signals : Natural := 0;
   Nbr_Dumped_Signals : Natural := 0;

   --  This is only valid during write_hierarchy.
   function Get_Signal_Number (Sig : Ghdl_Signal_Ptr) return Natural
   is
      function To_Integer_Address is new Ada.Unchecked_Conversion
        (Ghdl_Signal_Ptr, Integer_Address);
   begin
      return Natural (To_Integer_Address (Sig.Alink));
   end Get_Signal_Number;

   procedure Write_Signal_Number (Val_Addr : Address;
                                  Val_Name : Vstring;
                                  Val_Type : Ghdl_Rti_Access;
                                  Param_Type : Natural)
   is
      pragma Unreferenced (Val_Name);
      pragma Unreferenced (Val_Type);
      pragma Unreferenced (Param_Type);

      Num : Natural;

      function To_Ghdl_Signal_Ptr is new Ada.Unchecked_Conversion
        (Source => Integer_Address, Target => Ghdl_Signal_Ptr);
      Sig : Ghdl_Signal_Ptr;
   begin
      --  Convert to signal.
      Sig := To_Ghdl_Signal_Ptr (To_Addr_Acc (Val_Addr).all);

      --  Get signal number.
      Num := Get_Signal_Number (Sig);

      --  If the signal number is 0, then assign a valid signal number.
      if Num = 0 then
         Nbr_Dumped_Signals := Nbr_Dumped_Signals + 1;
         Sig.Dump_Table_Idx := Dump_Table_Index(Nbr_Dumped_Signals);
         Sig.Alink := To_Ghdl_Signal_Ptr
           (Integer_Address (Nbr_Dumped_Signals));
         Num := Nbr_Dumped_Signals;
      end if;

      --  Do the real job: write the signal number.
      Wave_Put_ULEB128 (Ghdl_E32 (Num));
   end Write_Signal_Number;

   procedure Foreach_Scalar_Signal_Number is new
     Grt.Rtis_Utils.Foreach_Scalar (Param_Type => Natural,
                                    Process => Write_Signal_Number);

   procedure Write_Signal_Numbers (Decl : VhpiHandleT)
   is
      Ctxt : Rti_Context;
      Sig : Ghdl_Rtin_Object_Acc;
   begin
      Ctxt := Avhpi_Get_Context (Decl);
      Sig := To_Ghdl_Rtin_Object_Acc (Avhpi_Get_Rti (Decl));
      Foreach_Scalar_Signal_Number
        (Ctxt, Sig.Obj_Type,
         Loc_To_Addr (Sig.Common.Depth, Sig.Loc, Ctxt), True, 0);
   end Write_Signal_Numbers;

   procedure Write_Hierarchy_El (Decl : VhpiHandleT)
   is
      Mode2hie : constant array (VhpiModeT) of Unsigned_8 :=
        (VhpiErrorMode => Ghw_Hie_Signal,
         VhpiInMode => Ghw_Hie_Port_In,
         VhpiOutMode => Ghw_Hie_Port_Out,
         VhpiInoutMode => Ghw_Hie_Port_Inout,
         VhpiBufferMode => Ghw_Hie_Port_Buffer,
         VhpiLinkageMode => Ghw_Hie_Port_Linkage);
      V : Unsigned_8;
   begin
      case Vhpi_Get_Kind (Decl) is
         when VhpiPortDeclK =>
            V := Mode2hie (Vhpi_Get_Mode (Decl));
         when VhpiSigDeclK =>
            V := Ghw_Hie_Signal;
         when VhpiForGenerateK =>
            V := Ghw_Hie_Generate_For;
         when VhpiIfGenerateK =>
            V := Ghw_Hie_Generate_If;
         when VhpiBlockStmtK =>
            V := Ghw_Hie_Block;
         when VhpiCompInstStmtK =>
            V := Ghw_Hie_Instance;
         when VhpiProcessStmtK =>
            V := Ghw_Hie_Process;
         when VhpiPackInstK =>
            V := Ghw_Hie_Package;
         when VhpiRootInstK =>
            V := Ghw_Hie_Instance;
         when others =>
            --raise Program_Error;
            Internal_Error ("write_hierarchy_el");
      end case;
      Wave_Put_Byte (V);
      Write_String_Id (Avhpi_Get_Base_Name (Decl));
      case Vhpi_Get_Kind (Decl) is
         when VhpiPortDeclK
           | VhpiSigDeclK =>
            Write_Object_Type (Decl);
            Write_Signal_Numbers (Decl);
         when VhpiForGenerateK =>
            Write_Generate_Type_And_Value (Decl);
         when others =>
            null;
      end case;
   end Write_Hierarchy_El;

   --  Create a hierarchy block.
   procedure Wave_Put_Hierarchy_Block (Inst : VhpiHandleT;
                                       Step : Step_Type;
                                       Match_List : Design.Match_List);

   procedure Wave_Put_Hierarchy_1
     (Inst : VhpiHandleT; Step : Step_Type; Match_List : Design.Match_List)
   is
      Decl_It : VhpiHandleT;
      Decl : VhpiHandleT;
      Error : AvhpiErrorT;
      Match_List_Child : Design.Match_List;
   begin
      Vhpi_Iterator (VhpiDecls, Inst, Decl_It, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;

      --  Extract signals.
      loop
         Vhpi_Scan (Decl_It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;

         case Vhpi_Get_Kind (Decl) is
            when VhpiPortDeclK
              | VhpiSigDeclK =>
               Match_List_Child := Get_Cursor
                 (Match_List, Avhpi_Get_Base_Name (Decl), Is_Signal => True);
               if Is_Displayed (Match_List_Child) then
                  case Step is
                     when Step_Name =>
                        Create_String_Id (Avhpi_Get_Base_Name (Decl));
                        Nbr_Scope_Signals := Nbr_Scope_Signals + 1;
                        Create_Object_Type (Decl);
                     when Step_Hierarchy =>
                        Write_Hierarchy_El (Decl);
                  end case;
               end if;
               --Wave_Put_Name (Decl);
               --Wave_Newline;
            when others =>
               null;
         end case;
      end loop;

      --  No sub-scopes for packages.
      if Vhpi_Get_Kind (Inst) = VhpiPackInstK then
         return;
      end if;

      --  Extract sub-scopes.
      Vhpi_Iterator (VhpiInternalRegions, Inst, Decl_It, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;

      loop
         Vhpi_Scan (Decl_It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;

         Nbr_Scopes := Nbr_Scopes + 1;

         Match_List_Child := Get_Cursor
           (Match_List, Avhpi_Get_Base_Name (Decl));
         if Is_Displayed (Match_List_Child) then
            case Vhpi_Get_Kind (Decl) is
               when VhpiIfGenerateK
                 | VhpiForGenerateK
                 | VhpiBlockStmtK
                 | VhpiCompInstStmtK =>
                  Wave_Put_Hierarchy_Block (Decl, Step, Match_List_Child);
               when VhpiProcessStmtK =>
                  case Step is
                     when Step_Name =>
                        Create_String_Id (Avhpi_Get_Base_Name (Decl));
                     when Step_Hierarchy =>
                        Write_Hierarchy_El (Decl);
                  end case;
               when others =>
                  Internal_Error ("wave_put_hierarchy_1");
   --                 Wave_Put ("unknown ");
   --                 Wave_Put (VhpiClassKindT'Image (Vhpi_Get_Kind (Decl)));
   --                 Wave_Newline;
            end case;
         end if;
      end loop;
   end Wave_Put_Hierarchy_1;

   procedure Wave_Put_Hierarchy_Block (Inst : VhpiHandleT;
                                       Step : Step_Type;
                                       Match_List : Design.Match_List) is
   begin
      case Step is
         when Step_Name =>
            Create_String_Id (Avhpi_Get_Base_Name (Inst));
            if Vhpi_Get_Kind (Inst) = VhpiForGenerateK then
               Create_Generate_Type (Inst);
            end if;
         when Step_Hierarchy =>
            Write_Hierarchy_El (Inst);
      end case;

      Wave_Put_Hierarchy_1 (Inst, Step, Match_List);

      if Step = Step_Hierarchy then
         Wave_Put_Byte (Ghw_Hie_Eos);
      end if;
   end Wave_Put_Hierarchy_Block;

   procedure Wave_Put_Hierarchy (Root : VhpiHandleT; Step : Step_Type)
   is
      Pack_It : VhpiHandleT;
      Pack : VhpiHandleT;
      Error : AvhpiErrorT;
      Match_List : Design.Match_List;
   begin
      --  First packages.
      Get_Package_Inst (Pack_It);
      loop
         Vhpi_Scan (Pack_It, Pack, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;
         Match_List := Get_Top_Cursor (Pkg, Avhpi_Get_Base_Name (Pack));
         if Is_Displayed (Match_List) then
            Wave_Put_Hierarchy_Block (Pack, Step, Match_List);
         end if;
      end loop;

      --  Then top entity.
      Match_List := Get_Top_Cursor (Entity, Avhpi_Get_Base_Name (Root));
      if Is_Displayed (Match_List) then
         Wave_Put_Hierarchy_Block (Root, Step, Match_List);
      end if;
   end Wave_Put_Hierarchy;

   procedure Disp_Str_AVL (Str : AVL_Nid; Indent : Natural)
   is
   begin
      if Str = AVL_Nil then
         return;
      end if;
      Disp_Str_AVL (Str_AVL.Table (Str).Left, Indent + 1);
      for I in 1 .. Indent loop
         Wave_Putc (' ');
      end loop;
      Wave_Puts (Str_Table.Table (Str_AVL.Table (Str).Val));
--        Wave_Putc ('(');
--        Put_I32 (Wave_Stream, Ghdl_I32 (Str));
--        Wave_Putc (')');
--        Put_I32 (Wave_Stream, Get_Height (Str));
      Wave_Newline;
      Disp_Str_AVL (Str_AVL.Table (Str).Right, Indent + 1);
   end Disp_Str_AVL;

   procedure Write_Strings
   is
   begin
--        Wave_Put ("AVL height: ");
--        Put_I32 (Wave_Stream, Ghdl_I32 (Check_AVL (Str_Root)));
--        Wave_Newline;
      Wave_Put ("strings length: ");
      Put_I32 (Wave_Stream, Ghdl_I32 (Strings_Len));
      Wave_Newline;
      Disp_Str_AVL (AVL_Root, 0);
      fflush (Wave_Stream);
   end Write_Strings;

   pragma Unreferenced (Write_Strings);

   procedure Freeze_Strings
   is
      type Str_Table1_Type is array (1 .. Str_Table.Last) of Ghdl_C_String;
      type Str_Table1_Acc is access Str_Table1_Type;
      Idx : AVL_Value;
      Table1 : Str_Table1_Acc;

      procedure Free is new Ada.Unchecked_Deallocation
        (Str_Table1_Type, Str_Table1_Acc);

      procedure Store_Strings (N : AVL_Nid) is
      begin
         if N = AVL_Nil then
            return;
         end if;
         Store_Strings (Str_AVL.Table (N).Left);
         Table1 (Idx) := Str_Table.Table (Str_AVL.Table (N).Val);
         Idx := Idx + 1;
         Store_Strings (Str_AVL.Table (N).Right);
      end Store_Strings;
   begin
      Table1 := new Str_Table1_Type;
      Idx := 1;
      Store_Strings (AVL_Root);
      Str_Table.Release;
      Str_AVL.Free;
      for I in Table1.all'Range loop
         Str_Table.Table (I) := Table1 (I);
      end loop;
      Free (Table1);
   end Freeze_Strings;

   procedure Write_Strings_Compress
   is
      Last : Ghdl_C_String;
      V : Ghdl_C_String;
      L : Natural;
      L1 : Natural;
   begin
      Wave_Section ("STR" & NUL);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_I32 (Ghdl_I32 (Str_Table.Last));
      Wave_Put_I32 (Ghdl_I32 (Strings_Len));
      for I in Str_Table.First .. Str_Table.Last loop
         V := Str_Table.Table (I);
         if I = Str_Table.First then
            L := 1;
         else
            Last := Str_Table.Table (I - 1);

            for I in Positive loop
               if V (I) /= Last (I) then
                  L := I;
                  exit;
               end if;
            end loop;
            L1 := L - 1;
            loop
               if L1 >= 32 then
                  Wave_Put_Byte (Unsigned_8 (L1 mod 32) + 16#80#);
               else
                  Wave_Put_Byte (Unsigned_8 (L1 mod 32));
               end if;
               L1 := L1 / 32;
               exit when L1 = 0;
            end loop;
         end if;

         if Boolean'(False) then
            Put ("string ");
            Put_I32 (stdout, Ghdl_I32 (I));
            Put (": ");
            Put (V);
            New_Line;
         end if;

         loop
            exit when V (L) = NUL;
            Wave_Putc (V (L));
            L := L + 1;
         end loop;
      end loop;
      --  Last string length.
      Wave_Put_Byte (0);
      --  End marker.
      Wave_Put ("EOS" & NUL);
   end Write_Strings_Compress;

   --  Convert rtik (for types).
   function Ghdl_Rtik_To_Ghw_Rtik (Kind : Ghdl_Rtik) return Ghw_Rtik is
   begin
      case Kind is
         when Ghdl_Rtik_Type_B1 =>
            return Ghw_Rtik_Type_B2;
         when Ghdl_Rtik_Type_E8 =>
            return Ghw_Rtik_Type_E8;
         when Ghdl_Rtik_Subtype_Array =>
            return Ghw_Rtik_Subtype_Array;
         when Ghdl_Rtik_Type_Array =>
            return Ghw_Rtik_Type_Array;
         when Ghdl_Rtik_Subtype_Unbounded_Array =>
            return Ghw_Rtik_Subtype_Unbounded_Array;
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Type_Unbounded_Record =>
            return Ghw_Rtik_Type_Record;
         when Ghdl_Rtik_Subtype_Record =>
            return Ghw_Rtik_Subtype_Record;
         when Ghdl_Rtik_Subtype_Unbounded_Record =>
            return Ghw_Rtik_Subtype_Unbounded_Record;
         when Ghdl_Rtik_Subtype_Scalar =>
            return Ghw_Rtik_Subtype_Scalar;
         when Ghdl_Rtik_Type_I32 =>
            return Ghw_Rtik_Type_I32;
         when Ghdl_Rtik_Type_I64 =>
            return Ghw_Rtik_Type_I64;
         when Ghdl_Rtik_Type_F64 =>
            return Ghw_Rtik_Type_F64;
         when Ghdl_Rtik_Type_P32 =>
            return Ghw_Rtik_Type_P32;
         when Ghdl_Rtik_Type_P64 =>
            return Ghw_Rtik_Type_P64;
         when others =>
            Internal_Error ("waves.ghdl_rtik_to_ghw_rtik: unhandled kind");
      end case;
   end Ghdl_Rtik_To_Ghw_Rtik;

   procedure Write_Range (Rti : Ghdl_Rti_Access; Rng : Ghdl_Range_Ptr)
   is
      Kind : Ghdl_Rtik;
      K : Unsigned_8;
   begin
      Kind := Rti.Kind;
      if Kind = Ghdl_Rtik_Subtype_Scalar then
         Kind := To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti).Basetype.Kind;
      end if;
      K := Ghw_Rtik'Pos (Ghdl_Rtik_To_Ghw_Rtik (Kind));
      case Kind is
         when Ghdl_Rtik_Type_B1 =>
            Wave_Put_Byte (K + Ghdl_Dir_Type'Pos (Rng.B1.Dir) * 16#80#);
            Wave_Put_Byte (Ghdl_B1'Pos (Rng.B1.Left));
            Wave_Put_Byte (Ghdl_B1'Pos (Rng.B1.Right));
         when Ghdl_Rtik_Type_E8 =>
            Wave_Put_Byte (K + Ghdl_Dir_Type'Pos (Rng.E8.Dir) * 16#80#);
            Wave_Put_Byte (Unsigned_8 (Rng.E8.Left));
            Wave_Put_Byte (Unsigned_8 (Rng.E8.Right));
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_P32 =>
            Wave_Put_Byte (K + Ghdl_Dir_Type'Pos (Rng.I32.Dir) * 16#80#);
            Wave_Put_SLEB128 (Rng.I32.Left);
            Wave_Put_SLEB128 (Rng.I32.Right);
         when Ghdl_Rtik_Type_P64
           | Ghdl_Rtik_Type_I64 =>
            Wave_Put_Byte (K + Ghdl_Dir_Type'Pos (Rng.P64.Dir) * 16#80#);
            Wave_Put_LSLEB128 (Rng.P64.Left);
            Wave_Put_LSLEB128 (Rng.P64.Right);
         when Ghdl_Rtik_Type_F64 =>
            Wave_Put_Byte (K + Ghdl_Dir_Type'Pos (Rng.F64.Dir) * 16#80#);
            Wave_Put_F64 (Rng.F64.Left);
            Wave_Put_F64 (Rng.F64.Right);
         when others =>
            Internal_Error ("waves.write_range: unhandled kind");
            --Internal_Error ("waves.write_range: unhandled kind "
            --                & Ghdl_Rtik'Image (Kind));
      end case;
   end Write_Range;

   procedure Write_Composite_Bounds (Rti : Ghdl_Rti_Access; Bounds : Address)
   is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64
           | Ghdl_Rtik_Type_P32
           | Ghdl_Rtik_Type_P64
           | Ghdl_Rtik_Type_F64 =>
            return;
         when Ghdl_Rtik_Type_Array =>
            declare
               Arr : constant Ghdl_Rtin_Type_Array_Acc :=
                 To_Ghdl_Rtin_Type_Array_Acc (Rti);
               Rng : Ghdl_Range_Ptr;
               Index_Type : Ghdl_Rti_Access;
               El_Type : Ghdl_Rti_Access;
               Bounds1 : Address;
            begin
               Bounds1 := Bounds;
               for I in 0 .. Arr.Nbr_Dim - 1 loop
                  Index_Type := Get_Base_Type (Arr.Indexes (I));
                  Extract_Range (Bounds1, Index_Type, Rng);
                  Write_Range (Index_Type, Rng);
               end loop;
               --  Write bounds only if the element subtype of the base type
               --  is unbounded.
               El_Type := Arr.Element;
               if Rtis_Utils.Is_Unbounded (El_Type) then
                  El_Type := Get_Base_Type (El_Type);
                  Bounds1 := Array_Layout_To_Element (Bounds1, El_Type);
                  Write_Composite_Bounds (El_Type, Bounds1);
               end if;
            end;
         when Ghdl_Rtik_Type_Record =>
            return;
         when  Ghdl_Rtik_Type_Unbounded_Record =>
            declare
               Rec : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Rti);
               El : Ghdl_Rtin_Element_Acc;
               El_Type : Ghdl_Rti_Access;
               Bounds1 : Address;
            begin
               for I in 1 .. Rec.Nbrel loop
                  El := To_Ghdl_Rtin_Element_Acc (Rec.Elements (I - 1));
                  --  Write bounds only if the element subtype of the base
                  --  type is unbounded.
                  El_Type := El.Eltype;
                  if Rtis_Utils.Is_Unbounded (El_Type) then
                     El_Type := Get_Base_Type (El_Type);
                     Bounds1 := Array_Layout_To_Element
                       (Bounds + El.Layout_Off, El_Type);
                     Write_Composite_Bounds (El_Type, Bounds1);
                  end if;
               end loop;
            end;
         when others =>
            Internal_Error ("waves.write_composite_bounds");
      end case;
   end Write_Composite_Bounds;

   procedure Write_Types
   is
      subtype Ghw_Rtik_Types is Ghw_Rtik
        range Ghw_Rtik_Type_B2 .. Ghw_Rtik_Subtype_Unbounded_Record;
      Kind : Ghw_Rtik_Types;
      Rti : Ghdl_Rti_Access;
      Ctxt : Rti_Context;
   begin
      --  Types header.
      Wave_Section ("TYP" & NUL);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_I32 (Ghdl_I32 (Types_Table.Last));

      for I in Types_Table.First .. Types_Table.Last loop
         Rti := Types_Table.Table (I).Type_Rti;
         Ctxt := Types_Table.Table (I).Context;

         if Rti.Kind = Ghdl_Rtik_Signal or Rti.Kind = Ghdl_Rtik_Port then
            --  Declare types for unbounded objects.
            declare
               Obj_Rti : constant Ghdl_Rtin_Object_Acc :=
                 To_Ghdl_Rtin_Object_Acc (Rti);
            begin
               case Obj_Rti.Obj_Type.Kind is
                  when Ghdl_Rtik_Type_Array =>
                     declare
                        Typ : constant Ghdl_Rti_Access := Obj_Rti.Obj_Type;
                        Addr : Ghdl_Uc_Array_Acc;
                     begin
                        Wave_Put_Byte (Ghw_Rtik'Pos (Ghw_Rtik_Subtype_Array));
                        Write_String_Id (null);
                        Write_Type_Id (Typ, Ctxt);
                        Addr := To_Ghdl_Uc_Array_Acc
                          (Loc_To_Addr (Rti.Depth, Obj_Rti.Loc, Ctxt));
                        Write_Composite_Bounds (Typ, Addr.Bounds);
                     end;
                  when Ghdl_Rtik_Subtype_Unbounded_Array =>
                     declare
                        St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                          To_Ghdl_Rtin_Subtype_Composite_Acc
                          (Obj_Rti.Obj_Type);
                        Addr : Ghdl_Uc_Array_Acc;
                     begin
                        Wave_Put_Byte (Ghw_Rtik'Pos (Ghw_Rtik_Subtype_Array));
                        Write_String_Id (null);
                        Write_Type_Id (St.Basetype, Ctxt);
                        Addr := To_Ghdl_Uc_Array_Acc
                          (Loc_To_Addr (Rti.Depth, Obj_Rti.Loc, Ctxt));
                        Write_Composite_Bounds (Get_Base_Type (St.Basetype),
                                                Addr.Bounds);
                     end;
                  when Ghdl_Rtik_Type_Unbounded_Record =>
                     declare
                        Typ : constant Ghdl_Rti_Access := Obj_Rti.Obj_Type;
                        Addr : Ghdl_Uc_Array_Acc;
                     begin
                        Wave_Put_Byte (Ghw_Rtik'Pos (Ghw_Rtik_Subtype_Record));
                        Write_String_Id (null);
                        Write_Type_Id (Typ, Ctxt);
                        Addr := To_Ghdl_Uc_Array_Acc
                          (Loc_To_Addr (Rti.Depth, Obj_Rti.Loc, Ctxt));
                        Write_Composite_Bounds (Typ, Addr.Bounds);
                     end;
                  when Ghdl_Rtik_Subtype_Unbounded_Record =>
                     declare
                        St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                          To_Ghdl_Rtin_Subtype_Composite_Acc
                          (Obj_Rti.Obj_Type);
                        Addr : Ghdl_Uc_Array_Acc;
                     begin
                        Wave_Put_Byte (Ghw_Rtik'Pos (Ghw_Rtik_Subtype_Record));
                        Write_String_Id (null);
                        Write_Type_Id (St.Basetype, Ctxt);
                        Addr := To_Ghdl_Uc_Array_Acc
                          (Loc_To_Addr (Rti.Depth, Obj_Rti.Loc, Ctxt));
                        Write_Composite_Bounds (Get_Base_Type (St.Basetype),
                                                Addr.Bounds);
                     end;
                  when others =>
                     Internal_Error ("waves.write_types: unhandled obj kind");
               end case;
            end;
         else
            --  Kind.
            Kind := Ghdl_Rtik_To_Ghw_Rtik (Rti.Kind);
            Wave_Put_Byte (Ghw_Rtik_Types'Pos (Kind));

            case Rti.Kind is
               when Ghdl_Rtik_Type_B1
                 | Ghdl_Rtik_Type_E8 =>
                  declare
                     Enum : constant Ghdl_Rtin_Type_Enum_Acc :=
                       To_Ghdl_Rtin_Type_Enum_Acc (Rti);
                  begin
                     Write_String_Id (Enum.Name);
                     Wave_Put_ULEB128 (Ghdl_E32 (Enum.Nbr));
                     for I in 1 .. Enum.Nbr loop
                        Write_String_Id (Enum.Names (I - 1));
                     end loop;
                  end;
               when Ghdl_Rtik_Type_I32
                 | Ghdl_Rtik_Type_I64
                 | Ghdl_Rtik_Type_F64 =>
                  declare
                     Base : constant Ghdl_Rtin_Type_Scalar_Acc :=
                       To_Ghdl_Rtin_Type_Scalar_Acc (Rti);
                  begin
                     Write_String_Id (Base.Name);
                  end;
               when Ghdl_Rtik_Type_P32
                 | Ghdl_Rtik_Type_P64 =>
                  declare
                     Base : constant Ghdl_Rtin_Type_Physical_Acc :=
                       To_Ghdl_Rtin_Type_Physical_Acc (Rti);
                     Unit : Ghdl_Rti_Access;
                  begin
                     Write_String_Id (Base.Name);
                     Wave_Put_ULEB128 (Ghdl_U32 (Base.Nbr));
                     for I in 1 .. Base.Nbr loop
                        Unit := Base.Units (I - 1);
                        Write_String_Id
                          (Rtis_Utils.Get_Physical_Unit_Name (Unit));
                        case Unit.Kind is
                           when Ghdl_Rtik_Unit64 =>
                              Wave_Put_LSLEB128
                                (To_Ghdl_Rtin_Unit64_Acc (Unit).Value);
                           when Ghdl_Rtik_Unitptr =>
                              case Rti.Kind is
                                 when Ghdl_Rtik_Type_P64 =>
                                    Wave_Put_LSLEB128
                                      (To_Ghdl_Rtin_Unitptr_Acc (Unit).
                                         Addr.I64);
                                 when Ghdl_Rtik_Type_P32 =>
                                    Wave_Put_SLEB128
                                      (To_Ghdl_Rtin_Unitptr_Acc (Unit).
                                         Addr.I32);
                                 when others =>
                                    Internal_Error
                                      ("wave.write_types(P32/P64-1)");
                              end case;
                           when others =>
                              Internal_Error
                                ("wave.write_types(P32/P64-2)");
                        end case;
                     end loop;
                  end;
               when Ghdl_Rtik_Subtype_Scalar =>
                  declare
                     Sub : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
                       To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti);
                  begin
                     Write_String_Id (Sub.Name);
                     Write_Type_Id (Sub.Basetype, Ctxt);
                     Write_Range
                       (Sub.Basetype,
                        To_Ghdl_Range_Ptr (Loc_To_Addr (Rti.Depth,
                                                        Sub.Range_Loc,
                                                        Ctxt)));
                  end;
               when Ghdl_Rtik_Type_Array =>
                  declare
                     Arr : constant Ghdl_Rtin_Type_Array_Acc :=
                       To_Ghdl_Rtin_Type_Array_Acc (Rti);
                  begin
                     Write_String_Id (Arr.Name);
                     Write_Type_Id (Arr.Element, Ctxt);
                     Wave_Put_ULEB128 (Ghdl_E32 (Arr.Nbr_Dim));
                     for I in 1 .. Arr.Nbr_Dim loop
                        Write_Type_Id (Arr.Indexes (I - 1), Ctxt);
                     end loop;
                  end;
               when Ghdl_Rtik_Subtype_Array =>
                  declare
                     Arr : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                       To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
                     Layout : Address;
                  begin
                     Write_String_Id (Arr.Name);
                     Write_Type_Id (Arr.Basetype, Ctxt);
                     Layout := Loc_To_Addr (Rti.Depth, Arr.Layout, Ctxt);
                     Write_Composite_Bounds (Get_Base_Type (Arr.Basetype),
                                             Array_Layout_To_Bounds (Layout));
                  end;
               when Ghdl_Rtik_Type_Record
                 | Ghdl_Rtik_Type_Unbounded_Record =>
                  declare
                     Rec : constant Ghdl_Rtin_Type_Record_Acc :=
                       To_Ghdl_Rtin_Type_Record_Acc (Rti);
                     El : Ghdl_Rtin_Element_Acc;
                  begin
                     Write_String_Id (Rec.Name);
                     Wave_Put_ULEB128 (Ghdl_E32 (Rec.Nbrel));
                     for I in 1 .. Rec.Nbrel loop
                        El := To_Ghdl_Rtin_Element_Acc (Rec.Elements (I - 1));
                        Write_String_Id (El.Name);
                        Write_Type_Id (El.Eltype, Ctxt);
                     end loop;
                  end;
               when Ghdl_Rtik_Subtype_Record =>
                  declare
                     Rec : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                       To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
                     Base : Ghdl_Rti_Access;
                     Layout : Address;
                  begin
                     Write_String_Id (Rec.Name);
                     Write_Type_Id (Rec.Basetype, Ctxt);
                     Base := Get_Base_Type (Rec.Basetype);
                     if Base.Kind = Ghdl_Rtik_Type_Unbounded_Record then
                        Layout := Loc_To_Addr
                          (Rec.Common.Depth, Rec.Layout, Ctxt);
                        Write_Composite_Bounds (Base, Layout);
                     end if;
                  end;
               when Ghdl_Rtik_Subtype_Unbounded_Record
                  | Ghdl_Rtik_Subtype_Unbounded_Array =>
                  declare
                     Rec : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                       To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
                  begin
                     Write_String_Id (Rec.Name);
                     Write_Type_Id (Rec.Basetype, Ctxt);
                  end;
               when others =>
                  Internal_Error ("wave.write_types");
                  --   Internal_Error ("wave.write_types: does not handle " &
                  --                   Ghdl_Rtik'Image (Rti.Kind));
            end case;
         end if;
      end loop;
      Wave_Put_Byte (0);
   end Write_Types;

   procedure Write_Known_Types
   is
      use Grt.Rtis_Types;

      Boolean_Type_Id : AVL_Nid;
      Bit_Type_Id : AVL_Nid;
      Std_Ulogic_Type_Id : AVL_Nid;

      function Search_Type_Id (Rti : Ghdl_Rti_Access) return AVL_Nid
      is
         Ctxt : Rti_Context;
         Tid : AVL_Nid;
      begin
         Find_Type (Rti, Null_Context, Ctxt, Tid);
         return Tid;
      end Search_Type_Id;
   begin
      Search_Types_RTI;

      Boolean_Type_Id := Search_Type_Id (Std_Standard_Boolean_RTI_Ptr);

      Bit_Type_Id := Search_Type_Id (Std_Standard_Bit_RTI_Ptr);

      if Ieee_Std_Logic_1164_Std_Ulogic_RTI_Ptr /= null then
         Std_Ulogic_Type_Id := Search_Type_Id
           (Ieee_Std_Logic_1164_Std_Ulogic_RTI_Ptr);
      else
         Std_Ulogic_Type_Id := AVL_Nil;
      end if;

      Wave_Section ("WKT" & NUL);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);

      if Boolean_Type_Id /= AVL_Nil then
         Wave_Put_Byte (1);
         Write_Type_Id (Boolean_Type_Id);
      end if;

      if Bit_Type_Id /= AVL_Nil then
         Wave_Put_Byte (2);
         Write_Type_Id (Bit_Type_Id);
      end if;

      if Std_Ulogic_Type_Id /= AVL_Nil then
         Wave_Put_Byte (3);
         Write_Type_Id (Std_Ulogic_Type_Id);
      end if;

      Wave_Put_Byte (0);
   end Write_Known_Types;

   --  Table of signals to be dumped.
   package Dump_Table is new Grt.Table
     (Table_Component_Type => Ghdl_Signal_Ptr,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 32);

   function Get_Dump_Entry (N : Natural) return Ghdl_Signal_Ptr is
   begin
      return Dump_Table.Table (N);
   end Get_Dump_Entry;

   pragma Unreferenced (Get_Dump_Entry);

   procedure Write_Hierarchy (Root : VhpiHandleT)
   is
      N : Natural;
   begin
      --  Check Alink is 0.
      for I in Sig_Table.First .. Sig_Table.Last loop
         if Sig_Table.Table (I).Alink /= null then
            Internal_Error ("wave.write_hierarchy");
         end if;
      end loop;

      Wave_Section ("HIE" & NUL);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_I32 (Ghdl_I32 (Nbr_Scopes));
      Wave_Put_I32 (Ghdl_I32 (Nbr_Scope_Signals));
      Wave_Put_I32 (Ghdl_I32 (Sig_Table.Last - Sig_Table.First + 1));
      Wave_Put_Hierarchy (Root, Step_Hierarchy);
      Wave_Put_Byte (0);

      Dump_Table.Set_Last (Nbr_Dumped_Signals);
      for I in Dump_Table.First .. Dump_Table.Last loop
         Dump_Table.Table (I) := null;
      end loop;

      --  Save and clear.
      for I in Sig_Table.First .. Sig_Table.Last loop
         N := Get_Signal_Number (Sig_Table.Table (I));
         if N /= 0 then
            if Dump_Table.Table (N) /= null then
               Internal_Error ("wave.write_hierarchy(2)");
            end if;
            Dump_Table.Table (N) := Sig_Table.Table (I);
            Sig_Table.Table (I).Alink := null;
         end if;
      end loop;
   end Write_Hierarchy;

   procedure Write_Signal_Value (Sig : Ghdl_Signal_Ptr) is
   begin
      --  FIXME: for some signals, the significant value is the driving value!
      Write_Value (Sig.Value_Ptr, Sig.Mode);
   end Write_Signal_Value;

   procedure Write_Snapshot is
   begin
      Wave_Section ("SNP" & NUL);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_Byte (0);
      Wave_Put_I64 (Ghdl_I64 (Current_Time));

      for I in Dump_Table.First .. Dump_Table.Last loop
         Write_Signal_Value (Dump_Table.Table (I));
      end loop;
      Wave_Put ("ESN" & NUL);
   end Write_Snapshot;

   procedure Wave_Start_Cb (Arg : System.Address)
   is
      pragma Unreferenced (Arg);
   begin
      Write_Snapshot;
   end Wave_Start_Cb;

   procedure Wave_Cycle;

   --  Called after elaboration.
   procedure Wave_Start
   is
      Root : VhpiHandleT;
      H : Callback_Handle;
   begin
      --  Do nothing if there is no VCD file to generate.
      if Wave_Stream = NULL_Stream then
         return;
      end if;

      Write_File_Header;

      --  FIXME: write infos
      --  * date
      --  * timescale
      --  * design name ?
      --  ...

      --  Put hierarchy.
      Get_Root_Inst (Root);
      -- Vcd_Search_Packages;
      Wave_Put_Hierarchy (Root, Step_Name);

      Wave_Opt.File.Finalize;

      if Str_Table.Last > 0 then
         Freeze_Strings;
      end if;

      -- Register_Cycle_Hook (Vcd_Cycle'Access);
      Write_Strings_Compress;
      Write_Types;
      Write_Known_Types;
      Write_Hierarchy (Root);

      Wave_Opt.Design.Last_Checks;

      --  End of header mark.
      Wave_Section ("EOH" & NUL);

      --  Write the first snapshot just before running processes for the first
      --  time.  At that point, signals are fully initialized.
      Register_Callback (Cb_Start_Of_Processes, H, Oneshot,
                         Wave_Start_Cb'Access);

      Register_Cycle_Hook (Wave_Cycle'Access);

      fflush (Wave_Stream);
   end Wave_Start;

   Wave_Time : Std_Time := 0;
   In_Cyc : Boolean := False;

   procedure Wave_Close_Cyc
   is
   begin
      Wave_Put_LSLEB128 (-1);
      Wave_Put ("ECY" & NUL);
      In_Cyc := False;
   end Wave_Close_Cyc;

   procedure Wave_Cycle
   is
      function Lt (Op1, Op2 : Positive) return Boolean
      is
         Left : Ghdl_Signal_Ptr;
         Right : Ghdl_Signal_Ptr;
      begin
         Left := Changed_Sig_Table.Table (Op1);
         Right := Changed_Sig_Table.Table (Op2);
         return Left.Dump_Table_Idx < Right.Dump_Table_Idx;
      end Lt;
      pragma Inline (Lt);

      procedure Swap (From, To : Positive)
      is
         Tmp : Ghdl_Signal_Ptr;
      begin
         Tmp := Changed_Sig_Table.Table (From);
         Changed_Sig_Table.Table (From) := Changed_Sig_Table.Table (To);
         Changed_Sig_Table.Table (To) := Tmp;
      end Swap;
      pragma Inline (Swap);

      procedure Sort is new Grt.Algos.Heap_Sort (Lt => Lt, Swap => Swap);

      Diff : Std_Time;
      Sig : Ghdl_Signal_Ptr;
      Last : Dump_Table_Index;
   begin
      if not In_Cyc then
         Wave_Section ("CYC" & NUL);
         Wave_Put_I64 (Ghdl_I64 (Current_Time));
         In_Cyc := True;
      else
         Diff := Current_Time - Wave_Time;
         Wave_Put_LSLEB128 (Ghdl_I64 (Diff));
      end if;
      Wave_Time := Current_Time;

      --  Dump signals.
      Last := 0;
      if Changed_Sig_Table.First <= Changed_Sig_Table.Last then
         Sort (Changed_Sig_Table.Last);
         for I in Changed_Sig_Table.First .. Changed_Sig_Table.Last loop
            Sig := Changed_Sig_Table.Table(I);
            if Sig.Flags.RO_Event then
               Wave_Put_ULEB128 (Ghdl_U32 (Sig.Dump_Table_Idx - Last));
               Last := Sig.Dump_Table_Idx;
               Write_Signal_Value (Sig);
               Sig.Flags.RO_Event := False;
            end if;
         end loop;
         Changed_Sig_Table.Set_Last (0);
      end if;
      Wave_Put_Byte (0);
   end Wave_Cycle;

   --  Called at the end of the simulation.
   procedure Wave_End is
   begin
      if Wave_Stream = NULL_Stream then
         return;
      end if;
      if In_Cyc then
         Wave_Close_Cyc;
      end if;
      Wave_Write_Directory;
      fclose (Wave_Stream);
   end Wave_End;

   Wave_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("ghw: save waveforms in ghw file format"),
      Option => Wave_Option'Access,
      Help => Wave_Help'Access,
      Init => Wave_Init'Access,
      Start => Wave_Start'Access,
      Finish => Wave_End'Access);

   procedure Register is
   begin
      Register_Hooks (Wave_Hooks'Access);
   end Register;
end Grt.Waves;
