--  GHDL Run Time (GRT) -  'name* subprograms.
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
--with Grt.Errors; use Grt.Errors;
with Ada.Unchecked_Conversion;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Processes; use Grt.Processes;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;
with Grt.Rstrings; use Grt.Rstrings;
with Grt.Options;

package body Grt.Names is
   function To_Str_String_32_Boundp is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Std_String_32_Boundp);

   function To_Str_String_64_Boundp is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Std_String_64_Boundp);

   function To_Std_String_Basep is new Ada.Unchecked_Conversion
     (Source => Ghdl_C_String, Target => Std_String_Basep);

   function To_Std_String_Basep is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Std_String_Basep);

   procedure Get_Name (Res : Std_String_Any_Ptr;
                       Ctxt : Rti_Context;
                       Name : Ghdl_Str_Len_Ptr;
                       Is_Path : Boolean)
   is
      procedure Memcpy (Dst : Address; Src : Address; Len : Integer);
      pragma Import (C, Memcpy);

      Len : Ghdl_Index_Type;
      Base : Std_String_Basep;

      Rstr : Rstring;
      R_Len : Ghdl_Index_Type;
   begin
      if Ctxt.Block /= null then
         Prepend (Rstr, ':');
         Get_Path_Name (Rstr, Ctxt, ':', not Is_Path);
         R_Len := Ghdl_Index_Type (Length (Rstr));
         Len := R_Len + Name.Len;
      else
         Len := Name.Len;
      end if;

      if Ctxt.Block /= null then
         Base := To_Std_String_Basep (Ghdl_Stack2_Allocate (Len));
         Memcpy (Base (0)'Address, Get_Address (Rstr), Natural (R_Len));
         Memcpy (Base (R_Len)'Address,
                 Name.Str (1)'Address,
                 Natural (Name.Len));
         Free (Rstr);
      else
         Base := To_Std_String_Basep (Name.Str);
      end if;

      if Options.Flag_Integer_64 then
         declare
            Bounds : Std_String_64_Boundp;
            Str : Std_String_64_Ptr;
         begin
            Bounds := To_Str_String_64_Boundp
              (Ghdl_Stack2_Allocate (Std_String_64_Bound'Size
                                     / System.Storage_Unit));
            Bounds.Dim_1.Left := 1;
            Bounds.Dim_1.Right := Ghdl_I64 (Len);
            Bounds.Dim_1.Dir := Dir_To;
            Bounds.Dim_1.Length := Len;
            Str := To_Std_String_64_Ptr (Res);
            Str.Bounds := Bounds;
            Str.Base := Base;
         end;
      else
         declare
            Bounds : Std_String_32_Boundp;
            Str : Std_String_32_Ptr;
         begin
            Bounds := To_Str_String_32_Boundp
              (Ghdl_Stack2_Allocate (Std_String_32_Bound'Size
                                     / System.Storage_Unit));
            Bounds.Dim_1.Left := 1;
            Bounds.Dim_1.Right := Ghdl_I32 (Len);
            Bounds.Dim_1.Dir := Dir_To;
            Bounds.Dim_1.Length := Len;
            Str := To_Std_String_32_Ptr (Res);
            Str.Bounds := Bounds;
            Str.Base := Base;
         end;
      end if;

   end Get_Name;

   procedure Ghdl_Get_Path_Name (Res : Std_String_Any_Ptr;
                                 Ctxt : Ghdl_Rti_Access;
                                 Base : Address;
                                 Name : Ghdl_Str_Len_Ptr)
   is
   begin
      Get_Name (Res, (Base, Ctxt), Name, True);
   end Ghdl_Get_Path_Name;

   procedure Ghdl_Get_Instance_Name (Res : Std_String_Any_Ptr;
                                     Ctxt : Ghdl_Rti_Access;
                                     Base : Address;
                                     Name : Ghdl_Str_Len_Ptr)
   is
   begin
      Get_Name (Res, (Base, Ctxt), Name, False);
   end Ghdl_Get_Instance_Name;

end Grt.Names;
