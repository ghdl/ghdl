--  Associate foreign names with implementation
--  Copyright (C) 2026 Tristan Gingold
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
with Hash;
with Interning;

with Foreigns;

with Vhdl.Errors; use Vhdl.Errors;

with Grt.Types; use Grt.Types;
with Grt.Dynload; use Grt.Dynload;

package body Trans_Foreign is
   --  Elaboration mode.
   type Shlib_Object_Type is record
      Name : String_Access;
      Handler : Address;
   end record;

   function Shlib_Build (Name : String) return Shlib_Object_Type
   is
      Name_Acc : constant String_Access := new String'(Name);
      C_Name : constant String := Name & NUL;
      Handler : Address;
   begin
      Handler :=
        Grt_Dynload_Open (Grt.Types.To_Ghdl_C_String (C_Name'Address));
      return (Name => Name_Acc,
              Handler => Handler);
   end Shlib_Build;

   function Shlib_Equal (Obj : Shlib_Object_Type; Param : String)
                        return Boolean is
   begin
      return Obj.Name.all = Param;
   end Shlib_Equal;

   package Shlib_Interning is new Interning
     (Key_Type => String,
      Object_Type => Shlib_Object_Type,
      Hash => Hash.String_Hash,
      Build => Shlib_Build,
      Equal => Shlib_Equal);

   function Get_Foreign_Address
     (Decl : Iir; Info : Vhdl.Back_End.Foreign_Info_Type) return Address
   is
      use Vhdl.Back_End;
         Name : constant String :=
           Info.Subprg_Name (1 .. Info.Subprg_Len);
         Lib : constant String :=
           Info.Lib_Name (1 .. Info.Lib_Len);
         Shlib : Shlib_Object_Type;
      Res : Address;
   begin
      if Info.Lib_Len = 0
        or else Lib = "null"
      then
         Res := Foreigns.Find_Foreign (Name);
         if Res = Null_Address then
            Error_Msg_Sem
              (+Decl, "unknown foreign VHPIDIRECT '" & Name & "'");
            return Null_Address;
         end if;
      else
         Shlib := Shlib_Interning.Get (Lib);
         if Shlib.Handler = Null_Address then
            Error_Msg_Sem
              (+Decl, "cannot load VHPIDIRECT shared library '" & Lib & "'");
            return Null_Address;
         end if;

         declare
            C_Name : constant String := Name & NUL;
         begin
            Res := Grt_Dynload_Symbol
              (Shlib.Handler, Grt.Types.To_Ghdl_C_String (C_Name'Address));
         end;
         if Res = Null_Address then
            Error_Msg_Sem
              (+Decl, "cannot resolve VHPIDIRECT symbol '" & Name & "'");
            return Null_Address;
         end if;
      end if;
      return Res;
   end Get_Foreign_Address;

   procedure Init is
   begin
      Shlib_Interning.Init;
   end Init;
end Trans_Foreign;
