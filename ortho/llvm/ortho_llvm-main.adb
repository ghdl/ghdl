--  LLVM back-end for ortho.
--  Copyright (C) 2014 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Ada.Unchecked_Conversion;

package body Ortho_LLVM.Main is
   Dbg_Str : constant String := "dbg";

   function To_String (C : Cstring) return String is
      function Strlen (C : Cstring) return Natural;
      pragma Import (C, Strlen);

      subtype Fat_String is String (Positive);
      type Fat_String_Acc is access Fat_String;

      function To_Fat_String_Acc is new
        Ada.Unchecked_Conversion (Cstring, Fat_String_Acc);
   begin
      return To_Fat_String_Acc (C)(1 .. Strlen (C));
   end To_String;

   procedure Init is
   begin
      Builder := CreateBuilder;
      Decl_Builder := CreateBuilder;

      Char_Type := New_Unsigned_Type (8);
      New_Type_Decl (Get_Identifier ("__llvm_char"), Char_Type);

      if False then
         Char_Ptr_Type := New_Access_Type (Char_Type);
         New_Type_Decl (Get_Identifier ("__llvm_char_ptr"), Char_Ptr_Type);

         Stacksave_Fun := AddFunction
           (Module, Stacksave_Name'Address,
            FunctionType (Get_LLVM_Type (Char_Ptr_Type),
                          TypeRefArray'(1 .. 0 => Null_TypeRef), 0, 0));

         Stackrestore_Fun := AddFunction
           (Module, Stackrestore_Name'Address,
            FunctionType
              (VoidType,
               TypeRefArray'(1 => Get_LLVM_Type (Char_Ptr_Type)), 1, 0));
      end if;

      if Flag_Debug then
         Debug_ID := GetMDKindID (Dbg_Str, Dbg_Str'Length);

         declare
            Atypes : TypeRefArray (1 .. 2);
            Ftype : TypeRef;
            Name : String := "llvm.dbg.declare" & ASCII.NUL;
         begin
            Atypes := (MetadataType, MetadataType);
            Ftype := FunctionType (VoidType, Atypes, Atypes'Length, 0);
            Llvm_Dbg_Declare := AddFunction (Module, Name'Address, Ftype);
            AddFunctionAttr (Llvm_Dbg_Declare,
                             NoUnwindAttribute + ReadNoneAttribute);
         end;
      end if;
   end Init;
end Ortho_LLVM.Main;
