--  GCC back-end for ortho.
--  Copyright (C) 2002-1014 Tristan Gingold
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
with Ada.Unchecked_Deallocation;
with Ortho_Gcc_Front; use Ortho_Gcc_Front;

package body Ortho_Gcc is

   function New_Lit (Lit : O_Cnode) return O_Enode is
   begin
      return O_Enode (Lit);
   end New_Lit;

   function New_Obj (Obj : O_Dnode) return O_Lnode is
   begin
      return O_Lnode (Obj);
   end New_Obj;

   function New_Global (Decl : O_Dnode) return O_Gnode is
   begin
      return O_Gnode (Decl);
   end New_Global;

   function New_Global_Selected_Element (Rec : O_Gnode; El : O_Fnode)
                                        return O_Gnode is
   begin
      return O_Gnode (New_Selected_Element (O_Lnode (Rec), El));
   end New_Global_Selected_Element;

   function New_Obj_Value (Obj : O_Dnode) return O_Enode is
   begin
      return O_Enode (Obj);
   end New_Obj_Value;

   procedure New_Debug_Filename_Decl (Filename : String) is
   begin
      null;
   end New_Debug_Filename_Decl;

   procedure New_Debug_Comment_Decl (Comment : String)
   is
      pragma Unreferenced (Comment);
   begin
      null;
   end New_Debug_Comment_Decl;

   procedure New_Debug_Comment_Stmt (Comment : String)
   is
      pragma Unreferenced (Comment);
   begin
      null;
   end New_Debug_Comment_Stmt;

   --  Representation of a C String: this is an access to a bounded string.
   --  Therefore, with GNAT, such an access is a thin pointer.
   subtype Fat_C_String is String (Positive);
   type C_String is access all Fat_C_String;
   pragma Convention (C, C_String);

   C_String_Null : constant C_String := null;

   --  Return the length of a C String (ie, the number of characters before
   --  the Nul).
   function C_String_Len (Str : C_String) return Natural;
   pragma Import (C, C_String_Len, "strlen");

   function Lang_Handle_Option (Opt : C_String; Arg : C_String)
                               return Integer;
   pragma Export (C, Lang_Handle_Option);

   function Lang_Parse_File (Filename : C_String) return Integer;
   pragma Export (C, Lang_Parse_File);

   function Lang_Handle_Option (Opt : C_String; Arg : C_String)
     return Integer
   is
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Name => String_Acc, Object => String);

      Res : Natural;
      Ada_Opt : String_Acc;
      Ada_Arg : String_Acc;
      Len : Natural;
   begin
      Len := C_String_Len (Opt);
      Ada_Opt := new String'(Opt (1 .. Len));
      if Arg /= C_String_Null then
         Len := C_String_Len (Arg);
         Ada_Arg := new String'(Arg (1 .. Len));
      else
         Ada_Arg := null;
      end if;
      Res := Ortho_Gcc_Front.Decode_Option (Ada_Opt, Ada_Arg);
      Unchecked_Deallocation (Ada_Opt);
      Unchecked_Deallocation (Ada_Arg);
      return Res;
   end Lang_Handle_Option;

   function Lang_Parse_File (Filename : C_String) return Integer
   is
      Len : Natural;
      File : String_Acc;
   begin
      if Filename = C_String_Null then
         File := null;
      else
         Len := C_String_Len (Filename);
         File := new String'(Filename.all (1 .. Len));
      end if;

      if Ortho_Gcc_Front.Parse (File) then
         return 1;
      else
         return 0;
      end if;
   end Lang_Parse_File;

end Ortho_Gcc;
