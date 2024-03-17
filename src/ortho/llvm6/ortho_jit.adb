--  LLVM back-end for ortho.
--  Copyright (C) 2024 Tristan Gingold
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

with Ortho_LLVM;

package body Ortho_Jit is
   Filename : constant String := "jit.ortho";

   procedure Llvm_Jit_Init;
   pragma Import (C, Llvm_Jit_Init);

   --  Initialize the whole engine.
   procedure Init is
   begin
      Ortho_LLVM.Init (Filename, Filename'Length);
      Llvm_Jit_Init;
   end Init;

   procedure Llvm_Jit_Set_Address (Decl : O_Dnode; Addr : Address);
   pragma Import (C, Llvm_Jit_Set_Address);

   procedure Set_Address (Decl : O_Dnode; Addr : Address) is
   begin
      Llvm_Jit_Set_Address (Decl, Addr);
   end Set_Address;

   function Llvm_Jit_Get_Address (Decl : O_Dnode) return Address;
   pragma Import (C, Llvm_Jit_Get_Address);

   function Get_Address (Decl : O_Dnode) return Address is
   begin
      return Llvm_Jit_Get_Address (Decl);
   end Get_Address;

   procedure Llvm_Jit_Finalize;
   pragma Import (C, Llvm_Jit_Finalize);

   --  Do link.
   procedure Link (Status : out Boolean) is
   begin
      Llvm_Jit_Finalize;

      --  FIXME: optim
      Status := False;
   end Link;

   procedure Finish is
   begin
      null;
   end Finish;

   function Llvm_Jit_Get_Byte_Size (Typ : O_Tnode) return Storage_Count;
   pragma Import (C, Llvm_Jit_Get_Byte_Size);

   function Get_Byte_Size (Atype : O_Tnode) return Storage_Count is
   begin
      return Llvm_Jit_Get_Byte_Size (Atype);
   end Get_Byte_Size;

   function Llvm_Jit_Get_Field_Offset (Field : O_Fnode) return Storage_Count;
   pragma Import (C, Llvm_Jit_Get_Field_Offset);

   function Get_Field_Offset (Field : O_Fnode) return Storage_Count is
   begin
      return Llvm_Jit_Get_Field_Offset (Field);
   end Get_Field_Offset;

   procedure Symbolize (Pc : Address;
                        Filename : out Address;
                        Lineno : out Natural;
                        Subprg : out Address)
   is
      pragma Unreferenced (Pc);
   begin
      Filename := Null_Address;
      Subprg := Null_Address;
      Lineno := 0;
   end Symbolize;

   function Decode_Option (Option : String) return Boolean
   is
      Opt : constant String (1 .. Option'Length) := Option;
   begin
      if Opt = "-g" or else Opt = "--be-g" then
         Ortho_LLVM.Set_Debug_Level (2);
         return True;
      elsif Opt = "-g0" then
         Ortho_LLVM.Set_Debug_Level (0);
         return True;
      elsif Opt = "--llvm-dump" then
         Ortho_LLVM.Set_Dump_LLVM (1);
         return True;
      end if;
      return False;
   end Decode_Option;

   procedure Disp_Help is
   begin
      null;
   end Disp_Help;

   function Get_Jit_Name return String is
   begin
      return "LLVM";
   end Get_Jit_Name;

end Ortho_Jit;
