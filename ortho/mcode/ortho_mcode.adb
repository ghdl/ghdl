--  Mcode back-end for ortho.
--  Copyright (C) 2006 Tristan Gingold
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
with Ada.Text_IO;
with Ortho_Code.Debug;
with Ortho_Ident;
-- with Binary_File;

package body Ortho_Mcode is
   procedure New_Debug_Line_Decl (Line : Natural)
   is
      pragma Unreferenced (Line);
   begin
      null;
   end New_Debug_Line_Decl;

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

   procedure Start_Const_Value (Const : in out O_Dnode)
   is
      pragma Unreferenced (Const);
   begin
      null;
   end Start_Const_Value;

   procedure Finish_Const_Value (Const : in out O_Dnode; Val : O_Cnode)
   is
      pragma Warnings (Off, Const);
   begin
      New_Const_Value (Const, Val);
   end Finish_Const_Value;

   function New_Obj_Value (Obj : O_Dnode) return O_Enode is
   begin
      return New_Value (New_Obj (Obj));
   end New_Obj_Value;

   function New_Constrained_Array_Type (Atype : O_Tnode; Length : O_Cnode)
                                       return O_Tnode
   is
      L_Type : O_Tnode;
   begin
      L_Type := Get_Const_Type (Length);
      if Get_Type_Kind (L_Type) /= OT_Unsigned then
         raise Syntax_Error;
      end if;
      return New_Constrained_Array_Type (Atype, Get_Const_U32 (Length));
   end New_Constrained_Array_Type;

   procedure Init is
   begin
      --  Create an anonymous pointer type.
      if New_Access_Type (O_Tnode_Null) /= O_Tnode_Ptr then
         raise Program_Error;
      end if;
      --  Do not finish the access, since this creates an infinite recursion
      --  in gdb (at least for GDB 6.3).
      --Finish_Access_Type (O_Tnode_Ptr, O_Tnode_Ptr);
      Ortho_Code.Abi.Init;
   end Init;

   procedure Finish is
   begin
      if False then
         Ortho_Code.Decls.Disp_All_Decls;
         --Ortho_Code.Exprs.Disp_All_Enode;
      end if;
      Ortho_Code.Abi.Finish;
      if Debug.Flag_Debug_Stat then
         Ada.Text_IO.Put_Line ("Statistics:");
         Ortho_Code.Exprs.Disp_Stats;
         Ortho_Code.Decls.Disp_Stats;
         Ortho_Code.Types.Disp_Stats;
         Ortho_Code.Consts.Disp_Stats;
         Ortho_Ident.Disp_Stats;
         -- Binary_File.Disp_Stats;
      end if;
   end Finish;

   procedure Free_All is
   begin
      Ortho_Code.Types.Finish;
      Ortho_Code.Exprs.Finish;
      Ortho_Code.Consts.Finish;
      Ortho_Code.Decls.Finish;
      Ortho_Ident.Finish;
   end Free_All;
end Ortho_Mcode;
