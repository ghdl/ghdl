--  Mcode back-end for ortho - Declarations handling.
--  Copyright (C) 2006 Tristan Gingold
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
with Tables;
with Ada.Text_IO;
with Ortho_Ident;
with Ortho_Code.Debug; use Ortho_Code.Debug;
with Ortho_Code.Exprs;
with Ortho_Code.Abi; use Ortho_Code.Abi;
with Ortho_Code.Flags;
with Ortho_Code.Types;

package body Ortho_Code.Decls is
   --  Common fields:
   --    kind: 4 bits
   --    storage: 2 bits
   --    flags (addr, 2): 2 bits
   --    reg : 8 bits
   --    depth : 16 bits
   --  Additionnal fields:
   --    OD_Type: Id, dtype
   --    OD_Var: Id, Dtype, symbol
   --    OD_Local: Id, Dtype, offset/reg
   --    OD_Const: Id, Dtype, Val, Symbol?
   --    OD_Function: Id, Dtype [interfaces follows], Symbol
   --    OD_Procedure: Id [interfaces follows], Symbol
   --    OD_Interface: Id, Dtype, offset/reg
   --    OD_Begin: Last
   --    OD_Body: Decl, Stmt, Parent
   type Dnode_Common (Kind : OD_Kind := OD_Type) is record
      Storage : O_Storage;

      --  True if the address of the declaration is taken.
      Flag_Addr : Boolean;

      Flag2 : Boolean;

      Reg : O_Reg;

      --  Depth of the declaration.
      Depth : O_Depth;

      case Kind is
         when OD_Type
           | OD_Const
           | OD_Var
           | OD_Local
           | OD_Function
           | OD_Procedure
           | OD_Interface =>
            --  Identifier of this declaration.
            Id : O_Ident;
            --  Type of this declaration.
            Dtype : O_Tnode;
            --  Symbol or offset.
            Ref : Int32;
            --  For const, val: the value.
            --  For subprg: size of pushed arguments.
            Info2 : Int32;
         when OD_Subprg_Ext =>
            --  Chain of interfaces.
            Subprg_Inter : O_Dnode;

         when OD_Block =>
            --  Last declaration of this block.
            Last : O_Dnode;
            --  Max stack offset.
            Block_Max_Stack : Uns32;
            --  Infos: may be used to store symbols.
            Block_Info1 : Int32;
            Block_Info2 : Int32;
         when OD_Body =>
            --  Corresponding declaration (function/procedure).
            Body_Decl : O_Dnode;
            --  Entry statement for this body.
            Body_Stmt : O_Enode;
            --  Parent (as a body) of this body or null if at top level.
            Body_Parent : O_Dnode;
            Body_Info : Int32;
         when OD_Init_Val =>
            --  Corresponding declaration.
            Val_Decl : O_Dnode;
            --  Value.
            Val_Val : O_Cnode;
      end case;
   end record;

   Use_Subprg_Ext : constant Boolean := False;

   pragma Pack (Dnode_Common);

   package Dnodes is new Tables
     (Table_Component_Type => Dnode_Common,
      Table_Index_Type => O_Dnode,
      Table_Low_Bound => O_Dnode_First,
      Table_Initial => 128);

   package TDnodes is new Tables
     (Table_Component_Type => O_Dnode,
      Table_Index_Type => O_Tnode,
      Table_Low_Bound => O_Tnode_First,
      Table_Initial => 8);

   Context : O_Dnode := O_Dnode_Null;

   function Get_Decl_Type (Decl : O_Dnode) return O_Tnode is
   begin
      return Dnodes.Table (Decl).Dtype;
   end Get_Decl_Type;

   function Get_Decl_Kind (Decl : O_Dnode) return OD_Kind is
   begin
      return Dnodes.Table (Decl).Kind;
   end Get_Decl_Kind;

   function Get_Decl_Storage (Decl : O_Dnode) return O_Storage is
   begin
      return Dnodes.Table (Decl).Storage;
   end Get_Decl_Storage;

   procedure Set_Decl_Storage (Decl : O_Dnode; Storage : O_Storage) is
   begin
      Dnodes.Table (Decl).Storage := Storage;
   end Set_Decl_Storage;

   function Get_Decl_Reg (Decl : O_Dnode) return O_Reg is
   begin
      return Dnodes.Table (Decl).Reg;
   end Get_Decl_Reg;

   procedure Set_Decl_Reg (Decl : O_Dnode; Reg : O_Reg) is
   begin
      Dnodes.Table (Decl).Reg := Reg;
   end Set_Decl_Reg;

   function Get_Decl_Depth (Decl : O_Dnode) return O_Depth is
   begin
      return Dnodes.Table (Decl).Depth;
   end Get_Decl_Depth;

   function Get_Decl_Chain (Decl : O_Dnode) return O_Dnode is
   begin
      case Get_Decl_Kind (Decl) is
         when OD_Block =>
            return Get_Block_Last (Decl) + 1;
         when OD_Body =>
            return Get_Block_Last (Decl + 1) + 1;
         when OD_Function
           | OD_Procedure =>
            --  Return the first interface.
            if Use_Subprg_Ext then
               return Decl + 2;
            else
               return Decl + 1;
            end if;
         when others =>
            return Decl + 1;
      end case;
   end Get_Decl_Chain;

   function Get_Body_Stmt (Bod : O_Dnode) return O_Enode is
   begin
      return Dnodes.Table (Bod).Body_Stmt;
   end Get_Body_Stmt;

   function Get_Body_Decl (Bod : O_Dnode) return O_Dnode is
   begin
      return Dnodes.Table (Bod).Body_Decl;
   end Get_Body_Decl;

   function Get_Body_Parent (Bod : O_Dnode) return O_Dnode is
   begin
      return Dnodes.Table (Bod).Body_Parent;
   end Get_Body_Parent;

   function Get_Body_Info (Bod : O_Dnode) return Int32 is
   begin
      return Dnodes.Table (Bod).Body_Info;
   end Get_Body_Info;

   procedure Set_Body_Info (Bod : O_Dnode; Info : Int32) is
   begin
      Dnodes.Table (Bod).Body_Info := Info;
   end Set_Body_Info;

   function Get_Decl_Ident (Decl : O_Dnode) return O_Ident is
   begin
      return Dnodes.Table (Decl).Id;
   end Get_Decl_Ident;

   function Get_Decl_Last return O_Dnode is
   begin
      return Dnodes.Last;
   end Get_Decl_Last;

   function Get_Block_Last (Blk : O_Dnode) return O_Dnode is
   begin
      return Dnodes.Table (Blk).Last;
   end Get_Block_Last;

   function Get_Block_Max_Stack (Blk : O_Dnode) return Uns32 is
   begin
      return Dnodes.Table (Blk).Block_Max_Stack;
   end Get_Block_Max_Stack;

   procedure Set_Block_Max_Stack (Blk : O_Dnode; Max : Uns32) is
   begin
      Dnodes.Table (Blk).Block_Max_Stack := Max;
   end Set_Block_Max_Stack;

   function Get_Block_Info1 (Blk : O_Dnode) return Int32 is
   begin
      return Dnodes.Table (Blk).Block_Info1;
   end Get_Block_Info1;

   procedure Set_Block_Info1 (Blk : O_Dnode; Info : Int32) is
   begin
      Dnodes.Table (Blk).Block_Info1 := Info;
   end Set_Block_Info1;

   function Get_Block_Info2 (Blk : O_Dnode) return Int32 is
   begin
      return Dnodes.Table (Blk).Block_Info2;
   end Get_Block_Info2;

   procedure Set_Block_Info2 (Blk : O_Dnode; Info : Int32) is
   begin
      Dnodes.Table (Blk).Block_Info2 := Info;
   end Set_Block_Info2;

   function Get_Subprg_Interfaces (Decl : O_Dnode) return O_Dnode
   is
      Res : O_Dnode;
   begin
      if Use_Subprg_Ext then
         Res := Decl + 2;
      else
         Res := Decl + 1;
      end if;

      if Get_Decl_Kind (Res) = OD_Interface then
         return Res;
      else
         return O_Dnode_Null;
      end if;
   end Get_Subprg_Interfaces;

   function Get_Interface_Chain (Decl : O_Dnode) return O_Dnode
   is
      Res : constant O_Dnode := Decl + 1;
   begin
      if Get_Decl_Kind (Res) = OD_Interface then
         return Res;
      else
         return O_Dnode_Null;
      end if;
   end Get_Interface_Chain;

   function Get_Val_Decl (Decl : O_Dnode) return O_Dnode is
   begin
      return Dnodes.Table (Decl).Val_Decl;
   end Get_Val_Decl;

   function Get_Val_Val (Decl : O_Dnode) return O_Cnode is
   begin
      return Dnodes.Table (Decl).Val_Val;
   end Get_Val_Val;

   Cur_Depth : O_Depth := O_Toplevel;

   procedure New_Type_Decl (Ident : O_Ident; Atype : O_Tnode) is
   begin
      Dnodes.Append (Dnode_Common'(Kind => OD_Type,
                                   Storage => O_Storage_Private,
                                   Depth => Cur_Depth,
                                   Reg => R_Nil,
                                   Id => Ident,
                                   Dtype => Atype,
                                   Ref => 0,
                                   Info2 => 0,
                                   others => False));
      if Flags.Flag_Type_Name then
         declare
            L : O_Tnode;
         begin
            L := TDnodes.Last;
            if Atype > L then
               TDnodes.Set_Last (Atype);
               TDnodes.Table (L + 1 .. Atype) := (others => O_Dnode_Null);
            end if;
         end;
         TDnodes.Table (Atype) := Dnodes.Last;
      end if;
   end New_Type_Decl;

   function Get_Type_Decl (Atype : O_Tnode) return O_Dnode is
   begin
      if Atype <= TDnodes.Last then
         return TDnodes.Table (Atype);
      else
         return O_Dnode_Null;
      end if;
   end Get_Type_Decl;

   procedure New_Const_Decl
     (Res : out O_Dnode;
      Ident : O_Ident;
      Storage : O_Storage;
      Atype : O_Tnode)
   is
   begin
      Dnodes.Append (Dnode_Common'(Kind => OD_Const,
                                   Storage => Storage,
                                   Depth => Cur_Depth,
                                   Reg => R_Nil,
                                   Id => Ident,
                                   Dtype => Atype,
                                   Ref => 0,
                                   Info2 => 0,
                                   others => False));
      Res := Dnodes.Last;
      if not Flag_Debug_Hli then
         Expand_Const_Decl (Res);
      end if;
   end New_Const_Decl;

   function Get_Init_Value (Decl : O_Dnode) return O_Cnode is
   begin
      return O_Cnode (Dnodes.Table (Decl).Info2);
   end Get_Init_Value;

   procedure New_Init_Value (Decl : O_Dnode; Val : O_Cnode) is
   begin
      pragma Assert (Get_Decl_Kind (Decl) = OD_Const
                       or else Get_Decl_Kind (Decl) = OD_Var);
      if Get_Init_Value (Decl) /= O_Cnode_Null then
         --  Value was already set.
         raise Syntax_Error;
      end if;
      Dnodes.Table (Decl).Info2 := Int32 (Val);
      if Flag_Debug_Hli then
         Dnodes.Append (Dnode_Common'(Kind => OD_Init_Val,
                                      Storage => O_Storage_Private,
                                      Depth => Cur_Depth,
                                      Reg => R_Nil,
                                      Val_Decl => Decl,
                                      Val_Val => Val,
                                      others => False));
      else
         Expand_Init_Value (Decl, Val);
      end if;
   end New_Init_Value;

   procedure New_Var_Decl (Res : out O_Dnode;
                           Ident : O_Ident;
                           Storage : O_Storage;
                           Atype : O_Tnode) is
   begin
      if Storage = O_Storage_Local then
         Dnodes.Append (Dnode_Common'(Kind => OD_Local,
                                      Storage => Storage,
                                      Depth => Cur_Depth,
                                      Reg => R_Nil,
                                      Id => Ident,
                                      Dtype => Atype,
                                      Ref => 0,
                                      Info2 => 0,
                                      others => False));
         Res := Dnodes.Last;
      else
         Dnodes.Append (Dnode_Common'(Kind => OD_Var,
                                      Storage => Storage,
                                      Depth => Cur_Depth,
                                      Reg => R_Nil,
                                      Id => Ident,
                                      Dtype => Atype,
                                      Ref => 0,
                                      Info2 => 0,
                                      others => False));
         Res := Dnodes.Last;
         if not Flag_Debug_Hli then
            Expand_Var_Decl (Res);
         end if;
      end if;
   end New_Var_Decl;

   Static_Chain_Id : O_Ident := O_Ident_Nul;

   procedure Add_Static_Chain (Interfaces : in out O_Inter_List)
   is
      Res : O_Dnode;
   begin
      if Static_Chain_Id = O_Ident_Nul then
         Static_Chain_Id := Ortho_Ident.Get_Identifier ("STATIC_CHAIN");
      end if;

      New_Interface_Decl (Interfaces, Res, Static_Chain_Id, O_Tnode_Ptr);
   end Add_Static_Chain;

   procedure Start_Subprogram_Decl (Interfaces : out O_Inter_List)
   is
      Storage : O_Storage;
      Decl : constant O_Dnode := Dnodes.Last;
   begin
      Storage := Get_Decl_Storage (Decl);
      if Cur_Depth /= O_Toplevel then
         case Storage is
            when O_Storage_External
              | O_Storage_Local =>
               null;
            when O_Storage_Public =>
               raise Syntax_Error;
            when O_Storage_Private =>
               Storage := O_Storage_Local;
               Set_Decl_Storage (Decl, Storage);
         end case;
      end if;
      if Use_Subprg_Ext then
         Dnodes.Append (Dnode_Common'(Kind => OD_Subprg_Ext,
                                      Storage => Storage,
                                      Depth => Cur_Depth,
                                      Reg => R_Nil,
                                      Subprg_Inter => O_Dnode_Null,
                                      others => False));
      end if;

      Start_Subprogram (Decl, Interfaces.Abi);
      Interfaces.Decl := Decl;
      if Storage = O_Storage_Local then
         Add_Static_Chain (Interfaces);
      end if;
   end Start_Subprogram_Decl;

   procedure Start_Function_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage;
      Rtype : O_Tnode)
   is
   begin
      Dnodes.Append (Dnode_Common'(Kind => OD_Function,
                                   Storage => Storage,
                                   Depth => Cur_Depth,
                                   Reg => R_Nil,
                                   Id => Ident,
                                   Dtype => Rtype,
                                   Ref => 0,
                                   Info2 => 0,
                                   others => False));
      Start_Subprogram_Decl (Interfaces);
   end Start_Function_Decl;

   procedure Start_Procedure_Decl
     (Interfaces : out O_Inter_List;
      Ident : O_Ident;
      Storage : O_Storage)
   is
   begin
      Dnodes.Append (Dnode_Common'(Kind => OD_Procedure,
                                   Storage => Storage,
                                   Depth => Cur_Depth,
                                   Reg => R_Nil,
                                   Id => Ident,
                                   Dtype => O_Tnode_Null,
                                   Ref => 0,
                                   Info2 => 0,
                                   others => False));
      Start_Subprogram_Decl (Interfaces);
   end Start_Procedure_Decl;

   procedure New_Interface_Decl
     (Interfaces : in out O_Inter_List;
      Res : out O_Dnode;
      Ident : O_Ident;
      Atype : O_Tnode)
   is
   begin
      Dnodes.Append (Dnode_Common'(Kind => OD_Interface,
                                   Storage => O_Storage_Local,
                                   Depth => Cur_Depth + 1,
                                   Reg => R_Nil,
                                   Id => Ident,
                                   Dtype => Atype,
                                   Ref => 0,
                                   Info2 => 0,
                                   others => False));
      Res := Dnodes.Last;
      New_Interface (Res, Interfaces.Abi);
   end New_Interface_Decl;

   procedure Set_Local_Offset (Decl : O_Dnode; Off : Int32) is
   begin
      Dnodes.Table (Decl).Ref := Off;
   end Set_Local_Offset;

   function Get_Local_Offset (Decl : O_Dnode) return Int32 is
   begin
      return Dnodes.Table (Decl).Ref;
   end Get_Local_Offset;

   function Get_Inter_Offset (Inter : O_Dnode) return Int32 is
   begin
      return Dnodes.Table (Inter).Ref;
   end Get_Inter_Offset;

   procedure Set_Decl_Info (Decl : O_Dnode; Ref : Int32) is
   begin
      Dnodes.Table (Decl).Ref := Ref;
   end Set_Decl_Info;

   function Get_Decl_Info (Decl : O_Dnode) return Int32 is
   begin
      return Dnodes.Table (Decl).Ref;
   end Get_Decl_Info;

   procedure Set_Subprg_Stack (Decl : O_Dnode; Val : Int32) is
   begin
      Dnodes.Table (Decl).Info2 := Val;
   end Set_Subprg_Stack;

   function Get_Subprg_Stack (Decl : O_Dnode) return Int32 is
   begin
      return Dnodes.Table (Decl).Info2;
   end Get_Subprg_Stack;

   procedure Finish_Subprogram_Decl
     (Interfaces : in out O_Inter_List; Res : out O_Dnode) is
   begin
      Res := Interfaces.Decl;
      Finish_Subprogram (Res, Interfaces.Abi);
   end Finish_Subprogram_Decl;

   Cur_Block : O_Dnode := O_Dnode_Null;

   function Start_Declare_Stmt return O_Dnode is
   begin
      Dnodes.Append (Dnode_Common'(Kind => OD_Block,
                                   Storage => O_Storage_Local,
                                   Depth => Cur_Depth,
                                   Reg => R_Nil,
                                   Last => O_Dnode_Null,
                                   Block_Max_Stack => 0,
                                   Block_Info1 => 0,
                                   Block_Info2 => 0,
                                   others => False));
      Cur_Block := Dnodes.Last;
      return Cur_Block;
   end Start_Declare_Stmt;

   procedure Finish_Declare_Stmt (Parent : O_Dnode) is
   begin
      Dnodes.Table (Cur_Block).Last := Dnodes.Last;
      Cur_Block := Parent;
   end Finish_Declare_Stmt;

   function Start_Subprogram_Body (Decl : O_Dnode; Stmt : O_Enode)
                                  return O_Dnode
   is
      Res : O_Dnode;
   begin
      Dnodes.Append (Dnode_Common'(Kind => OD_Body,
                                   Storage => O_Storage_Local,
                                   Depth => Cur_Depth,
                                   Reg => R_Nil,
                                   Body_Parent => Context,
                                   Body_Decl => Decl,
                                   Body_Stmt => Stmt,
                                   Body_Info => 0,
                                   others => False));
      Res := Dnodes.Last;
      Context := Res;
      Cur_Depth := Cur_Depth + 1;
      return Res;
   end Start_Subprogram_Body;

   procedure Finish_Subprogram_Body is
   begin
      Cur_Depth := Cur_Depth - 1;
      Context := Get_Body_Parent (Context);
   end Finish_Subprogram_Body;


--    function Image (Decl : O_Dnode) return String is
--    begin
--       return O_Dnode'Image (Decl);
--    end Image;

   procedure Disp_Decl_Name (Decl : O_Dnode)
   is
      use Ada.Text_IO;
      use Ortho_Ident;
      Id : O_Ident;
   begin
      Id := Get_Decl_Ident (Decl);
      if Is_Equal (Id, O_Ident_Nul) then
         declare
            Res : String := O_Dnode'Image (Decl);
         begin
            Res (1) := '?';
            Put (Res);
         end;
      else
         Put (Get_String (Id));
      end if;
   end Disp_Decl_Name;

   procedure Disp_Decl_Storage (Decl : O_Dnode)
   is
      use Ada.Text_IO;
   begin
      case Get_Decl_Storage (Decl) is
         when O_Storage_Local =>
            Put ("local");
         when O_Storage_External =>
            Put ("external");
         when O_Storage_Public =>
            Put ("public");
         when O_Storage_Private =>
            Put ("private");
      end case;
   end Disp_Decl_Storage;

   procedure Disp_Decl (Indent : Natural; Decl : O_Dnode)
   is
      use Ada.Text_IO;
      use Ortho_Code.Debug.Int32_IO;

      procedure Disp_Decl_Type (Decl : O_Dnode)
      is
         Dtype : constant O_Tnode := Get_Decl_Type (Decl);
      begin
         Put (Int32 (Dtype), 0);
         Put (", ");
         Disp_Mode (Types.Get_Type_Mode (Dtype));
      end Disp_Decl_Type;
   begin
      Set_Col (Count (Indent));
      Put (Int32 (Decl), 0);
      Set_Col (Count (7 + Indent));
      case Get_Decl_Kind (Decl) is
         when OD_Type =>
            Put ("type ");
            Disp_Decl_Name (Decl);
            Put (" is ");
            Disp_Decl_Type (Decl);
         when OD_Function =>
            Disp_Decl_Storage (Decl);
            Put (" function ");
            Disp_Decl_Name (Decl);
            Put (" return ");
            Disp_Decl_Type (Decl);
            Put ("  stack: ");
            Put (Get_Subprg_Stack (Decl), 0);
         when OD_Procedure =>
            Disp_Decl_Storage (Decl);
            Put (" procedure ");
            Disp_Decl_Name (Decl);
         when OD_Interface =>
            Put (" interface ");
            Disp_Decl_Name (Decl);
            Put (": ");
            Disp_Decl_Type (Decl);
            Put (", offset=");
            Put (Get_Inter_Offset (Decl), 0);
            Put (", reg=");
            Put (Image_Reg (Get_Decl_Reg (Decl)));
         when OD_Const =>
            Disp_Decl_Storage (Decl);
            Put (" const ");
            Disp_Decl_Name (Decl);
            Put (": ");
            Disp_Decl_Type (Decl);
         when OD_Init_Val =>
            Put ("constant ");
            Disp_Decl_Name (Get_Val_Decl (Decl));
            Put (": ");
            Put (Int32 (Get_Val_Val (Decl)), 0);
         when OD_Local =>
            Put ("local ");
            Disp_Decl_Name (Decl);
            Put (": ");
            Disp_Decl_Type (Decl);
            Put (", offset=");
            Put (Get_Inter_Offset (Decl), 0);
         when OD_Var =>
            Disp_Decl_Storage (Decl);
            Put (" var ");
            Disp_Decl_Name (Decl);
            Put (": ");
            Disp_Decl_Type (Decl);
         when OD_Body =>
            Put ("body of ");
            Put (Int32 (Get_Body_Decl (Decl)), 0);
            Put (", stmt at ");
            Put (Int32 (Get_Body_Stmt (Decl)), 0);
         when OD_Block =>
            Put ("block until ");
            Put (Int32 (Get_Block_Last (Decl)), 0);
         when OD_Subprg_Ext =>
            Put ("Subprg_Ext");
--           when others =>
--              Put (OD_Kind'Image (Get_Decl_Kind (Decl)));
      end case;
      New_Line;
   end Disp_Decl;

   procedure Disp_Decls (Indent : Natural; First, Last : O_Dnode)
   is
      N : O_Dnode;
   begin
      N := First;
      while N <= Last loop
         case Get_Decl_Kind (N) is
            when OD_Body =>
               Disp_Decl (Indent, N);
               Ortho_Code.Exprs.Disp_Subprg_Body
                 (Indent + 2, Get_Body_Stmt (N));
               N := N + 1;
            when OD_Block =>
               --  Skip inner bindings.
               N := Get_Block_Last (N) + 1;
            when others =>
               Disp_Decl (Indent, N);
               N := N + 1;
         end case;
      end loop;
   end Disp_Decls;

   procedure Disp_Block (Indent : Natural; Start : O_Dnode)
   is
      Last : O_Dnode;
   begin
      if Get_Decl_Kind (Start) /= OD_Block then
         Disp_Decl (Indent, Start);
         raise Program_Error;
      end if;
      Last := Get_Block_Last (Start);
      Disp_Decl (Indent, Start);
      Disp_Decls (Indent, Start + 1, Last);
   end Disp_Block;

   procedure Disp_All_Decls
   is
   begin
      if False then
         for I in Dnodes.First .. Dnodes.Last loop
            Disp_Decl (1, I);
         end loop;
      end if;

      Disp_Decls (1, Dnodes.First, Dnodes.Last);
   end Disp_All_Decls;

   procedure Debug_Decl (Decl : O_Dnode) is
   begin
      Disp_Decl (1, Decl);
   end Debug_Decl;

   pragma Unreferenced (Debug_Decl);

   procedure Disp_Stats
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Number of Dnodes: " & O_Dnode'Image (Dnodes.Last));
      Put_Line ("Number of TDnodes: " & O_Tnode'Image (TDnodes.Last));
   end Disp_Stats;

   procedure Mark (M : out Mark_Type) is
   begin
      M.Dnode := Dnodes.Last;
      M.TDnode := TDnodes.Last;
   end Mark;

   procedure Release (M : Mark_Type) is
   begin
      Dnodes.Set_Last (M.Dnode);
      TDnodes.Set_Last (M.TDnode);
   end Release;

   procedure Alloc_Zero is
   begin
      if not Flag_Debug_Hli then
         --  Expand not explicitly initialized variables.
         declare
            N : O_Dnode;
            Init : O_Cnode;
         begin
            N := Dnodes.First;
            while N <= Dnodes.Last loop
               if Get_Decl_Kind (N) = OD_Var then
                  case Get_Decl_Storage (N) is
                     when O_Storage_Private
                       | O_Storage_Public =>
                        Init := Get_Init_Value (N);
                        if Init = O_Cnode_Null then
                           Expand_Var_Zero (N);
                        end if;
                     when O_Storage_External =>
                        null;
                     when O_Storage_Local =>
                        raise Program_Error;
                  end case;
               end if;
               N := Get_Decl_Chain (N);
            end loop;
         end;
      end if;
   end Alloc_Zero;

   procedure Finish is
   begin
      Dnodes.Free;
      TDnodes.Free;
   end Finish;
end Ortho_Code.Decls;
