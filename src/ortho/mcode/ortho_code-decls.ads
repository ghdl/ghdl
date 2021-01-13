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
with Ortho_Code.Abi;

package Ortho_Code.Decls is
   --  Kind of a declaration.
   type OD_Kind is
     (
      OD_Type,
      OD_Const,

      --  Value of constant, initial value of variable.
      OD_Init_Val,

      --  Global and local variables.
      OD_Var, OD_Local,

      --  Subprograms.
      OD_Function, OD_Procedure,

      --  Additional node for a subprogram.  Internal use only.
      OD_Subprg_Ext,

      OD_Interface,
      OD_Body,
      OD_Block
     );

   --  Return the kind of declaration DECL.
   function Get_Decl_Kind (Decl : O_Dnode) return OD_Kind;

   --  Return the type of a declaration.
   function Get_Decl_Type (Decl : O_Dnode) return O_Tnode;

   --  Return the identifier of a declaration.
   function Get_Decl_Ident (Decl : O_Dnode) return O_Ident;

   --  Return the storage of a declaration.
   function Get_Decl_Storage (Decl : O_Dnode) return O_Storage;

   --  Return the depth of a declaration.
   function Get_Decl_Depth (Decl : O_Dnode) return O_Depth;

   --  Register for the declaration.
   function Get_Decl_Reg (Decl : O_Dnode) return O_Reg;
   procedure Set_Decl_Reg (Decl : O_Dnode; Reg : O_Reg);

   --  Return the next decl (in the same scope) after DECL.
   --  This skips declarations in an inner block, but returns interfaces for
   --  a subprogram.
   function Get_Decl_Chain (Decl : O_Dnode) return O_Dnode;

   --  Get the last declaration.
   function Get_Decl_Last return O_Dnode;

   --  Return the subprogram declaration correspondig to body BOD.
   function Get_Body_Decl (Bod : O_Dnode) return O_Dnode;

   --  Return the parent of a body.
   function Get_Body_Parent (Bod : O_Dnode) return O_Dnode;

   --  Get the entry statement of body DECL.
   function Get_Body_Stmt (Bod : O_Dnode) return O_Enode;

   --  Get/Set the info field of a body.
   function Get_Body_Info (Bod : O_Dnode) return Int32;
   procedure Set_Body_Info (Bod : O_Dnode; Info : Int32);

   --  Get the last declaration of block BLK.
   function Get_Block_Last (Blk : O_Dnode) return O_Dnode;

   --  Get/Set the block max stack offset.
   function Get_Block_Max_Stack (Blk : O_Dnode) return Uns32;
   procedure Set_Block_Max_Stack (Blk : O_Dnode; Max : Uns32);

   --  Info on blocks.
   function Get_Block_Info1 (Blk : O_Dnode) return Int32;
   procedure Set_Block_Info1 (Blk : O_Dnode; Info : Int32);
   function Get_Block_Info2 (Blk : O_Dnode) return Int32;
   procedure Set_Block_Info2 (Blk : O_Dnode; Info : Int32);

   --  Get the declaration and the value associated with a constant value.
   function Get_Val_Decl (Decl : O_Dnode) return O_Dnode;
   function Get_Val_Val (Decl : O_Dnode) return O_Cnode;

   --  Declare a type.
   --  This simply gives a name to a type.
   procedure New_Type_Decl (Ident : O_Ident; Atype : O_Tnode);

   --  If Flag_Type_Name is set, a map from type to name is maintained.
   function Get_Type_Decl (Atype : O_Tnode) return O_Dnode;

   --  Set/Get the offset (or register) of interface or local DECL.
   --  To be used by ABI.
   procedure Set_Local_Offset (Decl : O_Dnode; Off : Int32);
   function Get_Local_Offset (Decl : O_Dnode) return Int32;

   --  Get/Set user info on subprogram, variable, constant declaration.
   procedure Set_Decl_Info (Decl : O_Dnode; Ref : Int32);
   function Get_Decl_Info (Decl : O_Dnode) return Int32;

   --  Get/Set the stack size of subprogram arguments.
   procedure Set_Subprg_Stack (Decl : O_Dnode; Val : Int32);
   function Get_Subprg_Stack (Decl : O_Dnode) return Int32;

   --  Get the first interface of a subprogram declaration.
   function Get_Subprg_Interfaces (Decl : O_Dnode) return O_Dnode;

   --  Get the next interface.
   --  End of interface chain when result is O_Dnode_Null.
   function Get_Interface_Chain (Decl : O_Dnode) return O_Dnode;

   --  Declare a constant.
   --  This simply gives a name to a constant value or aggregate.
   --  A constant cannot be modified and its storage cannot be local.
   --  ATYPE must be constrained.
   procedure New_Const_Decl (Res : out O_Dnode;
                             Ident : O_Ident;
                             Storage : O_Storage;
                             Atype : O_Tnode);

   --  Set the value to DECL.
   procedure New_Init_Value (Decl : O_Dnode; Val : O_Cnode);

   --  Create a variable declaration.
   --  A variable can be local only inside a function.
   --  ATYPE must be constrained.
   procedure New_Var_Decl (Res : out O_Dnode;
                           Ident : O_Ident;
                           Storage : O_Storage;
                           Atype : O_Tnode);

   type O_Inter_List is limited private;

   --  Start a subprogram declaration.
   --  Note: nested subprograms are allowed, ie o_storage_local subprograms can
   --   be declared inside a subprograms.  It is not allowed to declare
   --   o_storage_external subprograms inside a subprograms.
   --  Return type and interfaces cannot be a composite type.
   procedure Start_Function_Decl (Interfaces : out O_Inter_List;
                                  Ident : O_Ident;
                                  Storage : O_Storage;
                                  Rtype : O_Tnode);
   --  For a subprogram without return value.
   procedure Start_Procedure_Decl (Interfaces : out O_Inter_List;
                                   Ident : O_Ident;
                                   Storage : O_Storage);

   --  Add an interface declaration to INTERFACES.
   procedure New_Interface_Decl (Interfaces : in out O_Inter_List;
                                 Res : out O_Dnode;
                                 Ident : O_Ident;
                                 Atype : O_Tnode);
   --  Finish the function declaration, get the node and a statement list.
   procedure Finish_Subprogram_Decl
     (Interfaces : in out O_Inter_List; Res : out O_Dnode);

   --  Start subprogram body of DECL.  STMT is the corresponding statement.
   --  Return the declaration for the body.
   function Start_Subprogram_Body (Decl : O_Dnode; Stmt : O_Enode)
                                  return O_Dnode;
   procedure Finish_Subprogram_Body;

   --  Start a declarative region.
   function Start_Declare_Stmt return O_Dnode;
   procedure Finish_Declare_Stmt (Parent : O_Dnode);

   procedure Disp_All_Decls;
   procedure Disp_Block (Indent : Natural; Start : O_Dnode);
   procedure Disp_Decl_Name (Decl : O_Dnode);
   procedure Disp_Decl (Indent : Natural; Decl : O_Dnode);
   procedure Disp_Stats;

   type Mark_Type is limited private;
   procedure Mark (M : out Mark_Type);
   procedure Release (M : Mark_Type);

   --  Allocate non explicitly initialized variables.
   procedure Alloc_Zero;

   procedure Finish;
private
   type O_Inter_List is record
      --  The declaration of the subprogram.
      Decl : O_Dnode;

      --  Last declared parameter.
      Last_Param : O_Dnode;

      --  Data for ABI.
      Abi : Ortho_Code.Abi.O_Abi_Subprg;
   end record;

   type Mark_Type is record
      Dnode : O_Dnode;
      TDnode : O_Tnode;
   end record;

end Ortho_Code.Decls;
