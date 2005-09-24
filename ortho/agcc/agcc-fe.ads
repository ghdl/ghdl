--  Ada bindings for GCC internals.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Agcc.Trees; use Agcc.Trees;
with Agcc.Machmode; use Agcc.Machmode;
with Agcc.Hwint; use Agcc.Hwint;
with Agcc.Options; use Agcc.Options;
with Interfaces.C_Streams; use Interfaces.C_Streams;
with C; use C;

package Agcc.Fe is
   --  Subprograms that must be defined by the front-end.

   --  Defined in langhooks.h
   function Lang_Init_Options (Argc : Integer; Argv : C_String_Array)
     return Integer;

   --  Front-end function expected by GCC.
   function Lang_Handle_Option (Code : Opt_Code;
                                Arg : C_String;
                                Value : Integer)
                               return Integer;

   type C_String_Acc is access C_String;
   pragma Convention (C, C_String_Acc);

   function Lang_Post_Options (Filename : C_String_Acc) return C_Bool;

   function Lang_Init return C_Bool;

   procedure Lang_Finish;

   --procedure Lang_Clear_Binding_Stack;

   --  Return the typed-based alias set for T, which may be an expression
   --  or a type.  Return -1 if we don't do anything special.
   --  O means can alias everything.
   function Lang_Get_Alias_Set (T : Tree) return HOST_WIDE_INT;

   --function Lang_Expand_Constant (N : Tree) return Tree;

   --function Lang_Safe_From_P (Target : Rtx; Exp : Tree) return C_Bool;

   procedure Lang_Parse_File (Debug : C_Bool);

   --  Called by the back-end or by the front-end when the address of EXP
   --  must be taken.
   --  This function should found the base object (if any), and mark it as
   --  addressable (via TREE_ADDRESSABLE).  It may emit a warning if this
   --  object cannot be addressable (front-end restriction).
   --  Returns TRUE in case of success, FALSE in case of failure.
   --  Note that the status is never checked by the back-end.
   function Mark_Addressable (Exp : Tree) return C_Bool;

   --  Possibly apply default attributes to function FUNC represented by
   --  a FUNCTION_DECL.
   procedure Insert_Default_Attributes (Func : Tree);

   --  Lexical scopes.
   --  Roughly speaking, it is used to mark declarations regions.

   --  Enter in a new lexical scope.  INSIDE should be FALSE (TRUE iff called
   --  from the inside of the front end, ie from gcc internal code).
   procedure Pushlevel (Inside : C_Bool);

   --  Add a declaration to the current scope.
   --  Note: GCC backend expect PUSHDECL to return its argument; however,
   --  it is only seldom used.  Both forms exist and are aliased with a third
   --  one which is exported under the C name.
   --  (Unfortunatly, it is not possible to export the function and to import
   --   the procedure).
   procedure Pushdecl (Decl : Tree);
   function Pushdecl (Decl : Tree) return Tree;

   --  This function has to be defined.
   function Exported_Pushdecl (Decl : Tree) return Tree;

   --  Get the declarations of the current scope.
   function Getdecls return Tree;

   procedure Set_Block (Block : Tree);

   -- Return non-zero if we are currently in the global binding level.
   function Global_Bindings_P return Integer;

   --  Insert BLOCK at the end of the list of subblocks of the
   --  current binding level.  This is used when a BIND_EXPR is expanded,
   --  to handle the BLOCK node inside the BIND_EXPR.
   procedure Insert_Block (Block : Tree);

   --  Exit the current scope.
   --  FUNCTIONBODY is TRUE iff the scope corresponds to a subprogram scope.
   --  Used forms (both imported).
   procedure Poplevel (Keep : C_Bool; Revers : C_Bool; Functionbody : C_Bool);
   function Poplevel (Keep : C_Bool; Revers : C_Bool; Functionbody : C_Bool)
     return Tree;

   --  Exported form.
   function Exported_Poplevel
     (Keep : C_Bool; Revers : C_Bool; Functionbody : C_Bool)
     return Tree;

   --  Perform all the initialization steps that are language-specific.
   --procedure Lang_Init;

   --  Perform all the finalization steps that are language-specific.
   --procedure Lang_Finish;

   --  Return an integer type with the number of bits of precision given by
   --  PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   --  it is a signed type.
   function Type_For_Size (Precision : Natural; Unsignedp : C_Bool)
     return Tree;

   --  Return a data type that has machine mode MODE.  UNSIGNEDP selects
   --  an unsigned type; otherwise a signed type is returned.
   function Type_For_Mode (Mode : Machine_Mode; Unsignedp : C_Bool)
     return Tree;

   --  Return the unsigned version of a TYPE_NODE, a scalar type.
   function Unsigned_Type (Type_Node : Tree) return Tree;

   --  Return the signed version of a TYPE_NODE, a scalar type.
   function Signed_Type (Type_Node : Tree) return Tree;

   --  Return a type the same as TYPE except unsigned or signed according to
   --  UNSIGNEDP.
   function Signed_Or_Unsigned_Type (Unsignedp : C_Bool; Atype : Tree)
     return Tree;

   --  Return a definition for a builtin function named NAME and whose data
   --  type is TYPE.  TYPE should be a function type with argument types.
   --  FUNCTION_CODE tells later passes how to compile calls to this function.
   --  See tree.h for its possible values.
   --
   --  If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   --  the name to be called if we can't opencode the function.
   function Builtin_Function
     (Name: System.Address;
      Ftype : Tree;
      Function_Code : Built_In_Function;
      Class : Built_In_Class;
      Library_Name : System.Address)
     return Tree;

   --  Set debug flag of the parser.
   procedure Set_Yydebug (Flag : C_Bool);


   --  Hooks for print-tree.c:
   procedure Print_Lang_Decl (File : FILEs; Node : Tree; Indent : natural);
   procedure Print_Lang_Type (File : FILEs; Node : Tree; Indent : Natural);
   procedure Print_Lang_Identifier
     (File : FILEs; Node : Tree; Indent : Natural);
   procedure Lang_Print_Xnode (File : FILEs; Node : Tree; Indent : Natural);

   --  Print any language-specific compilation statistics.
   procedure Print_Lang_Statistics;


   --  Finish to copy a ..._DECL node (the LANG_DECL_SPECIFIC field).
   procedure Copy_Lang_Decl (Node : Tree);

   --  Normalize boolean value EXPR.
   function Truthvalue_Conversion (Expr : Tree) return Tree;

   --  Procedure called in case of sizeof applied to an incomplete type.
   procedure Incomplete_Type_Error (Value : Tree; Atype : Tree);

   --  This function must be defined in the language-specific files.
   --  expand_expr calls it to build the cleanup-expression for a TARGET_EXPR.
   function Maybe_Build_Cleanup (Decl : Tree) return Tree;

   --Language_String : constant Chars;
   Flag_Traditional : Integer := 0;
private
   pragma Export (C, Lang_Init_Options);
   pragma Export (C, Lang_Handle_Option);
   pragma Export (C, Lang_Post_Options);
   pragma Export (C, Lang_Init);
   pragma Export (C, Lang_Finish);
   pragma Export (C, Lang_Get_Alias_Set);

   pragma Export (C, Lang_Parse_File);

   pragma Export (C, Mark_Addressable);
   pragma Export (C, Insert_Default_Attributes);

   pragma Import (C, Pushdecl);
   pragma Export (C, Exported_Pushdecl, "pushdecl");
   pragma Export (C, Pushlevel);
   pragma Export (C, Set_Block);
   pragma Export (C, Insert_Block);
   pragma Export (C, Global_Bindings_P);
   pragma Import (C, Poplevel);
   pragma Export (C, Exported_Poplevel, "poplevel");
   pragma Export (C, Getdecls);

   pragma Export (C, Type_For_Size);
   pragma Export (C, Type_For_Mode);
   pragma Export (C, Unsigned_Type);
   pragma Export (C, Signed_Type);
   pragma Export (C, Signed_Or_Unsigned_Type);

   pragma Export (C, Builtin_Function);


   pragma Export (C, Set_Yydebug);

   pragma Export (C, Print_Lang_Decl);
   pragma Export (C, Print_Lang_Type);
   pragma Export (C, Print_Lang_Identifier);
   pragma Export (C, Lang_Print_Xnode);

   pragma Export (C, Print_Lang_Statistics);
   pragma Export (C, Copy_Lang_Decl);

   pragma Export (C, Truthvalue_Conversion);
   pragma Export (C, Incomplete_Type_Error);
   pragma Export (C, Maybe_Build_Cleanup);

   pragma Export (C, Flag_Traditional);
end Agcc.Fe;

