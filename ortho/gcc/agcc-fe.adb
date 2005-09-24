--  Ortho implementation for GCC.
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
with Ada.Unchecked_Deallocation;
--with Agcc.Ggc; use Agcc.Ggc;
with Agcc.Tm; use Agcc.Tm;
with Agcc.Machmode; use Agcc.Machmode;
with Agcc.Diagnostic;
with Agcc.Input; use Agcc.Input;
with Agcc.Options; use Agcc.Options;
with Ortho_Gcc;
with Ortho_Gcc_Front; use Ortho_Gcc_Front;

package body Agcc.Fe is
   File_Name : String_Acc;

   Stdin_Filename : String_Acc := new String'("*stdin*" & Nul);

   function Lang_Init_Options (Argc : Integer; Argv : C_String_Array)
                              return Integer
   is
      pragma Unreferenced (Argc);
      pragma Unreferenced (Argv);
   begin
      return CL_vhdl;
   end Lang_Init_Options;

   function Lang_Handle_Option (Code : Opt_Code;
                                Arg : C_String;
                                Value : Integer)
     return Integer
   is
      pragma Unreferenced (Value);
      --type String_Acc_Array_Acc is access String_Acc_Array;

      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Name => String_Acc, Object => String);
      --procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
      --  (Name => String_Acc_Array_Acc, Object => String_Acc_Array);

      --C_Opt : C_String := Argv (0);
      --C_Arg : C_String;
      --Opt : String := C_Opt (1 .. C_String_Len (C_Opt));
      Res : Natural;
      Opt : String_Acc;
      Opt_Arg : String_Acc;
      Len : Natural;
   begin
      if Arg /= C_String_Null then
         Len := C_String_Len (Arg);
      else
         Len := 0;
      end if;
      Opt_Arg := null;
      case Code is
         when OPT_U_std_U =>
            Opt := new String'("--std=" & Arg (1 .. Len));
         when OPT_U_compile_standard =>
            Opt := new String'("--compile-standard");
         when OPT_U_bootstrap =>
            Opt := new String'("--bootstrap");
         when OPT_U_work_U =>
            Opt := new String'("--work=" & Arg (1 .. Len));
         when OPT_U_workdir_U =>
            Opt := new String'("--workdir=" & Arg (1 .. Len));
         when OPT_UP =>
            Opt := new String'("-P" & Arg (1 .. Len));
         when OPT_U_elab =>
            Opt := new String'("--elab");
            Opt_Arg := new String'(Arg (1 .. Len));
         when OPT_U_anaelab =>
            Opt := new String'("--anaelab");
            Opt_Arg := new String'(Arg (1 .. Len));
         when OPT_l =>
            Opt := new String'("-l");
            Opt_Arg := new String'(Arg (1 .. Len));
         when OPT_c =>
            Opt := new String'("-c");
            Opt_Arg := new String'(Arg (1 .. Len));
         when OPT_U_ghdl  =>
            Opt := new String'(Arg (1 .. Len));
         when OPT_U_warn_U  =>
            Opt := new String'("--warn-" & Arg (1 .. Len));
         when OPT_U_expect_failure =>
            Opt := new String'("--expect-failure");
         when OPT_U_no_vital_checks =>
            Opt := new String'("--no-vital-checks");
         when OPT_U_vital_checks =>
            Opt := new String'("--vital-checks");
         when OPT_fexplicit =>
            Opt := new String'("-fexplicit");
         when OPT_v =>
            Opt := new String'("-v");
         when others =>
            return 0;
      end case;
      Res := Ortho_Gcc_Front.Decode_Option (Opt, Opt_Arg);
      Unchecked_Deallocation (Opt);
      Unchecked_Deallocation (Opt_Arg);
      return Res;
   end Lang_Handle_Option;

   function Lang_Post_Options (Filename : C_String_Acc) return C_Bool
   is
      Filename_Len : Natural;
   begin
      if Filename.all = C_String_Null then
         File_Name := null;
         Filename.all := To_C_String (Stdin_Filename);
      else
         Filename_Len := C_String_Len (Filename.all);
         File_Name := new String'(Filename.all (1 .. Filename_Len));
      end if;

      --  Run the back-end.
      return C_False;
   end Lang_Post_Options;


   procedure Lang_Parse_File (Debug : C_Bool)
   is
      pragma Unreferenced (Debug);
   begin
      if not Ortho_Gcc_Front.Parse (File_Name) then
         Agcc.Diagnostic.Set_Errorcount (1);
      end if;
   end Lang_Parse_File;

   function Lang_Get_Alias_Set (T : Tree) return HOST_WIDE_INT
   is
      pragma Unreferenced (T);
   begin
      return -1;
   end Lang_Get_Alias_Set;

   --function Lang_Safe_From_P (Target : Rtx; Exp : Tree) return Boolean;

   function Mark_Addressable (Exp : Tree) return C_Bool
   is
      N : Tree;
      Code : Tree_Code;
   begin
      N := Exp;
      loop
         Code := Get_TREE_CODE (N);
         case Code is
            when VAR_DECL
              | CONST_DECL
              | PARM_DECL
              | RESULT_DECL =>
               Put_Var_Into_Stack (N, C_True);
               Set_TREE_ADDRESSABLE (N, C_True);
               return C_True;

            when COMPONENT_REF
              | ARRAY_REF =>
               N := Get_TREE_OPERAND (N, 0);

            when FUNCTION_DECL
              | CONSTRUCTOR =>
               Set_TREE_ADDRESSABLE (N, C_True);
               return C_True;

            when INDIRECT_REF =>
               return C_True;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Mark_Addressable;

   procedure Insert_Default_Attributes (Func : Tree)
   is
      pragma Unreferenced (Func);
   begin
      null;
   end Insert_Default_Attributes;

   --  These functions and variables deal with binding contours.

   --  For each binding contour we allocate a binding_level structure which
   --  records the entities defined or declared in that contour.
   --  Contours include:
   --
   --     the global one
   --     one for each subprogram definition
   --     one for each compound statement (declare block)
   --
   --  Binding contours are used to create GCC tree BLOCK nodes.

   --  BE CAREFUL: this structure is also declared in agcc-bindings.c
   type Binding_Level;
   type Binding_Level_Acc is access Binding_Level;
   type Binding_Level is record
      --  A chain of ..._DECL nodes for all variables, constants, functions,
      --  parameters and type declarations.  These ..._DECL nodes are chained
      --  through the TREE_CHAIN field. Note that these ..._DECL nodes are
      --  stored in the reverse of the order supplied to be compatible with
      --  the back-end.
      Names : Tree;

      --  For each level (except the global one), a chain of BLOCK nodes for
      --  all the levels that were entered and exited one level down from this
      --  one.
      Blocks : Tree;

      --  The back end may need, for its own internal processing, to create a
      --  BLOCK node. This field is set aside for this purpose. If this field
      --  is non-null when the level is popped, i.e. when poplevel is invoked,
      --  we will use such block instead of creating a new one from the
      --  'names' field, that is the ..._DECL nodes accumulated so far.
      --  Typically the routine 'pushlevel' will be called before setting this
      --  field, so that if the front-end had inserted ..._DECL nodes in the
      --  current block they will not be lost.
      Block_Created_By_Back_End : Tree;

      --  The binding level containing this one (the enclosing binding level).
      Level_Chain : Binding_Level_Acc;
   end record;
   pragma Convention (C, Binding_Level_Acc);
   pragma Convention (C, Binding_Level);

   --  The binding level currently in effect.
   Current_Binding_Level : Binding_Level_Acc := null;
   pragma Export (C, Current_Binding_Level);

   --  The outermost binding level. This binding level is created when the
   --  compiler is started and it will exist through the entire compilation.
   Global_Binding_Level : Binding_Level_Acc;

   --  Chain of unused binding levels, since they are never deallocated.
   Old_Binding_Level : Binding_Level_Acc := null;
   pragma Export (C, Old_Binding_Level);

   function Alloc_Binding_Level return Binding_Level_Acc;
   pragma Import (C, Alloc_Binding_Level);

   -- Binding level structures are initialized by copying this one.
   Clear_Binding_Level : constant Binding_Level :=
     (Names => NULL_TREE,
      Blocks => NULL_TREE,
      Block_Created_By_Back_End => NULL_TREE,
      Level_Chain => null);

   -- Return non-zero if we are currently in the global binding level.
   function Global_Bindings_P return Integer is
   begin
      if Current_Binding_Level = Global_Binding_Level then
         return 1;
      else
         return 0;
      end if;
   end Global_Bindings_P;

   --  Return the list of declarations in the current level. Note that this
   --  list is in reverse order (it has to be so for back-end compatibility).
   function Getdecls return Tree is
   begin
      return Current_Binding_Level.Names;
   end Getdecls;

   --  Nonzero if the current level needs to have a BLOCK made.
--    function Kept_Level_P return Boolean is
--    begin
--       return Current_Binding_Level.Names /= NULL_TREE;
--    end Kept_Level_P;

   --  Enter a new binding level. The input parameter is ignored, but has to
   --  be specified for back-end compatibility.
   procedure Pushlevel (Inside : C_Bool)
   is
      pragma Unreferenced (Inside);
      Newlevel : Binding_Level_Acc;

   begin
      if Old_Binding_Level /= null then
         Newlevel := Old_Binding_Level;
         Old_Binding_Level := Old_Binding_Level.Level_Chain;
      else
         Newlevel := Alloc_Binding_Level;
      end if;
      Newlevel.all := Clear_Binding_Level;

      --  Add this level to the front of the chain (stack) of levels that are
      --  active.
      Newlevel.Level_Chain := Current_Binding_Level;
      Current_Binding_Level := Newlevel;
   end Pushlevel;

   --  Exit a binding level.
   --  Pop the level off, and restore the state of the identifier-decl mappings
   --  that were in effect when this level was entered.
   --
   --  If KEEP is nonzero, this level had explicit declarations, so
   --  and create a "block" (a BLOCK node) for the level
   --  to record its declarations and subblocks for symbol table output.
   --
   --  If FUNCTIONBODY is nonzero, this level is the body of a function,
   --  so create a block as if KEEP were set and also clear out all
   --  label names.
   --
   --  If REVERSE is nonzero, reverse the order of decls before putting
   --  them into the BLOCK.
   function Exported_Poplevel
     (Keep : C_Bool; Revers : C_Bool; Functionbody : C_Bool)
     return Tree
   is
      --  Points to a BLOCK tree node. This is the BLOCK node construted for
      --  the binding level that we are about to exit and which is returned
      --  by this routine.
      Block_Node : Tree := NULL_TREE;

      Decl_Chain : Tree;
      Subblock_Chain : Tree;
      Subblock_Node : Tree;
      Block_Created_By_Back_End : Tree;

      N : Tree;
      Tmp : Binding_Level_Acc;
   begin
      Decl_Chain := Current_Binding_Level.Names;
      Block_Created_By_Back_End :=
        Current_Binding_Level.Block_Created_By_Back_End;
      Subblock_Chain := Current_Binding_Level.Blocks;

      --  Pop the current level, and save it on the chain of old binding
      --  levels.
      Tmp := Current_Binding_Level;
      Current_Binding_Level := Tmp.Level_Chain;
      Tmp.Level_Chain := Old_Binding_Level;
      Old_Binding_Level := Tmp;

      --  Reverse the list of XXXX_DECL nodes if desired.  Note that
      --  the ..._DECL nodes chained through the `names' field of
      --  current_binding_level are in reverse order except for PARM_DECL node,
      --  which are explicitely stored in the right order.
      if Revers /= C_False then
         Decl_Chain := Nreverse (Decl_Chain);
      end if;

      if Block_Created_By_Back_End /= NULL_TREE then
         Block_Node := Block_Created_By_Back_End;

         --  Check if we are about to discard some information that was
         --  gathered by the front-end. Nameley check if the back-end created
         --  a new block without calling pushlevel first. To understand why
         --  things are lost just look at the next case (i.e. no block
         --  created by back-end.  */
         if (Keep /= C_False or Functionbody /= C_False)
           and then (Decl_Chain /= NULL_TREE or Subblock_Chain /= NULL_TREE)
         then
            raise Program_Error;
         end if;
      elsif Keep /= C_False or Functionbody /= C_False then
         --  If there were any declarations in the current binding level, or if
         --  this binding level is a function body, or if there are any nested
         --  blocks then create a BLOCK node to record them for the life of
         --  this function.
         if Keep /= C_False then
            N := Decl_Chain;
         else
            N := NULL_TREE;
         end if;
         Block_Node := Build_Block
           (N, NULL_TREE, Subblock_Chain, NULL_TREE, NULL_TREE);
      end if;

      --  Record the BLOCK node just built as the subblock its enclosing scope.
      Subblock_Node := Subblock_Chain;
      while Subblock_Node /= NULL_TREE loop
         Set_BLOCK_SUPERCONTEXT (Subblock_Node, Block_Node);
         Subblock_Node := Get_TREE_CHAIN (Subblock_Node);
      end loop;

      --  Clear out the meanings of the local variables of this level.
      Subblock_Node := Decl_Chain;
      while Subblock_Node /= NULL_TREE loop

         if Get_DECL_NAME (Subblock_Node) /= NULL_TREE then
            --  If the identifier was used or addressed via a local
            --  extern decl, don't forget that fact.
            if Get_DECL_EXTERNAL (Subblock_Node) /= C_False then
               if Get_TREE_USED (Subblock_Node) /= C_False then
                  Set_TREE_USED (Get_DECL_NAME (Subblock_Node), C_True);
               end if;
               if Get_TREE_ADDRESSABLE (Subblock_Node) /= C_False then
                  Set_TREE_ADDRESSABLE
                    (Get_DECL_ASSEMBLER_NAME (Subblock_Node), C_True);
               end if;
            end if;
         end if;
         Subblock_Node := Get_TREE_CHAIN (Subblock_Node);
      end loop;

      if Functionbody /= C_False then
         --  This is the top level block of a function. The ..._DECL chain
         --  stored in BLOCK_VARS are the function's parameters (PARM_DECL
         --  nodes). Don't leave them in the BLOCK because they are found
         --  in the FUNCTION_DECL instead.
         Set_DECL_INITIAL (Current_Function_Decl, Block_Node);
         Set_BLOCK_VARS (Block_Node, NULL_TREE);
      elsif Block_Node /= NULL_TREE then
         if Block_Created_By_Back_End = NULL_TREE then
            Current_Binding_Level.Blocks
              := Chainon (Current_Binding_Level.Blocks, Block_Node);
         end if;
      elsif Subblock_Chain /= NULL_TREE then
         --  If we did not make a block for the level just exited, any blocks
         --  made for inner levels (since they cannot be recorded as subblocks
         --  in that level) must be carried forward so they will later become
         --  subblocks of something else.
         Current_Binding_Level.Blocks
           := Chainon (Current_Binding_Level.Blocks, Subblock_Chain);
      end if;

      if Block_Node /= NULL_TREE then
         Set_TREE_USED (Block_Node, C_True);
      end if;

      return Block_Node;
   end Exported_Poplevel;

   --  Insert BLOCK at the end of the list of subblocks of the
   --  current binding level.  This is used when a BIND_EXPR is expanded,
   --  to handle the BLOCK node inside the BIND_EXPR.
   procedure Insert_Block (Block : Tree) is
   begin
      Set_TREE_USED (Block, C_True);
      Current_Binding_Level.Blocks
        := Chainon (Current_Binding_Level.Blocks, Block);
   end Insert_Block;

   --  Set the BLOCK node for the innermost scope (the one we are
   --  currently in).
   procedure Set_Block (Block : Tree) is
   begin
      Current_Binding_Level.Block_Created_By_Back_End := Block;
   end Set_Block;

   --  Records a ..._DECL node DECL as belonging to the current lexical scope.
   --  Returns the ..._DECL node.
   function Exported_Pushdecl (Decl : Tree) return Tree
   is
   begin
      --  External objects aren't nested, other objects may be.
      if Get_DECL_EXTERNAL (Decl) /= C_False then
         Set_DECL_CONTEXT (Decl, NULL_TREE);
      else
         Set_DECL_CONTEXT (Decl, Current_Function_Decl);
      end if;

      --  Put the declaration on the list.  The list of declarations is in
      --  reverse order. The list will be reversed later if necessary.  This
      --  needs to be this way for compatibility with the back-end.
      Set_TREE_CHAIN (Decl, Current_Binding_Level.Names);
      Current_Binding_Level.Names := Decl;

      -- For the declaration of a type, set its name if it is not already set.
      if Get_TREE_CODE (Decl) = TYPE_DECL
        and then Get_TYPE_NAME (Get_TREE_TYPE (Decl)) = NULL_TREE
      then
         Set_TYPE_NAME (Get_TREE_TYPE (Decl), Decl); -- DECL_NAME (decl);
      end if;

      return Decl;
   end Exported_Pushdecl;

   --  This variable keeps a table for types for each precision so that we only
   --  allocate each of them once. Signed and unsigned types are kept separate.
   type Signed_And_Unsigned_Types_Array_Type is
     array (Natural range 0 .. MAX_BITS_PER_WORD, C_Boolean) of Tree;
   Signed_And_Unsigned_Types : Signed_And_Unsigned_Types_Array_Type :=
     (others => (others => NULL_TREE));
   pragma Export (C, Signed_And_Unsigned_Types);

   --  Return an integer type with the number of bits of precision given by
   --  PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   --  it is a signed type.
   function Type_For_Size (Precision : Natural; Unsignedp : C_Bool)
     return Tree
   is
      T : Tree;
   begin
      if Precision <= MAX_BITS_PER_WORD
        and then Signed_And_Unsigned_Types (Precision, Unsignedp) /= NULL_TREE
      then
         return Signed_And_Unsigned_Types (Precision, Unsignedp);
      end if;

      if Unsignedp /= C_False then
         T := Make_Unsigned_Type (Precision);
      else
         T := Make_Signed_Type (Precision);
      end if;
      if Precision <= MAX_BITS_PER_WORD then
         Signed_And_Unsigned_Types (Precision, Unsignedp) := T;
      end if;
      return T;
   end Type_For_Size;

   --  Return a data type that has machine mode MODE.  UNSIGNEDP selects
   --  an unsigned type; otherwise a signed type is returned.
   function Type_For_Mode (Mode : Machine_Mode; Unsignedp : C_Bool)
     return Tree
   is
   begin
      return Type_For_Size (GET_MODE_BITSIZE (Mode), Unsignedp);
   end Type_For_Mode;

   --  Return the unsigned version of a TYPE_NODE, a scalar type.
   function Unsigned_Type (Type_Node : Tree) return Tree
   is
   begin
      return Type_For_Size (Get_TYPE_PRECISION (Type_Node), C_True);
   end Unsigned_Type;

   --  Return the signed version of a TYPE_NODE, a scalar type.
   function Signed_Type (Type_Node : Tree) return Tree
   is
   begin
      return Type_For_Size (Get_TYPE_PRECISION (Type_Node), C_False);
   end Signed_Type;

   --  Return a type the same as TYPE except unsigned or signed according to
   --  UNSIGNEDP.
   function Signed_Or_Unsigned_Type (Unsignedp : C_Bool; Atype : Tree)
     return Tree
   is
   begin
      if INTEGRAL_TYPE_P (Atype) = C_False
        or else Get_TREE_UNSIGNED (Atype) = Unsignedp
      then
         return Atype;
      else
         return Type_For_Size (Get_TYPE_PRECISION (Atype), Unsignedp);
      end if;
   end Signed_Or_Unsigned_Type;


   --procedure Init_Type_For_Size;
   --pragma Import (C, Init_Type_For_Size);

   Int_Str : constant String := "int" & Nul;
   Char_Str : constant String := "char" & Nul;

   Builtin_Alloca_Str : constant String := "__builtin_alloca" & Nul;

   function Lang_Init return C_Bool
   is
      --File : String renames Filename (1 .. Filename_Len);
      Ptr_Ftype_Sizetype : Tree;
      Alloca_Function : Tree;
   begin
      --Error_Mark_Node := Make_Node (ERROR_MARK);
      --Set_TREE_TYPE (Error_Mark_Node, Error_Mark_Node);

      --Initialize_Sizetypes;

      --  The structure `tree_identifier' is the GCC tree data structure that
      --  holds IDENTIFIER_NODE nodes. We need to call `set_identifier_size'
      --  to tell GCC that we have not added any language specific fields to
      --  IDENTIFIER_NODE nodes.
      --Set_Identifier_Size (Tree_Identifier_Size);
      Input_Location.Line := 0;

      -- Make the binding_level structure for global names.
      Pushlevel (C_False);
      Global_Binding_Level := Current_Binding_Level;

      Build_Common_Tree_Nodes (C_False);
      Pushdecl (Build_Decl (TYPE_DECL, Get_Identifier (Int_Str'Address),
                            Integer_Type_Node));
      Pushdecl (Build_Decl (TYPE_DECL, Get_Identifier (Char_Str'Address),
                            Char_Type_Node));
      Set_Sizetype (Unsigned_Type_Node);
      Build_Common_Tree_Nodes_2 (C_False);

      --Init_Type_For_Size;

      --  Create alloc builtin.
      Ptr_Ftype_Sizetype := Build_Function_Type
        (Ptr_Type_Node,
         Tree_Cons (NULL_TREE, Get_TYPE_DOMAIN (Sizetype), NULL_TREE));
      Alloca_Function := Builtin_Function
        (Builtin_Alloca_Str'Address, Ptr_Ftype_Sizetype,
         BUILT_IN_ALLOCA, BUILT_IN_NORMAL, System.Null_Address);
      Ortho_Gcc.Alloca_Function_Ptr := Build1
        (ADDR_EXPR, Build_Pointer_Type (Ptr_Ftype_Sizetype), Alloca_Function);
--        Ggc_Add_Tree_Root (Ortho_Gcc.Alloca_Function_Ptr'Address, 1);

      Ortho_Gcc.Init;

      --  Continue.
      return C_True;
   end Lang_Init;

   procedure Lang_Finish is
   begin
      null;
   end Lang_Finish;

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
     return Tree
   is
      use System;
      Decl : Tree;
   begin
      Decl := Build_Decl (FUNCTION_DECL, Get_Identifier (Name), Ftype);
      Set_DECL_EXTERNAL (Decl, C_True);
      Set_TREE_PUBLIC (Decl, C_True);
      if Library_Name /= Null_Address then
         Set_DECL_ASSEMBLER_NAME (Decl, Get_Identifier (Library_Name));
      end if;
      Make_Decl_Rtl (Decl, NULL_Chars, C_True);
      Pushdecl (Decl);
      Set_DECL_BUILT_IN_CLASS (Decl, Class);
      Set_DECL_FUNCTION_CODE (Decl, Function_Code);
      return Decl;
   end Builtin_Function;

   procedure Set_Yydebug (Flag : C_Bool)
   is
      pragma Unreferenced (Flag);
   begin
      null;
   end Set_Yydebug;

   procedure Print_Lang_Decl (File : FILEs; Node : Tree; Indent : natural)
   is
      pragma Unreferenced (File);
      pragma Unreferenced (Node);
      pragma Unreferenced (Indent);
   begin
      null;
   end Print_Lang_Decl;

   procedure Print_Lang_Type (File : FILEs; Node : Tree; Indent : Natural)
   is
      pragma Unreferenced (File);
      pragma Unreferenced (Node);
      pragma Unreferenced (Indent);
   begin
      null;
   end Print_Lang_Type;

   procedure Print_Lang_Identifier
     (File : FILEs; Node : Tree; Indent : Natural)
   is
      pragma Unreferenced (File);
      pragma Unreferenced (Node);
      pragma Unreferenced (Indent);
   begin
      null;
   end Print_Lang_Identifier;

   procedure Lang_Print_Xnode (File : FILEs; Node : Tree; Indent : Natural)
   is
      pragma Unreferenced (File);
      pragma Unreferenced (Node);
      pragma Unreferenced (Indent);
   begin
      --  There is no X nodes.
      raise Program_Error;
   end Lang_Print_Xnode;

   procedure Print_Lang_Statistics is
   begin
      null;
   end Print_Lang_Statistics;

   procedure Copy_Lang_Decl (Node : Tree)
   is
      pragma Unreferenced (Node);
   begin
      null;
   end Copy_Lang_Decl;

   function Truthvalue_Conversion (Expr : Tree) return Tree
   is
      Expr_Type : Tree;
      type Conv_Array is array (Boolean) of Tree;
      Conv : Conv_Array;
   begin
      Expr_Type := Get_TREE_TYPE (Expr);
      if Get_TREE_CODE (Expr_Type) /= BOOLEAN_TYPE then
         Conv := (True => Integer_One_Node,
                  False => Integer_Zero_Node);
      else
         Conv := (False => Get_TYPE_MIN_VALUE (Expr_Type),
                  True => Get_TYPE_MAX_VALUE (Expr_Type));
      end if;

      --  From java/decl.c
      --  It is simpler and generates better code to have only TRUTH_*_EXPR
      --  or comparison expressions as truth values at this level.

      case Get_TREE_CODE (Expr) is
         when EQ_EXPR
           | NE_EXPR
           | LE_EXPR
           | GE_EXPR
           | LT_EXPR
           | GT_EXPR
           | TRUTH_ANDIF_EXPR
           | TRUTH_ORIF_EXPR
           | TRUTH_AND_EXPR
           | TRUTH_OR_EXPR
           | ERROR_MARK =>
            return Expr;

         when INTEGER_CST =>
            if Integer_Zerop (Expr) = C_False then
               --  EXPR is not 0, so EXPR is interpreted as TRUE.
               return Conv (True);
            else
               return Conv (False);
            end if;

         when REAL_CST =>
            if Real_Zerop (Expr) = C_False then
               return Conv (True);
            else
               return Conv (False);
            end if;

         when others =>
            raise Program_Error;
      end case;
   end Truthvalue_Conversion;

   procedure Incomplete_Type_Error (Value : Tree; Atype : Tree)
   is
      pragma Unreferenced (Value);
      pragma Unreferenced (Atype);
   begin
      --  Can never happen.
      raise Program_Error;
   end Incomplete_Type_Error;

   function Maybe_Build_Cleanup (Decl : Tree) return Tree
   is
      pragma Unreferenced (Decl);
   begin
      return NULL_TREE;
   end Maybe_Build_Cleanup;

   Language_Name : constant String := "GNU vhdl" & Nul;
   pragma Export (C, Language_Name);
end Agcc.Fe;
