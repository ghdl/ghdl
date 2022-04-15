--  Iir to ortho translator.
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
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with Ortho_Nodes; use Ortho_Nodes;
with Ortho_Ident; use Ortho_Ident;
with Vhdl.Nodes; use Vhdl.Nodes;
with Types; use Types;

package Trans is

   --  Ortho type node for STD.BOOLEAN.
   Std_Boolean_Type_Node         : O_Tnode;
   Std_Boolean_True_Node         : O_Cnode;
   Std_Boolean_False_Node        : O_Cnode;
   --  Array of STD.BOOLEAN.
   Std_Boolean_Array_Type        : O_Tnode;
   --  Std_ulogic indexed array of STD.Boolean.
   Std_Ulogic_Boolean_Array_Type : O_Tnode;
   --  Ortho type node for string template pointer.
   Std_String_Ptr_Node           : O_Tnode;
   Std_String_Node               : O_Tnode;

   --  Ortho type for std.standard.integer.
   Std_Integer_Otype : O_Tnode;

   --  Ortho type for std.standard.real.
   Std_Real_Otype : O_Tnode;

   --  Ortho type node for std.standard.time.
   Std_Time_Otype : O_Tnode;

   --  Node for the variable containing the current filename.
   Current_Filename_Node : O_Dnode := O_Dnode_Null;
   Current_Library_Unit  : Iir := Null_Iir;

   --  Global declarations.
   Ghdl_Ptr_Type           : O_Tnode;
   Sizetype                : O_Tnode;
   Ghdl_I32_Type           : O_Tnode;
   Ghdl_I64_Type           : O_Tnode;
   Ghdl_Real_Type          : O_Tnode;
   --  Constant character.
   Char_Type_Node          : O_Tnode;
   --  Array of char.
   Chararray_Type          : O_Tnode;
   --  Pointer to array of char.
   Char_Ptr_Type           : O_Tnode;
   --  Array of char ptr.
   Char_Ptr_Array_Type     : O_Tnode;
   Char_Ptr_Array_Ptr_Type : O_Tnode;

   Ghdl_Index_Type : O_Tnode;
   Ghdl_Index_0    : O_Cnode;
   Ghdl_Index_1    : O_Cnode;
   Ghdl_Index_2    : O_Cnode;
   Ghdl_Index_4    : O_Cnode;
   Ghdl_Index_8    : O_Cnode;
   Ghdl_Index_Ptr_Align  : O_Cnode;  --  Alignment of a pointer

   --  Type for a file (this is in fact a index in a private table).
   Ghdl_File_Index_Type     : O_Tnode;
   Ghdl_File_Index_Ptr_Type : O_Tnode;

   --  Record containing a len and string fields.
   Ghdl_Str_Len_Type_Node       : O_Tnode;
   Ghdl_Str_Len_Type_Len_Field  : O_Fnode;
   Ghdl_Str_Len_Type_Str_Field  : O_Fnode;
   Ghdl_Str_Len_Ptr_Node        : O_Tnode;
   Ghdl_Str_Len_Array_Type_Node : O_Tnode;

   --  Location.
   Ghdl_Location_Type_Node     : O_Tnode;
   Ghdl_Location_Filename_Node : O_Fnode;
   Ghdl_Location_Line_Node     : O_Fnode;
   Ghdl_Location_Col_Node      : O_Fnode;
   Ghdl_Location_Ptr_Node      : O_Tnode;

   --  Allocate memory for a block.
   Ghdl_Alloc_Ptr : O_Dnode;

   --  bool type.
   Ghdl_Bool_Type : O_Tnode;
   type Enode_Boolean_Array is array (Boolean) of O_Cnode;
   Ghdl_Bool_Nodes : Enode_Boolean_Array;
   Ghdl_Bool_False_Node : O_Cnode renames Ghdl_Bool_Nodes (False);
   Ghdl_Bool_True_Node : O_Cnode renames Ghdl_Bool_Nodes (True);

   Ghdl_Bool_Array_Type : O_Tnode;
   Ghdl_Bool_Array_Ptr  : O_Tnode;

   --  Size record
   Ghdl_Sizes_Type : O_Tnode;
   Ghdl_Sizes_Val : O_Fnode;
   Ghdl_Sizes_Sig : O_Fnode;

   --  Access to size.
   Ghdl_Sizes_Ptr : O_Tnode;

   --  Comparaison type.
   Ghdl_Compare_Type : O_Tnode;
   Ghdl_Compare_Lt   : O_Cnode;
   Ghdl_Compare_Eq   : O_Cnode;
   Ghdl_Compare_Gt   : O_Cnode;

   --  Dir type.
   Ghdl_Dir_Type_Node   : O_Tnode;
   Ghdl_Dir_To_Node     : O_Cnode;
   Ghdl_Dir_Downto_Node : O_Cnode;

   --  Signals.
   Ghdl_Scalar_Bytes               : O_Tnode;
   Ghdl_Signal_Type                : O_Tnode;
   Ghdl_Signal_Driving_Value_Field : O_Fnode;
   Ghdl_Signal_Last_Value_Field    : O_Fnode;
   Ghdl_Signal_Last_Event_Field    : O_Fnode;
   Ghdl_Signal_Last_Active_Field   : O_Fnode;
   Ghdl_Signal_Value_Field         : O_Fnode;
   Ghdl_Signal_Event_Field         : O_Fnode;
   Ghdl_Signal_Active_Field        : O_Fnode;
   Ghdl_Signal_Has_Active_Field    : O_Fnode;

   Ghdl_Signal_Ptr     : O_Tnode;
   Ghdl_Signal_Ptr_Ptr : O_Tnode;

   Check_Stack_Allocation_Threshold : O_Cnode;

   type Object_Kind_Type is (Mode_Value, Mode_Signal);

   --  Well known identifiers.
   Wki_This          : O_Ident;
   Wki_Size          : O_Ident;
   Wki_Res           : O_Ident;
   Wki_Dir_To        : O_Ident;
   Wki_Dir_Downto    : O_Ident;
   Wki_Left          : O_Ident;
   Wki_Right         : O_Ident;
   Wki_Dir           : O_Ident;
   Wki_Length        : O_Ident;
   Wki_I             : O_Ident;
   Wki_Instance      : O_Ident;
   Wki_Arch_Instance : O_Ident;
   Wki_Name          : O_Ident;
   Wki_Sig           : O_Ident;
   Wki_Obj           : O_Ident;
   Wki_Rti           : O_Ident;
   Wki_Parent        : O_Ident;
   Wki_Filename      : O_Ident;
   Wki_Line          : O_Ident;
   Wki_Lo            : O_Ident;
   Wki_Hi            : O_Ident;
   Wki_Mid           : O_Ident;
   Wki_Cmp           : O_Ident;
   Wki_Upframe       : O_Ident;
   Wki_Frame         : O_Ident;
   Wki_Val           : O_Ident;
   Wki_L_Len         : O_Ident;
   Wki_R_Len         : O_Ident;
   Wki_Base          : O_Ident;
   Wki_Bounds        : O_Ident;
   Wki_Locvars       : O_Ident;
   Wki_Flag          : O_Ident;

   --  ALLOCATION_KIND defines the type of memory storage.
   --  ALLOC_STACK means the object is allocated on the local stack and
   --    deallocated at the end of the function.
   --  ALLOC_SYSTEM for object created during design elaboration and whose
   --    life is infinite.
   --  ALLOC_RETURN for unconstrained object returns by function.
   --  ALLOC_HEAP for object created by new.
   type Allocation_Kind is
     (Alloc_Stack, Alloc_Return, Alloc_Heap, Alloc_System);

   --  Sometimes useful to factorize code.  Defines what has to be translated.
   type Subprg_Translate_Kind is
     (Subprg_Translate_Only_Spec,
      Subprg_Translate_Spec_And_Body,
      Subprg_Translate_Only_Body);
   subtype Subprg_Translate_Spec is Subprg_Translate_Kind range
     Subprg_Translate_Only_Spec .. Subprg_Translate_Spec_And_Body;
   subtype Subprg_Translate_Body is Subprg_Translate_Kind range
     Subprg_Translate_Spec_And_Body .. Subprg_Translate_Only_Body;

   --  Return the value of field FIELD of lnode L that is contains
   --   a pointer to a record.
   --  This is equivalent to:
   --  new_value (new_selected_element (new_access_element (new_value (l)),
   --                                   field))
   function New_Value_Selected_Acc_Value (L : O_Lnode; Field : O_Fnode)
                                          return O_Enode;
   function New_Selected_Acc_Value (L : O_Lnode; Field : O_Fnode)
                                    return O_Lnode;

   function New_Indexed_Acc_Value (L : O_Lnode; I : O_Enode) return O_Lnode;

   --  Equivalent to new_access_element (new_value (l))
   function New_Acc_Value (L : O_Lnode) return O_Lnode;

   --  Return PTR + OFFSET as a RES_PTR value.  The offset is the number of
   --  bytes.  RES_PTR must be an access type and the type of PTR must be an
   --  access.
   function Add_Pointer
     (Ptr : O_Enode; Offset : O_Enode; Res_Ptr : O_Tnode) return O_Enode;

   type Elab_Kind is (Elab_Decls, Elab_Stmts);
   type O_Dnode_Elab is array (Elab_Kind) of O_Dnode;

   package Chap10 is
      --  There are three data storage kind: global, local or instance.
      --  For example, a constant can have:
      --  * a global storage when declared inside a package.  This storage
      --    can be accessed from any point.
      --  * a local storage when declared in a subprogram.  This storage
      --    can be accessed from the subprogram, is created when the subprogram
      --    is called and destroy when the subprogram exit.
      --  * an instance storage when declared inside a process.  This storage
      --    can be accessed from the process via an instance pointer, is
      --    created during elaboration.
      --procedure Push_Global_Factory (Storage : O_Storage);
      --procedure Pop_Global_Factory;
      procedure Set_Global_Storage (Storage : O_Storage);

      --  Set the global scope handling.
      Global_Storage : O_Storage;

      --  Scope for variables.  This is used both to build instances (so it
      --  contains the record type that contains objects declared in that
      --  scope) and to use instances (it contains the path to access to these
      --  objects).
      type Var_Scope_Type is private;

      type Var_Scope_Acc is access all Var_Scope_Type;
      for Var_Scope_Acc'Storage_Size use 0;

      Null_Var_Scope : constant Var_Scope_Type;

      type Var_Type is private;
      Null_Var : constant Var_Type;

      --  Return the record type for SCOPE.
      function Get_Scope_Type (Scope : Var_Scope_Type) return O_Tnode;

      --  Return the size for instances of SCOPE.
      function Get_Scope_Size (Scope : Var_Scope_Type) return O_Cnode;

      --  Return True iff SCOPE is defined.
      function Has_Scope_Type (Scope : Var_Scope_Type) return Boolean;

      --  Create an empty and incomplete scope type for SCOPE using NAME.
      procedure Predeclare_Scope_Type
        (Scope : in out Var_Scope_Type; Name : O_Ident);

      --  Declare a pointer PTR_TYPE with NAME to scope type SCOPE.
      procedure Declare_Scope_Acc
        (Scope : Var_Scope_Type; Name : O_Ident; Ptr_Type : out O_Tnode);

      --  Start to build an instance.
      --  If INSTANCE_TYPE is not O_TNODE_NULL, it must be an uncompleted
      --  record type, that will be completed.
      procedure Push_Instance_Factory (Scope : Var_Scope_Acc);

      --  Likewise but for a frame.
      procedure Push_Frame_Factory (Scope : Var_Scope_Acc;
                                    Persistant : Boolean);

      --  Manually add a field to the current instance being built.
      function Add_Instance_Factory_Field (Name : O_Ident; Ftype : O_Tnode)
                                           return O_Fnode;

      --  In the scope being built, add a field NAME that contain sub-scope
      --  CHILD.  CHILD is modified so that accesses to CHILD objects is done
      --  via SCOPE.
      procedure Add_Scope_Field
        (Name : O_Ident; Child : in out Var_Scope_Type);

      --  Return the offset of field for CHILD in its parent scope.
      function Get_Scope_Offset (Child : Var_Scope_Type; Otype : O_Tnode)
                                 return O_Cnode;

      --  Finish the building of the current instance and return the type
      --  built.
      procedure Pop_Instance_Factory (Scope : Var_Scope_Acc);
      procedure Pop_Frame_Factory (Scope : Var_Scope_Acc);

      --  Create a new scope, in which variable are created locally
      --  (ie, on the stack).  Always created unlocked.
      procedure Push_Local_Factory;

      --  Destroy a local scope.
      procedure Pop_Local_Factory;

      --  Create a special scope for declarations in statements.  The scope
      --  structure is opaque (typically a union).
      procedure Create_Union_Scope
        (Scope : out Var_Scope_Type; Stype : O_Tnode);

      --  Set_Scope defines how to access to variables of SCOPE.
      --  Variables defined in SCOPE can be accessed via field SCOPE_FIELD
      --  of scope SCOPE_PARENT.
      procedure Set_Scope_Via_Field
        (Scope       : in out Var_Scope_Type;
         Scope_Field : O_Fnode; Scope_Parent : Var_Scope_Acc);

      --  Variables defined in SCOPE can be accessed by dereferencing
      --  field SCOPE_FIELD defined in SCOPE_PARENT.
      procedure Set_Scope_Via_Field_Ptr
        (Scope       : in out Var_Scope_Type;
         Scope_Field : O_Fnode; Scope_Parent : Var_Scope_Acc);

      --  Variables/scopes defined in SCOPE can be accessed via
      --  dereference of parameter SCOPE_PARAM.
      procedure Set_Scope_Via_Param_Ptr
        (Scope : in out Var_Scope_Type; Scope_Param : O_Dnode);

      --  Variables/scopes defined in SCOPE can be accessed via DECL.
      procedure Set_Scope_Via_Decl
        (Scope : in out Var_Scope_Type; Decl : O_Dnode);

      --  Variables/scopes defined in SCOPE can be accessed by derefencing
      --  VAR.
      procedure Set_Scope_Via_Var_Ptr
        (Scope : in out Var_Scope_Type; Var : Var_Type);

      --  Variables/scopes defined in SCOPE can be accesses through VAR.
      procedure Set_Scope_Via_Var
        (Scope : in out Var_Scope_Type; Var : Var_Type);

      --  No more accesses to SCOPE_TYPE are allowed.  Scopes must be cleared
      --  before being set.
      procedure Clear_Scope (Scope : in out Var_Scope_Type);

      --  True if SCOPE is a null-scope (eg. was cleared).
      function Is_Null (Scope : Var_Scope_Type) return Boolean;

      --  Reset the identifier.
      type Id_Mark_Type is limited private;
      type Local_Identifier_Type is private;

      procedure Reset_Identifier_Prefix;
      procedure Push_Identifier_Prefix (Mark : out Id_Mark_Type;
                                        Name : String;
                                        Val  : Iir_Int32 := 0);
      procedure Push_Identifier_Prefix (Mark : out Id_Mark_Type;
                                        Name : Name_Id;
                                        Val  : Iir_Int32 := 0);
      procedure Push_Identifier_Prefix_Uniq (Mark : out Id_Mark_Type);
      procedure Pop_Identifier_Prefix (Mark : in Id_Mark_Type);

      --  Save/restore the local identifier number; this is used by package
      --  body, which has the same prefix as the package declaration, so it
      --  must continue local identifiers numbers.
      --  This is used by subprogram bodies too.
      procedure Save_Local_Identifier (Id : out Local_Identifier_Type);
      procedure Restore_Local_Identifier (Id : Local_Identifier_Type);

      --  Create an identifier from IIR node ID without the prefix.
      function Create_Identifier_Without_Prefix (Id : Iir) return O_Ident;
      function Create_Identifier_Without_Prefix
        (Id : Iir; Str : String) return O_Ident;
      function Create_Identifier_Without_Prefix
        (Id : Name_Id; Str : String) return O_Ident;

      --  Create an identifier from the current prefix.
      function Create_Identifier return O_Ident;

      --  Create an identifier from IIR node ID with prefix.
      function Create_Identifier (Id : Iir; Str : String := "")
                                  return O_Ident;
      function Create_Identifier
        (Id : Iir; Val : Iir_Int32; Str : String := "")
         return O_Ident;
      function Create_Identifier (Id : Name_Id; Str : String := "")
                                  return O_Ident;
      --  Create a prefixed identifier from a string.
      function Create_Identifier (Str : String) return O_Ident;

      --  Create an identifier for an elaboration procedure.
      function Create_Elab_Identifier (Kind : Elab_Kind) return O_Ident;

      --  Create an identifier for a variable.
      --  IE, if the variable is global, prepend the prefix,
      --   if the variable belong to an instance, no prefix is added.
      type Var_Ident_Type is private;
      function Create_Var_Identifier (Id : Iir) return Var_Ident_Type;
      function Create_Var_Identifier (Id : String) return Var_Ident_Type;
      function Create_Var_Identifier (Id : Iir; Str : String; Val : Natural)
                                      return Var_Ident_Type;
      function Create_Uniq_Identifier return Var_Ident_Type;

      --  Create variable NAME of type VTYPE in the current scope.
      --  If the current scope is the global scope, then a variable is
      --   created at the top level (using decl_global_storage).
      --  If the current scope is not the global scope, then a field is added
      --   to the current scope.
      function Create_Var
        (Name    : Var_Ident_Type;
         Vtype   : O_Tnode;
         Storage : O_Storage := Global_Storage)
         return Var_Type;

      --  Create a global variable.
      function Create_Global_Var
        (Name : O_Ident; Vtype : O_Tnode; Storage : O_Storage)
         return Var_Type;

      --  Create a global constant and initialize it to INITIAL_VALUE.
      function Create_Global_Const
        (Name          : O_Ident;
         Vtype         : O_Tnode;
         Storage       : O_Storage;
         Initial_Value : O_Cnode)
         return Var_Type;
      procedure Define_Global_Const (Const : in out Var_Type; Val : O_Cnode);

      --  Return the (real) reference to a variable created by Create_Var.
      function Get_Var (Var : Var_Type) return O_Lnode;

      --  Return a reference to the instance of type ITYPE.
      function Get_Instance_Ref (Scope : Var_Scope_Type) return O_Lnode;

      --  Return the address of the instance for block BLOCK.
      function Get_Instance_Access (Block : Iir) return O_Enode;

      --  Return the storage for the variable VAR.
      function Get_Alloc_Kind_For_Var (Var : Var_Type) return Allocation_Kind;

      --  Return TRUE iff VAR is stable, ie get_var (VAR) can be referenced
      --  several times.
      function Is_Var_Stable (Var : Var_Type) return Boolean;

      --  Used only to generate RTI.
      function Is_Var_Field (Var : Var_Type) return Boolean;
      function Get_Var_Offset (Var : Var_Type; Otype : O_Tnode) return O_Cnode;
      function Get_Var_Label (Var : Var_Type) return O_Dnode;

      --  For package instantiation.

      --  Associate INST_SCOPE as the instantiated scope for ORIG_SCOPE.
      procedure Push_Instantiate_Var_Scope
        (Inst_Scope : Var_Scope_Acc; Orig_Scope : Var_Scope_Acc);

      --  Remove the association for INST_SCOPE.
      procedure Pop_Instantiate_Var_Scope
        (Inst_Scope : Var_Scope_Acc);

      --  Get the associated instantiated scope for SCOPE.
      function Instantiated_Var_Scope (Scope : Var_Scope_Acc)
                                       return Var_Scope_Acc;

      --  Create a copy of VAR using instantiated scope (if needed).
      function Instantiate_Var (Var : Var_Type) return Var_Type;

      --  Create a copy of SCOPE using instantiated scope (if needed).
      function Instantiate_Var_Scope (Scope : Var_Scope_Type)
                                     return Var_Scope_Type;

      --  Utility function: convert identifier of N to a string, encoding
      --  extended characters in extended identifiers (this is different from
      --  image_identifier that simply returns the identifier, without special
      --  handling of extended identifiers).
      function Identifier_To_String (N : Iir) return String;
   private
      type Local_Identifier_Type is new Natural;
      type Id_Mark_Type is record
         Len      : Natural;
         Local_Id : Local_Identifier_Type;
      end record;

      type Var_Ident_Type is record
         Id : O_Ident;
      end record;

      --  An instance contains all the data (variable, signals, constant...)
      --  which are declared by an entity and an architecture.
      --  (An architecture inherits the data of its entity).
      --
      --  The processes and implicit guard signals of an entity/architecture
      --  are translated into functions.  The first argument of these functions
      --  is a pointer to the instance.

      type Inst_Build_Kind_Type is
        (
         --  Variables are declared locally.
         Local,

         --  Variables are global.
         Global,

         --  A record frame is created, whose lifetime is the lifetime of the
         --  subprogram.  Variables become fields of the record frame, and
         --  dynamic memory is allocated from the stack.
         Stack_Frame,

         --  A record frame is created, whose lifetime is longer than the
         --  lifetime of the subprogram (for subprogram with suspension).
         --  Variables become fields, and dynamic memory is allocated from the
         --  secondary stack.
         Persistant_Frame,

         --  An instance record is created, which is never free.  Dynamic
         --  memory is allocated from the heap.
         Instance);

      type Inst_Build_Type (Kind : Inst_Build_Kind_Type);
      type Inst_Build_Acc is access Inst_Build_Type;
      type Inst_Build_Type (Kind : Inst_Build_Kind_Type) is record
         Prev          : Inst_Build_Acc;
         Prev_Id_Start : Natural;
         case Kind is
            when Local =>
               --  Previous global storage.
               Prev_Global_Storage : O_Storage;
            when Global =>
               null;
            when Instance | Stack_Frame | Persistant_Frame =>
               Scope               : Var_Scope_Acc;
               Elements            : O_Element_List;
         end case;
      end record;

      --  Kind of variable:
      --  VAR_NONE: the variable doesn't exist.
      --  VAR_GLOBAL: the variable is a global variable (static or not).
      --  VAR_LOCAL: the variable is on the stack.
      --  VAR_SCOPE: the variable is in the instance record.
      type Var_Kind is (Var_None, Var_Global, Var_Local, Var_Scope);

      type Var_Type (Kind : Var_Kind := Var_None) is record
         case Kind is
            when Var_None =>
               null;
            when Var_Global
               | Var_Local =>
               E       : O_Dnode;
            when Var_Scope =>
               --  To remember allocator for this variable.
               I_Build_Kind : Inst_Build_Kind_Type;

               I_Field : O_Fnode;
               I_Scope : Var_Scope_Acc;
         end case;
      end record;

      Null_Var : constant Var_Type := (Kind => Var_None);

      type Var_Scope_Kind is (Var_Scope_None,
                              Var_Scope_Ptr,
                              Var_Scope_Decl,
                              Var_Scope_Field,
                              Var_Scope_Field_Ptr);

      type Var_Scope_Type (Kind : Var_Scope_Kind := Var_Scope_None) is record
         Scope_Type : O_Tnode := O_Tnode_Null;

         case Kind is
            when Var_Scope_None =>
               --  Not set, cannot be referenced.
               null;
            when Var_Scope_Ptr
               | Var_Scope_Decl =>
               --  Instance for entity, architecture, component, subprogram,
               --  resolver, process, guard function, PSL directive, PSL cover,
               --  PSL assert, component instantiation elaborator
               D       : O_Dnode;
            when Var_Scope_Field
               | Var_Scope_Field_Ptr =>
               --  For an entity: the architecture.
               --  For an architecture: ptr to a generate subblock.
               --  For a subprogram: parent frame
               Field   : O_Fnode;
               Up_Link : Var_Scope_Acc;
         end case;
      end record;

      Null_Var_Scope : constant Var_Scope_Type := (Scope_Type => O_Tnode_Null,
                                                   Kind => Var_Scope_None);

   end Chap10;
   use Chap10;

   package Subprgs is
      --  Subprograms instances.
      --
      --  Subprograms declared inside entities, architecture, blocks
      --   or processes (but not inside packages) may access to data declared
      --   outside the subprogram (and this with a life longer than the
      --   subprogram life).  These data correspond to constants, variables,
      --   files, signals or types.  However these data are not shared between
      --   instances of the same entity, architecture...  Subprograms instances
      --   is the way subprograms access to these data.
      --  One subprogram instance corresponds to a record.

      --  Type to save an old instance builder.  Subprograms may have at most
      --  one instance.  If they need severals (for example a protected
      --  subprogram), the most recent one will have a reference to the
      --  previous one.
      type Subprg_Instance_Stack is limited private;

      --  Declare an instance to be added for subprograms.
      --  SCOPE is the scope to pass to the subprogram.
      --  PTR_TYPE is a pointer to SCOPE.
      --  IDENT is an identifier for the interface.
      --  The previous instance is stored to PREV.  It must be restored with
      --  Pop_Subprg_Instance.
      --  Add_Subprg_Instance_Interfaces will add an interface of name IDENT
      --   and type PTR_TYPE for every instance declared by
      --   Push_Subprg_Instance.
      procedure Push_Subprg_Instance (Scope    : Var_Scope_Acc;
                                      Ptr_Type : O_Tnode;
                                      Ident    : O_Ident;
                                      Prev     : out Subprg_Instance_Stack);

      --  Since local subprograms has a direct access to its father interfaces,
      --  they do not required instances interfaces.
      --  These procedures are provided to temporarly disable the addition of
      --  instances interfaces. Use Pop_Subpg_Instance to restore to the
      --  previous state.
      procedure Clear_Subprg_Instance (Prev : out Subprg_Instance_Stack);

      --  Revert of the previous subprogram.
      --  Instances must be removed in opposite order they are added.
      procedure Pop_Subprg_Instance (Ident : O_Ident;
                                     Prev  : Subprg_Instance_Stack);

      --  True iff there is currently a subprogram instance.
      function Has_Current_Subprg_Instance return Boolean;

      --  Contains the subprogram interface for the instance.
      type Subprg_Instance_Type is private;
      Null_Subprg_Instance : constant Subprg_Instance_Type;

      --  Add interfaces during the creation of a subprogram.
      procedure Add_Subprg_Instance_Interfaces
        (Interfaces : in out O_Inter_List; Vars : out Subprg_Instance_Type);

      --  Add a field in the current factory that reference the current
      --  instance.
      procedure Add_Subprg_Instance_Field
        (Field : out O_Fnode; Prev_Scope : out Var_Scope_Acc);

      --  Associate values to the instance interface during invocation of a
      --  subprogram.
      procedure Add_Subprg_Instance_Assoc
        (Assocs : in out O_Assoc_List; Vars : Subprg_Instance_Type);

      --  Get the value to be associated to the instance interface.
      function Get_Subprg_Instance (Vars : Subprg_Instance_Type)
                                    return O_Enode;

      --  True iff VARS is associated with an instance.
      function Has_Subprg_Instance (Vars : Subprg_Instance_Type)
                                    return Boolean;

      --  Assign the instance field FIELD of VAR.
      procedure Set_Subprg_Instance_Field
        (Var : O_Dnode; Field : O_Fnode; Vars : Subprg_Instance_Type);

      --  To be called at the beginning and end of a subprogram body creation.
      --  Call PUSH_SCOPE for the subprogram intances.
      procedure Start_Subprg_Instance_Use (Vars : Subprg_Instance_Type);
      procedure Finish_Subprg_Instance_Use (Vars : Subprg_Instance_Type);

      --  Call Push_Scope to reference instance from FIELD.
      procedure Start_Prev_Subprg_Instance_Use_Via_Field
        (Prev_Scope : Var_Scope_Acc; Field : O_Fnode);
      procedure Finish_Prev_Subprg_Instance_Use_Via_Field
        (Prev_Scope : Var_Scope_Acc; Field : O_Fnode);

      --  Same as above, but for IIR.
      procedure Create_Subprg_Instance (Interfaces : in out O_Inter_List;
                                        Subprg     : Iir);

      procedure Start_Subprg_Instance_Use (Subprg : Iir);
      procedure Finish_Subprg_Instance_Use (Subprg : Iir);

      function Instantiate_Subprg_Instance (Inst : Subprg_Instance_Type)
                                            return Subprg_Instance_Type;
   private
      type Subprg_Instance_Type is record
         Inter      : O_Dnode;
         Inter_Type : O_Tnode;
         Scope      : Var_Scope_Acc;
      end record;
      Null_Subprg_Instance : constant Subprg_Instance_Type :=
        (O_Dnode_Null, O_Tnode_Null, null);

      type Subprg_Instance_Stack is record
         Scope    : Var_Scope_Acc;
         Ptr_Type : O_Tnode;
         Ident    : O_Ident;
      end record;

      Null_Subprg_Instance_Stack : constant Subprg_Instance_Stack :=
        (null, O_Tnode_Null, O_Ident_Nul);

      Current_Subprg_Instance : Subprg_Instance_Stack :=
        Null_Subprg_Instance_Stack;
   end Subprgs;

   type Ortho_Info_Kind is
     (
      Kind_Type,
      Kind_Incomplete_Type,
      Kind_Index,
      Kind_Enum_Lit,
      Kind_Subprg,
      Kind_Operator,
      Kind_Call,
      Kind_Call_Assoc,
      Kind_Object,
      Kind_Signal,
      Kind_Alias,
      Kind_Iterator,
      Kind_Interface,
      Kind_Disconnect,
      Kind_Process,
      Kind_Psl_Directive,
      Kind_Loop,
      Kind_Loop_State,
      Kind_Locvar_State,
      Kind_Block,
      Kind_Generate,
      Kind_Component,
      Kind_Field,
      Kind_Package,
      Kind_Package_Instance,
      Kind_Config,
      Kind_Assoc,
      Kind_Inertial_Assoc,
      Kind_Design_File,
      Kind_Library,
      Kind_Expr_Eval
     );

   type Ortho_Info_Type_Kind is
     (
      Kind_Type_Scalar,
      Kind_Type_Array,
      Kind_Type_Record,
      Kind_Type_File,
      Kind_Type_Protected
     );
   type O_Tnode_Array is array (Object_Kind_Type) of O_Tnode;
   type O_Fnode_Array is array (Object_Kind_Type) of O_Fnode;
   type O_Dnode_Array is array (Object_Kind_Type) of O_Dnode;
   type Var_Type_Array is array (Object_Kind_Type) of Var_Type;

   type Rti_Depth_Type is new Natural range 0 .. 255;

   --  Additional info for complex types.
   type Complex_Type_Info is record
      --  Parameters for type builders.
      --  NOTE: this is only set for types (and *not* for subtypes).
      Builder_Instance     : Subprgs.Subprg_Instance_Type;
      Builder_Layout_Param : O_Dnode;
      Builder_Proc         : O_Dnode := O_Dnode_Null;
   end record;
   type Complex_Type_Arr_Info is array (Object_Kind_Type) of Complex_Type_Info;

   --  Alignment of a type.
   --  This is only for Mode_Value (for Mode_Signal, the alignment is
   --  Align_Ptr).
   --  The size of complex types is determined at run-time, and the code to
   --  compute it is generated by translation.  But to know the size, the
   --  alignment must also be known.  It is assumed that allocators (malloc or
   --  alloca) always return a pointer with the maximum alignment.
   --  Eg:  type cpl_rec is record
   --         b : boolean;
   --         v : integer_array (1 to n);  -- n is a non-locally constant.
   --       end record;
   --  The static part contains only field 'b'.  The whole size is of cpl_rec
   --  is:  sizeof (b) + align(v) + n * sizeof(integer) + align(cpl_rec).
   --  This makes a lot of suppositions about the ABI:
   --    * elementary types (including doubles) are always naturally aligned
   --    * fields are aligned as their type
   --    * records are aligned to their maximum field
   --    * pointers have the same size
   --    * finally, pointers are either 32 or 64 bits.
   --  Note: deviation from the ABI may result in incorrect code as an object
   --   that is statically constrained may be viewed as a complex/unbounded
   --   object too.
   --  Note: These suppositions are true on x86-64, on windows32.
   --        but not for double on linux-x86!!
   type Alignment_Type is
     (
      --  When alignment is not known.
      Align_Undef,

      --  For enumerations, integers, physical types.
      Align_8, Align_16, Align_32,

      --  For an access.  We suppose that pointers are either 32 or 64 bits.
      --  So Align_Ptr >= Align_32 but Align_64 >= Align_Ptr
      Align_Ptr,

      --  For float64 (floating point types), large integers or large physical
      --  types.
      Align_64);

   function Align_Val (Algn : Alignment_Type) return O_Cnode;

   --  Mode of the type; roughly speaking, this corresponds to its size
   --  (for scalars) or its layout (for composite types).
   --  Used to select library subprograms for signals.
   type Type_Mode_Type is
     (
      --  Unknown mode.
      Type_Mode_Unknown,

      --  Boolean type, with 2 elements.
      Type_Mode_B1,
      --  Enumeration with at most 256 elements.
      Type_Mode_E8,
      --  Enumeration with more than 256 elements.
      Type_Mode_E32,
      --  Integer types.
      Type_Mode_I32,
      Type_Mode_I64,
      --  Physical types.
      Type_Mode_P32,
      Type_Mode_P64,
      --  Floating point type.
      Type_Mode_F64,
      --  File type.
      Type_Mode_File,
      --  Thin access.
      Type_Mode_Acc,

      --  Access to an unbounded type (this is a thin pointer to bounds
      --  followed by values).
      Type_Mode_Bounds_Acc,

      --  Record whose size is known at compile-time.  Can be a boxed record
      --  if the base type is unbounded.
      Type_Mode_Static_Record,
      --  Constrained record, but size is not known at compile time.  Can be
      --  a boxed record if the base type is unbounded.
      Type_Mode_Complex_Record,
      --  Record with unbounded component(s).
      Type_Mode_Unbounded_Record,

      --  Unbounded array type (used for unconstrained arrays).
      Type_Mode_Unbounded_Array,
      --  Constrainted array type, with size known at compile-time.
      Type_Mode_Static_Array,
      --  Constrained array type (for constrained arrays), but size is
      --  not known at compile time.
      Type_Mode_Complex_Array,
      --  Protected type (always handled as a complex type).
      Type_Mode_Protected);

   --  For backward source compatibility, to be removed (TODO).
   Type_Mode_Fat_Array : constant Type_Mode_Type := Type_Mode_Unbounded_Array;

   subtype Type_Mode_Valid is Type_Mode_Type range
     Type_Mode_B1 .. Type_Mode_Type'Last;

   subtype Type_Mode_Discrete is Type_Mode_Type range
     Type_Mode_B1 .. Type_Mode_I64;

   subtype Type_Mode_Scalar is Type_Mode_Type range
     Type_Mode_B1 .. Type_Mode_F64;

   subtype Type_Mode_Integers is Type_Mode_Type range
     Type_Mode_I32 .. Type_Mode_I64;

   --  Composite types, with the vhdl meaning: record and arrays.
   subtype Type_Mode_Composite is Type_Mode_Type range
     Type_Mode_Static_Record .. Type_Mode_Protected;

   subtype Type_Mode_Non_Composite is Type_Mode_Type range
     Type_Mode_B1 .. Type_Mode_Bounds_Acc;

   --  Array types.
   subtype Type_Mode_Arrays is Type_Mode_Type range
     Type_Mode_Unbounded_Array .. Type_Mode_Complex_Array;

   subtype Type_Mode_Bounded_Arrays is Type_Mode_Type range
     Type_Mode_Static_Array .. Type_Mode_Complex_Array;

   --  Record types.
   subtype Type_Mode_Records is Type_Mode_Type range
     Type_Mode_Static_Record .. Type_Mode_Unbounded_Record;

   subtype Type_Mode_Bounded_Records is Type_Mode_Type range
     Type_Mode_Static_Record .. Type_Mode_Complex_Record;

   --  Thin types, ie types whose length is a scalar.
   subtype Type_Mode_Thin is Type_Mode_Type range
     Type_Mode_B1 .. Type_Mode_Bounds_Acc;

   --  Aggregate types, ie types whose length is longer than a scalar.
   subtype Type_Mode_Aggregate is Type_Mode_Type range
     Type_Mode_Static_Record .. Type_Mode_Protected;
   subtype Type_Mode_Fat is Type_Mode_Aggregate;

   subtype Type_Mode_Unbounded is Type_Mode_Type range
     Type_Mode_Unbounded_Record .. Type_Mode_Unbounded_Array;

   --  Subprogram call argument mechanism.
   --  In VHDL, the evaluation is strict: actual parameters are evaluated
   --  before the call.  This is the usual strategy of most compiled languages
   --  (the main exception being Algol-68 call by name).
   --
   --  Call semantic is described in
   --  LRM08 4.2.2.2 Constant and variable parameters.
   --
   --  At the semantic (and LRM level), there are two call convention: either
   --  call by value or call by reference.  That vocabulary should be used in
   --  trans for the semantic level: call convention and call-by.  According to
   --  the LRM, all scalars use the call by value convention.  It is possible
   --  to change the actual after the call for inout parameters, using
   --  pass-by value mechanism and copy-in/copy-out.
   --
   --  At the low-level (generated code), there are two mechanisms: either
   --  pass by copy or pass by address.  Again, that vocabulary should be used
   --  in trans for the low-level: mechanism and pass-by.
   --
   --  A call by reference is always passed by address; while a call by value
   --  can use a pass-by address to a copy of the value.  The later being
   --  used for fat accesses.  With Ortho, only scalars and pointers can be
   --  passed by copy.

   --  In GHDL, all non-composite types use the call-by value convention, and
   --  composite types use the call-by reference convention.  For fat accesses,
   --  a copy of the value is passed by address.

   type Call_Mechanism is (Pass_By_Copy, Pass_By_Address);
   type Call_Mechanism_Array is array (Object_Kind_Type) of Call_Mechanism;

   --  These parameters are passed by copy, ie the argument of the subprogram
   --  is the value of the object.
   subtype Type_Mode_Pass_By_Copy is Type_Mode_Thin;

   --  The parameters are passed by address, ie the argument of the
   --  subprogram is an address to the object.
   subtype Type_Mode_Pass_By_Address is Type_Mode_Aggregate;

   --  Call conventions.
   subtype Type_Mode_Call_By_Value is Type_Mode_Non_Composite;
   subtype Type_Mode_Call_By_Reference is Type_Mode_Composite;

   --  Additional informations for a resolving function.
   type Subprg_Resolv_Info is record
      Resolv_Func  : O_Dnode;
      --  Parameter nodes.
      Var_Instance : Subprgs.Subprg_Instance_Type;

      --  Signals
      Var_Vals      : O_Dnode;
      --  Driving vector.
      Var_Vec       : O_Dnode;
      --  Length of Vector.
      Var_Vlen      : O_Dnode;
      Var_Nbr_Drv   : O_Dnode;
      Var_Nbr_Ports : O_Dnode;
   end record;
   type Subprg_Resolv_Info_Acc is access Subprg_Resolv_Info;

   --  In order to support resume feature of non-sensitized processes and
   --  procedure, a state variable is added to encode vertices of the control
   --  flow graph (only suspendable vertices are considered: an inner loop
   --  that doesn't suspend is not decomposed by this mechanism).
   type State_Type is new Nat32;

   --  Translation of types.
   --  (Where you understand that VHDL is more complex than C...)
   --
   --  1) For scalar types (integers, physical types, enumeration, floating
   --     point types) and pointers, the type is fully known during analysis
   --     and translation:
   --     a) for integers and physical types, the size is defined by the range.
   --        GHDL uses either 32-bit or 64-bit types.
   --     b) for enumeration, the size is defined by the number of literals.
   --        GHDL uses either 8-bit or 32-bit types.
   --     c) for floating-point type, GHDL always uses 64-bit types (Float64).
   --     d) for access types, GHDL uses pointers.  This is slightly more
   --        complex as sometimes it can be a fat pointer, which is a record
   --        of two pointers.  But in all cases, the size is known.
   --
   --  For composite subtypes (arrays and records), there are several cases:
   --
   --  2) Composite types whose sub-elements are statically constrained.
   --     Eg:  subtype byte is bit_vector (7 downto 0);
   --     Eg:  subtype word is std_logic_vector (31 downto 0);
   --     Eg:  type my_bus is record
   --             req: bit;
   --             ack: bit;
   --             data: byte;
   --          end record;
   --     This still corresponds to C: sizes and offsets are known during
   --     translation.
   --     However, for arrays a bound variable is created.  This variable
   --     contains the bounds of the array (left, right and direction) and the
   --     length of each bound.  This is used both for 'introspection' and for
   --     conversion to fat pointers.
   --
   --  3) Unbounded types.  This is quite usual for parameters.
   --     Eg:  procedure disp_hex (v : std_logic_vector);
   --     The bounds of an unbounded types are only known during execution, and
   --     thus must be passed with the argument.
   --     This is not the same case as an object declared with an unbounded
   --     type; in that case the bounds are computed during elaboration (or
   --     dynamic elaboration).
   --     Eg: constant c : std_logic_vector := xxx;
   --
   --     For these unbounded types, the interface is translated as a fat
   --     pointer, which is a structure containing a base pointer and a bound
   --     pointer.  The base pointer points to the data while the bound pointer
   --     points to the bounds.
   --
   --     In some case, we need to convert from a bounded representation to an
   --     unbounded representation.  This happens while calling a subprogram
   --     with a bounded object (and corresponds to a subtype conversion in
   --     VHDL terms).  In that case a fat pointer is created, using the object
   --     as data and the bounds variable as the bounds.  The opposite
   --     conversion can also happen and we just need to check that the bounds
   --     are matching and to keep only the data part.
   --
   --  4) Complex types.  Complex is a word used only by GHDL (not defined by
   --     VHDL).  You need to realize that VHDL types are more powerful than C
   --     types as you can declare a type whose size is not known by the
   --     compiler.
   --     Eg:  constant length : natural := call_to_a_complex_function(5);
   --          subtype my_word is std_logic_vector (1 to length);
   --          type my_bus is record
   --             d : my_word;
   --             req : std_logic_vector;
   --          end record;
   --     Clearly, LENGTH is not known during analysis.  In many cases it
   --     could be known during elaboration but this is not enough as such a
   --     construct could also be used within subprograms using a parameter to
   --     define a bound.
   --
   --     Because the size of these objects is not known during compilation,
   --     the objects are allocated dynamically (either on the heap or on the
   --     stack) during (dynamic) elaboration.  They also comes with a bound
   --     variable.
   --
   --     For arrays, the bound variable describes the index of the array and
   --     the bounds of the elements (if the element is unbounded).
   --
   --     For records, the bound variable describes the offset and the bounds
   --     of the non-static elements.
   --

   --  OLD:
   --  Complex types.
   --
   --  A complex type is not a VHDL notion, but a translation notion.
   --  A complex type is a composite type whose size is not known at compile
   --  type. This happends in VHDL because a bound can be globally static.
   --  Therefore, the length of an array may not be known at compile type,
   --  and this propagates to composite types (record and array) if they
   --  have such an element. This is different from unconstrained arrays.
   --
   --  This occurs frequently in VHDL, and could even happen within
   --  subprograms.
   --
   --  Such types are always dynamically allocated (on the stack or on the
   --  heap). They must be continuous in memory so that they could be copied
   --  via memcpy/memmove.
   --
   --  At runtime, the size of such type is computed. A builder procedure
   --  is also created to setup inner pointers. This builder procedure should
   --  be called at initialization, but also after a copy.
   --
   --  Example:
   --  1) subtype bv_type is bit_vector (l to h);
   --     variable a : bv_type
   --
   --     This is represented by a pointer to an array of bit. No need for
   --     builder procedure, as the element type is not complex. But there
   --     is a size variable for the size of bv_type
   --
   --  2) type rec1_type is record
   --       f1 : integer;
   --       f2 : bv_type;
   --     end record;
   --
   --     This is represented by a pointer to a record. The 'f2' field is
   --     an offset to an array of bit. The size of the object is the size
   --     of the record (with f2 as a pointer) + the size of bv_type.
   --     The alinment of the object is the maximum alignment of its sub-
   --     objects: rec1 and bv_type.
   --     A builder procedure is needed to initialize the 'f2' field.
   --     The memory layout is:
   --     +--------------+
   --     | rec1:     f1 |
   --     |           f2 |---+
   --     +--------------+   |
   --     | bv_type      |<--+
   --     | ...          |
   --     +--------------+
   --
   --  3) type rec2_type is record
   --      g1: rec1_type;
   --      g2: bv_type;
   --      g3: bv_type;
   --    end record;
   --
   --    This is represented by a pointer to a record.  All the three fields
   --    are offset (relative to rec2). Alignment is the maximum alignment of
   --    the sub-objects (rec2, rec1, bv_type x 3).
   --     The memory layout is:
   --     +--------------+
   --     | rec2:     g1 |---+
   --     |           g2 |---|---+
   --     |           g3 |---|---|---+
   --     +--------------+   |   |   |
   --     | rec1:     f1 |<--+   |   |
   --     |           f2 |---+   |   |
   --     +--------------+   |   |   |
   --     | bv_type (f2) |<--+   |   |
   --     | ...          |       |   |
   --     +--------------+       |   |
   --     | bv_type (g2) |<------+   |
   --     | ...          |           |
   --     +--------------+           |
   --     | bv_type (g3) |<----------+
   --     | ...          |
   --     +--------------+
   --
   --  4) type bv_arr_type is array (natural range <>) of bv_type;
   --     arr2 : bv_arr_type (1 to 4)
   --
   --     This should be represented by a pointer to bv_type.
   --     The memory layout is:
   --     +--------------+
   --     | bv_type  (1) |
   --     | ...          |
   --     +--------------+
   --     | bv_type  (2) |
   --     | ...          |
   --     +--------------+
   --     | bv_type  (3) |
   --     | ...          |
   --     +--------------+
   --     | bv_type  (4) |
   --     | ...          |
   --     +--------------+

   type Assoc_Conv_Info is record
      --  The subprogram created to do the conversion.
      Subprg              : O_Dnode;
      --  The local base block
      Instance_Block      : Iir;
      --   and its address.
      Instance_Field      : O_Fnode;
      --  The instantiated entity (if any).
      Instantiated_Entity : Iir;
      --   and its address.
      Instantiated_Field  : O_Fnode;
      --  The object if the subprogram is a method
      Method_Object       : O_Fnode;
      In_Sig_Field        : O_Fnode;
      In_Val_Field        : O_Fnode;
      Out_Sig_Field       : O_Fnode;
      Out_Val_Field       : O_Fnode;
      Record_Type         : O_Tnode;
      Record_Ptr_Type     : O_Tnode;
   end record;

   type Direct_Driver_Type is record
      Sig : Iir;
      Var : Var_Type;
   end record;
   type Direct_Driver_Arr is array (Natural range <>) of Direct_Driver_Type;
   type Direct_Drivers_Acc is access Direct_Driver_Arr;

   type Ortho_Info_Type (Kind : Ortho_Info_Kind);
   type Ortho_Info_Acc is access Ortho_Info_Type;

   subtype Type_Info_Acc is Ortho_Info_Acc (Kind_Type);
   subtype Incomplete_Type_Info_Acc is Ortho_Info_Acc (Kind_Incomplete_Type);
   subtype Index_Info_Acc is Ortho_Info_Acc (Kind_Index);
   subtype Subprg_Info_Acc is Ortho_Info_Acc (Kind_Subprg);
   subtype Operator_Info_Acc is Ortho_Info_Acc (Kind_Operator);
   subtype Interface_Info_Acc is Ortho_Info_Acc (Kind_Interface);
   subtype Call_Info_Acc is Ortho_Info_Acc (Kind_Call);
   subtype Call_Assoc_Info_Acc is Ortho_Info_Acc (Kind_Call_Assoc);
   subtype Object_Info_Acc is Ortho_Info_Acc (Kind_Object);
   subtype Signal_Info_Acc is Ortho_Info_Acc (Kind_Signal);
   subtype Alias_Info_Acc is Ortho_Info_Acc (Kind_Alias);
   subtype Proc_Info_Acc is Ortho_Info_Acc (Kind_Process);
   subtype Psl_Info_Acc is Ortho_Info_Acc (Kind_Psl_Directive);
   subtype Loop_Info_Acc is Ortho_Info_Acc (Kind_Loop);
   subtype Loop_State_Info_Acc is Ortho_Info_Acc (Kind_Loop_State);
   subtype Block_Info_Acc is Ortho_Info_Acc (Kind_Block);
   subtype Generate_Info_Acc is Ortho_Info_Acc (Kind_Generate);
   subtype Comp_Info_Acc is Ortho_Info_Acc (Kind_Component);
   subtype Field_Info_Acc is Ortho_Info_Acc (Kind_Field);
   subtype Config_Info_Acc is Ortho_Info_Acc (Kind_Config);
   subtype Assoc_Info_Acc is Ortho_Info_Acc (Kind_Assoc);
   subtype Inertial_Info_Acc is Ortho_Info_Acc (Kind_Inertial_Assoc);
   subtype Inter_Info_Acc is Ortho_Info_Acc (Kind_Interface);
   subtype Design_File_Info_Acc is Ortho_Info_Acc (Kind_Design_File);
   subtype Library_Info_Acc is Ortho_Info_Acc (Kind_Library);

   procedure Init_Node_Infos;
   procedure Update_Node_Infos;
   procedure Free_Node_Infos;

   procedure Set_Info (Target : Iir; Info : Ortho_Info_Acc);

   procedure Clear_Info (Target : Iir);

   function Get_Info (Target : Iir) return Ortho_Info_Acc;
   pragma Inline (Get_Info);

   --  Create an ortho_info field of kind KIND for iir node TARGET, and
   --  return it.
   function Add_Info (Target : Iir; Kind : Ortho_Info_Kind)
                      return Ortho_Info_Acc;

   procedure Free_Info (Target : Iir);

   procedure Free_Type_Info (Info : in out Type_Info_Acc);

   function Get_Ortho_Literal (Target : Iir) return O_Cnode;

   function Get_Ortho_Type (Target : Iir; Is_Sig : Object_Kind_Type)
                            return O_Tnode;

   --  Return true is INFO is a type info for a composite type, ie:
   --  * a record
   --  * an array (fat or thin)
   --  * a fat pointer.
   function Is_Composite (Info : Type_Info_Acc) return Boolean;
   pragma Inline (Is_Composite);

   --  Type is bounded but layout and size are known only during elaboration.
   function Is_Complex_Type (Tinfo : Type_Info_Acc) return Boolean;

   --  Type size is known at compile-time.
   function Is_Static_Type (Tinfo : Type_Info_Acc) return Boolean;

   --  True iff TINFO is base + bounds.
   function Is_Unbounded_Type (Tinfo : Type_Info_Acc) return Boolean;
   pragma Inline (Is_Unbounded_Type);

   type Hexstr_Type is array (Integer range 0 .. 15) of Character;
   N2hex : constant Hexstr_Type := "0123456789abcdef";

   type Ortho_Info_Basetype_Type
     (Kind : Ortho_Info_Type_Kind := Kind_Type_Scalar) is record
      --  For all types:
      --  This is the maximum depth of RTI, that is the max of the depth of
      --  the type itself and every types it depends on.
      Rti_Max_Depth : Rti_Depth_Type;

      Align : Alignment_Type;

      case Kind is
         when Kind_Type_Scalar =>
            --  For scalar types:
            --  Ortho type for the range record type.
            Range_Type : O_Tnode;

            --  Ortho type for an access to the range record type.
            Range_Ptr_Type : O_Tnode;

            --  Fields of TYPE_RANGE_TYPE.
            Range_Left   : O_Fnode;
            Range_Right  : O_Fnode;
            Range_Dir    : O_Fnode;
            Range_Length : O_Fnode;

         when Kind_Type_Array
           | Kind_Type_Record =>
            --  For unbounded types:
            --  The base type.
            Base_Type       : O_Tnode_Array;
            Base_Ptr_Type   : O_Tnode_Array;
            --  The dope vector.
            --  For arrays:
            --    range of indexes
            --    layout of element (if element is unbounded)
            --  For record:
            --    offsets of complex elements
            --    layout of unbounded elements
            Bounds_Type     : O_Tnode;
            Bounds_Ptr_Type : O_Tnode;

            --  For arrays with unbounded element, the layout field of the
            --  bounds type.
            Bounds_El       : O_Fnode;

            --  Size + bounds.
            --  Always created for arrays, created for unbounded and complex
            --  records.
            Layout_Type     : O_Tnode;
            Layout_Ptr_Type : O_Tnode;

            --  Size and bounds fields of the layout type.
            Layout_Size     : O_Fnode;
            Layout_Bounds   : O_Fnode;

            --  The ortho type is a fat pointer to the base and the bounds.
            --  These are the fields of the fat pointer.
            Base_Field   : O_Fnode_Array;
            Bounds_Field : O_Fnode_Array;

            --  Parameters for type builders.
            --  NOTE: this is only set for types (and *not* for subtypes).
            Builder      : Complex_Type_Arr_Info;

         when Kind_Type_File =>
            --  Constant containing the signature of the file.
            File_Signature : O_Dnode;

         when Kind_Type_Protected =>
            Prot_Scope : aliased Var_Scope_Type;
            Prot_Prev_Scope : Var_Scope_Acc;

            --  Init procedure for the protected type.
            Prot_Init_Subprg           : O_Dnode;
            Prot_Init_Instance         : Subprgs.Subprg_Instance_Type;
            --  Final procedure.
            Prot_Final_Subprg          : O_Dnode;
            Prot_Final_Instance        : Subprgs.Subprg_Instance_Type;
            --  The outer instance, if any.
            Prot_Subprg_Instance_Field : O_Fnode;
            --  The LOCK field in the object type
            Prot_Lock_Field            : O_Fnode;
      end case;
   end record;

   type Subtype_Fields_Type is record
      Tinfo : Type_Info_Acc;
      Fields : O_Fnode_Array;
   end record;

   Subtype_Fields_Null : constant Subtype_Fields_Type :=
     (Tinfo => null, Fields => (others => O_Fnode_Null));

   type Subtype_Fields_Array is
     array (Iir_Index32 range <>) of Subtype_Fields_Type;
   type Subtype_Fields_Array_Acc is access Subtype_Fields_Array;

   type Ortho_Info_Subtype_Type
     (Kind : Ortho_Info_Type_Kind := Kind_Type_Scalar) is record
      case Kind is
         when Kind_Type_Scalar =>
            --  For scalar types:
            --  True if no need to check against low/high bound.
            Nocheck_Low : Boolean := False;
            Nocheck_Hi  : Boolean := False;

            --  For scalar types:
            --  Range_Var is the same as its type mark (there is no need to
            --  create a new range var if the range is the same).
            Same_Range : Boolean := False;

            --  Tree for the range record declaration.
            Range_Var : Var_Type := Null_Var;

         when Kind_Type_Array
           | Kind_Type_Record =>
            --  Variable containing the layout for a constrained type.
            Composite_Layout : Var_Type;

            --  Subtype definition that owns this subtype (as an element).
            --  Used to refere to the layout.
            Subtype_Owner : Type_Info_Acc := null;
            Owner_Field : Field_Info_Acc := null;

            --  For static record subtype: the fields of the constraints.
            Rec_Fields : Subtype_Fields_Array_Acc;

         when Kind_Type_File =>
            null;

         when Kind_Type_Protected =>
            null;
      end case;
   end record;

   --    Ortho_Info_Type_Scalar_Init : constant Ortho_Info_Type_Type :=
   --      (Kind => Kind_Type_Scalar,
   --       Range_Type => O_Tnode_Null,
   --       Range_Ptr_Type => O_Tnode_Null,
   --       Range_Var => null,
   --       Range_Left => O_Fnode_Null,
   --       Range_Right => O_Fnode_Null,
   --       Range_Dir => O_Fnode_Null,
   --       Range_Length => O_Fnode_Null);

   Ortho_Info_Basetype_Array_Init : constant Ortho_Info_Basetype_Type :=
     (Kind => Kind_Type_Array,
      Rti_Max_Depth => 0,
      Align => Align_Undef,
      Base_Type => (O_Tnode_Null, O_Tnode_Null),
      Base_Ptr_Type => (O_Tnode_Null, O_Tnode_Null),
      Bounds_Type => O_Tnode_Null,
      Bounds_Ptr_Type => O_Tnode_Null,
      Bounds_El => O_Fnode_Null,
      Layout_Type => O_Tnode_Null,
      Layout_Ptr_Type => O_Tnode_Null,
      Layout_Size => O_Fnode_Null,
      Layout_Bounds => O_Fnode_Null,
      Base_Field => (O_Fnode_Null, O_Fnode_Null),
      Bounds_Field => (O_Fnode_Null, O_Fnode_Null),
      Builder => (others => (Builder_Instance => Subprgs.Null_Subprg_Instance,
                             Builder_Layout_Param => O_Dnode_Null,
                             Builder_Proc => O_Dnode_Null)));

   Ortho_Info_Subtype_Array_Init : constant Ortho_Info_Subtype_Type :=
     (Kind => Kind_Type_Array,
      Composite_Layout => Null_Var,
      Subtype_Owner => null,
      Owner_Field => null,
      Rec_Fields => null);

   Ortho_Info_Basetype_Record_Init : constant Ortho_Info_Basetype_Type :=
     (Kind => Kind_Type_Record,
      Rti_Max_Depth => 0,
      Align => Align_Undef,
      Base_Type => (O_Tnode_Null, O_Tnode_Null),
      Base_Ptr_Type => (O_Tnode_Null, O_Tnode_Null),
      Bounds_Type => O_Tnode_Null,
      Bounds_Ptr_Type => O_Tnode_Null,
      Bounds_El => O_Fnode_Null,
      Layout_Type => O_Tnode_Null,
      Layout_Ptr_Type => O_Tnode_Null,
      Layout_Size => O_Fnode_Null,
      Layout_Bounds => O_Fnode_Null,
      Base_Field => (O_Fnode_Null, O_Fnode_Null),
      Bounds_Field => (O_Fnode_Null, O_Fnode_Null),
      Builder => (others => (Builder_Instance => Subprgs.Null_Subprg_Instance,
                             Builder_Layout_Param => O_Dnode_Null,
                             Builder_Proc => O_Dnode_Null)));

   Ortho_Info_Subtype_Record_Init : constant Ortho_Info_Subtype_Type :=
     (Kind => Kind_Type_Record,
      Composite_Layout => Null_Var,
      Subtype_Owner => null,
      Owner_Field => null,
      Rec_Fields => null);

   Ortho_Info_Basetype_File_Init : constant Ortho_Info_Basetype_Type :=
     (Kind => Kind_Type_File,
      Rti_Max_Depth => 0,
      Align => Align_Undef,
      File_Signature => O_Dnode_Null);

   Ortho_Info_Basetype_Prot_Init : constant Ortho_Info_Basetype_Type :=
     (Kind => Kind_Type_Protected,
      Rti_Max_Depth => 0,
      Align => Align_Undef,
      Prot_Scope => Null_Var_Scope,
      Prot_Prev_Scope => null,
      Prot_Init_Subprg => O_Dnode_Null,
      Prot_Init_Instance => Subprgs.Null_Subprg_Instance,
      Prot_Final_Subprg => O_Dnode_Null,
      Prot_Subprg_Instance_Field => O_Fnode_Null,
      Prot_Final_Instance => Subprgs.Null_Subprg_Instance,
      Prot_Lock_Field => O_Fnode_Null);


   --  In order to unify and have a common handling of Enode/Lnode/Dnode,
   --  let's introduce Mnode (yes, another node).
   --
   --  Mnodes can be converted to Enode/Lnode via the M2xx functions.  If
   --  an Mnode are referenced more than once, they must be stabilized (this
   --  will create a new variable if needed as Enode and Lnode can be
   --  referenced only once).
   --
   --  An Mnode is a typed union, containing either an Lnode or a Enode.
   --  See Mstate for a description of the union.
   --  The real data is contained insisde a record, so that the discriminant
   --  can be changed.
   type Mnode;

   --  State of an Mmode.
   type Mstate is
     (
      --  The Mnode contains an Enode for a value.
      --  This Mnode can be used only once.
      Mstate_Ev,

      --  The Mnode contains an Enode for a pointer.
      --  This Mnode can be used only once.
      Mstate_Ep,

      --  The Mnode contains an Lnode representing a value.
      --  This Lnode can be used only once.
      Mstate_Lv,

      --  The Mnode contains an Lnode representing a pointer.
      --  This Lnode can be used only once.
      Mstate_Lp,

      --  The Mnode contains an Dnode for a variable representing a value.
      --  This Dnode may be used several times.
      Mstate_Dv,

      --  The Mnode contains an Dnode for a variable representing a pointer.
      --  This Dnode may be used several times.
      Mstate_Dp,

      --  Null Mnode.
      Mstate_Null,

      --  The Mnode is invalid (such as already used).
      Mstate_Bad);

   type Mnode1 (State : Mstate := Mstate_Bad) is record
      --  Additionnal informations about the objects: kind and type.
      K : Object_Kind_Type;
      T : Type_Info_Acc;

      --  Ortho type of the object.
      Vtype : O_Tnode;

      --  Type for a pointer to the object.
      Ptype : O_Tnode;

      case State is
         when Mstate_Ev =>
            Ev : O_Enode;
         when Mstate_Ep =>
            Ep : O_Enode;
         when Mstate_Lv =>
            Lv : O_Lnode;
         when Mstate_Lp =>
            Lp : O_Lnode;
         when Mstate_Dv =>
            Dv : O_Dnode;
         when Mstate_Dp =>
            Dp : O_Dnode;
         when Mstate_Bad
            | Mstate_Null =>
            null;
      end case;
   end record;
   --pragma Pack (Mnode1);

   type Mnode is record
      M1 : Mnode1;
   end record;

   --  Null Mnode.
   Mnode_Null : constant Mnode := Mnode'(M1 => (State => Mstate_Null,
                                                K => Mode_Value,
                                                Ptype => O_Tnode_Null,
                                                Vtype => O_Tnode_Null,
                                                T => null));

   type Mnode_Array is array (Object_Kind_Type) of Mnode;

   --  Object kind of a Mnode
   function Get_Object_Kind (M : Mnode) return Object_Kind_Type;

   --  Transform VAR to Mnode.
   function Get_Var
     (Var : Var_Type; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
     return Mnode;

   --  Likewise, but VAR is a pointer to the value.
   function Get_Varp
     (Var : Var_Type; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
     return Mnode;

   --  Return a stabilized node for M.
   --  The former M is not usuable anymore.
   function Stabilize (M : Mnode; Can_Copy : Boolean := False) return Mnode;

   --  Stabilize M.
   procedure Stabilize (M : in out Mnode);

   --  If M is not stable, create a variable containing the value of M.
   --  M must be scalar (or access).
   function Stabilize_Value (M : Mnode) return Mnode;

   --  Create a temporary of type INFO and kind KIND.
   function Create_Temp (Info : Type_Info_Acc;
                         Kind : Object_Kind_Type := Mode_Value)
                         return Mnode;

   function Get_Type_Info (M : Mnode) return Type_Info_Acc;
   pragma Inline (Get_Type_Info);

   --  Creation of Mnodes.

   function E2M (E : O_Enode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                return Mnode;
   function E2M (E : O_Enode;
                 T : Type_Info_Acc;
                 Kind  : Object_Kind_Type;
                 Vtype : O_Tnode;
                 Ptype : O_Tnode)
                return Mnode;

   --  From a Lnode, general form (can be used for ranges, bounds, base...)
   function Lv2M (L     : O_Lnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                 return Mnode;

   --  From a Lnode, only for values.
   function Lv2M (L : O_Lnode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                 return Mnode;

   --  From a Lnode that designates a pointer, general form.
   function Lp2M (L     : O_Lnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                 return Mnode;

   --  From a Lnode that designates a pointer to a value.
   function Lp2M (L : O_Lnode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                 return Mnode;

   --  From a variable declaration, general form.
   function Dv2M (D     : O_Dnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                  return Mnode;

   --  From a variable for a value.
   function Dv2M (D : O_Dnode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                 return Mnode;

   --  From a pointer variable, general form.
   function Dp2M (D     : O_Dnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                  return Mnode;

   --  From a pointer to a value variable.
   function Dp2M (D : O_Dnode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                 return Mnode;

   function M2Lv (M : Mnode) return O_Lnode;

   function M2Lp (M : Mnode) return O_Lnode;

   function M2Dp (M : Mnode) return O_Dnode;

   function M2Dv (M : Mnode) return O_Dnode;

   function T2M (Atype : Iir; Kind : Object_Kind_Type) return Mnode;

   function M2E (M : Mnode) return O_Enode;

   function M2Addr (M : Mnode) return O_Enode;

   --    function Is_Null (M : Mnode) return Boolean is
   --    begin
   --       return M.M1.State = Mstate_Null;
   --    end Is_Null;

   function Is_Stable (M : Mnode) return Boolean;

   function Varv2M (Var      : Var_Type;
                    Var_Type : Type_Info_Acc;
                    Mode     : Object_Kind_Type;
                    Vtype    : O_Tnode;
                    Ptype    : O_Tnode)
                    return Mnode;

   --  Convert a Lnode for a sub object to an MNODE.
   function Lo2M (L : O_Lnode; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
                  return Mnode;

   function Lo2M (D : O_Dnode; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
                  return Mnode;

   type Ortho_Info_Type (Kind : Ortho_Info_Kind) is record
      --  For a simple memory management: use mark and sweep to free all infos.
      Mark : Boolean := False;

      case Kind is
         when Kind_Type =>
            --  Mode of the type.
            Type_Mode : Type_Mode_Type := Type_Mode_Unknown;

            --  If true, the type is (still) incomplete.
            Type_Incomplete : Boolean := False;

            --  For array only.  True if the type is constrained with locally
            --  static bounds.  May have non locally-static bounds in some
            --  of its sub-element (ie being a complex type).
            Type_Locally_Constrained : Boolean := False;

            --  Ortho node which represents the type.
            --  Type                             -> Ortho type
            --   scalar                          ->  scalar
            --   bounded record (complex or not) ->  record
            --   constrained non-complex array   ->  constrained array
            --   constrained complex array       ->  the element
            --   unbounded array or record       ->  fat pointer
            --   access to unconstrained array   ->  fat pointer
            --   access (others)                 ->  access
            --   file                            ->  file_index_type
            --   protected                       ->  instance
            Ortho_Type : O_Tnode_Array;

            --  Ortho pointer to the type.  This is always an access to the
            --  ortho_type.
            Ortho_Ptr_Type : O_Tnode_Array;

            --  More info according to the type.
            B : Ortho_Info_Basetype_Type;
            S : Ortho_Info_Subtype_Type;

            --  Run-time information.
            Type_Rti : O_Dnode := O_Dnode_Null;

         when Kind_Incomplete_Type =>
            --  The declaration of the incomplete type.
            Incomplete_Type  : Iir;

         when Kind_Index =>
            --  For index_subtype_declaration, the field containing
            --  the bounds of that index, in the array bounds record.
            Index_Field : O_Fnode;

         when Kind_Field =>
            --  For element whose type is static: field in the record.
            --  For element whose type is not static: offset field in the
            --    bounds.
            Field_Node : O_Fnode_Array := (O_Fnode_Null, O_Fnode_Null);

            --  The field in the layout record for the layout of the
            --  element (for unbounded element).
            Field_Bound : O_Fnode := O_Fnode_Null;

         when Kind_Enum_Lit =>
            --  Ortho tree which represents the expression, used for
            --  enumeration literals.
            Lit_Node : O_Cnode;

         when Kind_Subprg =>
            --  True if the function can return a value stored in the secondary
            --  stack.  In this case, the caller must deallocate the area
            --  allocated by the callee when the value was used.
            Use_Stack2 : Boolean := False;

            --  Subprogram declaration node.
            Subprg_Node : O_Dnode;

            --  For a function:
            --    If the return value is not composite, then this field
            --      must be O_DNODE_NULL.
            --    If the return value is a composite type, then the caller must
            --    give to the callee an area to put the result.  This area is
            --    given via an (hidden to the user) interface.  Furthermore,
            --    the function is translated into a procedure.
            --  For a procedure:
            --    Interface for parameters.
            Res_Interface : O_Dnode := O_Dnode_Null;

            --  Field in the frame for a pointer to the PARAMS structure.  This
            --  is necessary when nested subprograms need to access to
            --  paramters. of this subprogram.
            Subprg_Params_Var : Var_Type := Null_Var;

            --  For a procedure, record containing the parameters.
            Subprg_Params_Type : O_Tnode := O_Tnode_Null;
            Subprg_Params_Ptr  : O_Tnode := O_Tnode_Null;

            --  Field in the parameter struct for the suspend state. Also the
            --  suspend state is not a parameter, it is initialized by the
            --  caller.
            Subprg_State_Field : O_Fnode := O_Fnode_Null;

            --  Field in the parameter struct for local variables.
            Subprg_Locvars_Field : O_Fnode := O_Fnode_Null;
            Subprg_Locvars_Scope : aliased Var_Scope_Type;

            --  Access to the declarations within this subprogram.
            Subprg_Frame_Scope : aliased Var_Scope_Type;

            --  Instances for the subprograms.
            Subprg_Instance : Subprgs.Subprg_Instance_Type :=
              Subprgs.Null_Subprg_Instance;

            Subprg_Resolv : Subprg_Resolv_Info_Acc := null;

            --  Local identifier number, set by spec, continued by body.
            Subprg_Local_Id : Local_Identifier_Type;

            --  If set, return should be converted into exit out of the
            --  SUBPRG_EXIT loop and the value should be assigned to
            --  SUBPRG_RESULT, if any.
            Subprg_Exit   : O_Snode := O_Snode_Null;
            Subprg_Result : O_Dnode := O_Dnode_Null;

         when Kind_Operator =>
            --  For an implicit subprogram like type operators or file
            --  subprograms.

            --  Use secondary stack (not referenced).
            Operator_Stack2 : Boolean := False;

            --  True if the body was generated.  Many operators share the same
            --  subprogram.
            Operator_Body : Boolean := False;

            --  Subprogram declaration node.
            Operator_Node : O_Dnode;

            --  Instances for the subprograms.
            Operator_Instance : Subprgs.Subprg_Instance_Type :=
              Subprgs.Null_Subprg_Instance;

            --  Parameters
            Operator_Left, Operator_Right : O_Dnode;
            Operator_Res : O_Dnode;

         when Kind_Call =>
            Call_State_Scope : aliased Var_Scope_Type;
            Call_State_Mark : Var_Type := Null_Var;
            Call_Params_Var : Var_Type := Null_Var;

         when Kind_Call_Assoc =>
            --  Variable containing a reference to the actual, for scalar
            --  copyout.  The value is passed in the parameter.
            Call_Assoc_Ref : Var_Type := Null_Var;

            --  Variable containing the value, the bounds and the fat vector.
            Call_Assoc_Value : Var_Type_Array := (others => Null_Var);
            Call_Assoc_Bounds : Var_Type := Null_Var;
            Call_Assoc_Fat : Var_Type_Array := (others => Null_Var);

         when Kind_Object =>
            --  For constants: set when the object is defined as a constant.
            Object_Static   : Boolean;
            --  The object itself.
            Object_Var      : Var_Type;
            --  RTI constant for the object.
            Object_Rti      : O_Dnode := O_Dnode_Null;

         when Kind_Signal =>
            --  The current value of the signal.
            --  Also the initial value of collapsed ports.
            Signal_Val      : Var_Type := Null_Var;
            --  Pointer to the value, for ports.
            Signal_Valp     : Var_Type := Null_Var;
            --  A pointer to the signal (contains meta data).
            Signal_Sig      : Var_Type;
            --  Direct driver for signal (if any).
            Signal_Driver   : Var_Type := Null_Var;
            --  RTI constant for the object.
            Signal_Rti      : O_Dnode := O_Dnode_Null;
            --  Function to compute the value of object (used for implicit
            --   guard signal declaration).
            Signal_Function : O_Dnode := O_Dnode_Null;

         when Kind_Alias =>
            Alias_Var  : Var_Type_Array;
            Alias_Kind : Object_Kind_Type;

         when Kind_Iterator =>
            --  True if the range should be copied as it may change during
            --  the loop.
            Iterator_Range_Copy : Boolean;
            --  Iterator variable.
            Iterator_Var : Var_Type;
            --  Iterator right bound (used only if the iterator is a range
            --  expression).
            Iterator_Right : Var_Type;
            --  Iterator range pointer (used only if the iterator is not a
            --  range expression).
            Iterator_Range : Var_Type;

         when Kind_Interface =>
            --  Call mechanism (by copy or by address) for the interface.
            Interface_Mechanism : Call_Mechanism_Array;

            --  Ortho declaration for the interface. If not null, there is
            --  a corresponding ortho parameter for the interface. While
            --  translating nested subprograms (that are unnested),
            --  Interface_Field may be set to the corresponding field in the
            --  FRAME record. So:
            --   Decl: not null, Field:     null: parameter
            --   Decl: not null, Field: not null: parameter with a copy in
            --                                    the FRAME record.
            --   Decl: null,     Field:     null: not possible
            --   Decl: null,     Field: not null: field in RESULT record
            Interface_Decl  : O_Dnode_Array := (others => O_Dnode_Null);
            --  Field of the PARAMS record for arguments of procedure.
            --  In that case, Interface_Node must be null.
            Interface_Field : O_Fnode_Array := (others => O_Fnode_Null);

         when Kind_Expr_Eval =>
            --  Result of an evaluation.
            Expr_Eval : Mnode;

         when Kind_Disconnect =>
            --  Variable which contains the time_expression of the
            --  disconnection specification
            Disconnect_Var : Var_Type;

         when Kind_Process =>
            Process_Scope : aliased Var_Scope_Type;

            --  Subprogram for the process.
            Process_Subprg : O_Dnode;

            --  Variable (in the frame) containing the current state (a
            --  number) used to resume the process.
            Process_State : Var_Type := Null_Var;

            --  Union containing local declarations for statements.
            Process_Locvar_Scope : aliased Var_Scope_Type;

            --  List of drivers if Flag_Direct_Drivers.
            Process_Drivers : Direct_Drivers_Acc := null;

            --  RTI for the process.
            Process_Rti_Const : O_Dnode := O_Dnode_Null;

         when Kind_Psl_Directive =>
            Psl_Scope : aliased Var_Scope_Type;

            --  Procedure for the state machine.
            Psl_Proc_Subprg       : O_Dnode;
            --  Procedure for finalization.  Handles EOS.
            Psl_Proc_Final_Subprg : O_Dnode;

            --  Type of the state vector.
            Psl_Vect_Type : O_Tnode;

            --  State vector variable.
            Psl_Vect_Var : Var_Type;

            --  Simplified Assertion state (for dumping)
            Psl_State_Var : Var_Type;

            --  Number of times assertion finished
            --  For cover points: Number of coveres
            --  For assertions: Number of failures
            Psl_Finish_Count_Var : Var_Type;

            -- Number of times assertion was started
            Psl_Start_Count_Var : Var_Type;

            --  RTI for the process.
            Psl_Rti_Const : O_Dnode := O_Dnode_Null;

         when Kind_Loop =>
            --  Labels for the loop.
            --  Used for exit/next from while-loop, and to exit from for-loop.
            Label_Exit : O_Snode;
            --  Used to next from for-loop, with an exit statment.
            Label_Next : O_Snode;

         when Kind_Loop_State =>
            --  Likewise but for a suspendable loop.
            --  State next: evaluate condition for a while-loop, update
            --  iterator for a for-loop.
            Loop_State_Next : State_Type;
            --  Body of a for-loop, not used for a while-loop.
            Loop_State_Body: State_Type;
            --  State after the loop.
            Loop_State_Exit  : State_Type;
            --  Access to declarations of the iterator.
            Loop_State_Scope : aliased Var_Scope_Type;
            Loop_Locvar_Scope : aliased Var_Scope_Type;

         when Kind_Locvar_State =>
            Locvar_Scope : aliased Var_Scope_Type;

         when Kind_Block =>
            --  Access to declarations of this block.
            Block_Scope : aliased Var_Scope_Type;

            --  Instance type (ortho record) for declarations contained in the
            --  block/entity/architecture.
            Block_Decls_Ptr_Type : O_Tnode;

            --  For Entity: field in the instance type containing link to
            --              parent.
            --  For an instantiation: link in the parent block to the instance.
            Block_Link_Field : O_Fnode;

            --  For an entity: must be o_fnode_null.
            --  For an architecture: the entity field.
            --  For a block, a component or a generate block: field in the
            --    parent instance which contains the declarations for this
            --    block.
            Block_Parent_Field : O_Fnode;

            --  For a generate block: field in the block providing a chain to
            --  the previous block (note: this may not be the parent, but
            --  is a parent).
            Block_Origin_Field     : O_Fnode;
            --  For an iterative block: boolean field set when the block
            --  is configured.  This is used to check if the block was already
            --  configured since index and slice are not compelled to be
            --  locally static.
            Block_Configured_Field : O_Fnode;

            --  For iterative generate block: array of instances.
            Block_Decls_Array_Type     : O_Tnode;
            Block_Decls_Array_Ptr_Type : O_Tnode;

            --  For if-generate generate statement body: the identifier of the
            --  body.  Used to know which block_configuration applies to the
            --  block.
            Block_Id : Nat32;

            --  Subprograms which elaborates the block (for entity or arch).
            Block_Elab_Subprg   : O_Dnode_Elab;

            --  Size of the block instance.
            Block_Instance_Size : O_Dnode;

            --  Only for an entity: procedure that elaborate the packages this
            --  units depend on.  That must be done before elaborating the
            --  entity and before evaluating default expressions in generics.
            Block_Elab_Pkg_Subprg : O_Dnode;

            --  RTI constant for the block.
            Block_Rti_Const : O_Dnode := O_Dnode_Null;

         when Kind_Generate =>
            --  Like Block_Parent_Field: field in the instance for the
            --  sub-block.  Always a Ghdl_Ptr_Type, as there are many possible
            --  types for the sub-block instance (if/case generate).
            Generate_Parent_Field : O_Fnode;

            --  Identifier number of the generate statement body.  Used for
            --  configuring sub-block, and for grt to index the rti.
            Generate_Body_Id : O_Fnode;

            --  RTI for the generate statement.
            Generate_Rti_Const : O_Dnode := O_Dnode_Null;

         when Kind_Component =>
            --  How to access to component interfaces.
            Comp_Scope : aliased Var_Scope_Type;

            --  Instance for the component.
            Comp_Ptr_Type  : O_Tnode;
            --  Field containing a pointer to the instance link.
            Comp_Link      : O_Fnode;
            --  RTI for the component.
            Comp_Rti_Const : O_Dnode;

         when Kind_Config =>
            --  Subprogram that configure the block.
            Config_Subprg : O_Dnode;
            Config_Instance : O_Dnode;

         when Kind_Package =>
            --  Subprogram which elaborate the package spec/body.
            --  External units should call the body elaborator.
            --  The spec elaborator is called only from the body elaborator.
            Package_Elab_Spec_Subprg : O_Dnode;
            Package_Elab_Body_Subprg : O_Dnode;

            --  Instance for the elaborators.
            Package_Elab_Spec_Instance : Subprgs.Subprg_Instance_Type;
            Package_Elab_Body_Instance : Subprgs.Subprg_Instance_Type;

            --  Variable set to true when the package is elaborated.
            Package_Elab_Var : Var_Type;

            --  RTI constant for the package.
            Package_Rti_Const : O_Dnode;

            --  Access to declarations of the spec.
            Package_Spec_Scope : aliased Var_Scope_Type;

            --  Instance type for uninstantiated package
            Package_Spec_Ptr_Type : O_Tnode;

            Package_Body_Scope    : aliased Var_Scope_Type;
            Package_Body_Ptr_Type : O_Tnode;

            --  Field to the spec within the body.
            Package_Spec_Field : O_Fnode;

            --  Local id, set by package declaration, continued by package
            --  body.
            Package_Local_Id : Local_Identifier_Type;

         when Kind_Package_Instance =>
            --  The variables containing the instance.  There are two variables
            --  for interface package: one for the spec, one for the body.
            --  For package instantiation, only the variable for the body is
            --  used.  The variable for spec is added so that packages with
            --  package interfaces don't need to know the body of their
            --  interfaces.
            Package_Instance_Spec_Var : Var_Type;
            Package_Instance_Body_Var : Var_Type;

            --  Elaboration procedure for the instance.
            Package_Instance_Elab_Subprg : O_Dnode;

            Package_Instance_Spec_Scope : aliased Var_Scope_Type;
            Package_Instance_Body_Scope : aliased Var_Scope_Type;

         when Kind_Assoc =>
            --  Association informations.
            Assoc_In  : Assoc_Conv_Info;
            Assoc_Out : Assoc_Conv_Info;

         when Kind_Inertial_Assoc =>
            Inertial_Proc : O_Dnode;
            Inertial_Inst : O_Dnode;
            Inertial_Block : Iir;

         when Kind_Design_File =>
            Design_Filename : O_Dnode;

         when Kind_Library =>
            Library_Rti_Const : O_Dnode;
      end case;
   end record;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => Ortho_Info_Acc, Object => Ortho_Info_Type);

   package Helpers is
      --  Generate code to initialize a ghdl_index_type variable V to 0.
      procedure Init_Var (V : O_Dnode);

      --  Generate code to increment/decrement a ghdl_index_type variable V.
      procedure Inc_Var (V : O_Dnode);
      procedure Dec_Var (V : O_Dnode);

      --  Generate code to exit from loop LABEL iff COND is true.
      procedure Gen_Exit_When (Label : O_Snode; Cond : O_Enode);

      --  Low-level stack2 mark and release.
      procedure Set_Stack2_Mark (Var : O_Lnode);
      procedure Release_Stack2 (Var : O_Lnode);

      --  Create a region for temporary variables.  The region is only created
      --  on demand (at the first Create_Temp*), so you must be careful not
      --  to nest with control statement.  For example, the following
      --  sequence is not correct:
      --    Open_Temp
      --    Start_If_Stmt
      --    ... Create_Temp ...
      --    Finish_If_Stmt
      --    Close_Temp
      --  Because the first Create_Temp is within the if statement, the
      --  declare block will be created within the if statement, and must
      --  have been closed before the end of the if statement.
      procedure Open_Temp;

      --  Create a temporary variable.
      function Create_Temp (Atype : O_Tnode) return O_Dnode;
      --  Create a temporary variable of ATYPE and initialize it with VALUE.
      function Create_Temp_Init (Atype : O_Tnode; Value : O_Enode)
                                 return O_Dnode;
      --  Create a temporary variable of ATYPE and initialize it with the
      --  address of NAME.
      function Create_Temp_Ptr (Atype : O_Tnode; Name : O_Lnode)
                               return O_Dnode;

      function Create_Temp_Bounds (Tinfo : Type_Info_Acc) return Mnode;

      --  Create a mark in the temporary region for the stack2.
      --  FIXME: maybe a flag must be added to CLOSE_TEMP where it is known
      --   stack2 can be released.
      procedure Create_Temp_Stack2_Mark;

      --  Close the temporary region.
      procedure Close_Temp;

      --  Like Open_Temp, but will never create a declare region. To be used
      --  only within a subprogram, to use the declare region of the
      --  subprogram.
      procedure Open_Local_Temp;
      procedure Close_Local_Temp;

      --  Return TRUE if stack2 will be released.  Used for fine-tuning only
      --  (return statement).
      function Has_Stack2_Mark return Boolean;
      --  Manually release stack2.  Used for fine-tuning only.
      procedure Stack2_Release;

      --  Used only in procedure calls to disable the release of stack2, as
      --  it might be part of the state of the call.  Must be called just after
      --  Open_Temp.
      procedure Disable_Stack2_Release;

      --  Free all old temp.
      --  Used only to free memory.
      procedure Free_Old_Temp;

      --  Return a ghdl_index_type literal for NUM.
      function New_Index_Lit (Num : Unsigned_64) return O_Cnode;

      --  Create a uniq identifier.
      subtype Uniq_Identifier_String is String (1 .. 11);
      function Create_Uniq_Identifier return Uniq_Identifier_String;
      function Create_Uniq_Identifier return O_Ident;
   end Helpers;
end Trans;
