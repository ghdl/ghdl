.. _REF:Style:

Coding Style
############

Ada subset: use only a simple (VHDL like) subset of Ada: no tasking, no
controlled types... VHDL users should easily understand that subset.
Allowed Ada95 features: the standard library, child packages.
Use assertions.

We try to follow the 'GNU Coding Standards' when possible: comments before
declarations, one space at the end of sentences, finish sentences with a dot.
But: 2 spaces for indentation in code blocks.

No trailing spaces, no TAB (HT).

Subprograms must have a comment before to describe them, like:

.. code-block:: Ada
	 
   --  Analyze the concurrent statements of PARENT.
   procedure Sem_Concurrent_Statement_Chain (Parent : Iir);

The line before the comment must be a blank line (unless this is the first
declaration). Don't repeat the comment before the subprogram body.

* For subprograms:

  1. Declare on one line when possible:
	
     .. code-block:: Ada
  
        function Translate_Static_Aggregate (Aggr : Iir) return O_Cnode
  
  2. If not possible, put the return on the next line:
	
     .. code-block:: Ada
  
        function Translate_Static_String (Str_Type : Iir; Str_Ident : Name_Id)
                                         return O_Cnode
  
  3. If not possible, put parameters and return on the next line:
	
     .. code-block:: Ada
  
        function Create_String_Literal_Var_Inner
          (Str : Iir; Element_Type : Iir; Str_Type : O_Tnode) return Var_Type
  
  4. If not possible, return on the next line:
	
     .. code-block:: Ada
  
        function Translate_Shortcut_Operator
          (Imp : Iir_Implicit_Function_Declaration; Left, Right : Iir)
          return O_Enode
  
  5. If not possible, one parameter per line, just after subprogram name:
	
     .. code-block:: Ada
  
        procedure Translate_Static_Aggregate_1 (List : in out O_Array_Aggr_List;
                                                Aggr : Iir;
                                                Info : Iir;
                                                El_Type : Iir)

  6. If not possible, add a return after subprogram name:
	
     .. code-block:: Ada
  
        function Translate_Predefined_TF_Array_Element
          (Op : Predefined_Boolean_Logical;
           Left, Right : Iir;
           Res_Type : Iir;
           Loc : Iir)
          return O_Enode
  
  7. If not possible, ask yourself what is wrong!  Shorten a name.

* Rule for the 'is': on a new line only if the declarative part is not empty:
	
     .. code-block:: Ada
  
        procedure Translate_Assign (Target : Mnode; Expr : Iir; Target_Type : Iir)
        is
          Val : O_Enode;
        begin
				
  vs.
	
     .. code-block:: Ada
  
        function Translate_Static_Range_Dir (Expr : Iir) return O_Cnode is
        begin

  If the parameter line is too long with the 'is', put in on a separate line:
	
     .. code-block:: Ada
  
        procedure Predeclare_Scope_Type
          (Scope : in out Var_Scope_Type; Name : O_Ident) is

* Generic instantiation: put the generic actual part on a new line:
	
     .. code-block:: Ada
  
        procedure Free is new Ada.Unchecked_Deallocation
          (Action_List, Action_List_Acc);

* For if/then statement:

  1. 'then' on the same line:
	
     .. code-block:: Ada
  
        if Get_Expr_Staticness (Decl) = Locally then
  
  2. If not possible, 'then' is alone on its line aligned with the 'if':
	
     .. code-block:: Ada
  
        if Expr = Null_Iir
          or else Get_Kind (Expr) = Iir_Kind_Overflow_Literal
        then
  
  3. For a multiline condition, 'or else' and 'and then' should start lines.

* 'Local' variable declaration:
  Do not initialize variables, constants must be declared before variables:
	
     .. code-block:: Ada
  
        is
          N_Info : constant Iir := Get_Sub_Aggregate_Info (Info);
          Assoc  : Iir;
          Sub    : Iir;
        begin
				
  If the initialization expression has a side effect (such as allocation), do
  not use a constant.
