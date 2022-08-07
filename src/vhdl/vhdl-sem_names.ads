--  Semantic analysis.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Sem_Names is
   --  In VHDL, most of name notations are ambiguous:
   --   P.N is either
   --     an expanded name or
   --     a selected name for an element (with a possible implicit dereference)
   --   P (A1, A2, ...) can be
   --     an indexed name (with a possible implicit dereference)
   --     a slice name (with a possible implicit dereference)
   --     a subprogram call
   --     a type conversion

   --  The name analysis resolves two ambiguities: notation and overload.
   --  In a first pass, all possible meaning are collected as an overload
   --  list in the Named_Entity field of the name.  Prefixes in that list
   --  are always declarations and not simple or expanded names.  This is done
   --  to avoid creating nodes for simple or expanded names, as they cannot be
   --  shared in the prefixes because they can have several meanings.
   --
   --  In a second pass, when the caller has resolved the overloading (using
   --  the context), the name is rewritten: parenthesis and selected names are
   --  replaced (by slice, index, call, element selection...).  Prefixes are
   --  simple or expanded names (and never declarations).  Checks are also
   --  performed on the result (pure, all sensitized).
   --
   --  The result of the name analysis may not be a name: a function_call or
   --  a type conversion are not names.

   --  Analyze NAME: perform the first pass only.  In case of error, a message
   --  is displayed and the named entity is error_mark.
   procedure Sem_Name (Name : Iir; Keep_Alias : Boolean := False);

   --  Finish analysis of NAME, if necessary.  The named entity must not
   --  be an overload list (ie the overload resolution must have been done).
   --  Do remaining checks, transform function names into calls...
   function Finish_Sem_Name (Name : Iir) return Iir;

   --  Analyze NAME as a type mark.  NAME must be either a simple name or an
   --  expanded name, and the denoted entity must be either a type or a subtype
   --  declaration.  Return the name (possibly modified) and set named_entity
   --  and type.  In case of error, the type is error_mark.  NAME may have
   --  already been analyzed by Sem_Name.
   --  Incomplete types are allowed only if INCOMPLETE is True.
   function Sem_Type_Mark (Name : Iir; Incomplete : Boolean := False)
                          return Iir;

   --  Same as Sem_Name but without any side-effect:
   --  * do not report error
   --  * do not set xrefs
   --  Currently, only simple names (and expanded names) are handled.
   --  This is to be used during sem of associations.  Because there is no side
   --  effect, NAME is not modified (but is decorated with Named_Entity)
   procedure Sem_Name_Soft (Name : Iir);

   --  Remove every named_entity of NAME.
   --  If NAME is Null_Iir then this is no op.
   --  To be used only for names (weakly) analyzed by sem_name_soft.
   procedure Sem_Name_Clean (Name : Iir);

   --  Return an interpretation for identifier ID.
   --  Used mostly by Sem_Simple_Name but also for unassociated interface
   --  subprogram whose default is <>.
   --  Do not follow aliases is KEEP_ALIAS is true (used for attribute
   --  specification),
   --  do not report error if SOFT is true (used for associations of
   --  overloaded names).
   --
   --  Returns either the interpretation, an overload list or a error_mark.
   function Sem_Identifier_Name (Id : Name_Id;
                                 Loc : Iir;
                                 Keep_Alias : Boolean;
                                 Soft : Boolean) return Iir;

   --  If NAME is a selected name whose prefix is a protected variable, set
   --  method_object of CALL.
   procedure Name_To_Method_Object (Call : Iir; Name : Iir);

   --  Convert name NAME to an expression (ie, can create function call).
   --  A_TYPE is the expected type of the expression.
   --  FIXME: it is unclear whether the result must be an expression or not
   --  (ie, it *must* have a type, but may be a range).
   function Name_To_Expression (Name : Iir; A_Type : Iir) return Iir;

   --  Finish analyze of NAME and expect a range (either a type or subtype
   --  declaration or a range attribute).  Return Error_Mark in case of error.
   function Name_To_Range (Name : Iir) return Iir;

   --  Convert name NAME to a type definition.  Return an error if NAME does
   --  not designate a type (and emit an error message).  NAME must be a fully
   --  analyzed name (cannot be an Iir_Kind_Attribute_Name).
   function Name_To_Type_Definition (Name : Iir) return Iir;

   -- Return true if AN_IIR is an overload list.
   function Is_Overload_List (An_Iir: Iir) return Boolean;
   pragma Inline (Is_Overload_List);

   -- Create an overload list, that must be destroyed by Destroy_Overload_List.
   function Get_Overload_List return Iir_Overload_List;
   pragma Inline (Get_Overload_List);

   function Create_Overload_List (List : Iir_List) return Iir_Overload_List;
   pragma Inline (Create_Overload_List);

   --  Free the list node (and the list itself).
   procedure Free_Overload_List (N : in out Iir_Overload_List);

   --  Display an error message if the overload resolution for EXPR find more
   --  than one interpretation.
   procedure Error_Overload (Expr: Iir);

   --  Disp the overload list LIST.
   procedure Disp_Overload_List (List : Iir_List; Loc : Iir);

   --  Convert a list to either Null_Iir, an element or an overload list.
   function Simplify_Overload_List (List : Iir_List) return Iir;

   --  Add new interpretation DECL to RES.
   --  Create an overload_list if necessary.
   --  Before the first call, RES should be set to NULL_IIR.
   procedure Add_Result (Res : in out Iir; Decl : Iir);

   --  Return TRUE if ATYPE is defined: not Null_Iir, not an overload list and
   --  not a wildcard.
   function Is_Defined_Type (Atype : Iir) return Boolean;

   --  Free a Parenthesis_Name.  This is a special case as in general the
   --  Association_Chain field must be freed too.
   procedure Free_Parenthesis_Name (Name : Iir; Res : Iir);

   --  Return TRUE iff TYPE1 and TYPE2 are closely related.
   function Are_Types_Closely_Related (Type1, Type2 : Iir) return Boolean;

   --  From the list LIST of function or enumeration literal, extract the
   --  list of (return) types.
   --  If there is only one type, return it.
   --  If there is no types, return NULL.
   --  Otherwise, return the list as an overload list.
   function Create_List_Of_Types (List : Iir_List) return Iir;

   function Sem_Index_Specification (Name : Iir_Parenthesis_Name; Itype : Iir)
                                    return Iir;

   --  Analyze denoting name NAME.  NAME must be either a simple name or an
   --  expanded name and so is the result.
   function Sem_Denoting_Name (Name: Iir) return Iir;

   --  Like Sem_Denoting_Name but expect a terminal name.
   function Sem_Terminal_Name (Name : Iir) return Iir;

   --  Analyze an external name.
   procedure Sem_External_Name (Name : Iir);

   --  Emit an error for NAME that doesn't match its class CLASS_NAME.
   procedure Error_Class_Match (Name : Iir; Class_Name : String);
end Vhdl.Sem_Names;
