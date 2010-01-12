--  Semantic analysis.
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Iirs; use Iirs;

package Sem_Names is
   --  Semantize NAME as long as it consists in named entities.
   --  Set Named_Entity field of NAME, with:
   --  * the named entity (if any)
   --  * an overload_list of named entity
   --  * error_mark (in case of error, the message error is displayed).
   procedure Sem_Name (Name : Iir; Keep_Alias : Boolean);

   --  Finish semantisation of NAME, if necessary.
   --  This make remaining checks, transforms function names into calls...
   procedure Maybe_Finish_Sem_Name (Name : Iir);

   --  Same as Sem_Name but without any side-effect:
   --  * do not report error
   --  * do not set xrefs
   --  Currently, only simple names (and expanded names) are handled.
   --  This is to be used during sem of associations.
   procedure Sem_Name_Soft (Name : Iir);

   --  Remove every named_entity of NAME.
   --  If NAME is Null_Iir then this is no op.
   --  To be used only for names (weakly) semantized by sem_name_soft.
   procedure Sem_Name_Clean (Name : Iir);

   --  Return TRUE if NAME is a name that designate an object.
   --  Only in this case, base_name is defined.
   function Is_Object_Name (Name : Iir) return Boolean;

   --  Return an object node if NAME designates an object (ie either is an
   --  object or a name for an object).
   --  Otherwise, returns NULL_IIR.
   function Name_To_Object (Name : Iir) return Iir;

   --  If NAME is a selected name whose prefix is a protected variable, set
   --  method_object of CALL.
   procedure Name_To_Method_Object (Call : Iir; Name : Iir);

   --  Convert name EXPR to an expression (ie, can create function call).
   --  A_TYPE is the expected type of the expression.
   --  FIXME: it is unclear wether the result must be an expression or not
   --  (ie, it *must* have a type, but may be a range).
   function Name_To_Expression (Name : Iir; A_Type : Iir) return Iir;

   -- Return true if AN_IIR is an overload list.
   function Is_Overload_List (An_Iir: Iir) return Boolean;
   pragma Inline (Is_Overload_List);

   -- Create an overload list.
   -- must be destroyed with free_iir.
   function Get_Overload_List return Iir_Overload_List;
   pragma Inline (Get_Overload_List);
   function Create_Overload_List (List : Iir_List) return Iir_Overload_List;
   pragma Inline (Create_Overload_List);

   procedure Error_Overload (Expr: Iir);

   procedure Disp_Overload_List (List : Iir_List; Loc : Iir);

   --  Convert a list to either Null_Iir, an element or an overload list.
   function Simplify_Overload_List (List : Iir_List) return Iir;

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

   --  Kind of declaration to find.
   --  Decl_entity: an entity declaration (used for binding_indication).
   --  Decl_Any : no checks is performed.

   type Decl_Kind_Type is
     (Decl_Type, Decl_Incomplete_Type,
      Decl_Component, Decl_Unit, Decl_Label,
      Decl_Group_Template, Decl_Entity, Decl_Configuration, Decl_Attribute);

   --  Find a uniq declaration for name NAME, which can be a simple_name,
   --  an identifier or a selected_name.
   --  Disp an error message if:
   --   NAME (or any prefix of it) is undefined
   --   NAME is overloaded
   --   NAME does not belong to KIND.
   --  In these case, null_iir  is returned.
   --  Otherwise, the declaration is returned, and NAME is freed.
   --  If NAME is a selected_name, dependencies can be added to the current
   --  design unit.
   function Find_Declaration (Name: Iir; Kind: Decl_Kind_Type) return Iir;
end Sem_Names;
