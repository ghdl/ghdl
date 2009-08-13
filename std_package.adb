--  std.standard package declarations.
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
with Types; use Types;
with Files_Map;
with Name_Table;
with Str_Table;
with Std_Names; use Std_Names;
with Flags; use Flags;
with Iirs_Utils;
with Sem;
with Sem_Decls;
with Iir_Chains;

package body Std_Package is
   type Bound_Array is array (Boolean) of Iir_Int64;
   Low_Bound : constant Bound_Array := (False => -(2 ** 31),
                                        True => -(2 ** 63));
   High_Bound : constant Bound_Array := (False => (2 ** 31) - 1,
                                         True => (2 ** 63) - 1);

   Std_Location: Location_Type := Location_Nil;
   Std_Filename : Name_Id := Null_Identifier;

   function Create_Std_Iir (Kind : Iir_Kind) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Iir (Kind);
      Set_Location (Res, Std_Location);
      return Res;
   end Create_Std_Iir;

   function Create_Std_Decl (Kind : Iir_Kind) return Iir
   is
      Res : Iir;
   begin
      Res := Create_Std_Iir (Kind);
      Set_Parent (Res, Standard_Package);
      return Res;
   end Create_Std_Decl;

   procedure Create_First_Nodes
   is
   begin
      Std_Filename := Name_Table.Get_Identifier ("*std_standard*");
      Std_Location := Files_Map.Source_File_To_Location
        (Files_Map.Create_Virtual_Source_File (Std_Filename));

      if Create_Iir_Error /= Error_Mark then
         raise Internal_Error;
      end if;
      Set_Location (Error_Mark, Std_Location);

      if Create_Std_Iir (Iir_Kind_Integer_Type_Definition)
        /= Universal_Integer_Type_Definition
      then
         raise Internal_Error;
      end if;

      if Create_Std_Iir (Iir_Kind_Floating_Type_Definition)
        /= Universal_Real_Type_Definition
      then
         raise Internal_Error;
      end if;

      if Create_Std_Iir (Iir_Kind_Integer_Type_Definition)
        /= Convertible_Integer_Type_Definition
      then
         raise Internal_Error;
      end if;

      if Create_Std_Iir (Iir_Kind_Floating_Type_Definition)
        /= Convertible_Real_Type_Definition
      then
         raise Internal_Error;
      end if;
   end Create_First_Nodes;

   procedure Create_Std_Standard_Package (Parent : Iir_Library_Declaration)
   is
      function Get_Std_Character (Char: Character) return Name_Id
        renames Name_Table.Get_Identifier;

      procedure Set_Std_Identifier (Decl : Iir; Name : Name_Id) is
      begin
         Set_Identifier (Decl, Name);
         Set_Visible_Flag (Decl, True);
      end Set_Std_Identifier;

      function Create_Std_Integer (Val : Iir_Int64; Lit_Type : Iir)
        return Iir_Integer_Literal
      is
         Res : Iir_Integer_Literal;
      begin
         Res := Create_Std_Iir (Iir_Kind_Integer_Literal);
         Set_Value (Res, Val);
         Set_Type (Res, Lit_Type);
         Set_Expr_Staticness (Res, Locally);
         return Res;
      end Create_Std_Integer;

      function Create_Std_Fp (Val : Iir_Fp64; Lit_Type : Iir)
        return Iir_Floating_Point_Literal
      is
         Res : Iir_Floating_Point_Literal;
      begin
         Res := Create_Std_Iir (Iir_Kind_Floating_Point_Literal);
         Set_Fp_Value (Res, Val);
         Set_Type (Res, Lit_Type);
         Set_Expr_Staticness (Res, Locally);
         return Res;
      end Create_Std_Fp;

      function Create_Std_Range_Expr (Left, Right : Iir; Rtype : Iir)
        return Iir
      is
         Res : Iir;
      begin
         Res := Create_Std_Iir (Iir_Kind_Range_Expression);
         Set_Left_Limit (Res, Left);
         Set_Direction (Res, Iir_To);
         Set_Right_Limit (Res, Right);
         Set_Expr_Staticness (Res, Locally);
         Set_Type (Res, Rtype);
         return Res;
      end Create_Std_Range_Expr;

      function Create_Std_Literal
        (Name : Name_Id; Sub_Type : Iir_Enumeration_Type_Definition)
        return Iir_Enumeration_Literal
      is
         Res : Iir_Enumeration_Literal;
         List : Iir_List;
      begin
         Res := Create_Std_Decl (Iir_Kind_Enumeration_Literal);
         List := Get_Enumeration_Literal_List (Sub_Type);
         Set_Std_Identifier (Res, Name);
         Set_Type (Res, Sub_Type);
         Set_Expr_Staticness (Res, Locally);
         Set_Name_Staticness (Res, Locally);
         Set_Base_Name (Res, Res);
         Set_Enumeration_Decl (Res, Res);
         Set_Enum_Pos (Res, Iir_Int32 (Get_Nbr_Elements (List)));
         Sem.Compute_Subprogram_Hash (Res);
         Append_Element (List, Res);
         return Res;
      end Create_Std_Literal;

      --  Append a declaration DECL to Standard_Package.
      Last_Decl : Iir := Null_Iir;
      procedure Add_Decl (Decl : Iir) is
      begin
         if Last_Decl = Null_Iir then
            Set_Declaration_Chain (Standard_Package, Decl);
         else
            Set_Chain (Last_Decl, Decl);
         end if;
         Last_Decl := Decl;
      end Add_Decl;

      procedure Add_Implicit_Operations (Decl : Iir)
      is
         Nxt : Iir;
      begin
         Sem_Decls.Create_Implicit_Operations (Decl, True);
         loop
            Nxt := Get_Chain (Last_Decl);
            exit when Nxt = Null_Iir;
            Last_Decl := Nxt;
         end loop;
      end Add_Implicit_Operations;

      procedure Create_Integer_Type (Type_Definition : Iir;
                                     Type_Decl : out Iir;
                                     Type_Name : Name_Id)
      is
      begin
         --Integer_Type_Definition :=
         --  Create_Std_Iir (Iir_Kind_Integer_Type_Definition);
         Set_Base_Type (Type_Definition, Type_Definition);
         Set_Type_Staticness (Type_Definition, Locally);
         Set_Signal_Type_Flag (Type_Definition, True);
         Set_Has_Signal_Flag (Type_Definition, not Flags.Flag_Whole_Analyze);

         Type_Decl := Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Type_Decl, Type_Name);
         Set_Type (Type_Decl, Type_Definition);
         Set_Type_Declarator (Type_Definition, Type_Decl);
      end Create_Integer_Type;

      procedure Create_Integer_Subtype (Type_Definition : Iir;
                                        Type_Decl : Iir;
                                        Subtype_Definition : out Iir;
                                        Subtype_Decl : out Iir)
      is
         Constraint : Iir;
      begin
         Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Subtype_Definition);
         Set_Base_Type (Subtype_Definition, Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Integer (Low_Bound (Flags.Flag_Integer_64),
                                Universal_Integer_Type_Definition),
            Create_Std_Integer (High_Bound (Flags.Flag_Integer_64),
                                Universal_Integer_Type_Definition),
            Universal_Integer_Type_Definition);
         Set_Range_Constraint (Subtype_Definition, Constraint);
         Set_Type_Staticness (Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Subtype_Definition, True);
         Set_Has_Signal_Flag (Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  type is
         Subtype_Decl := Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Subtype_Decl, Get_Identifier (Type_Decl));
         Set_Type (Subtype_Decl, Subtype_Definition);
         Set_Type_Declarator (Subtype_Definition, Subtype_Decl);
         Set_Subtype_Definition (Type_Decl, Subtype_Definition);
      end Create_Integer_Subtype;

   begin
      Std_Standard_File := Create_Std_Iir (Iir_Kind_Design_File);
      Set_Parent (Std_Standard_File, Parent);
      Set_Design_File_Filename (Std_Standard_File, Std_Filename);
      Std_Standard_Unit := Create_Std_Iir (Iir_Kind_Design_Unit);
      Set_Std_Identifier (Std_Standard_Unit, Name_Standard);
      Set_First_Design_Unit (Std_Standard_File, Std_Standard_Unit);
      Set_Last_Design_Unit (Std_Standard_File, Std_Standard_Unit);
      Set_Design_File (Std_Standard_Unit, Std_Standard_File);
      Set_Date_State (Std_Standard_Unit, Date_Analyze);
      Set_Dependence_List (Std_Standard_Unit, Create_Iir_List);

      declare
         use Str_Table;
         Std_Time_Stamp : constant Time_Stamp_String :=
           "20020601000000.000";
         Id : Time_Stamp_Id;
      begin
         Id := Time_Stamp_Id (Str_Table.Start);
         for I in Time_Stamp_String'Range loop
            Str_Table.Append (Std_Time_Stamp (I));
         end loop;
         Str_Table.Finish;
         Set_Analysis_Time_Stamp (Std_Standard_File, Id);
      end;

      Set_Date (Std_Standard_Unit, Date_Valid'First);

      -- Adding "package STANDARD is"
      Standard_Package := Create_Std_Iir (Iir_Kind_Package_Declaration);
      Set_Identifier (Standard_Package, Name_Standard);
      Set_Need_Body (Standard_Package, False);

      Set_Library_Unit (Std_Standard_Unit, Standard_Package);
      Set_Design_Unit (Standard_Package, Std_Standard_Unit);

      -- boolean
      begin
         -- (false, true)
         Boolean_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Base_Type (Boolean_Type_Definition, Boolean_Type_Definition);
         Set_Enumeration_Literal_List
           (Boolean_Type_Definition, Create_Iir_List);
         Boolean_False := Create_Std_Literal
           (Name_False, Boolean_Type_Definition);
         Boolean_True := Create_Std_Literal
           (Name_True, Boolean_Type_Definition);
         Set_Type_Staticness (Boolean_Type_Definition, Locally);
         Set_Signal_Type_Flag (Boolean_Type_Definition, True);
         Set_Has_Signal_Flag (Boolean_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         -- type boolean is
         Boolean_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (Boolean_Type, Name_Boolean);
         Set_Type (Boolean_Type, Boolean_Type_Definition);
         Add_Decl (Boolean_Type);
         Set_Type_Declarator (Boolean_Type_Definition, Boolean_Type);

         Iirs_Utils.Create_Range_Constraint_For_Enumeration_Type
           (Boolean_Type_Definition);
         Add_Implicit_Operations (Boolean_Type);
      end;

      -- bit.
      begin
         -- ('0', '1')
         Bit_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Enumeration_Literal_List
           (Bit_Type_Definition, Create_Iir_List);
         Set_Base_Type (Bit_Type_Definition, Bit_Type_Definition);
         Bit_0 := Create_Std_Literal
           (Get_Std_Character ('0'), Bit_Type_Definition);
         Bit_1 := Create_Std_Literal
           (Get_Std_Character ('1'), Bit_Type_Definition);
         Set_Type_Staticness (Bit_Type_Definition, Locally);
         Set_Signal_Type_Flag (Bit_Type_Definition, True);
         Set_Has_Signal_Flag (Bit_Type_Definition,
                              not Flags.Flag_Whole_Analyze);
         Set_Only_Characters_Flag (Bit_Type_Definition, True);

         -- type bit is
         Bit_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (Bit_Type, Name_Bit);
         Set_Type (Bit_Type, Bit_Type_Definition);
         Add_Decl (Bit_Type);
         Set_Type_Declarator (Bit_Type_Definition, Bit_Type);

         Iirs_Utils.Create_Range_Constraint_For_Enumeration_Type
           (Bit_Type_Definition);
         Add_Implicit_Operations (Bit_Type);
      end;

      -- characters.
      declare
         El: Iir;
         pragma Unreferenced (El);
      begin
         Character_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Base_Type (Character_Type_Definition, Character_Type_Definition);
         Set_Enumeration_Literal_List
           (Character_Type_Definition, Create_Iir_List);

         for I in Name_Nul .. Name_Usp loop
            El := Create_Std_Literal (I, Character_Type_Definition);
         end loop;
         for I in Character'(' ') .. Character'('~') loop
            El := Create_Std_Literal
              (Get_Std_Character (I), Character_Type_Definition);
         end loop;
         El := Create_Std_Literal (Name_Del, Character_Type_Definition);
         if Vhdl_Std /= Vhdl_87 then
            for I in Name_C128 .. Name_C159 loop
               El := Create_Std_Literal (I, Character_Type_Definition);
            end loop;
            for I in Character'Val (160) .. Character'Val (255) loop
               El := Create_Std_Literal
                 (Get_Std_Character (I), Character_Type_Definition);
            end loop;
         end if;
         Set_Type_Staticness (Character_Type_Definition, Locally);
         Set_Signal_Type_Flag (Character_Type_Definition, True);
         Set_Has_Signal_Flag (Character_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         -- type character is
         Character_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (Character_Type, Name_Character);
         Set_Type (Character_Type, Character_Type_Definition);
         Add_Decl (Character_Type);
         Set_Type_Declarator (Character_Type_Definition,
                              Character_Type);

         Iirs_Utils.Create_Range_Constraint_For_Enumeration_Type
           (Character_Type_Definition);
         Add_Implicit_Operations (Character_Type);
      end;

      -- severity level.
      begin
         -- (note, warning, error, failure)
         Severity_Level_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Base_Type (Severity_Level_Type_Definition,
                        Severity_Level_Type_Definition);
         Set_Enumeration_Literal_List
           (Severity_Level_Type_Definition, Create_Iir_List);

         Severity_Level_Note := Create_Std_Literal
           (Name_Note, Severity_Level_Type_Definition);
         Severity_Level_Warning := Create_Std_Literal
           (Name_Warning, Severity_Level_Type_Definition);
         Severity_Level_Error := Create_Std_Literal
           (Name_Error, Severity_Level_Type_Definition);
         Severity_Level_Failure := Create_Std_Literal
           (Name_Failure, Severity_Level_Type_Definition);
         Set_Type_Staticness (Severity_Level_Type_Definition, Locally);
         Set_Signal_Type_Flag (Severity_Level_Type_Definition, True);
         Set_Has_Signal_Flag (Severity_Level_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         -- type severity_level is
         Severity_Level_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (Severity_Level_Type, Name_Severity_Level);
         Set_Type (Severity_Level_Type, Severity_Level_Type_Definition);
         Add_Decl (Severity_Level_Type);
         Set_Type_Declarator (Severity_Level_Type_Definition,
                              Severity_Level_Type);

         Iirs_Utils.Create_Range_Constraint_For_Enumeration_Type
           (Severity_Level_Type_Definition);
         Add_Implicit_Operations (Severity_Level_Type);
      end;

      -- universal types.
      begin
         Create_Integer_Type (Universal_Integer_Type_Definition,
                              Universal_Integer_Type,
                              Name_Universal_Integer);
         Add_Decl (Universal_Integer_Type);

         Create_Integer_Subtype (Universal_Integer_Type_Definition,
                                 Universal_Integer_Type,
                                 Universal_Integer_Subtype_Definition,
                                 Universal_Integer_Subtype);

         Add_Decl (Universal_Integer_Subtype);
         Set_Subtype_Definition (Universal_Integer_Type,
                                 Universal_Integer_Subtype_Definition);

         --  Do not create implicit operations yet, since "**" needs integer
         --  type.
      end;

      --  Universal integer constant 1.
      Universal_Integer_One :=
        Create_Std_Integer (1, Universal_Integer_Type_Definition);

      -- Universal real.
      declare
         Constraint : Iir_Range_Expression;
      begin
         Set_Base_Type (Universal_Real_Type_Definition,
                        Universal_Real_Type_Definition);
         Set_Type_Staticness (Universal_Real_Type_Definition, Locally);
         Set_Signal_Type_Flag (Universal_Real_Type_Definition, True);
         Set_Has_Signal_Flag (Universal_Real_Type_Definition, False);

         Universal_Real_Type :=
           Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Universal_Real_Type, Name_Universal_Real);
         Set_Type (Universal_Real_Type, Universal_Real_Type_Definition);
         Set_Type_Declarator (Universal_Real_Type_Definition,
                              Universal_Real_Type);
         Add_Decl (Universal_Real_Type);

         Universal_Real_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Floating_Subtype_Definition);
         Set_Base_Type (Universal_Real_Subtype_Definition,
                        Universal_Real_Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Fp (Iir_Fp64'First, Universal_Real_Type_Definition),
            Create_Std_Fp (Iir_Fp64'Last, Universal_Real_Type_Definition),
            Universal_Real_Type_Definition);
         Set_Range_Constraint (Universal_Real_Subtype_Definition, Constraint);
         Set_Type_Staticness (Universal_Real_Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Universal_Real_Subtype_Definition, True);
         Set_Has_Signal_Flag (Universal_Real_Subtype_Definition, False);

         --  type is
         Universal_Real_Subtype :=
           Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Identifier (Universal_Real_Subtype, Name_Universal_Real);
         Set_Type (Universal_Real_Subtype, Universal_Real_Subtype_Definition);
         Set_Type_Declarator (Universal_Real_Subtype_Definition,
                              Universal_Real_Subtype);
         Set_Subtype_Definition (Universal_Real_Type,
                                 Universal_Real_Subtype_Definition);

         Add_Decl (Universal_Real_Subtype);

         --  Do not create implicit operations yet, since "**" needs integer
         --  type.
      end;

      -- Convertible type.
      begin
         Create_Integer_Type (Convertible_Integer_Type_Definition,
                              Convertible_Integer_Type,
                              Name_Convertible_Integer);
         Create_Integer_Subtype (Convertible_Integer_Type_Definition,
                                 Convertible_Integer_Type,
                                 Convertible_Integer_Subtype_Definition,
                                 Convertible_Integer_Subtype);

         --  Not added in std.standard.
      end;

      begin
         Set_Base_Type (Convertible_Real_Type_Definition,
                        Convertible_Real_Type_Definition);
         Set_Type_Staticness (Convertible_Real_Type_Definition, Locally);
         Set_Signal_Type_Flag (Convertible_Real_Type_Definition, True);
         Set_Has_Signal_Flag (Convertible_Real_Type_Definition, False);

         Convertible_Real_Type :=
           Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Convertible_Real_Type, Name_Convertible_Real);
         Set_Type (Convertible_Real_Type, Convertible_Real_Type_Definition);
         Set_Type_Declarator (Convertible_Real_Type_Definition,
                              Convertible_Real_Type);
      end;

      -- integer type.
      begin
         Integer_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Type_Definition);
         Create_Integer_Type (Integer_Type_Definition,
                              Integer_Type,
                              Name_Integer);
         Add_Decl (Integer_Type);

         Add_Implicit_Operations (Integer_Type);
         Add_Implicit_Operations (Universal_Integer_Type);
         Add_Implicit_Operations (Universal_Real_Type);

         Create_Integer_Subtype (Integer_Type_Definition,
                                 Integer_Type,
                                 Integer_Subtype_Definition,
                                 Integer_Subtype);
         Add_Decl (Integer_Subtype);
      end;

      -- Real type.
      declare
         Constraint : Iir_Range_Expression;
      begin
         Real_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Floating_Type_Definition);
         Set_Base_Type (Real_Type_Definition, Real_Type_Definition);
         Set_Type_Staticness (Real_Type_Definition, Locally);
         Set_Signal_Type_Flag (Real_Type_Definition, True);
         Set_Has_Signal_Flag (Real_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         Real_Type := Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Real_Type, Name_Real);
         Set_Type (Real_Type, Real_Type_Definition);
         Set_Type_Declarator (Real_Type_Definition, Real_Type);
         Add_Decl (Real_Type);

         Add_Implicit_Operations (Real_Type);

         Real_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Floating_Subtype_Definition);
         Set_Base_Type (Real_Subtype_Definition, Real_Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Fp (Iir_Fp64'First, Universal_Real_Type_Definition),
            Create_Std_Fp (Iir_Fp64'Last, Universal_Real_Type_Definition),
             Universal_Real_Type_Definition);
         Set_Range_Constraint (Real_Subtype_Definition, Constraint);
         Set_Type_Staticness (Real_Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Real_Subtype_Definition, True);
         Set_Has_Signal_Flag (Real_Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         Real_Subtype := Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Real_Subtype, Name_Real);
         Set_Type (Real_Subtype, Real_Subtype_Definition);
         Set_Type_Declarator (Real_Subtype_Definition, Real_Subtype);
         Add_Decl (Real_Subtype);

         Set_Subtype_Definition (Real_Type, Real_Subtype_Definition);
      end;

      -- natural subtype
      declare
         Constraint : Iir_Range_Expression;
      begin
         Natural_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Subtype_Definition);
         Set_Base_Type (Natural_Subtype_Definition, Integer_Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Integer (0, Integer_Type_Definition),
            Create_Std_Integer (High_Bound (Flags.Flag_Integer_64),
                                Integer_Type_Definition),
            Integer_Type_Definition);
         Set_Range_Constraint (Natural_Subtype_Definition, Constraint);
         Set_Type_Staticness (Natural_Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Natural_Subtype_Definition, True);
         Set_Has_Signal_Flag (Natural_Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         Natural_Subtype := Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Natural_Subtype, Name_Natural);
         Set_Type (Natural_Subtype, Natural_Subtype_Definition);
         Add_Decl (Natural_Subtype);
         Set_Type_Declarator (Natural_Subtype_Definition, Natural_Subtype);
      end;

      -- positive subtype
      declare
         Constraint : Iir_Range_Expression;
      begin
         Positive_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Subtype_Definition);
         Set_Base_Type (Positive_Subtype_Definition,
                        Integer_Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Integer (1, Integer_Type_Definition),
            Create_Std_Integer (High_Bound (Flags.Flag_Integer_64),
                                Integer_Type_Definition),
             Integer_Type_Definition);
         Set_Range_Constraint (Positive_Subtype_Definition, Constraint);
         Set_Type_Staticness (Positive_Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Positive_Subtype_Definition, True);
         Set_Has_Signal_Flag (Positive_Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         Positive_Subtype := Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Positive_Subtype, Name_Positive);
         Set_Type (Positive_Subtype, Positive_Subtype_Definition);
         Add_Decl (Positive_Subtype);
         Set_Type_Declarator (Positive_Subtype_Definition, Positive_Subtype);
      end;

      -- string type.
      -- type string is array (positive range <>) of character;
      begin
         String_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Array_Type_Definition);
         Set_Base_Type (String_Type_Definition, String_Type_Definition);
         Set_Index_Subtype_List (String_Type_Definition, Create_Iir_List);
         Append_Element (Get_Index_Subtype_List (String_Type_Definition),
                         Positive_Subtype_Definition);
         Set_Element_Subtype (String_Type_Definition,
                              Character_Type_Definition);
         Set_Type_Staticness (String_Type_Definition, None);
         Set_Signal_Type_Flag (String_Type_Definition, True);
         Set_Has_Signal_Flag (String_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         String_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (String_Type, Name_String);
         Set_Type (String_Type, String_Type_Definition);
         Add_Decl (String_Type);
         Set_Type_Declarator (String_Type_Definition, String_Type);

         Add_Implicit_Operations (String_Type);
      end;

      -- bit_vector type.
      -- type bit_vector is array (natural range <>) of bit;
      begin
         Bit_Vector_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Array_Type_Definition);
         Set_Base_Type (Bit_Vector_Type_Definition,
                        Bit_Vector_Type_Definition);
         Set_Index_Subtype_List (Bit_Vector_Type_Definition, Create_Iir_List);
         Append_Element (Get_Index_Subtype_List (Bit_Vector_Type_Definition),
                         Natural_Subtype_Definition);
         Set_Element_Subtype (Bit_Vector_Type_Definition, Bit_Type_Definition);
         Set_Type_Staticness (Bit_Vector_Type_Definition, None);
         Set_Signal_Type_Flag (Bit_Vector_Type_Definition, True);
         Set_Has_Signal_Flag (Bit_Vector_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         Bit_Vector_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (Bit_Vector_Type, Name_Bit_Vector);
         Set_Type (Bit_Vector_Type, Bit_Vector_Type_Definition);
         Add_Decl (Bit_Vector_Type);
         Set_Type_Declarator (Bit_Vector_Type_Definition, Bit_Vector_Type);

         Add_Implicit_Operations (Bit_Vector_Type);
      end;

      -- time definition
      declare
         Time_Staticness : Iir_Staticness;
         Last_Unit : Iir_Unit_Declaration;
         use Iir_Chains.Unit_Chain_Handling;

         function Create_Std_Phys_Lit (Value : Iir_Int64;
                                       Unit : Iir_Unit_Declaration)
           return Iir_Physical_Int_Literal
         is
            Lit: Iir_Physical_Int_Literal;
         begin
            Lit := Create_Std_Iir (Iir_Kind_Physical_Int_Literal);
            Set_Value (Lit, Value);
            Set_Unit_Name (Lit, Unit);
            Set_Type (Lit, Time_Type_Definition);
            Set_Expr_Staticness (Lit, Time_Staticness);
            return Lit;
         end Create_Std_Phys_Lit;

         procedure Create_Unit (Unit : out Iir_Unit_Declaration;
                                Multiplier_Value : Iir_Int64;
                                Multiplier : in Iir_Unit_Declaration;
                                Name : Name_Id)
         is
            Lit: Iir_Physical_Int_Literal;
         begin
            Unit := Create_Std_Iir (Iir_Kind_Unit_Declaration);
            Set_Std_Identifier (Unit, Name);
            Set_Type (Unit, Time_Type_Definition);

            Lit := Create_Std_Phys_Lit (Multiplier_Value, Multiplier);
            Set_Physical_Literal (Unit, Lit);
            Lit := Create_Std_Phys_Lit
              (Multiplier_Value
               * Get_Value (Get_Physical_Unit_Value (Multiplier)),
               Get_Unit_Name (Get_Physical_Unit_Value (Multiplier)));
            Set_Physical_Unit_Value (Unit, Lit);

            Set_Expr_Staticness (Unit, Time_Staticness);
            Append (Last_Unit, Time_Type_Definition, Unit);
         end Create_Unit;

         Time_Fs_Unit: Iir_Unit_Declaration;
         Time_Ps_Unit: Iir_Unit_Declaration;
         Time_Ns_Unit: Iir_Unit_Declaration;
         Time_Us_Unit: Iir_Unit_Declaration;
         Time_Ms_Unit: Iir_Unit_Declaration;
         Time_Sec_Unit: Iir_Unit_Declaration;
         Time_Min_Unit: Iir_Unit_Declaration;
         Time_Hr_Unit: Iir_Unit_Declaration;
         Constraint : Iir_Range_Expression;
      begin
         if Vhdl_Std >= Vhdl_93c then
            Time_Staticness := Globally;
         else
            Time_Staticness := Locally;
         end if;

         Time_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Physical_Type_Definition);
         Set_Base_Type (Time_Type_Definition, Time_Type_Definition);
         Set_Type_Staticness (Time_Type_Definition, Locally);--Time_Staticness
         Set_Signal_Type_Flag (Time_Type_Definition, True);
         Set_Has_Signal_Flag (Time_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         Build_Init (Last_Unit);

         Time_Fs_Unit := Create_Std_Iir (Iir_Kind_Unit_Declaration);
         Set_Std_Identifier (Time_Fs_Unit, Name_Fs);
         Set_Type (Time_Fs_Unit, Time_Type_Definition);
         Set_Expr_Staticness (Time_Fs_Unit, Time_Staticness);
         Set_Physical_Unit_Value
           (Time_Fs_Unit, Create_Std_Phys_Lit (1, Time_Fs_Unit));
         Append (Last_Unit, Time_Type_Definition, Time_Fs_Unit);

         Create_Unit (Time_Ps_Unit, 1000, Time_Fs_Unit, Name_Ps);
         Create_Unit (Time_Ns_Unit, 1000, Time_Ps_Unit, Name_Ns);
         Create_Unit (Time_Us_Unit, 1000, Time_Ns_Unit, Name_Us);
         Create_Unit (Time_Ms_Unit, 1000, Time_Us_Unit, Name_Ms);
         Create_Unit (Time_Sec_Unit, 1000, Time_Ms_Unit, Name_Sec);
         Create_Unit (Time_Min_Unit, 60, Time_Sec_Unit, Name_Min);
         Create_Unit (Time_Hr_Unit, 60, Time_Min_Unit, Name_Hr);

         --  type is
         Time_Type := Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Time_Type, Name_Time);
         Set_Type (Time_Type, Time_Type_Definition);
         Set_Type_Declarator (Time_Type_Definition, Time_Type);
         Add_Decl (Time_Type);

         Add_Implicit_Operations (Time_Type);

         Time_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Physical_Subtype_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Phys_Lit (Low_Bound (Flags.Flag_Time_64),
                                 Time_Fs_Unit),
            Create_Std_Phys_Lit (High_Bound (Flags.Flag_Time_64),
                                 Time_Fs_Unit),
            Time_Type_Definition);
         Set_Range_Constraint (Time_Subtype_Definition, Constraint);
         Set_Base_Type (Time_Subtype_Definition, Time_Type_Definition);
         --Set_Type_Mark (Time_Subtype_Definition, Time_Type_Definition);
         Set_Type_Staticness (Time_Subtype_Definition, Time_Staticness);
         Set_Signal_Type_Flag (Time_Subtype_Definition, True);
         Set_Has_Signal_Flag (Time_Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  subtype
         Time_Subtype := Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Time_Subtype, Name_Time);
         Set_Type (Time_Subtype, Time_Subtype_Definition);
         Set_Type_Declarator (Time_Subtype_Definition, Time_Subtype);
         Add_Decl (Time_Subtype);
         Set_Subtype_Definition (Time_Type, Time_Subtype_Definition);

         -- The default time base.
         case Flags.Time_Resolution is
            when 'f' =>
               Time_Base := Time_Fs_Unit;
            when 'p' =>
               Time_Base := Time_Ps_Unit;
            when 'n' =>
               Time_Base := Time_Ns_Unit;
            when 'u' =>
               Time_Base := Time_Us_Unit;
            when 'm' =>
               Time_Base := Time_Ms_Unit;
            when 's' =>
               Time_Base := Time_Sec_Unit;
            when 'M' =>
               Time_Base := Time_Min_Unit;
            when 'h' =>
               Time_Base := Time_Hr_Unit;
            when others =>
               raise Internal_Error;
         end case;

         --  VHDL93
         --  subtype DELAY_LENGTH is TIME range 0 to TIME'HIGH
         if Vhdl_Std >= Vhdl_93c then
            Delay_Length_Subtype_Definition :=
              Create_Std_Iir (Iir_Kind_Physical_Subtype_Definition);
            Set_Type_Mark (Delay_Length_Subtype_Definition,
                           Time_Subtype_Definition);
            Constraint := Create_Std_Range_Expr
              (Create_Std_Phys_Lit (0, Time_Fs_Unit),
               Create_Std_Phys_Lit (High_Bound (Flags.Flag_Time_64),
                                    Time_Fs_Unit),
               Time_Type_Definition);
            Set_Range_Constraint (Delay_Length_Subtype_Definition, Constraint);
            Set_Base_Type
              (Delay_Length_Subtype_Definition, Time_Type_Definition);
            Set_Type_Staticness
              (Delay_Length_Subtype_Definition, Time_Staticness);
            Set_Signal_Type_Flag (Delay_Length_Subtype_Definition, True);
            Set_Has_Signal_Flag (Delay_Length_Subtype_Definition,
                                 not Flags.Flag_Whole_Analyze);

            Delay_Length_Subtype :=
              Create_Std_Decl (Iir_Kind_Subtype_Declaration);
            Set_Std_Identifier (Delay_Length_Subtype, Name_Delay_Length);
            Set_Type (Delay_Length_Subtype, Delay_Length_Subtype_Definition);
            Set_Type_Declarator
              (Delay_Length_Subtype_Definition, Delay_Length_Subtype);
            Add_Decl (Delay_Length_Subtype);
         else
            Delay_Length_Subtype_Definition := Null_Iir;
            Delay_Length_Subtype := Null_Iir;
         end if;
      end;

      --  VHDL87:
      --  function NOW return TIME
      --
      --  impure function NOW return DELAY_LENGTH.
      declare
         Function_Now : Iir_Implicit_Function_Declaration;
      begin
         Function_Now :=
           Create_Std_Decl (Iir_Kind_Implicit_Function_Declaration);
         Set_Std_Identifier (Function_Now, Std_Names.Name_Now);
         if Vhdl_Std = Vhdl_87 then
            Set_Return_Type (Function_Now, Time_Subtype_Definition);
         else
            Set_Return_Type (Function_Now, Delay_Length_Subtype_Definition);
         end if;
         if Vhdl_Std = Vhdl_02 then
            Set_Pure_Flag (Function_Now, True);
         else
            Set_Pure_Flag (Function_Now, False);
         end if;
         Set_Implicit_Definition (Function_Now, Iir_Predefined_Now_Function);
         Sem.Compute_Subprogram_Hash (Function_Now);
         Add_Decl (Function_Now);
      end;

      --  VHDL93:
      --  type file_open_kind is (read_mode, write_mode, append_mode);
      if Vhdl_Std >= Vhdl_93c then
         File_Open_Kind_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Base_Type (File_Open_Kind_Type_Definition,
                        File_Open_Kind_Type_Definition);
         Set_Enumeration_Literal_List
           (File_Open_Kind_Type_Definition, Create_Iir_List);

         File_Open_Kind_Read_Mode := Create_Std_Literal
           (Name_Read_Mode, File_Open_Kind_Type_Definition);
         File_Open_Kind_Write_Mode := Create_Std_Literal
           (Name_Write_Mode, File_Open_Kind_Type_Definition);
         File_Open_Kind_Append_Mode := Create_Std_Literal
           (Name_Append_Mode, File_Open_Kind_Type_Definition);
         Set_Type_Staticness (File_Open_Kind_Type_Definition, Locally);
         Set_Signal_Type_Flag (File_Open_Kind_Type_Definition, True);
         Set_Has_Signal_Flag (File_Open_Kind_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  type file_open_kind is
         File_Open_Kind_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (File_Open_Kind_Type, Name_File_Open_Kind);
         Set_Type (File_Open_Kind_Type, File_Open_Kind_Type_Definition);
         Add_Decl (File_Open_Kind_Type);
         Set_Type_Declarator (File_Open_Kind_Type_Definition,
                              File_Open_Kind_Type);
         Iirs_Utils.Create_Range_Constraint_For_Enumeration_Type
           (File_Open_Kind_Type_Definition);
         Add_Implicit_Operations (File_Open_Kind_Type);
      else
         File_Open_Kind_Type := Null_Iir;
         File_Open_Kind_Type_Definition := Null_Iir;
         File_Open_Kind_Read_Mode := Null_Iir;
         File_Open_Kind_Write_Mode := Null_Iir;
         File_Open_Kind_Append_Mode := Null_Iir;
      end if;

      --  VHDL93:
      --  type file_open_status is
      --      (open_ok, status_error, name_error, mode_error);
      if Vhdl_Std >= Vhdl_93c then
         File_Open_Status_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Base_Type (File_Open_Status_Type_Definition,
                        File_Open_Status_Type_Definition);
         Set_Enumeration_Literal_List
           (File_Open_Status_Type_Definition, Create_Iir_List);

         File_Open_Status_Open_Ok := Create_Std_Literal
           (Name_Open_Ok, File_Open_Status_Type_Definition);
         File_Open_Status_Status_Error := Create_Std_Literal
           (Name_Status_Error, File_Open_Status_Type_Definition);
         File_Open_Status_Name_Error := Create_Std_Literal
           (Name_Name_Error, File_Open_Status_Type_Definition);
         File_Open_Status_Mode_Error := Create_Std_Literal
           (Name_Mode_Error, File_Open_Status_Type_Definition);
         Set_Type_Staticness (File_Open_Status_Type_Definition, Locally);
         Set_Signal_Type_Flag (File_Open_Status_Type_Definition, True);
         Set_Has_Signal_Flag (File_Open_Status_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  type file_open_kind is
         File_Open_Status_Type := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (File_Open_Status_Type, Name_File_Open_Status);
         Set_Type (File_Open_Status_Type, File_Open_Status_Type_Definition);
         Add_Decl (File_Open_Status_Type);
         Set_Type_Declarator (File_Open_Status_Type_Definition,
                              File_Open_Status_Type);
         Iirs_Utils.Create_Range_Constraint_For_Enumeration_Type
           (File_Open_Status_Type_Definition);
         Add_Implicit_Operations (File_Open_Status_Type);
      else
         File_Open_Status_Type := Null_Iir;
         File_Open_Status_Type_Definition := Null_Iir;
         File_Open_Status_Open_Ok := Null_Iir;
         File_Open_Status_Status_Error := Null_Iir;
         File_Open_Status_Name_Error := Null_Iir;
         File_Open_Status_Mode_Error := Null_Iir;
      end if;

      --  VHDL93:
      --  attribute FOREIGN: string;
      if Vhdl_Std >= Vhdl_93c then
         Foreign_Attribute := Create_Std_Decl (Iir_Kind_Attribute_Declaration);
         Set_Std_Identifier (Foreign_Attribute, Name_Foreign);
         Set_Type (Foreign_Attribute, String_Type_Definition);
         Add_Decl (Foreign_Attribute);
      else
         Foreign_Attribute := Null_Iir;
      end if;
   end Create_Std_Standard_Package;
end Std_Package;
