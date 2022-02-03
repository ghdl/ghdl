--  std.standard package declarations.
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

with Files_Map;
with Name_Table;
with Str_Table;
with Std_Names; use Std_Names;
with Flags; use Flags;
with Vhdl.Utils;
with Vhdl.Sem_Utils;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;

package body Vhdl.Std_Package is
   type Bound_Array is array (Boolean) of Int64;
   Low_Bound : constant Bound_Array := (False => -(2 ** 31),
                                        True => -(2 ** 63));
   High_Bound : constant Bound_Array := (False => (2 ** 31) - 1,
                                         True => (2 ** 63) - 1);

   Std_Filename : Name_Id := Null_Identifier;

   --  Could be public.
   Time_Fs_Unit: Iir_Unit_Declaration;
   Time_Ps_Unit: Iir_Unit_Declaration;
   Time_Ns_Unit: Iir_Unit_Declaration;
   Time_Us_Unit: Iir_Unit_Declaration;
   Time_Ms_Unit: Iir_Unit_Declaration;
   Time_Sec_Unit: Iir_Unit_Declaration;
   Time_Min_Unit: Iir_Unit_Declaration;
   Time_Hr_Unit: Iir_Unit_Declaration;

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

   function Create_Std_Type_Mark (Ref : Iir) return Iir
   is
      Res : Iir;
   begin
      Res := Utils.Build_Simple_Name (Ref, Std_Location);
      Set_Type (Res, Get_Type (Ref));
      return Res;
   end Create_Std_Type_Mark;

   procedure Create_First_Nodes
   is
      procedure Create_Known_Iir (Kind : Iir_Kind; Val : Iir) is
      begin
         if Create_Std_Iir (Kind) /= Val then
            raise Internal_Error;
         end if;
      end Create_Known_Iir;
   begin
      Std_Filename := Name_Table.Get_Identifier ("*std_standard*");
      Std_Source_File := Files_Map.Create_Virtual_Source_File (Std_Filename);
      Std_Location := Files_Map.File_To_Location (Std_Source_File);

      if Create_Iir_Error /= Error_Mark then
         raise Internal_Error;
      end if;
      Set_Location (Error_Mark, Std_Location);
      Set_Signal_Type_Flag (Error_Mark, True);

      Create_Known_Iir (Iir_Kind_Integer_Type_Definition,
                        Universal_Integer_Type_Definition);
      Create_Known_Iir (Iir_Kind_Floating_Type_Definition,
                        Universal_Real_Type_Definition);

      Create_Known_Iir (Iir_Kind_Integer_Type_Definition,
                        Convertible_Integer_Type_Definition);
      Create_Known_Iir (Iir_Kind_Floating_Type_Definition,
                        Convertible_Real_Type_Definition);

      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Any_Type);
      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Any_Aggregate_Type);
      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Any_String_Type);
      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Any_Access_Type);
      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Any_Integer_Type);
      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Psl_Bit_Type);
      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Psl_Bitvector_Type);
      Create_Known_Iir (Iir_Kind_Wildcard_Type_Definition,
                        Wildcard_Psl_Boolean_Type);
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

      function Create_Std_Integer (Val : Int64; Lit_Type : Iir)
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

      function Create_Std_Fp (Val : Fp64; Lit_Type : Iir)
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
         Set_Left_Limit_Expr (Res, Left);
         Set_Direction (Res, Dir_To);
         Set_Right_Limit (Res, Right);
         Set_Right_Limit_Expr (Res, Right);
         Set_Expr_Staticness (Res, Locally);
         Set_Type (Res, Rtype);
         return Res;
      end Create_Std_Range_Expr;

      function Create_Std_Literal (Name : Name_Id;
                                   Pos : Natural;
                                   Sub_Type : Iir_Enumeration_Type_Definition)
                                  return Iir_Enumeration_Literal
      is
         List : constant Iir_Flist := Get_Enumeration_Literal_List (Sub_Type);
         Res : Iir_Enumeration_Literal;
      begin
         Res := Create_Std_Decl (Iir_Kind_Enumeration_Literal);
         Set_Std_Identifier (Res, Name);
         Set_Type (Res, Sub_Type);
         Set_Expr_Staticness (Res, Locally);
         Set_Name_Staticness (Res, Locally);
         Set_Enum_Pos (Res, Iir_Int32 (Pos));
         Vhdl.Sem_Utils.Compute_Subprogram_Hash (Res);
         Set_Nth_Element (List, Pos, Res);
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
         Vhdl.Sem_Utils.Create_Implicit_Operations (Decl, True);

         --  Update Last_Decl
         loop
            Nxt := Get_Chain (Last_Decl);
            exit when Nxt = Null_Iir;
            Last_Decl := Nxt;
         end loop;
      end Add_Implicit_Operations;

      --  Find implicit declaration of "**" for type declaration TYPE_DECL
      --  and append it at the current end of std_package.
      procedure Relocate_Exp_At_End (Type_Decl : Iir)
      is
         Prev_El, El : Iir;
      begin
         pragma Assert
           (Get_Kind (Type_Decl) = Iir_Kind_Anonymous_Type_Declaration);
         El := Type_Decl;
         loop
            Prev_El := El;
            El := Get_Chain (El);
            pragma Assert (Get_Kind (El) = Iir_Kind_Function_Declaration);
            exit when
              Get_Implicit_Definition (El) = Iir_Predefined_Integer_Exp;
            exit when
              Get_Implicit_Definition (El) = Iir_Predefined_Floating_Exp;
         end loop;

         --  EL must not be the last element, otherwise Add_Decl will break
         --  the chain.
         pragma Assert (Is_Valid (Get_Chain (El)));

         --  Remove from the chain.
         Set_Chain (Prev_El, Get_Chain (El));
         Set_Chain (El, Null_Iir);

         --  Append.
         Add_Decl (El);
      end Relocate_Exp_At_End;

      procedure Create_Std_Type (Decl : out Iir; Def : Iir; Name : Name_Id) is
      begin
         Decl := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Std_Identifier (Decl, Name);
         Set_Type_Definition (Decl, Def);
         Add_Decl (Decl);
         Set_Type_Declarator (Def, Decl);
      end Create_Std_Type;

      procedure Create_Integer_Type (Type_Definition : Iir;
                                     Type_Decl : out Iir;
                                     Type_Name : Name_Id)
      is
      begin
         --Integer_Type_Definition :=
         --  Create_Std_Iir (Iir_Kind_Integer_Type_Definition);
         Set_Type_Staticness (Type_Definition, Locally);
         Set_Signal_Type_Flag (Type_Definition, True);
         Set_Has_Signal_Flag (Type_Definition, not Flags.Flag_Whole_Analyze);

         Type_Decl := Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Type_Decl, Type_Name);
         Set_Type_Definition (Type_Decl, Type_Definition);
         Set_Type_Declarator (Type_Definition, Type_Decl);
      end Create_Integer_Type;

      procedure Create_Integer_Subtype (Type_Definition : Iir;
                                        Type_Decl : Iir;
                                        Subtype_Definition : out Iir;
                                        Subtype_Decl : out Iir;
                                        Is_64 : Boolean)
      is
         Constraint : Iir;
      begin
         Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Subtype_Definition);
         Set_Parent_Type (Subtype_Definition, Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Integer (Low_Bound (Is_64),
                                Universal_Integer_Type_Definition),
            Create_Std_Integer (High_Bound (Is_64),
                                Universal_Integer_Type_Definition),
            Universal_Integer_Type_Definition);
         Set_Range_Constraint (Subtype_Definition, Constraint);
         Set_Type_Staticness (Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Subtype_Definition, True);
         Set_Has_Signal_Flag (Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  subtype is
         Subtype_Decl := Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Subtype_Decl, Get_Identifier (Type_Decl));
         Set_Type (Subtype_Decl, Subtype_Definition);
         Set_Subtype_Indication (Subtype_Decl, Subtype_Definition);
         Set_Type_Declarator (Subtype_Definition, Subtype_Decl);
         Set_Subtype_Definition (Type_Decl, Subtype_Definition);
      end Create_Integer_Subtype;

      --  Create an array of EL_TYPE, indexed by Natural.
      procedure Create_Array_Type
        (Def : out Iir; Decl : out Iir; El_Decl : Iir; Name : Name_Id)
      is
         Index_List : Iir_Flist;
         Index : Iir;
         Element : Iir;
      begin
         Element := Create_Std_Type_Mark (El_Decl);
         Index := Create_Std_Type_Mark (Natural_Subtype_Declaration);

         Def := Create_Std_Iir (Iir_Kind_Array_Type_Definition);

         Index_List := Create_Iir_Flist (1);
         Set_Index_Subtype_Definition_List (Def, Index_List);
         Set_Index_Subtype_List (Def, Index_List);
         Set_Nth_Element (Index_List, 0, Index);

         Set_Element_Subtype_Indication (Def, Element);
         Set_Element_Subtype (Def, Get_Type (El_Decl));
         Set_Type_Staticness (Def, None);
         Set_Signal_Type_Flag (Def, True);
         Set_Has_Signal_Flag (Def, not Flags.Flag_Whole_Analyze);

         Create_Std_Type (Decl, Def, Name);

         Add_Implicit_Operations (Decl);
      end Create_Array_Type;

      --  Create:
      --  function TO_STRING (VALUE: inter_type) return STRING;
      procedure Create_To_String (Inter_Type : Iir;
                                  Imp : Iir_Predefined_Functions;
                                  Name : Name_Id := Std_Names.Name_To_String;
                                  Inter2_Id : Name_Id := Null_Identifier;
                                  Inter2_Type : Iir := Null_Iir)
      is
         Decl : Iir_Function_Declaration;
         Inter : Iir_Interface_Constant_Declaration;
         Inter2 : Iir_Interface_Constant_Declaration;
      begin
         Decl := Create_Std_Decl (Iir_Kind_Function_Declaration);
         Set_Std_Identifier (Decl, Name);
         Set_Return_Type (Decl, String_Type_Definition);
         Set_Pure_Flag (Decl, True);
         Set_Implicit_Definition (Decl, Imp);

         Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
         Set_Identifier (Inter, Std_Names.Name_Value);
         Set_Type (Inter, Inter_Type);
         Set_Mode (Inter, Iir_In_Mode);
         Set_Visible_Flag (Inter, True);
         Set_Interface_Declaration_Chain (Decl, Inter);

         if Inter2_Id /= Null_Identifier then
            Inter2 := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
            Set_Identifier (Inter2, Inter2_Id);
            Set_Type (Inter2, Inter2_Type);
            Set_Mode (Inter2, Iir_In_Mode);
            Set_Visible_Flag (Inter2, True);
            Set_Chain (Inter, Inter2);
         end if;

         Vhdl.Sem_Utils.Compute_Subprogram_Hash (Decl);
         Add_Decl (Decl);
      end Create_To_String;

      --  Create:
      --  function NAME (signal S : I inter_type) return BOOLEAN;
      procedure Create_Edge_Function
        (Name : Name_Id; Func : Iir_Predefined_Functions; Inter_Type : Iir)
      is
         Decl : Iir_Function_Declaration;
         Inter : Iir_Interface_Constant_Declaration;
      begin
         Decl := Create_Std_Decl (Iir_Kind_Function_Declaration);
         Set_Std_Identifier (Decl, Name);
         Set_Return_Type (Decl, Boolean_Type_Definition);
         Set_Pure_Flag (Decl, True);
         Set_Implicit_Definition (Decl, Func);

         Inter := Create_Iir (Iir_Kind_Interface_Signal_Declaration);
         Set_Identifier (Inter, Std_Names.Name_S);
         Set_Type (Inter, Inter_Type);
         Set_Mode (Inter, Iir_In_Mode);
         Set_Visible_Flag (Inter, True);
         Set_Interface_Declaration_Chain (Decl, Inter);

         Vhdl.Sem_Utils.Compute_Subprogram_Hash (Decl);
         Add_Decl (Decl);
      end Create_Edge_Function;

      procedure Create_Wildcard_Type (Def : Iir; Name : String)
      is
         Decl : Iir;
      begin
         Decl := Create_Std_Decl (Iir_Kind_Type_Declaration);
         Set_Identifier (Decl, Name_Table.Get_Identifier (Name));
         Set_Type_Staticness (Def, None);
         Set_Type_Definition (Decl, Def);
         Set_Type_Declarator (Def, Decl);

         Set_Chain (Decl, Wildcard_Type_Declaration_Chain);
         Wildcard_Type_Declaration_Chain := Decl;
      end Create_Wildcard_Type;

      function Is64 (B : Boolean) return Scalar_Size is
      begin
         if B then
            return Scalar_64;
         else
            return Scalar_32;
         end if;
      end Is64;
   begin
      --  Create design file.
      Std_Standard_File := Create_Std_Iir (Iir_Kind_Design_File);
      Set_Parent (Std_Standard_File, Parent);
      Set_Design_File_Filename (Std_Standard_File, Std_Filename);

      declare
         Std_Time_Stamp : constant Time_Stamp_String :=
           "20020601000000.000";
         Id : Time_Stamp_Id;
      begin
         Id := Time_Stamp_Id (Str_Table.Create_String8);
         for I in Time_Stamp_String'Range loop
            Str_Table.Append_String8_Char (Std_Time_Stamp (I));
         end loop;
         Set_Analysis_Time_Stamp (Std_Standard_File, Id);
      end;

      --  Create design unit.
      Std_Standard_Unit := Create_Std_Iir (Iir_Kind_Design_Unit);
      Set_Identifier (Std_Standard_Unit, Name_Standard);
      Set_First_Design_Unit (Std_Standard_File, Std_Standard_Unit);
      Set_Last_Design_Unit (Std_Standard_File, Std_Standard_Unit);
      Set_Design_File (Std_Standard_Unit, Std_Standard_File);
      Set_Date_State (Std_Standard_Unit, Date_Analyze);
      Set_Dependence_List (Std_Standard_Unit, Create_Iir_List);

      Set_Date (Std_Standard_Unit, Date_Valid'First);

      -- Adding "package STANDARD is"
      Standard_Package := Create_Std_Iir (Iir_Kind_Package_Declaration);
      Set_Std_Identifier (Standard_Package, Name_Standard);
      Set_Need_Body (Standard_Package, False);

      Set_Library_Unit (Std_Standard_Unit, Standard_Package);
      Set_Design_Unit (Standard_Package, Std_Standard_Unit);

      -- boolean
      begin
         -- (false, true)
         Boolean_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Enumeration_Literal_List
           (Boolean_Type_Definition, Create_Iir_Flist (2));
         Boolean_False := Create_Std_Literal
           (Name_False, 0, Boolean_Type_Definition);
         Boolean_True := Create_Std_Literal
           (Name_True, 1, Boolean_Type_Definition);
         Set_Type_Staticness (Boolean_Type_Definition, Locally);
         Set_Signal_Type_Flag (Boolean_Type_Definition, True);
         Set_Has_Signal_Flag (Boolean_Type_Definition,
                              not Flags.Flag_Whole_Analyze);
         Set_Scalar_Size (Boolean_Type_Definition, Scalar_8);

         -- type boolean is
         Create_Std_Type (Boolean_Type_Declaration, Boolean_Type_Definition,
                          Name_Boolean);

         Utils.Create_Range_Constraint_For_Enumeration_Type
           (Boolean_Type_Definition);
         Add_Implicit_Operations (Boolean_Type_Declaration);
      end;

      if Vhdl_Std >= Vhdl_08 then
         --  Rising_Edge and Falling_Edge
         Create_Edge_Function
           (Std_Names.Name_Rising_Edge, Iir_Predefined_Boolean_Rising_Edge,
            Boolean_Type_Definition);
         Create_Edge_Function
           (Std_Names.Name_Falling_Edge, Iir_Predefined_Boolean_Falling_Edge,
            Boolean_Type_Definition);
      end if;

      -- bit.
      begin
         -- ('0', '1')
         Bit_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Enumeration_Literal_List
           (Bit_Type_Definition, Create_Iir_Flist (2));
         Set_Is_Character_Type (Bit_Type_Definition, True);
         Bit_0 := Create_Std_Literal
           (Get_Std_Character ('0'), 0, Bit_Type_Definition);
         Bit_1 := Create_Std_Literal
           (Get_Std_Character ('1'), 1, Bit_Type_Definition);
         Set_Type_Staticness (Bit_Type_Definition, Locally);
         Set_Signal_Type_Flag (Bit_Type_Definition, True);
         Set_Has_Signal_Flag (Bit_Type_Definition,
                              not Flags.Flag_Whole_Analyze);
         Set_Only_Characters_Flag (Bit_Type_Definition, True);
         Set_Scalar_Size (Bit_Type_Definition, Scalar_8);

         -- type bit is
         Create_Std_Type (Bit_Type_Declaration, Bit_Type_Definition, Name_Bit);

         Utils.Create_Range_Constraint_For_Enumeration_Type
           (Bit_Type_Definition);
         Add_Implicit_Operations (Bit_Type_Declaration);
      end;

      if Vhdl_Std >= Vhdl_08 then
         --  Rising_Edge and Falling_Edge
         Create_Edge_Function
           (Std_Names.Name_Rising_Edge, Iir_Predefined_Bit_Rising_Edge,
            Bit_Type_Definition);
         Create_Edge_Function
           (Std_Names.Name_Falling_Edge, Iir_Predefined_Bit_Falling_Edge,
            Bit_Type_Definition);
      end if;

      -- characters.
      declare
         El: Iir;
         pragma Unreferenced (El);
         Len : Natural;
      begin
         Character_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Is_Character_Type (Character_Type_Definition, True);
         if Vhdl_Std = Vhdl_87 then
            Len := 128;
         else
            Len := 256;
         end if;
         Set_Enumeration_Literal_List
           (Character_Type_Definition, Create_Iir_Flist (Len));
         Set_Scalar_Size (Character_Type_Definition, Scalar_8);

         for I in Name_Nul .. Name_Usp loop
            El := Create_Std_Literal
              (I, Natural (I - Name_Nul), Character_Type_Definition);
         end loop;
         for I in Character'(' ') .. Character'('~') loop
            El := Create_Std_Literal
              (Get_Std_Character (I), Character'Pos (I),
               Character_Type_Definition);
         end loop;
         El := Create_Std_Literal (Name_Del, 127, Character_Type_Definition);
         if Vhdl_Std /= Vhdl_87 then
            for I in Name_C128 .. Name_C159 loop
               El := Create_Std_Literal
                 (I, 128 + Natural (I - Name_C128), Character_Type_Definition);
            end loop;
            for I in Character'Val (160) .. Character'Val (255) loop
               El := Create_Std_Literal
                 (Get_Std_Character (I), Character'Pos (I),
                  Character_Type_Definition);
            end loop;
         end if;
         Set_Type_Staticness (Character_Type_Definition, Locally);
         Set_Signal_Type_Flag (Character_Type_Definition, True);
         Set_Has_Signal_Flag (Character_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         -- type character is
         Create_Std_Type
           (Character_Type_Declaration, Character_Type_Definition,
            Name_Character);

         Utils.Create_Range_Constraint_For_Enumeration_Type
           (Character_Type_Definition);
         Add_Implicit_Operations (Character_Type_Declaration);
      end;

      -- severity level.
      begin
         -- (note, warning, error, failure)
         Severity_Level_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Enumeration_Literal_List
           (Severity_Level_Type_Definition, Create_Iir_Flist (4));
         Set_Scalar_Size (Severity_Level_Type_Definition, Scalar_8);

         Severity_Level_Note := Create_Std_Literal
           (Name_Note, 0, Severity_Level_Type_Definition);
         Severity_Level_Warning := Create_Std_Literal
           (Name_Warning, 1, Severity_Level_Type_Definition);
         Severity_Level_Error := Create_Std_Literal
           (Name_Error, 2, Severity_Level_Type_Definition);
         Severity_Level_Failure := Create_Std_Literal
           (Name_Failure, 3, Severity_Level_Type_Definition);
         Set_Type_Staticness (Severity_Level_Type_Definition, Locally);
         Set_Signal_Type_Flag (Severity_Level_Type_Definition, True);
         Set_Has_Signal_Flag (Severity_Level_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         -- type severity_level is
         Create_Std_Type
           (Severity_Level_Type_Declaration, Severity_Level_Type_Definition,
            Name_Severity_Level);

         Utils.Create_Range_Constraint_For_Enumeration_Type
           (Severity_Level_Type_Definition);
         Add_Implicit_Operations (Severity_Level_Type_Declaration);
      end;

      -- universal integer
      begin
         Create_Integer_Type (Universal_Integer_Type_Definition,
                              Universal_Integer_Type_Declaration,
                              Name_Universal_Integer);
         Add_Decl (Universal_Integer_Type_Declaration);
         Set_Scalar_Size (Universal_Integer_Type_Definition,
                          Is64 (Flags.Flag_Time_64 or Flags.Flag_Integer_64));

         Create_Integer_Subtype (Universal_Integer_Type_Definition,
                                 Universal_Integer_Type_Declaration,
                                 Universal_Integer_Subtype_Definition,
                                 Universal_Integer_Subtype_Declaration,
                                 Flags.Flag_Time_64 or Flags.Flag_Integer_64);

         Add_Decl (Universal_Integer_Subtype_Declaration);
         Set_Subtype_Definition (Universal_Integer_Type_Declaration,
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
         Set_Type_Staticness (Universal_Real_Type_Definition, Locally);
         Set_Signal_Type_Flag (Universal_Real_Type_Definition, True);
         Set_Has_Signal_Flag (Universal_Real_Type_Definition, False);

         --  type universal_real is ...
         Universal_Real_Type_Declaration :=
           Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Universal_Real_Type_Declaration, Name_Universal_Real);
         Set_Type_Definition (Universal_Real_Type_Declaration,
                              Universal_Real_Type_Definition);
         Set_Type_Declarator (Universal_Real_Type_Definition,
                              Universal_Real_Type_Declaration);
         Add_Decl (Universal_Real_Type_Declaration);

         Universal_Real_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Floating_Subtype_Definition);
         Set_Parent_Type (Universal_Real_Subtype_Definition,
                          Universal_Real_Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Fp (Fp64'First, Universal_Real_Type_Definition),
            Create_Std_Fp (Fp64'Last, Universal_Real_Type_Definition),
            Universal_Real_Type_Definition);
         Set_Range_Constraint (Universal_Real_Subtype_Definition, Constraint);
         Set_Type_Staticness (Universal_Real_Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Universal_Real_Subtype_Definition, True);
         Set_Has_Signal_Flag (Universal_Real_Subtype_Definition, False);

         --  subtype universal_real is ...
         Universal_Real_Subtype_Declaration :=
           Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Identifier (Universal_Real_Subtype_Declaration,
                         Name_Universal_Real);
         Set_Type (Universal_Real_Subtype_Declaration,
                   Universal_Real_Subtype_Definition);
         Set_Subtype_Indication (Universal_Real_Subtype_Declaration,
                                 Universal_Real_Subtype_Definition);
         Set_Type_Declarator (Universal_Real_Subtype_Definition,
                              Universal_Real_Subtype_Declaration);
         Set_Subtype_Definition (Universal_Real_Type_Declaration,
                                 Universal_Real_Subtype_Definition);

         Add_Decl (Universal_Real_Subtype_Declaration);

         --  Do not create implicit operations yet, since "**" needs integer
         --  type.
      end;

      -- Convertible type.
      begin
         Create_Integer_Type (Convertible_Integer_Type_Definition,
                              Convertible_Integer_Type_Declaration,
                              Name_Convertible_Integer);
         Set_Scalar_Size (Convertible_Integer_Type_Definition,
                          Is64 (Flags.Flag_Time_64 or Flags.Flag_Integer_64));
         Create_Integer_Subtype (Convertible_Integer_Type_Definition,
                                 Convertible_Integer_Type_Declaration,
                                 Convertible_Integer_Subtype_Definition,
                                 Convertible_Integer_Subtype_Declaration,
                                 Flags.Flag_Time_64 or Flags.Flag_Integer_64);

         --  Not added in std.standard.
      end;

      begin
         Set_Type_Staticness (Convertible_Real_Type_Definition, Locally);
         Set_Signal_Type_Flag (Convertible_Real_Type_Definition, True);
         Set_Has_Signal_Flag (Convertible_Real_Type_Definition, False);

         Convertible_Real_Type_Declaration :=
           Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Convertible_Real_Type_Declaration,
                         Name_Convertible_Real);
         Set_Type_Definition (Convertible_Real_Type_Declaration,
                              Convertible_Real_Type_Definition);
         Set_Type_Declarator (Convertible_Real_Type_Definition,
                              Convertible_Real_Type_Declaration);
      end;

      -- integer type.
      begin
         Integer_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Type_Definition);
         Create_Integer_Type (Integer_Type_Definition,
                              Integer_Type_Declaration,
                              Name_Integer);
         Set_Scalar_Size (Integer_Type_Definition,
                          Is64 (Flags.Flag_Integer_64));
         Add_Decl (Integer_Type_Declaration);

         --  Now that Integer is declared, create operations for universal
         --  types.  They will be inserted just after the type declaration,
         --  but cannot be done before as "**" relies on Integer.
         Add_Implicit_Operations (Universal_Integer_Type_Declaration);
         Add_Implicit_Operations (Universal_Real_Type_Declaration);

         --  Don't define "**" for universal types before the declaration of
         --  Integer, so move them.
         Relocate_Exp_At_End (Universal_Integer_Type_Declaration);
         Relocate_Exp_At_End (Universal_Real_Type_Declaration);

         Add_Implicit_Operations (Integer_Type_Declaration);

         Create_Integer_Subtype (Integer_Type_Definition,
                                 Integer_Type_Declaration,
                                 Integer_Subtype_Definition,
                                 Integer_Subtype_Declaration,
                                 Flags.Flag_Integer_64);
         Add_Decl (Integer_Subtype_Declaration);
      end;

      -- Real type.
      declare
         Constraint : Iir_Range_Expression;
      begin
         Real_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Floating_Type_Definition);
         Set_Type_Staticness (Real_Type_Definition, Locally);
         Set_Signal_Type_Flag (Real_Type_Definition, True);
         Set_Has_Signal_Flag (Real_Type_Definition,
                              not Flags.Flag_Whole_Analyze);
         Set_Scalar_Size (Real_Type_Definition, Scalar_64);

         Real_Type_Declaration :=
           Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Real_Type_Declaration, Name_Real);
         Set_Type_Definition (Real_Type_Declaration, Real_Type_Definition);
         Set_Type_Declarator (Real_Type_Definition, Real_Type_Declaration);
         Add_Decl (Real_Type_Declaration);

         Add_Implicit_Operations (Real_Type_Declaration);

         Real_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Floating_Subtype_Definition);
         Set_Parent_Type (Real_Subtype_Definition, Real_Type_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Fp (Fp64'First, Universal_Real_Type_Definition),
            Create_Std_Fp (Fp64'Last, Universal_Real_Type_Definition),
             Universal_Real_Type_Definition);
         Set_Range_Constraint (Real_Subtype_Definition, Constraint);
         Set_Type_Staticness (Real_Subtype_Definition, Locally);
         Set_Signal_Type_Flag (Real_Subtype_Definition, True);
         Set_Has_Signal_Flag (Real_Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         Real_Subtype_Declaration :=
           Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Real_Subtype_Declaration, Name_Real);
         Set_Type (Real_Subtype_Declaration, Real_Subtype_Definition);
         Set_Subtype_Indication (Real_Subtype_Declaration,
                                 Real_Subtype_Definition);
         Set_Type_Declarator
           (Real_Subtype_Definition, Real_Subtype_Declaration);
         Add_Decl (Real_Subtype_Declaration);

         Set_Subtype_Definition
           (Real_Type_Declaration, Real_Subtype_Definition);
      end;

      -- time definition
      declare
         Time_Staticness : Iir_Staticness;
         First_Unit, Last_Unit : Iir_Unit_Declaration;

         function Create_Std_Phys_Lit (Value : Int64; Unit : Iir)
                                      return Iir_Physical_Int_Literal
         is
            Lit: Iir_Physical_Int_Literal;
            Unit_Name : Iir;
         begin
            Lit := Create_Std_Iir (Iir_Kind_Physical_Int_Literal);
            Set_Value (Lit, Value);
            pragma Assert (Get_Kind (Unit) = Iir_Kind_Unit_Declaration);

            Unit_Name := Create_Std_Iir (Iir_Kind_Simple_Name);
            Set_Identifier (Unit_Name, Get_Identifier (Unit));
            Set_Named_Entity (Unit_Name, Unit);
            Set_Unit_Name (Lit, Unit_Name);

            Set_Type (Lit, Time_Type_Definition);
            Set_Expr_Staticness (Lit, Time_Staticness);
            return Lit;
         end Create_Std_Phys_Lit;

         procedure Create_Unit (Unit : out Iir_Unit_Declaration;
                                Multiplier_Value : Int64;
                                Multiplier : in Iir_Unit_Declaration;
                                Name : Name_Id)
         is
            Lit, Lit1 : Iir_Physical_Int_Literal;
         begin
            --  The unit.
            Unit := Create_Std_Decl (Iir_Kind_Unit_Declaration);
            Set_Std_Identifier (Unit, Name);
            Set_Type (Unit, Time_Type_Definition);

            --  The physical literal.
            Lit1 := Create_Std_Phys_Lit (Multiplier_Value, Multiplier);

            --  The computed value of the physical literal.
            Lit := Create_Std_Iir (Iir_Kind_Integer_Literal);
            Set_Value (Lit,
                       Multiplier_Value
                         * Get_Value (Get_Physical_Literal (Multiplier)));
            Set_Literal_Origin (Lit, Lit1);
            Set_Physical_Literal (Unit, Lit);

            Set_Expr_Staticness (Unit, Time_Staticness);
            Set_Name_Staticness (Unit, Locally);
            Chain_Append (First_Unit, Last_Unit, Unit);
         end Create_Unit;

         Constraint : Iir_Range_Expression;
      begin
         if Vhdl_Std >= Vhdl_93 then
            Time_Staticness := Globally;
         else
            Time_Staticness := Locally;
         end if;

         Time_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Physical_Type_Definition);
         Set_Type_Staticness (Time_Type_Definition, Locally);--Time_Staticness
         Set_Signal_Type_Flag (Time_Type_Definition, True);
         Set_Has_Signal_Flag (Time_Type_Definition,
                              not Flags.Flag_Whole_Analyze);
         Set_End_Has_Reserved_Id (Time_Type_Definition, True);
         Set_Scalar_Size (Time_Type_Definition,
                          Is64 (Flags.Flag_Time_64));

         Chain_Init (First_Unit, Last_Unit);

         Time_Fs_Unit := Create_Std_Decl (Iir_Kind_Unit_Declaration);
         Set_Std_Identifier (Time_Fs_Unit, Name_Fs);
         Set_Type (Time_Fs_Unit, Time_Type_Definition);
         Set_Expr_Staticness (Time_Fs_Unit, Time_Staticness);
         Set_Name_Staticness (Time_Fs_Unit, Locally);
         declare
            Lit : Iir;
         begin
            Lit := Create_Std_Iir (Iir_Kind_Integer_Literal);
            Set_Value (Lit, 1);
            Set_Physical_Literal (Time_Fs_Unit, Lit);
            Set_Literal_Origin (Lit, Create_Std_Phys_Lit (1, Time_Fs_Unit));
         end;
         Chain_Append (First_Unit, Last_Unit, Time_Fs_Unit);

         Create_Unit (Time_Ps_Unit, 1000, Time_Fs_Unit, Name_Ps);
         Create_Unit (Time_Ns_Unit, 1000, Time_Ps_Unit, Name_Ns);
         Create_Unit (Time_Us_Unit, 1000, Time_Ns_Unit, Name_Us);
         Create_Unit (Time_Ms_Unit, 1000, Time_Us_Unit, Name_Ms);
         Create_Unit (Time_Sec_Unit, 1000, Time_Ms_Unit, Name_Sec);
         Create_Unit (Time_Min_Unit, 60, Time_Sec_Unit, Name_Min);
         Create_Unit (Time_Hr_Unit, 60, Time_Min_Unit, Name_Hr);

         --  type is
         Time_Type_Declaration :=
           Create_Std_Decl (Iir_Kind_Anonymous_Type_Declaration);
         Set_Identifier (Time_Type_Declaration, Name_Time);
         Set_Type_Definition (Time_Type_Declaration, Time_Type_Definition);
         Set_Type_Declarator (Time_Type_Definition, Time_Type_Declaration);
         Set_Unit_Chain (Time_Type_Definition, First_Unit);
         Add_Decl (Time_Type_Declaration);

         Add_Implicit_Operations (Time_Type_Declaration);

         Time_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Physical_Subtype_Definition);
         Constraint := Create_Std_Range_Expr
           (Create_Std_Phys_Lit (Low_Bound (Flags.Flag_Time_64),
                                 Time_Fs_Unit),
            Create_Std_Phys_Lit (High_Bound (Flags.Flag_Time_64),
                                 Time_Fs_Unit),
            Time_Type_Definition);
         Set_Range_Constraint (Time_Subtype_Definition, Constraint);
         Set_Parent_Type (Time_Subtype_Definition, Time_Type_Definition);
         --Set_Subtype_Type_Mark (Time_Subtype_Definition,
         --                       Time_Type_Definition);
         Set_Type_Staticness (Time_Subtype_Definition, Time_Staticness);
         Set_Signal_Type_Flag (Time_Subtype_Definition, True);
         Set_Has_Signal_Flag (Time_Subtype_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  subtype time is
         Time_Subtype_Declaration :=
           Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Time_Subtype_Declaration, Name_Time);
         Set_Type (Time_Subtype_Declaration, Time_Subtype_Definition);
         Set_Subtype_Indication (Time_Subtype_Declaration,
                                 Time_Subtype_Definition);
         Set_Type_Declarator (Time_Subtype_Definition,
                              Time_Subtype_Declaration);
         Add_Decl (Time_Subtype_Declaration);
         Set_Subtype_Definition
           (Time_Type_Declaration, Time_Subtype_Definition);

         --  VHDL93
         --  subtype DELAY_LENGTH is TIME range 0 to TIME'HIGH
         if Vhdl_Std >= Vhdl_93 then
            Delay_Length_Subtype_Definition :=
              Create_Std_Iir (Iir_Kind_Physical_Subtype_Definition);
            Set_Subtype_Type_Mark
              (Delay_Length_Subtype_Definition,
               Create_Std_Type_Mark (Time_Subtype_Declaration));
            Constraint := Create_Std_Range_Expr
              (Create_Std_Phys_Lit (0, Time_Fs_Unit),
               Create_Std_Phys_Lit (High_Bound (Flags.Flag_Time_64),
                                    Time_Fs_Unit),
               Time_Type_Definition);
            Set_Range_Constraint (Delay_Length_Subtype_Definition, Constraint);
            Set_Parent_Type
              (Delay_Length_Subtype_Definition, Time_Type_Definition);
            Set_Type_Staticness
              (Delay_Length_Subtype_Definition, Time_Staticness);
            Set_Signal_Type_Flag (Delay_Length_Subtype_Definition, True);
            Set_Has_Signal_Flag (Delay_Length_Subtype_Definition,
                                 not Flags.Flag_Whole_Analyze);

            --  subtype delay_length is ...
            Delay_Length_Subtype_Declaration :=
              Create_Std_Decl (Iir_Kind_Subtype_Declaration);
            Set_Std_Identifier (Delay_Length_Subtype_Declaration,
                                Name_Delay_Length);
            Set_Type (Delay_Length_Subtype_Declaration,
                      Delay_Length_Subtype_Definition);
            Set_Type_Declarator (Delay_Length_Subtype_Definition,
                                 Delay_Length_Subtype_Declaration);
            Set_Subtype_Indication (Delay_Length_Subtype_Declaration,
                                    Delay_Length_Subtype_Definition);
            Add_Decl (Delay_Length_Subtype_Declaration);
         else
            Delay_Length_Subtype_Definition := Null_Iir;
            Delay_Length_Subtype_Declaration := Null_Iir;
         end if;
      end;

      --  AMS-VHDL:
      --  type DOMAIN_TYPE is
      --    (QUIESCENT_DOMAIN, TIME_DOMAIN, FREQUENCY_DOMAIN);
      if AMS_Vhdl then
         Domain_Type_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Enumeration_Literal_List
           (Domain_Type_Type_Definition, Create_Iir_Flist (3));
         Set_Scalar_Size (Domain_Type_Type_Definition, Scalar_8);

         Domain_Type_Quiescent_Domain := Create_Std_Literal
           (Name_Quiescent_Domain, 0, Domain_Type_Type_Definition);
         Domain_Type_Time_Domain := Create_Std_Literal
           (Name_Time_Domain, 1, Domain_Type_Type_Definition);
         Domain_Type_Frequency_Domain := Create_Std_Literal
           (Name_Frequency_Domain, 2, Domain_Type_Type_Definition);
         Set_Type_Staticness (Domain_Type_Type_Definition, Locally);
         Set_Signal_Type_Flag (Domain_Type_Type_Definition, True);
         Set_Has_Signal_Flag (Domain_Type_Type_Definition, True);

         --  type domain_type is
         Create_Std_Type
           (Domain_Type_Type_Declaration, Domain_Type_Type_Definition,
            Name_Domain_Type);

         Utils.Create_Range_Constraint_For_Enumeration_Type
           (Domain_Type_Type_Definition);
         Add_Implicit_Operations (Domain_Type_Type_Declaration);

         --  signal DOMAIN : DOMAIN_TYPE := QUIESCENT_DOMAIN;
         declare
            Init : Iir;
         begin
            Domain_Signal := Create_Std_Decl (Iir_Kind_Signal_Declaration);
            Set_Std_Identifier (Domain_Signal, Std_Names.Name_Domain);
            Set_Type (Domain_Signal, Domain_Type_Type_Definition);
            Set_Subtype_Indication
              (Domain_Signal,
               Create_Std_Type_Mark (Domain_Type_Type_Declaration));
            Set_Expr_Staticness (Domain_Signal, None);
            Set_Name_Staticness (Domain_Signal, Locally);

            Init := Create_Std_Iir (Iir_Kind_Simple_Name);
            Set_Identifier (Init, Name_Quiescent_Domain);
            Set_Named_Entity (Init, Domain_Type_Quiescent_Domain);
            Set_Type (Init, Domain_Type_Type_Definition);
            Set_Expr_Staticness (Init, Locally);
            Set_Name_Staticness (Init, Locally);

            Set_Default_Value (Domain_Signal, Init);

            Add_Decl (Domain_Signal);
         end;
      else
         Domain_Type_Type_Declaration := Null_Iir;
         Domain_Type_Type_Definition := Null_Iir;
         Domain_Type_Quiescent_Domain := Null_Iir;
         Domain_Type_Time_Domain := Null_Iir;
         Domain_Type_Frequency_Domain := Null_Iir;
         Domain_Signal := Null_Iir;
      end if;

      --  VHDL87:
      --  function NOW return TIME
      --
      --  impure function NOW return DELAY_LENGTH.
      declare
         Function_Now : Iir_Function_Declaration;
         Pure : Boolean;
      begin
         Function_Now := Create_Std_Decl (Iir_Kind_Function_Declaration);
         Set_Std_Identifier (Function_Now, Std_Names.Name_Now);
         if Vhdl_Std = Vhdl_87 then
            Set_Return_Type (Function_Now, Time_Subtype_Definition);
         else
            Set_Return_Type (Function_Now, Delay_Length_Subtype_Definition);
         end if;
         case Vhdl_Std is
            when Vhdl_87
               | Vhdl_02 =>
               Pure := True;
            when Vhdl_93
               | Vhdl_00
               | Vhdl_08
               | Vhdl_19 =>
               Pure := False;
         end case;
         Set_Pure_Flag (Function_Now, Pure);
         Set_Implicit_Definition (Function_Now, Iir_Predefined_Now_Function);
         Vhdl.Sem_Utils.Compute_Subprogram_Hash (Function_Now);
         Add_Decl (Function_Now);
      end;

      if AMS_Vhdl then
         --  AMS-LRM17 16.3
         --  impure function NOW return REAL;
         declare
            Function_Now : Iir_Function_Declaration;
         begin
            Function_Now := Create_Std_Decl (Iir_Kind_Function_Declaration);
            Set_Std_Identifier (Function_Now, Std_Names.Name_Now);
            Set_Return_Type (Function_Now, Real_Subtype_Definition);
            Set_Pure_Flag (Function_Now, False);
            Set_Implicit_Definition
              (Function_Now, Iir_Predefined_Real_Now_Function);
            Vhdl.Sem_Utils.Compute_Subprogram_Hash (Function_Now);
            Add_Decl (Function_Now);
         end;

         --  AMS-LRM17 16.3
         --  impure function FREQUENCY return REAL;
         declare
            Function_Freq : Iir_Function_Declaration;
         begin
            Function_Freq := Create_Std_Decl (Iir_Kind_Function_Declaration);
            Set_Std_Identifier (Function_Freq, Std_Names.Name_Frequency);
            Set_Return_Type (Function_Freq, Real_Subtype_Definition);
            Set_Pure_Flag (Function_Freq, False);
            Set_Implicit_Definition
              (Function_Freq, Iir_Predefined_Frequency_Function);
            Vhdl.Sem_Utils.Compute_Subprogram_Hash (Function_Freq);
            Add_Decl (Function_Freq);
         end;
      end if;

      -- natural subtype
      declare
         Constraint : Iir_Range_Expression;
      begin
         Natural_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Subtype_Definition);
         Set_Parent_Type (Natural_Subtype_Definition, Integer_Type_Definition);
         Set_Subtype_Type_Mark
           (Natural_Subtype_Definition,
            Create_Std_Type_Mark (Integer_Subtype_Declaration));
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

         Natural_Subtype_Declaration :=
           Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Natural_Subtype_Declaration, Name_Natural);
         Set_Type (Natural_Subtype_Declaration, Natural_Subtype_Definition);
         Set_Subtype_Indication (Natural_Subtype_Declaration,
                                 Natural_Subtype_Definition);
         Add_Decl (Natural_Subtype_Declaration);
         Set_Type_Declarator (Natural_Subtype_Definition,
                              Natural_Subtype_Declaration);
      end;

      -- positive subtype
      declare
         Constraint : Iir_Range_Expression;
      begin
         Positive_Subtype_Definition :=
           Create_Std_Iir (Iir_Kind_Integer_Subtype_Definition);
         Set_Parent_Type (Positive_Subtype_Definition,
                          Integer_Type_Definition);
         Set_Subtype_Type_Mark
           (Positive_Subtype_Definition,
            Create_Std_Type_Mark (Integer_Subtype_Declaration));
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

         Positive_Subtype_Declaration :=
           Create_Std_Decl (Iir_Kind_Subtype_Declaration);
         Set_Std_Identifier (Positive_Subtype_Declaration, Name_Positive);
         Set_Type (Positive_Subtype_Declaration, Positive_Subtype_Definition);
         Set_Subtype_Indication (Positive_Subtype_Declaration,
                                 Positive_Subtype_Definition);
         Add_Decl (Positive_Subtype_Declaration);
         Set_Type_Declarator (Positive_Subtype_Definition,
                              Positive_Subtype_Declaration);
      end;

      -- string type.
      -- type string is array (positive range <>) of character;
      declare
         Element : Iir;
         Index_List : Iir_Flist;
      begin
         Element := Create_Std_Type_Mark (Character_Type_Declaration);

         String_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Array_Type_Definition);
         Index_List := Create_Iir_Flist (1);
         Set_Nth_Element (Index_List, 0,
                         Create_Std_Type_Mark (Positive_Subtype_Declaration));
         Set_Index_Subtype_Definition_List (String_Type_Definition,
                                            Index_List);
         Set_Index_Subtype_List (String_Type_Definition, Index_List);
         Set_Element_Subtype_Indication (String_Type_Definition, Element);
         Set_Element_Subtype (String_Type_Definition,
                              Character_Type_Definition);
         Set_Type_Staticness (String_Type_Definition, None);
         Set_Signal_Type_Flag (String_Type_Definition, True);
         Set_Has_Signal_Flag (String_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         Create_Std_Type
           (String_Type_Declaration, String_Type_Definition, Name_String);

         Add_Implicit_Operations (String_Type_Declaration);
      end;

      if Vhdl_Std >= Vhdl_08 then
         --  type Boolean_Vector is array (Natural range <>) of Boolean;
         Create_Array_Type
           (Boolean_Vector_Type_Definition, Boolean_Vector_Type_Declaration,
            Boolean_Type_Declaration, Name_Boolean_Vector);
      end if;

      -- bit_vector type.
      -- type bit_vector is array (natural range <>) of bit;
      Create_Array_Type
        (Bit_Vector_Type_Definition, Bit_Vector_Type_Declaration,
         Bit_Type_Declaration, Name_Bit_Vector);

      --  LRM08 5.3.2.4 Predefined operations on array types
      --  The following operations are implicitly declared in package
      --  STD.STANDARD immediately following the declaration of type
      --  BIT_VECTOR:
      if Vhdl_Std >= Vhdl_08 then
         Create_To_String (Bit_Vector_Type_Definition,
                           Iir_Predefined_Bit_Vector_To_Ostring,
                           Name_To_Ostring);
         Create_To_String (Bit_Vector_Type_Definition,
                           Iir_Predefined_Bit_Vector_To_Hstring,
                           Name_To_Hstring);
      end if;

      --  VHDL 2008
      --  Vector types
      if Vhdl_Std >= Vhdl_08 then
         -- type integer_vector is array (natural range <>) of Integer;
         Create_Array_Type
           (Integer_Vector_Type_Definition, Integer_Vector_Type_Declaration,
            Integer_Subtype_Declaration, Name_Integer_Vector);
      end if;

      if Vhdl_Std >= Vhdl_08 or else AMS_Vhdl then
         -- type Real_vector is array (natural range <>) of Real;
         Create_Array_Type
           (Real_Vector_Type_Definition, Real_Vector_Type_Declaration,
            Real_Subtype_Declaration, Name_Real_Vector);
      end if;

      if Vhdl_Std >= Vhdl_08 then
         -- type Time_vector is array (natural range <>) of Time;
         Create_Array_Type
           (Time_Vector_Type_Definition, Time_Vector_Type_Declaration,
            Time_Subtype_Declaration, Name_Time_Vector);
      end if;

      --  VHDL93:
      --  type file_open_kind is (read_mode, write_mode, append_mode);
      if Vhdl_Std >= Vhdl_93 then
         File_Open_Kind_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Enumeration_Literal_List
           (File_Open_Kind_Type_Definition, Create_Iir_Flist (3));
         Set_Scalar_Size (File_Open_Kind_Type_Definition, Scalar_8);

         File_Open_Kind_Read_Mode := Create_Std_Literal
           (Name_Read_Mode, 0, File_Open_Kind_Type_Definition);
         File_Open_Kind_Write_Mode := Create_Std_Literal
           (Name_Write_Mode, 1, File_Open_Kind_Type_Definition);
         File_Open_Kind_Append_Mode := Create_Std_Literal
           (Name_Append_Mode, 2, File_Open_Kind_Type_Definition);
         Set_Type_Staticness (File_Open_Kind_Type_Definition, Locally);
         Set_Signal_Type_Flag (File_Open_Kind_Type_Definition, True);
         Set_Has_Signal_Flag (File_Open_Kind_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  type file_open_kind is
         Create_Std_Type
           (File_Open_Kind_Type_Declaration, File_Open_Kind_Type_Definition,
            Name_File_Open_Kind);

         Utils.Create_Range_Constraint_For_Enumeration_Type
           (File_Open_Kind_Type_Definition);
         Add_Implicit_Operations (File_Open_Kind_Type_Declaration);
      else
         File_Open_Kind_Type_Declaration := Null_Iir;
         File_Open_Kind_Type_Definition := Null_Iir;
         File_Open_Kind_Read_Mode := Null_Iir;
         File_Open_Kind_Write_Mode := Null_Iir;
         File_Open_Kind_Append_Mode := Null_Iir;
      end if;

      --  VHDL93:
      --  type file_open_status is
      --      (open_ok, status_error, name_error, mode_error);
      if Vhdl_Std >= Vhdl_93 then
         File_Open_Status_Type_Definition :=
           Create_Std_Iir (Iir_Kind_Enumeration_Type_Definition);
         Set_Enumeration_Literal_List
           (File_Open_Status_Type_Definition, Create_Iir_Flist (4));
         Set_Scalar_Size (File_Open_Status_Type_Definition, Scalar_8);

         File_Open_Status_Open_Ok := Create_Std_Literal
           (Name_Open_Ok, 0, File_Open_Status_Type_Definition);
         File_Open_Status_Status_Error := Create_Std_Literal
           (Name_Status_Error, 1, File_Open_Status_Type_Definition);
         File_Open_Status_Name_Error := Create_Std_Literal
           (Name_Name_Error, 2, File_Open_Status_Type_Definition);
         File_Open_Status_Mode_Error := Create_Std_Literal
           (Name_Mode_Error, 3, File_Open_Status_Type_Definition);
         Set_Type_Staticness (File_Open_Status_Type_Definition, Locally);
         Set_Signal_Type_Flag (File_Open_Status_Type_Definition, True);
         Set_Has_Signal_Flag (File_Open_Status_Type_Definition,
                              not Flags.Flag_Whole_Analyze);

         --  type file_open_kind is
         Create_Std_Type (File_Open_Status_Type_Declaration,
                          File_Open_Status_Type_Definition,
                          Name_File_Open_Status);
         Utils.Create_Range_Constraint_For_Enumeration_Type
           (File_Open_Status_Type_Definition);
         Add_Implicit_Operations (File_Open_Status_Type_Declaration);
      else
         File_Open_Status_Type_Declaration := Null_Iir;
         File_Open_Status_Type_Definition := Null_Iir;
         File_Open_Status_Open_Ok := Null_Iir;
         File_Open_Status_Status_Error := Null_Iir;
         File_Open_Status_Name_Error := Null_Iir;
         File_Open_Status_Mode_Error := Null_Iir;
      end if;

      --  VHDL93:
      --  attribute FOREIGN: string;
      if Vhdl_Std >= Vhdl_93 then
         Foreign_Attribute := Create_Std_Decl (Iir_Kind_Attribute_Declaration);
         Set_Std_Identifier (Foreign_Attribute, Name_Foreign);
         Set_Type_Mark (Foreign_Attribute,
                        Create_Std_Type_Mark (String_Type_Declaration));
         Set_Type (Foreign_Attribute, String_Type_Definition);
         Add_Decl (Foreign_Attribute);
      else
         Foreign_Attribute := Null_Iir;
      end if;

      if Vhdl_Std >= Vhdl_08 then
         Create_To_String (Boolean_Type_Definition,
                           Iir_Predefined_Enum_To_String);
         Create_To_String (Bit_Type_Definition,
                           Iir_Predefined_Enum_To_String);
         Create_To_String (Character_Type_Definition,
                           Iir_Predefined_Enum_To_String);
         Create_To_String (Severity_Level_Type_Definition,
                           Iir_Predefined_Enum_To_String);
         Create_To_String (Universal_Integer_Type_Definition,
                           Iir_Predefined_Integer_To_String);
         Create_To_String (Universal_Real_Type_Definition,
                           Iir_Predefined_Floating_To_String);
         Create_To_String (Integer_Type_Definition,
                           Iir_Predefined_Integer_To_String);
         Create_To_String (Real_Type_Definition,
                           Iir_Predefined_Floating_To_String);
         Create_To_String (Time_Type_Definition,
                           Iir_Predefined_Physical_To_String);
         Create_To_String (File_Open_Kind_Type_Definition,
                           Iir_Predefined_Enum_To_String);
         Create_To_String (File_Open_Status_Type_Definition,
                           Iir_Predefined_Enum_To_String);

         --  Predefined overload TO_STRING operations
         Create_To_String (Real_Type_Definition,
                           Iir_Predefined_Real_To_String_Digits,
                           Name_To_String,
                           Name_Digits,
                           Natural_Subtype_Definition);
         Create_To_String (Real_Type_Definition,
                           Iir_Predefined_Real_To_String_Format,
                           Name_To_String,
                           Name_Format,
                           String_Type_Definition);
         Create_To_String (Time_Type_Definition,
                           Iir_Predefined_Time_To_String_Unit,
                           Name_To_String,
                           Name_Unit,
                           Time_Subtype_Definition);
      end if;

      --  Wilcard types.
      --  Create the declaration and give them meaningful (and invalid) names
      --  so that error messages are clear for the user.
      Wildcard_Type_Declaration_Chain := Null_Iir;
      Create_Wildcard_Type (Wildcard_Any_Type, "any type");
      Create_Wildcard_Type (Wildcard_Any_Aggregate_Type, "any aggregate type");
      Create_Wildcard_Type (Wildcard_Any_String_Type, "any string type");
      Create_Wildcard_Type (Wildcard_Any_Access_Type, "any access type");

      Error_Type := Utils.Create_Error_Type (Wildcard_Any_Type);
      Set_Error_Origin (Error_Type, Null_Iir);
      Create_Wildcard_Type (Error_Type, "unknown type");
   end Create_Std_Standard_Package;

   procedure Set_Time_Resolution (Resolution : Character)
   is
      procedure Change_Unit (Lit : Iir; Unit : Iir)
      is
         Name : constant Iir := Get_Unit_Name (Lit);
      begin
         Set_Identifier (Name, Get_Identifier (Unit));
         Set_Named_Entity (Name, Unit);
      end Change_Unit;

      Unit : Iir;
      Prim : Iir;
      Rng : Iir;
   begin
      case Resolution is
         when 'f' =>
            Prim := Time_Fs_Unit;
         when 'p' =>
            Prim := Time_Ps_Unit;
         when 'n' =>
            Prim := Time_Ns_Unit;
         when 'u' =>
            Prim := Time_Us_Unit;
         when 'm' =>
            Prim := Time_Ms_Unit;
         when 's' =>
            Prim := Time_Sec_Unit;
         when 'M' =>
            Prim := Time_Min_Unit;
         when 'h' =>
            Prim := Time_Hr_Unit;
         when others =>
            raise Internal_Error;
      end case;

      --  Adjust range of TIME subtype.
      Rng := Get_Range_Constraint (Time_Subtype_Definition);
      Change_Unit (Get_Left_Limit (Rng), Prim);
      Change_Unit (Get_Right_Limit (Rng), Prim);

      --  Adjust range of DELAY_LENGTH.
      if Vhdl_Std >= Vhdl_93 then
         Rng := Get_Range_Constraint (Delay_Length_Subtype_Definition);
         Change_Unit (Get_Left_Limit (Rng), Prim);
         Change_Unit (Get_Right_Limit (Rng), Prim);
      end if;

      Unit := Get_Unit_Chain (Time_Type_Definition);
      while Unit /= Null_Iir loop
         declare
            Lit : constant Iir := Get_Physical_Literal (Unit);
            Orig : constant Iir := Get_Literal_Origin (Lit);
            Lit_Unit : Iir;
         begin
            if Prim = Null_Iir then
               --  Primary already set, just recompute values.
               Lit_Unit := Get_Physical_Literal
                 (Get_Named_Entity (Get_Unit_Name (Orig)));
               Set_Value (Lit, Get_Value (Orig) * Get_Value (Lit_Unit));
            elsif Unit = Prim then
               Set_Value (Lit, 1);
               Prim := Null_Iir;
            else
               Set_Value (Lit, 0);
            end if;
         end;
         Unit := Get_Chain (Unit);
      end loop;
   end Set_Time_Resolution;

   function Get_Minimal_Time_Resolution return Character is
   begin
      --  In vhdl87, time units are locally static, so they are evaluated
      --  during analysis.  The units cannot be changed later.
      if Vhdl_Std = Vhdl_87 then
         return 'f';
      end if;

      if Get_Use_Flag (Time_Fs_Unit) then
         return 'f';
      end if;
      if Get_Use_Flag (Time_Ps_Unit) then
         return 'p';
      end if;
      if Get_Use_Flag (Time_Ns_Unit) then
         return 'n';
      end if;
      if Get_Use_Flag (Time_Us_Unit) then
         return 'u';
      end if;
      if Get_Use_Flag (Time_Ms_Unit) then
         return 'm';
      end if;
      if Get_Use_Flag (Time_Sec_Unit) then
         return 's';
      end if;
      if Get_Use_Flag (Time_Min_Unit) then
         return 'M';
      end if;
      if Get_Use_Flag (Time_Hr_Unit) then
         return 'h';
      end if;
      return '?';
   end Get_Minimal_Time_Resolution;
end Vhdl.Std_Package;
