--  Semantic utilities.
--  Copyright (C) 2018 Tristan Gingold
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
with Ada.Unchecked_Conversion;
with Types; use Types;
with Flags; use Flags;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Ieee.Std_Logic_1164;
with Std_Names;
with Vhdl.Std_Package; use Vhdl.Std_Package;

package body Vhdl.Sem_Utils is
   procedure Compute_Subprogram_Hash (Subprg : Iir)
   is
      type Hash_Type is mod 2**32;
      function To_Hash is new Ada.Unchecked_Conversion
        (Source => Iir, Target => Hash_Type);
      function To_Int32 is new Ada.Unchecked_Conversion
        (Source => Hash_Type, Target => Iir_Int32);

      Kind : Iir_Kind;
      Hash : Hash_Type;
      Sig : Hash_Type;
      Inter : Iir;
      Itype : Iir;
   begin
      Kind := Get_Kind (Subprg);
      if Kind = Iir_Kind_Function_Declaration
        or else Kind = Iir_Kind_Enumeration_Literal
      then
         Itype := Get_Base_Type (Get_Return_Type (Subprg));
         Hash := To_Hash (Itype);
         Sig := 8;
      else
         Sig := 1;
         Hash := 0;
      end if;

      if Kind /= Iir_Kind_Enumeration_Literal then
         Inter := Get_Interface_Declaration_Chain (Subprg);
         while Inter /= Null_Iir loop
            if Get_Kind (Inter) in Iir_Kinds_Interface_Object_Declaration then
               Itype := Get_Base_Type (Get_Type (Inter));
               Sig := Sig + 1;
               Hash := Hash * 7 + To_Hash (Itype);
               Hash := Hash + Hash / 2**28;
            else
               --  Non-object parameter are not allowed.
               pragma Assert (Flags.Flag_Force_Analysis);
               null;
            end if;
            Inter := Get_Chain (Inter);
         end loop;
      end if;
      Set_Subprogram_Hash (Subprg, To_Int32 (Hash + Sig));
   end Compute_Subprogram_Hash;

   --  LRM93 7.2.2
   --  A discrete array is a one-dimensional array whose elements are of a
   --  discrete type.
   function Is_Discrete_Array (Def : Iir) return Boolean
   is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            null;
         when others =>
            raise Internal_Error;
            -- return False;
      end case;
      if not Is_One_Dimensional_Array_Type (Def) then
         return False;
      end if;
      if Get_Kind (Get_Element_Subtype (Def))
        not in Iir_Kinds_Discrete_Type_Definition
      then
         return False;
      end if;
      return True;
   end Is_Discrete_Array;

   function Create_Anonymous_Interface (Atype : Iir)
     return Iir_Interface_Constant_Declaration
   is
      Inter : Iir_Interface_Constant_Declaration;
   begin
      Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
      Location_Copy (Inter, Atype);
      Set_Identifier (Inter, Null_Identifier);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Type (Inter, Atype);
      return Inter;
   end Create_Anonymous_Interface;

   --  Create an implicit/predefined function for DECL.
   function Create_Implicit_Function (Name : Name_Id;
                                      Decl : Iir;
                                      Def : Iir_Predefined_Functions;
                                      Interface_Chain : Iir;
                                      Return_Type : Iir)
                                     return Iir
   is
      Operation : Iir_Function_Declaration;
   begin
      Operation := Create_Iir (Iir_Kind_Function_Declaration);
      Location_Copy (Operation, Decl);
      Set_Parent (Operation, Get_Parent (Decl));
      Set_Interface_Declaration_Chain (Operation, Interface_Chain);
      Set_Return_Type (Operation, Return_Type);
      Set_Implicit_Definition (Operation, Def);
      Set_Identifier (Operation, Name);
      Set_Visible_Flag (Operation, True);
      Compute_Subprogram_Hash (Operation);
      return Operation;
   end Create_Implicit_Function;

   procedure Create_Implicit_File_Primitives
     (Decl : Iir_Type_Declaration; Type_Definition : Iir_File_Type_Definition)
   is
      Type_Mark : constant Iir := Get_File_Type_Mark (Type_Definition);
      Type_Mark_Type : constant Iir := Get_Type (Type_Mark);
      Proc: Iir_Procedure_Declaration;
      Func: Iir_Function_Declaration;
      Inter: Iir;
      Loc : Location_Type;
      File_Interface_Kind : Iir_Kind;
      First_Interface, Last_Interface : Iir;
      Last : Iir;
   begin
      Last := Decl;
      Loc := Get_Location (Decl);

      if Flags.Vhdl_Std >= Vhdl_93 then
         for I in 1 .. 2 loop
            --  Create the implicit file_open (form 1) declaration.
            --  Create the implicit file_open (form 2) declaration.
            Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
            Set_Location (Proc, Loc);
            Set_Parent (Proc, Get_Parent (Decl));
            Set_Identifier (Proc, Std_Names.Name_File_Open);
            Set_Visible_Flag (Proc, True);
            Set_Wait_State (Proc, False);
            Chain_Init (First_Interface, Last_Interface);
            case I is
               when 1 =>
                  Set_Implicit_Definition (Proc, Iir_Predefined_File_Open);
               when 2 =>
                  Set_Implicit_Definition (Proc,
                                           Iir_Predefined_File_Open_Status);
                  --  status : out file_open_status.
                  Inter :=
                    Create_Iir (Iir_Kind_Interface_Variable_Declaration);
                  Set_Location (Inter, Loc);
                  Set_Identifier (Inter, Std_Names.Name_Status);
                  Set_Type (Inter,
                            Std_Package.File_Open_Status_Type_Definition);
                  Set_Mode (Inter, Iir_Out_Mode);
                  Set_Visible_Flag (Inter, True);
                  Chain_Append (First_Interface, Last_Interface, Inter);
            end case;
            --  File F : FT
            Inter := Create_Iir (Iir_Kind_Interface_File_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_F);
            Set_Type (Inter, Type_Definition);
            Set_Mode (Inter, Iir_Inout_Mode);
            Set_Visible_Flag (Inter, True);
            Chain_Append (First_Interface, Last_Interface, Inter);
            --  External_Name : in STRING
            Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_External_Name);
            Set_Type (Inter, Std_Package.String_Type_Definition);
            Set_Mode (Inter, Iir_In_Mode);
            Set_Visible_Flag (Inter, True);
            Chain_Append (First_Interface, Last_Interface, Inter);
            --  Open_Kind : in File_Open_Kind := Read_Mode.
            Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
            Set_Location (Inter, Loc);
            Set_Identifier (Inter, Std_Names.Name_Open_Kind);
            Set_Type (Inter, Std_Package.File_Open_Kind_Type_Definition);
            Set_Mode (Inter, Iir_In_Mode);
            Set_Default_Value
              (Inter,
               Build_Simple_Name (Std_Package.File_Open_Kind_Read_Mode, Loc));
            Set_Visible_Flag (Inter, True);
            Chain_Append (First_Interface, Last_Interface, Inter);
            Set_Interface_Declaration_Chain (Proc, First_Interface);
            Compute_Subprogram_Hash (Proc);
            -- Add it to the list.
            Insert_Incr (Last, Proc);
         end loop;

         --  Create the implicit file_close declaration.
         Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
         Set_Identifier (Proc, Std_Names.Name_File_Close);
         Set_Location (Proc, Loc);
         Set_Parent (Proc, Get_Parent (Decl));
         Set_Implicit_Definition (Proc, Iir_Predefined_File_Close);
         Set_Visible_Flag (Proc, True);
         Set_Wait_State (Proc, False);
         Inter := Create_Iir (Iir_Kind_Interface_File_Declaration);
         Set_Identifier (Inter, Std_Names.Name_F);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Type_Definition);
         Set_Mode (Inter, Iir_Inout_Mode);
         Set_Visible_Flag (Inter, True);
         Set_Interface_Declaration_Chain (Proc, Inter);
         Compute_Subprogram_Hash (Proc);
         -- Add it to the list.
         Insert_Incr (Last, Proc);
      end if;

      if Flags.Vhdl_Std = Vhdl_87 then
         File_Interface_Kind := Iir_Kind_Interface_Variable_Declaration;
      else
         File_Interface_Kind := Iir_Kind_Interface_File_Declaration;
      end if;

      -- Create the implicit procedure read declaration.
      Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
      Set_Identifier (Proc, Std_Names.Name_Read);
      Set_Location (Proc, Loc);
      Set_Parent (Proc, Get_Parent (Decl));
      Set_Visible_Flag (Proc, True);
      Set_Wait_State (Proc, False);
      Chain_Init (First_Interface, Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Visible_Flag (Inter, True);
      Chain_Append (First_Interface, Last_Interface, Inter);
      Inter := Create_Iir (Iir_Kind_Interface_Variable_Declaration);
      Set_Identifier (Inter, Std_Names.Name_Value);
      Set_Location (Inter, Loc);
      Set_Subtype_Indication (Inter, Build_Simple_Name (Decl, Loc));
      Set_Type (Inter, Type_Mark_Type);
      Set_Mode (Inter, Iir_Out_Mode);
      Set_Visible_Flag (Inter, True);
      Chain_Append (First_Interface, Last_Interface, Inter);
      if Get_Kind (Type_Mark_Type) in Iir_Kinds_Array_Type_Definition
        and then Get_Constraint_State (Type_Mark_Type) /= Fully_Constrained
      then
         Inter := Create_Iir (Iir_Kind_Interface_Variable_Declaration);
         Set_Identifier (Inter, Std_Names.Name_Length);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Std_Package.Natural_Subtype_Definition);
         Set_Mode (Inter, Iir_Out_Mode);
         Set_Visible_Flag (Inter, True);
         Chain_Append (First_Interface, Last_Interface, Inter);
         Set_Implicit_Definition (Proc, Iir_Predefined_Read_Length);
      else
         Set_Implicit_Definition (Proc, Iir_Predefined_Read);
      end if;
      Set_Interface_Declaration_Chain (Proc, First_Interface);
      Compute_Subprogram_Hash (Proc);
      -- Add it to the list.
      Insert_Incr (Last, Proc);

      -- Create the implicit procedure write declaration.
      Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
      Set_Identifier (Proc, Std_Names.Name_Write);
      Set_Location (Proc, Loc);
      Set_Parent (Proc, Get_Parent (Decl));
      Set_Visible_Flag (Proc, True);
      Set_Wait_State (Proc, False);
      Chain_Init (First_Interface, Last_Interface);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_Out_Mode);
      Set_Name_Staticness (Inter, Locally);
      Set_Expr_Staticness (Inter, None);
      Set_Visible_Flag (Inter, True);
      Chain_Append (First_Interface, Last_Interface, Inter);
      Inter := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
      Set_Identifier (Inter, Std_Names.Name_Value);
      Set_Location (Inter, Loc);
      Set_Subtype_Indication (Inter, Build_Simple_Name (Decl, Loc));
      Set_Type (Inter, Type_Mark_Type);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Visible_Flag (Inter, True);
      Chain_Append (First_Interface, Last_Interface, Inter);
      Set_Implicit_Definition (Proc, Iir_Predefined_Write);
      Set_Interface_Declaration_Chain (Proc, First_Interface);
      Compute_Subprogram_Hash (Proc);
      -- Add it to the list.
      Insert_Incr (Last, Proc);

      --  Create the implicit procedure flush declaration
      if Flags.Vhdl_Std >= Vhdl_08 then
         Proc := Create_Iir (Iir_Kind_Procedure_Declaration);
         Set_Identifier (Proc, Std_Names.Name_Flush);
         Set_Location (Proc, Loc);
         Set_Parent (Proc, Get_Parent (Decl));
         Set_Visible_Flag (Proc, True);
         Set_Wait_State (Proc, False);
         Inter := Create_Iir (File_Interface_Kind);
         Set_Identifier (Inter, Std_Names.Name_F);
         Set_Location (Inter, Loc);
         Set_Type (Inter, Type_Definition);
         Set_Name_Staticness (Inter, Locally);
         Set_Expr_Staticness (Inter, None);
         Set_Visible_Flag (Inter, True);
         Set_Implicit_Definition (Proc, Iir_Predefined_Flush);
         Set_Interface_Declaration_Chain (Proc, Inter);
         Compute_Subprogram_Hash (Proc);
         -- Add it to the list.
         Insert_Incr (Last, Proc);
      end if;

      -- Create the implicit function endfile declaration.
      Func := Create_Iir (Iir_Kind_Function_Declaration);
      Set_Identifier (Func, Std_Names.Name_Endfile);
      Set_Location (Func, Loc);
      Set_Parent (Func, Get_Parent (Decl));
      Set_Visible_Flag (Func, True);
      Inter := Create_Iir (File_Interface_Kind);
      Set_Identifier (Inter, Std_Names.Name_F);
      Set_Location (Inter, Loc);
      Set_Type (Inter, Type_Definition);
      Set_Mode (Inter, Iir_In_Mode);
      Set_Visible_Flag (Inter, True);
      Set_Return_Type (Func, Std_Package.Boolean_Type_Definition);
      Set_Implicit_Definition (Func, Iir_Predefined_Endfile);
      Set_Interface_Declaration_Chain (Func, Inter);
      Compute_Subprogram_Hash (Func);
      -- Add it to the list.
      Insert_Incr (Last, Func);

   end Create_Implicit_File_Primitives;

   procedure Create_Implicit_Operations
     (Decl : Iir; Is_Std_Standard : Boolean := False)
   is
      use Std_Names;
      Binary_Chain : Iir;
      Unary_Chain : Iir;
      Type_Definition : Iir;
      Last : Iir;

      procedure Add_Operation (Name : Name_Id;
                               Def : Iir_Predefined_Functions;
                               Interface_Chain : Iir;
                               Return_Type : Iir)
      is
         Operation : Iir_Function_Declaration;
      begin
         Operation := Create_Implicit_Function
           (Name, Decl, Def, Interface_Chain, Return_Type);
         Insert_Incr (Last, Operation);
      end Add_Operation;

      procedure Add_Relational (Name : Name_Id; Def : Iir_Predefined_Functions)
      is
      begin
         Add_Operation
           (Name, Def, Binary_Chain, Std_Package.Boolean_Type_Definition);
      end Add_Relational;

      procedure Add_Binary (Name : Name_Id; Def : Iir_Predefined_Functions) is
      begin
         Add_Operation (Name, Def, Binary_Chain, Type_Definition);
      end Add_Binary;

      procedure Add_Unary (Name : Name_Id; Def : Iir_Predefined_Functions) is
      begin
         Add_Operation (Name, Def, Unary_Chain, Type_Definition);
      end Add_Unary;

      procedure Add_To_String (Def : Iir_Predefined_Functions) is
      begin
         Add_Operation (Name_To_String, Def,
                        Unary_Chain, String_Type_Definition);
      end Add_To_String;

      procedure Add_Min_Max (Name : Name_Id; Def : Iir_Predefined_Functions)
      is
         Left, Right : Iir;
      begin
         Left := Create_Anonymous_Interface (Type_Definition);
         Set_Identifier (Left, Name_L);
         Right := Create_Anonymous_Interface (Type_Definition);
         Set_Identifier (Right, Name_R);
         Set_Chain (Left, Right);
         Add_Operation (Name, Def, Left, Type_Definition);
      end Add_Min_Max;

      procedure Add_Vector_Min_Max
        (Name : Name_Id; Def : Iir_Predefined_Functions)
      is
         Left : Iir;
      begin
         Left := Create_Anonymous_Interface (Type_Definition);
         Set_Identifier (Left, Name_L);
         Add_Operation
           (Name, Def, Left, Get_Element_Subtype (Type_Definition));
      end Add_Vector_Min_Max;

      procedure Add_Shift_Operators
      is
         Inter_Chain : Iir_Interface_Constant_Declaration;
         Inter_Int : Iir;
      begin
         Inter_Chain := Create_Anonymous_Interface (Type_Definition);

         Inter_Int := Create_Iir (Iir_Kind_Interface_Constant_Declaration);
         Location_Copy (Inter_Int, Decl);
         Set_Identifier (Inter_Int, Null_Identifier);
         Set_Mode (Inter_Int, Iir_In_Mode);
         Set_Type (Inter_Int, Std_Package.Integer_Subtype_Definition);

         Set_Chain (Inter_Chain, Inter_Int);

         Add_Operation
           (Name_Sll, Iir_Predefined_Array_Sll, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Srl, Iir_Predefined_Array_Srl, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Sla, Iir_Predefined_Array_Sla, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Sra, Iir_Predefined_Array_Sra, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Rol, Iir_Predefined_Array_Rol, Inter_Chain, Type_Definition);
         Add_Operation
           (Name_Ror, Iir_Predefined_Array_Ror, Inter_Chain, Type_Definition);
      end Add_Shift_Operators;
   begin
      Last := Decl;

      Type_Definition := Get_Base_Type (Get_Type_Definition (Decl));
      if Get_Kind (Type_Definition) /= Iir_Kind_File_Type_Definition then
         Unary_Chain := Create_Anonymous_Interface (Type_Definition);
         Binary_Chain := Create_Anonymous_Interface (Type_Definition);
         Set_Chain (Binary_Chain, Unary_Chain);
      end if;

      case Get_Kind (Type_Definition) is
         when Iir_Kind_Enumeration_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Enum_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Enum_Inequality);
            Add_Relational (Name_Op_Greater, Iir_Predefined_Enum_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Enum_Greater_Equal);
            Add_Relational (Name_Op_Less, Iir_Predefined_Enum_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Enum_Less_Equal);

            if Flags.Vhdl_Std >= Vhdl_08 then
               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Enum_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Enum_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Enum_To_String);
               end if;

               --  LRM08 9.2.3 Relational operators
               --  The matching relational operators are predefined for the
               --  [predefined type BIT and for the] type STD_ULOGIC defined
               --  in package STD_LOGIC_1164.
               if Type_Definition = Ieee.Std_Logic_1164.Std_Ulogic_Type then
                  Add_Binary (Name_Op_Match_Equality,
                              Iir_Predefined_Std_Ulogic_Match_Equality);
                  Add_Binary (Name_Op_Match_Inequality,
                              Iir_Predefined_Std_Ulogic_Match_Inequality);
                  Add_Binary (Name_Op_Match_Less,
                              Iir_Predefined_Std_Ulogic_Match_Less);
                  Add_Binary (Name_Op_Match_Less_Equal,
                              Iir_Predefined_Std_Ulogic_Match_Less_Equal);
                  Add_Binary (Name_Op_Match_Greater,
                              Iir_Predefined_Std_Ulogic_Match_Greater);
                  Add_Binary (Name_Op_Match_Greater_Equal,
                              Iir_Predefined_Std_Ulogic_Match_Greater_Equal);
               end if;
            end if;

         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            declare
               Element_Type : Iir;

               Element_Array_Inter_Chain : Iir;
               Array_Element_Inter_Chain : Iir;
               Element_Element_Inter_Chain : Iir;
            begin
               Add_Relational
                 (Name_Op_Equality, Iir_Predefined_Array_Equality);
               Add_Relational
                 (Name_Op_Inequality, Iir_Predefined_Array_Inequality);
               if Is_Discrete_Array (Type_Definition) then
                  Add_Relational
                    (Name_Op_Greater, Iir_Predefined_Array_Greater);
                  Add_Relational
                    (Name_Op_Greater_Equal,
                     Iir_Predefined_Array_Greater_Equal);
                  Add_Relational
                    (Name_Op_Less, Iir_Predefined_Array_Less);
                  Add_Relational
                    (Name_Op_Less_Equal, Iir_Predefined_Array_Less_Equal);

                  --  LRM08 5.3.2.4 Predefined operations on array types
                  --  Given a type declaration that declares a discrete array
                  --  type T, the following operatons are implicitly declared
                  --  immediately following the type declaration:
                  --   function MINIMUM (L, R : T) return T;
                  --   function MAXIMUM (L, R : T) return T;
                  if Vhdl_Std >= Vhdl_08 then
                     Add_Min_Max (Name_Maximum, Iir_Predefined_Array_Maximum);
                     Add_Min_Max (Name_Minimum, Iir_Predefined_Array_Minimum);
                  end if;
               end if;

               Element_Type := Get_Element_Subtype (Type_Definition);

               if Is_One_Dimensional_Array_Type (Type_Definition) then
                  --  LRM93 7.2.4 Adding operators
                  --  The concatenation operator & is predefined for any
                  --  one-dimensional array type.
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Array_Array_Concat,
                                 Binary_Chain,
                                 Type_Definition);

                  Element_Array_Inter_Chain :=
                    Create_Anonymous_Interface (Element_Type);
                  Set_Chain (Element_Array_Inter_Chain, Unary_Chain);
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Element_Array_Concat,
                                 Element_Array_Inter_Chain,
                                 Type_Definition);

                  Array_Element_Inter_Chain :=
                    Create_Anonymous_Interface (Type_Definition);
                  Set_Chain (Array_Element_Inter_Chain,
                             Create_Anonymous_Interface (Element_Type));
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Array_Element_Concat,
                                 Array_Element_Inter_Chain,
                                 Type_Definition);

                  Element_Element_Inter_Chain :=
                    Create_Anonymous_Interface (Element_Type);
                  Set_Chain (Element_Element_Inter_Chain,
                             Create_Anonymous_Interface (Element_Type));
                  Add_Operation (Name_Op_Concatenation,
                                 Iir_Predefined_Element_Element_Concat,
                                 Element_Element_Inter_Chain,
                                 Type_Definition);

                  --  LRM08 5.3.2.4 Predefined operations on array types
                  --  In addition, given a type declaration that declares a
                  --  one-dimensional array type T whose elements are of a
                  --  sclar type E, the following operations are implicitly
                  --  declared immediately following the type declaration:
                  --   function MINIMUM (L : T) return E;
                  --   function MAXIMUM (L : T) return E;
                  if Vhdl_Std >= Vhdl_08
                    and then (Get_Kind (Element_Type) in
                                Iir_Kinds_Scalar_Type_And_Subtype_Definition)
                  then
                     Add_Vector_Min_Max
                       (Name_Maximum, Iir_Predefined_Vector_Maximum);
                     Add_Vector_Min_Max
                       (Name_Minimum, Iir_Predefined_Vector_Minimum);
                  end if;

                  if Element_Type = Std_Package.Boolean_Type_Definition
                    or else Element_Type = Std_Package.Bit_Type_Definition
                  then
                     --  LRM93 7.2.1 Logical operators
                     --  LRM08 9.2.2 Logical operators
                     --  The binary logical operators AND, OR, NAND, NOR, XOR,
                     --  and XNOR, and the unary logical operator NOT are
                     --  defined for predefined types BIT and BOOLEAN.  They
                     --  are also defined for any one-dimensional array type
                     --  whose element type is BIT or BOOLEAN.

                     Add_Unary (Name_Not, Iir_Predefined_TF_Array_Not);

                     Add_Binary (Name_And, Iir_Predefined_TF_Array_And);
                     Add_Binary (Name_Or, Iir_Predefined_TF_Array_Or);
                     Add_Binary (Name_Nand, Iir_Predefined_TF_Array_Nand);
                     Add_Binary (Name_Nor, Iir_Predefined_TF_Array_Nor);
                     Add_Binary (Name_Xor, Iir_Predefined_TF_Array_Xor);
                     if Flags.Vhdl_Std > Vhdl_87 then
                        Add_Binary (Name_Xnor, Iir_Predefined_TF_Array_Xnor);

                        --  LRM93 7.2.3 Shift operators
                        --  The shift operators SLL, SRL, SLA, SRA, ROL and
                        --  ROR are defined for any one-dimensional array type
                        --  whose element type is either of the predefined
                        --  types BIT or BOOLEAN.
                        Add_Shift_Operators;
                     end if;

                     --  LRM08 9.2.2 Logical operators
                     --  For the binary operators AND, OR, NAND, NOR, XOR and
                     --  XNOR, the operands shall both be [of the same base
                     --  type,] or one operand shall be of a scalar type and
                     --  the other operand shall be a one-dimensional array
                     --  whose element type is the scalar type.  The result
                     --  type is the same as the base type of the operands if
                     --  [both operands are scalars of the same base type or]
                     --  both operands are arrays, or the same as the base type
                     --  of the array operand if one operand is a scalar and
                     --  the other operand is an array.
                     if Flags.Vhdl_Std >= Vhdl_08 then
                        Add_Operation
                          (Name_And, Iir_Predefined_TF_Element_Array_And,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_And, Iir_Predefined_TF_Array_Element_And,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Or, Iir_Predefined_TF_Element_Array_Or,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Or, Iir_Predefined_TF_Array_Element_Or,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nand, Iir_Predefined_TF_Element_Array_Nand,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nand, Iir_Predefined_TF_Array_Element_Nand,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nor, Iir_Predefined_TF_Element_Array_Nor,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Nor, Iir_Predefined_TF_Array_Element_Nor,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xor, Iir_Predefined_TF_Element_Array_Xor,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xor, Iir_Predefined_TF_Array_Element_Xor,
                           Array_Element_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xnor, Iir_Predefined_TF_Element_Array_Xnor,
                           Element_Array_Inter_Chain, Type_Definition);
                        Add_Operation
                          (Name_Xnor, Iir_Predefined_TF_Array_Element_Xnor,
                           Array_Element_Inter_Chain, Type_Definition);
                     end if;

                     if Flags.Vhdl_Std >= Vhdl_08 then
                        --  LRM08 9.2.2 Logical operations
                        --  The unary logical operators AND, OR, NAND, NOR,
                        --  XOR, and XNOR are referred to as logical reduction
                        --  operators.  The logical reduction operators are
                        --  predefined for any one-dimensional array type whose
                        --  element type is BIT or BOOLEAN.  The result type
                        --  for the logical reduction operators is the same as
                        --  the element type of the operand.
                        Add_Operation
                          (Name_And, Iir_Predefined_TF_Reduction_And,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Or, Iir_Predefined_TF_Reduction_Or,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Nand, Iir_Predefined_TF_Reduction_Nand,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Nor, Iir_Predefined_TF_Reduction_Nor,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Xor, Iir_Predefined_TF_Reduction_Xor,
                           Unary_Chain, Element_Type);
                        Add_Operation
                          (Name_Xnor, Iir_Predefined_TF_Reduction_Xnor,
                           Unary_Chain, Element_Type);
                     end if;
                  end if;

                  --  LRM08 9.2.3 Relational operators
                  --  The matching equality and matching inequality operatotrs
                  --  are also defined for any one-dimensional array type
                  --  whose element type is BIT or STD_ULOGIC.
                  if Flags.Vhdl_Std >= Vhdl_08 then
                     if Element_Type = Std_Package.Bit_Type_Definition then
                        Add_Operation
                          (Name_Op_Match_Equality,
                           Iir_Predefined_Bit_Array_Match_Equality,
                           Binary_Chain, Element_Type);
                        Add_Operation
                          (Name_Op_Match_Inequality,
                           Iir_Predefined_Bit_Array_Match_Inequality,
                           Binary_Chain, Element_Type);
                     elsif Element_Type = Ieee.Std_Logic_1164.Std_Ulogic_Type
                     then
                        Add_Operation
                          (Name_Op_Match_Equality,
                           Iir_Predefined_Std_Ulogic_Array_Match_Equality,
                           Binary_Chain, Element_Type);
                        Add_Operation
                          (Name_Op_Match_Inequality,
                           Iir_Predefined_Std_Ulogic_Array_Match_Inequality,
                           Binary_Chain, Element_Type);
                     end if;
                  end if;

                  --  LRM08 5.3.2.4  Predefined operations on array type
                  --
                  --  Given a type declaration that declares a one-dimensional
                  --  array type T whose element type is a character type that
                  --  contains only character literals, the following operation
                  --  is implicitely declared immediately following the type
                  --  declaration
                  if Vhdl_Std >= Vhdl_08
                    and then String_Type_Definition /= Null_Iir
                    and then (Get_Kind (Element_Type)
                                = Iir_Kind_Enumeration_Type_Definition)
                    and then Get_Only_Characters_Flag (Element_Type)
                  then
                     Add_Operation (Name_To_String,
                                    Iir_Predefined_Array_Char_To_String,
                                    Unary_Chain,
                                    String_Type_Definition);
                  end if;
               end if;
            end;

         when Iir_Kind_Access_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Access_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Access_Inequality);
            declare
               Deallocate_Proc: Iir_Procedure_Declaration;
               Var_Interface: Iir_Interface_Variable_Declaration;
            begin
               Deallocate_Proc :=
                 Create_Iir (Iir_Kind_Procedure_Declaration);
               Location_Copy (Deallocate_Proc, Decl);
               Set_Identifier (Deallocate_Proc, Std_Names.Name_Deallocate);
               Set_Implicit_Definition
                 (Deallocate_Proc, Iir_Predefined_Deallocate);
               Set_Parent (Deallocate_Proc, Get_Parent (Decl));

               Var_Interface :=
                 Create_Iir (Iir_Kind_Interface_Variable_Declaration);
               Location_Copy (Var_Interface, Decl);
               Set_Identifier (Var_Interface, Std_Names.Name_P);
               Set_Parent (Var_Interface, Deallocate_Proc);
               Set_Type (Var_Interface, Type_Definition);
               Set_Mode (Var_Interface, Iir_Inout_Mode);
               --Set_Purity_State (Deallocate_Proc, Impure);
               Set_Wait_State (Deallocate_Proc, False);
               Set_Visible_Flag (Deallocate_Proc, True);

               Set_Interface_Declaration_Chain
                 (Deallocate_Proc, Var_Interface);
               Compute_Subprogram_Hash (Deallocate_Proc);
               Insert_Incr (Last, Deallocate_Proc);
            end;

         when Iir_Kind_Record_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Record_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Record_Inequality);

         when Iir_Kind_Integer_Type_Definition =>
            Add_Relational (Name_Op_Equality, Iir_Predefined_Integer_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Integer_Inequality);
            Add_Relational (Name_Op_Greater, Iir_Predefined_Integer_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Integer_Greater_Equal);
            Add_Relational (Name_Op_Less, Iir_Predefined_Integer_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Integer_Less_Equal);

            Add_Binary (Name_Op_Plus, Iir_Predefined_Integer_Plus);
            Add_Binary (Name_Op_Minus, Iir_Predefined_Integer_Minus);

            Add_Unary (Name_Op_Minus, Iir_Predefined_Integer_Negation);
            Add_Unary (Name_Op_Plus, Iir_Predefined_Integer_Identity);

            Add_Binary (Name_Op_Mul, Iir_Predefined_Integer_Mul);
            Add_Binary (Name_Op_Div, Iir_Predefined_Integer_Div);
            Add_Binary (Name_Mod, Iir_Predefined_Integer_Mod);
            Add_Binary (Name_Rem, Iir_Predefined_Integer_Rem);

            Add_Unary (Name_Abs, Iir_Predefined_Integer_Absolute);

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain
                 (Inter_Chain,
                  Create_Anonymous_Interface (Integer_Type_Definition));
               Add_Operation (Name_Op_Exp, Iir_Predefined_Integer_Exp,
                              Inter_Chain, Type_Definition);
            end;

            if Vhdl_Std >= Vhdl_08 then
               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Integer_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Integer_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Integer_To_String);
               end if;
            end if;

         when Iir_Kind_Floating_Type_Definition =>
            Add_Relational
              (Name_Op_Equality, Iir_Predefined_Floating_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Floating_Inequality);
            Add_Relational
              (Name_Op_Greater, Iir_Predefined_Floating_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Floating_Greater_Equal);
            Add_Relational
              (Name_Op_Less, Iir_Predefined_Floating_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Floating_Less_Equal);

            Add_Binary (Name_Op_Plus, Iir_Predefined_Floating_Plus);
            Add_Binary (Name_Op_Minus, Iir_Predefined_Floating_Minus);

            Add_Unary (Name_Op_Minus, Iir_Predefined_Floating_Negation);
            Add_Unary (Name_Op_Plus, Iir_Predefined_Floating_Identity);

            Add_Binary (Name_Op_Mul, Iir_Predefined_Floating_Mul);
            Add_Binary (Name_Op_Div, Iir_Predefined_Floating_Div);

            Add_Unary (Name_Abs, Iir_Predefined_Floating_Absolute);

            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain
                 (Inter_Chain,
                  Create_Anonymous_Interface (Integer_Type_Definition));
               Add_Operation (Name_Op_Exp, Iir_Predefined_Floating_Exp,
                              Inter_Chain, Type_Definition);
            end;

            if Vhdl_Std >= Vhdl_08 then
               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Floating_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Floating_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Floating_To_String);
               end if;
            end if;

         when Iir_Kind_Physical_Type_Definition =>
            Add_Relational
              (Name_Op_Equality, Iir_Predefined_Physical_Equality);
            Add_Relational
              (Name_Op_Inequality, Iir_Predefined_Physical_Inequality);
            Add_Relational
              (Name_Op_Greater, Iir_Predefined_Physical_Greater);
            Add_Relational
              (Name_Op_Greater_Equal, Iir_Predefined_Physical_Greater_Equal);
            Add_Relational
              (Name_Op_Less, Iir_Predefined_Physical_Less);
            Add_Relational
              (Name_Op_Less_Equal, Iir_Predefined_Physical_Less_Equal);

            Add_Binary (Name_Op_Plus, Iir_Predefined_Physical_Plus);
            Add_Binary (Name_Op_Minus, Iir_Predefined_Physical_Minus);

            Add_Unary (Name_Op_Minus, Iir_Predefined_Physical_Negation);
            Add_Unary (Name_Op_Plus, Iir_Predefined_Physical_Identity);

            --  Physical OP integer, with OP is multiplication and division.
            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain
                 (Inter_Chain,
                  Create_Anonymous_Interface (Integer_Type_Definition));
               Add_Operation (Name_Op_Mul, Iir_Predefined_Physical_Integer_Mul,
                              Inter_Chain, Type_Definition);
               Add_Operation (Name_Op_Div, Iir_Predefined_Physical_Integer_Div,
                              Inter_Chain, Type_Definition);
            end;

            --  Integer * physical
            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain :=
                 Create_Anonymous_Interface (Integer_Type_Definition);
               Set_Chain (Inter_Chain, Unary_Chain);
               Add_Operation (Name_Op_Mul, Iir_Predefined_Integer_Physical_Mul,
                              Inter_Chain, Type_Definition);
            end;

            --  Physical OP real, wher OP is multiplication and division.
            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain := Create_Anonymous_Interface (Type_Definition);
               Set_Chain (Inter_Chain,
                          Create_Anonymous_Interface (Real_Type_Definition));
               Add_Operation (Name_Op_Mul, Iir_Predefined_Physical_Real_Mul,
                              Inter_Chain, Type_Definition);
               Add_Operation (Name_Op_Div, Iir_Predefined_Physical_Real_Div,
                              Inter_Chain, Type_Definition);
            end;

            --  Real * physical
            declare
               Inter_Chain : Iir;
            begin
               Inter_Chain :=
                 Create_Anonymous_Interface (Real_Type_Definition);
               Set_Chain (Inter_Chain, Unary_Chain);
               Add_Operation (Name_Op_Mul, Iir_Predefined_Real_Physical_Mul,
                              Inter_Chain, Type_Definition);
            end;

            --  Physical / physical -> integer.
            Add_Operation (Name_Op_Div, Iir_Predefined_Physical_Physical_Div,
                           Binary_Chain,
                           Std_Package.Convertible_Integer_Type_Definition);

            Add_Unary (Name_Abs, Iir_Predefined_Physical_Absolute);

            if Vhdl_Std >= Vhdl_08 then
               Add_Binary (Name_Mod, Iir_Predefined_Physical_Mod);
               Add_Binary (Name_Rem, Iir_Predefined_Physical_Rem);

               --  LRM08 5.2.6 Predefined operations on scalar types
               --  Given a type declaration that declares a scalar type T, the
               --  following operations are implicitely declared immediately
               --  following the type declaration (except for the TO_STRING
               --  operations in package STANDARD [...])
               Add_Min_Max (Name_Minimum, Iir_Predefined_Physical_Minimum);
               Add_Min_Max (Name_Maximum, Iir_Predefined_Physical_Maximum);
               if not Is_Std_Standard then
                  Add_To_String (Iir_Predefined_Physical_To_String);
               end if;
            end if;

         when Iir_Kind_File_Type_Definition =>
            Create_Implicit_File_Primitives (Decl, Type_Definition);

         when Iir_Kind_Protected_Type_Declaration =>
            null;

         when others =>
            Error_Kind ("create_predefined_operations", Type_Definition);
      end case;

      if not Is_Std_Standard then
         return;
      end if;
      if Decl = Std_Package.Boolean_Type_Declaration then
         Add_Binary (Name_And, Iir_Predefined_Boolean_And);
         Add_Binary (Name_Or, Iir_Predefined_Boolean_Or);
         Add_Binary (Name_Nand, Iir_Predefined_Boolean_Nand);
         Add_Binary (Name_Nor, Iir_Predefined_Boolean_Nor);
         Add_Binary (Name_Xor, Iir_Predefined_Boolean_Xor);
         if Flags.Vhdl_Std > Vhdl_87 then
            Add_Binary (Name_Xnor, Iir_Predefined_Boolean_Xnor);
         end if;
         Add_Unary (Name_Not, Iir_Predefined_Boolean_Not);
      elsif Decl = Std_Package.Bit_Type_Declaration then
         Add_Binary (Name_And, Iir_Predefined_Bit_And);
         Add_Binary (Name_Or, Iir_Predefined_Bit_Or);
         Add_Binary (Name_Nand, Iir_Predefined_Bit_Nand);
         Add_Binary (Name_Nor, Iir_Predefined_Bit_Nor);
         Add_Binary (Name_Xor, Iir_Predefined_Bit_Xor);
         if Flags.Vhdl_Std > Vhdl_87 then
            Add_Binary (Name_Xnor, Iir_Predefined_Bit_Xnor);
         end if;
         Add_Unary (Name_Not, Iir_Predefined_Bit_Not);
         if Flags.Vhdl_Std >= Vhdl_08 then
            Add_Binary (Name_Op_Match_Equality,
                        Iir_Predefined_Bit_Match_Equality);
            Add_Binary (Name_Op_Match_Inequality,
                        Iir_Predefined_Bit_Match_Inequality);
            Add_Binary (Name_Op_Match_Less,
                        Iir_Predefined_Bit_Match_Less);
            Add_Binary (Name_Op_Match_Less_Equal,
                        Iir_Predefined_Bit_Match_Less_Equal);
            Add_Binary (Name_Op_Match_Greater,
                        Iir_Predefined_Bit_Match_Greater);
            Add_Binary (Name_Op_Match_Greater_Equal,
                        Iir_Predefined_Bit_Match_Greater_Equal);

            --  LRM08 9.2.9 Condition operator
            --  The unary operator ?? is predefined for type BIT defined in
            --  package STANDARD.
            Add_Operation (Name_Op_Condition, Iir_Predefined_Bit_Condition,
                           Unary_Chain, Std_Package.Boolean_Type_Definition);

         end if;
      elsif Decl = Std_Package.Universal_Real_Type_Declaration then
         declare
            Inter_Chain : Iir;
         begin
            Inter_Chain := Create_Anonymous_Interface (Type_Definition);
            Set_Chain
              (Inter_Chain,
               Create_Anonymous_Interface (Universal_Integer_Type_Definition));
            Add_Operation (Name_Op_Mul, Iir_Predefined_Universal_R_I_Mul,
                           Inter_Chain, Type_Definition);
            Add_Operation (Name_Op_Div, Iir_Predefined_Universal_R_I_Div,
                           Inter_Chain, Type_Definition);
         end;

         declare
            Inter_Chain : Iir;
         begin
            Inter_Chain :=
              Create_Anonymous_Interface (Universal_Integer_Type_Definition);
            Set_Chain (Inter_Chain, Unary_Chain);
            Add_Operation (Name_Op_Mul, Iir_Predefined_Universal_I_R_Mul,
                           Inter_Chain, Type_Definition);
         end;
      end if;
   end Create_Implicit_Operations;
end Vhdl.Sem_Utils;
