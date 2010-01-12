--  Tree node definitions.
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
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Errorout; use Errorout;
with Nodes; use Nodes;
with Lists; use Lists;

package body Iirs is
   function Is_Null (Node : Iir) return Boolean is
   begin
      return Node = Null_Iir;
   end Is_Null;

   function Is_Null_List (Node : Iir_List) return Boolean is
   begin
      return Node = Null_Iir_List;
   end Is_Null_List;

   ---------------------------------------------------
   -- General subprograms that operate on every iir --
   ---------------------------------------------------

   -- This is the procedure to call when an internal consistancy test has
   -- failed.
   -- The main idea is the consistancy test *MUST* have no side effect,
   -- except calling this procedure.  To speed up, this procedure could
   -- be a no-op.
   procedure Failed (Func: String := ""; Node : Iir := Null_Iir)
   is
   begin
      if Func /= "" then
         Error_Kind (Func, Node);
      end if;
      raise Internal_Error;
   end Failed;

   function Get_Format (Kind : Iir_Kind) return Format_Type;

   --  Statistics.
   procedure Disp_Stats
   is
      use Ada.Text_IO;
      type Num_Array is array (Iir_Kind) of Natural;
      Num : Num_Array := (others => 0);
      type Format_Array is array (Format_Type) of Natural;
      Formats : Format_Array := (others => 0);
      Kind : Iir_Kind;
      I : Iir;
      Last_I : Iir;
      Format : Format_Type;
   begin
      I := Error_Node + 1;
      Last_I := Get_Last_Node;
      while I < Last_I loop
         Kind := Get_Kind (I);
         Num (Kind) := Num (Kind) + 1;
         Format := Get_Format (Kind);
         Formats (Format) := Formats (Format) + 1;
         case Format is
            when Format_Medium =>
               I := I + 2;
            when Format_Short
              | Format_Fp
              | Format_Int =>
               I := I + 1;
         end case;
      end loop;

      Put_Line ("Stats per iir_kind:");
      for J in Iir_Kind loop
         if Num (J) /= 0 then
            Put_Line (' ' & Iir_Kind'Image (J) & ':'
                      & Natural'Image (Num (J)));
         end if;
      end loop;
      Put_Line ("Stats per formats:");
      for J in Format_Type loop
         Put_Line (' ' & Format_Type'Image (J) & ':'
                   & Natural'Image (Formats (J)));
      end loop;
   end Disp_Stats;

   function Iir_Predefined_Shortcut_P (Func : Iir_Predefined_Functions)
     return Boolean is
   begin
      case Func is
         when Iir_Predefined_Bit_And
           | Iir_Predefined_Bit_Or
           | Iir_Predefined_Bit_Nand
           | Iir_Predefined_Bit_Nor
           | Iir_Predefined_Boolean_And
           | Iir_Predefined_Boolean_Or
           | Iir_Predefined_Boolean_Nand
           | Iir_Predefined_Boolean_Nor =>
            return True;
         when others =>
            return False;
      end case;
   end Iir_Predefined_Shortcut_P;

   function Create_Proxy (Proxy: Iir) return Iir_Proxy is
      Res : Iir_Proxy;
   begin
      Res := Create_Iir (Iir_Kind_Proxy);
      Set_Proxy (Res, Proxy);
      return Res;
   end Create_Proxy;

   --

   function Create_Iir_Error return Iir
   is
      Res : Iir;
   begin
      Res := Create_Node (Format_Short);
      Set_Nkind (Res, Iir_Kind'Pos (Iir_Kind_Error));
      Set_Base_Type (Res, Res);
      return Res;
   end Create_Iir_Error;

   procedure Location_Copy (Target: Iir; Src: Iir) is
   begin
      Set_Location (Target, Get_Location (Src));
   end Location_Copy;

   -- Get kind
   function Get_Kind (An_Iir: Iir) return Iir_Kind
   is
      --  Speed up: avoid to check that nkind is in the bounds of Iir_Kind.
      pragma Suppress (Range_Check);
   begin
      return Iir_Kind'Val (Get_Nkind (An_Iir));
   end Get_Kind;

--    function Clone_Iir (Src : Iir; New_Kind : Iir_Kind) return Iir
--    is
--       Res : Iir;
--    begin
--       Res := new Iir_Node (New_Kind);
--       Res.Flag1 := Src.Flag1;
--       Res.Flag2 := Src.Flag2;
--       Res.Flag3 := Src.Flag3;
--       Res.Flag4 := Src.Flag4;
--       Res.Flag5 := Src.Flag5;
--       Res.Flag6 := Src.Flag6;
--       Res.Flag7 := Src.Flag7;
--       Res.Flag8 := Src.Flag8;
--       Res.State1 := Src.State1;
--       Res.State2 := Src.State2;
--       Res.State3 := Src.State3;
--       Res.Staticness1 := Src.Staticness1;
--       Res.Staticness2 := Src.Staticness2;
--       Res.Odigit1 := Src.Odigit1;
--       Res.Odigit2 := Src.Odigit2;
--       Res.Location := Src.Location;
--       Res.Back_End_Info := Src.Back_End_Info;
--       Res.Identifier := Src.Identifier;
--       Res.Field1 := Src.Field1;
--       Res.Field2 := Src.Field2;
--       Res.Field3 := Src.Field3;
--       Res.Field4 := Src.Field4;
--       Res.Field5 := Src.Field5;
--       Res.Nbr2 := Src.Nbr2;
--       Res.Nbr3 := Src.Nbr3;

--       Src.Identifier := Null_Identifier;
--       Src.Field1 := null;
--       Src.Field2 := null;
--       Src.Field3 := null;
--       Src.Field4 := null;
--       Src.Field5 := null;
--       return Res;
--    end Clone_Iir;


   -----------------
   -- design file --
   -----------------

   -- Iir_Design_File

--   type Int_Access_Type is new Integer;
--   for Int_Access_Type'Size use System.Word_Size; --Iir_Identifier_Acc'Size;

   --  Safe conversions.
--    function Iir_To_Int_Access_Type is
--       new Ada.Unchecked_Conversion (Source => Iir,
--                                     Target => Int_Access_Type);
--    function Int_Access_Type_To_Iir is
--       new Ada.Unchecked_Conversion (Source => Int_Access_Type,
--                                     Target => Iir);

--    function To_Iir (V : Integer) return Iir is
--    begin
--       return Int_Access_Type_To_Iir (Int_Access_Type (V));
--    end To_Iir;

--    function To_Integer (N : Iir) return Integer is
--    begin
--       return Integer (Iir_To_Int_Access_Type (N));
--    end To_Integer;

   procedure Set_Pos_Line_Off (Design_Unit: Iir_Design_Unit;
                               Pos : Source_Ptr; Line, Off: Natural) is
   begin
      Set_Field1 (Design_Unit, Node_Type (Pos));
      Set_Field11 (Design_Unit, Node_Type (Off));
      Set_Field12 (Design_Unit, Node_Type (Line));
   end Set_Pos_Line_Off;

   procedure Get_Pos_Line_Off (Design_Unit: Iir_Design_Unit;
                               Pos : out Source_Ptr; Line, Off: out Natural) is
   begin
      Pos := Source_Ptr (Get_Field1 (Design_Unit));
      Off := Natural (Get_Field11 (Design_Unit));
      Line := Natural (Get_Field12 (Design_Unit));
   end Get_Pos_Line_Off;

   -----------
   -- Lists --
   -----------
   --  Layout of lists:
   --  A list is stored into an IIR.
   --  There are two bounds for a list:
   --    the current number of elements
   --    the maximum number of elements.
   --  Using a maximum number of element bound (which can be increased) avoid
   --  to reallocating memory at each insertion.

   function Time_Stamp_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => Time_Stamp_Id, Target => Iir);

   function Iir_To_Time_Stamp_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Time_Stamp_Id);

   function Iir_To_Iir_List is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_List);
   function Iir_List_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_List, Target => Iir);

   function Iir_To_Token_Type (N : Iir) return Token_Type is
   begin
      return Token_Type'Val (N);
   end Iir_To_Token_Type;

   function Token_Type_To_Iir (T : Token_Type) return Iir is
   begin
      return Token_Type'Pos (T);
   end Token_Type_To_Iir;

--     function Iir_To_Iir_Index32 (N : Iir) return Iir_Index32 is
--     begin
--        return Iir_Index32 (N);
--     end Iir_To_Iir_Index32;

--     function Iir_Index32_To_Iir (V : Iir_Index32) return Iir is
--     begin
--        return Iir_Index32'Pos (V);
--     end Iir_Index32_To_Iir;

   function Iir_To_Name_Id (N : Iir) return Name_Id is
   begin
      return Iir'Pos (N);
   end Iir_To_Name_Id;
   pragma Inline (Iir_To_Name_Id);

   function Name_Id_To_Iir (V : Name_Id) return Iir is
   begin
      return Name_Id'Pos (V);
   end Name_Id_To_Iir;

   function Iir_To_Iir_Int32 is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Iir_Int32);

   function Iir_Int32_To_Iir is new Ada.Unchecked_Conversion
     (Source => Iir_Int32, Target => Iir);

   function Iir_To_Location_Type (N : Iir) return Location_Type is
   begin
      return Location_Type (N);
   end Iir_To_Location_Type;

   function Location_Type_To_Iir (L : Location_Type) return Iir is
   begin
      return Iir (L);
   end Location_Type_To_Iir;

   function Iir_To_String_Id is new Ada.Unchecked_Conversion
     (Source => Iir, Target => String_Id);
   function String_Id_To_Iir is new Ada.Unchecked_Conversion
     (Source => String_Id, Target => Iir);

   function Iir_To_Int32 is new Ada.Unchecked_Conversion
     (Source => Iir, Target => Int32);
   function Int32_To_Iir is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Iir);

   function Iir_To_PSL_Node is new Ada.Unchecked_Conversion
     (Source => Iir, Target => PSL_Node);

   function PSL_Node_To_Iir is new Ada.Unchecked_Conversion
     (Source => PSL_Node, Target => Iir);

   function Iir_To_PSL_NFA is new Ada.Unchecked_Conversion
     (Source => Iir, Target => PSL_NFA);

   function PSL_NFA_To_Iir is new Ada.Unchecked_Conversion
     (Source => PSL_NFA, Target => Iir);

   --  Subprograms
   function Get_Format (Kind : Iir_Kind) return Format_Type is
   begin
      case Kind is
         when Iir_Kind_Error
           | Iir_Kind_Library_Clause
           | Iir_Kind_Use_Clause
           | Iir_Kind_Character_Literal
           | Iir_Kind_Null_Literal
           | Iir_Kind_String_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Proxy
           | Iir_Kind_Waveform_Element
           | Iir_Kind_Conditional_Waveform
           | Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Entity_Aspect_Configuration
           | Iir_Kind_Entity_Aspect_Open
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Entity_Class
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Signature
           | Iir_Kind_Aggregate_Info
           | Iir_Kind_Procedure_Call
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition
           | Iir_Kind_Range_Expression
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Subtype_Definition
           | Iir_Kind_Overload_List
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call
           | Iir_Kind_Aggregate
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Psl_Expression
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Simple_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Behavior_Attribute
           | Iir_Kind_Structure_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            return Format_Short;
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Bit_String_Literal
           | Iir_Kind_Block_Header
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            return Format_Medium;
         when Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            return Format_Fp;
         when Iir_Kind_Integer_Literal
           | Iir_Kind_Physical_Int_Literal =>
            return Format_Int;
      end case;
   end Get_Format;

   function Create_Iir (Kind : Iir_Kind) return Iir
   is
      Res : Iir;
      Format : Format_Type;
   begin
      Format := Get_Format (Kind);
      Res := Create_Node (Format);
      Set_Nkind (Res, Iir_Kind'Pos (Kind));
      return Res;
   end Create_Iir;

   procedure Check_Kind_For_First_Design_Unit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("First_Design_Unit", Target);
      end case;
   end Check_Kind_For_First_Design_Unit;

   function Get_First_Design_Unit (Design : Iir) return Iir is
   begin
      Check_Kind_For_First_Design_Unit (Design);
      return Get_Field5 (Design);
   end Get_First_Design_Unit;

   procedure Set_First_Design_Unit (Design : Iir; Chain : Iir) is
   begin
      Check_Kind_For_First_Design_Unit (Design);
      Set_Field5 (Design, Chain);
   end Set_First_Design_Unit;

   procedure Check_Kind_For_Last_Design_Unit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("Last_Design_Unit", Target);
      end case;
   end Check_Kind_For_Last_Design_Unit;

   function Get_Last_Design_Unit (Design : Iir) return Iir is
   begin
      Check_Kind_For_Last_Design_Unit (Design);
      return Get_Field6 (Design);
   end Get_Last_Design_Unit;

   procedure Set_Last_Design_Unit (Design : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Last_Design_Unit (Design);
      Set_Field6 (Design, Chain);
   end Set_Last_Design_Unit;

   procedure Check_Kind_For_Library_Declaration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Library_Clause =>
            null;
         when others =>
            Failed ("Library_Declaration", Target);
      end case;
   end Check_Kind_For_Library_Declaration;

   function Get_Library_Declaration (Design : Iir) return Iir is
   begin
      Check_Kind_For_Library_Declaration (Design);
      return Get_Field1 (Design);
   end Get_Library_Declaration;

   procedure Set_Library_Declaration (Design : Iir; Library : Iir) is
   begin
      Check_Kind_For_Library_Declaration (Design);
      Set_Field1 (Design, Library);
   end Set_Library_Declaration;

   procedure Check_Kind_For_File_Time_Stamp (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("File_Time_Stamp", Target);
      end case;
   end Check_Kind_For_File_Time_Stamp;

   function Get_File_Time_Stamp (Design : Iir) return Time_Stamp_Id is
   begin
      Check_Kind_For_File_Time_Stamp (Design);
      return Iir_To_Time_Stamp_Id (Get_Field4 (Design));
   end Get_File_Time_Stamp;

   procedure Set_File_Time_Stamp (Design : Iir; Stamp : Time_Stamp_Id) is
   begin
      Check_Kind_For_File_Time_Stamp (Design);
      Set_Field4 (Design, Time_Stamp_Id_To_Iir (Stamp));
   end Set_File_Time_Stamp;

   procedure Check_Kind_For_Analysis_Time_Stamp (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("Analysis_Time_Stamp", Target);
      end case;
   end Check_Kind_For_Analysis_Time_Stamp;

   function Get_Analysis_Time_Stamp (Design : Iir) return Time_Stamp_Id is
   begin
      Check_Kind_For_Analysis_Time_Stamp (Design);
      return Iir_To_Time_Stamp_Id (Get_Field3 (Design));
   end Get_Analysis_Time_Stamp;

   procedure Set_Analysis_Time_Stamp (Design : Iir; Stamp : Time_Stamp_Id) is
   begin
      Check_Kind_For_Analysis_Time_Stamp (Design);
      Set_Field3 (Design, Time_Stamp_Id_To_Iir (Stamp));
   end Set_Analysis_Time_Stamp;

   procedure Check_Kind_For_Library (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("Library", Target);
      end case;
   end Check_Kind_For_Library;

   function Get_Library (File : Iir_Design_File) return Iir is
   begin
      Check_Kind_For_Library (File);
      return Get_Field0 (File);
   end Get_Library;

   procedure Set_Library (File : Iir_Design_File; Lib : Iir) is
   begin
      Check_Kind_For_Library (File);
      Set_Field0 (File, Lib);
   end Set_Library;

   procedure Check_Kind_For_File_Dependence_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("File_Dependence_List", Target);
      end case;
   end Check_Kind_For_File_Dependence_List;

   function Get_File_Dependence_List (File : Iir_Design_File) return Iir_List
      is
   begin
      Check_Kind_For_File_Dependence_List (File);
      return Iir_To_Iir_List (Get_Field1 (File));
   end Get_File_Dependence_List;

   procedure Set_File_Dependence_List (File : Iir_Design_File; Lst : Iir_List)
      is
   begin
      Check_Kind_For_File_Dependence_List (File);
      Set_Field1 (File, Iir_List_To_Iir (Lst));
   end Set_File_Dependence_List;

   procedure Check_Kind_For_Design_File_Filename (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("Design_File_Filename", Target);
      end case;
   end Check_Kind_For_Design_File_Filename;

   function Get_Design_File_Filename (File : Iir_Design_File) return Name_Id
      is
   begin
      Check_Kind_For_Design_File_Filename (File);
      return Name_Id'Val (Get_Field12 (File));
   end Get_Design_File_Filename;

   procedure Set_Design_File_Filename (File : Iir_Design_File; Name : Name_Id)
      is
   begin
      Check_Kind_For_Design_File_Filename (File);
      Set_Field12 (File, Name_Id'Pos (Name));
   end Set_Design_File_Filename;

   procedure Check_Kind_For_Design_File_Directory (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File =>
            null;
         when others =>
            Failed ("Design_File_Directory", Target);
      end case;
   end Check_Kind_For_Design_File_Directory;

   function Get_Design_File_Directory (File : Iir_Design_File) return Name_Id
      is
   begin
      Check_Kind_For_Design_File_Directory (File);
      return Name_Id'Val (Get_Field11 (File));
   end Get_Design_File_Directory;

   procedure Set_Design_File_Directory (File : Iir_Design_File; Dir : Name_Id)
      is
   begin
      Check_Kind_For_Design_File_Directory (File);
      Set_Field11 (File, Name_Id'Pos (Dir));
   end Set_Design_File_Directory;

   procedure Check_Kind_For_Design_File (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Design_File", Target);
      end case;
   end Check_Kind_For_Design_File;

   function Get_Design_File (Unit : Iir_Design_Unit) return Iir_Design_File is
   begin
      Check_Kind_For_Design_File (Unit);
      return Get_Field0 (Unit);
   end Get_Design_File;

   procedure Set_Design_File (Unit : Iir_Design_Unit; File : Iir_Design_File)
      is
   begin
      Check_Kind_For_Design_File (Unit);
      Set_Field0 (Unit, File);
   end Set_Design_File;

   procedure Check_Kind_For_Design_File_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Library_Declaration =>
            null;
         when others =>
            Failed ("Design_File_Chain", Target);
      end case;
   end Check_Kind_For_Design_File_Chain;

   function Get_Design_File_Chain (Library : Iir) return Iir_Design_File is
   begin
      Check_Kind_For_Design_File_Chain (Library);
      return Get_Field1 (Library);
   end Get_Design_File_Chain;

   procedure Set_Design_File_Chain (Library : Iir; Chain : Iir_Design_File) is
   begin
      Check_Kind_For_Design_File_Chain (Library);
      Set_Field1 (Library, Chain);
   end Set_Design_File_Chain;

   procedure Check_Kind_For_Library_Directory (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Library_Declaration =>
            null;
         when others =>
            Failed ("Library_Directory", Target);
      end case;
   end Check_Kind_For_Library_Directory;

   function Get_Library_Directory (Library : Iir) return Name_Id is
   begin
      Check_Kind_For_Library_Directory (Library);
      return Name_Id'Val (Get_Field11 (Library));
   end Get_Library_Directory;

   procedure Set_Library_Directory (Library : Iir; Dir : Name_Id) is
   begin
      Check_Kind_For_Library_Directory (Library);
      Set_Field11 (Library, Name_Id'Pos (Dir));
   end Set_Library_Directory;

   procedure Check_Kind_For_Date (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Library_Declaration =>
            null;
         when others =>
            Failed ("Date", Target);
      end case;
   end Check_Kind_For_Date;

   function Get_Date (Target : Iir) return Date_Type is
   begin
      Check_Kind_For_Date (Target);
      return Date_Type'Val (Get_Field10 (Target));
   end Get_Date;

   procedure Set_Date (Target : Iir; Date : Date_Type) is
   begin
      Check_Kind_For_Date (Target);
      Set_Field10 (Target, Date_Type'Pos (Date));
   end Set_Date;

   procedure Check_Kind_For_Context_Items (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Context_Items", Target);
      end case;
   end Check_Kind_For_Context_Items;

   function Get_Context_Items (Design_Unit : Iir) return Iir is
   begin
      Check_Kind_For_Context_Items (Design_Unit);
      return Get_Field1 (Design_Unit);
   end Get_Context_Items;

   procedure Set_Context_Items (Design_Unit : Iir; Items_Chain : Iir) is
   begin
      Check_Kind_For_Context_Items (Design_Unit);
      Set_Field1 (Design_Unit, Items_Chain);
   end Set_Context_Items;

   procedure Check_Kind_For_Dependence_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Dependence_List", Target);
      end case;
   end Check_Kind_For_Dependence_List;

   function Get_Dependence_List (Unit : Iir) return Iir_List is
   begin
      Check_Kind_For_Dependence_List (Unit);
      return Iir_To_Iir_List (Get_Field8 (Unit));
   end Get_Dependence_List;

   procedure Set_Dependence_List (Unit : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Dependence_List (Unit);
      Set_Field8 (Unit, Iir_List_To_Iir (List));
   end Set_Dependence_List;

   procedure Check_Kind_For_Analysis_Checks_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Analysis_Checks_List", Target);
      end case;
   end Check_Kind_For_Analysis_Checks_List;

   function Get_Analysis_Checks_List (Unit : Iir) return Iir_List is
   begin
      Check_Kind_For_Analysis_Checks_List (Unit);
      return Iir_To_Iir_List (Get_Field9 (Unit));
   end Get_Analysis_Checks_List;

   procedure Set_Analysis_Checks_List (Unit : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Analysis_Checks_List (Unit);
      Set_Field9 (Unit, Iir_List_To_Iir (List));
   end Set_Analysis_Checks_List;

   procedure Check_Kind_For_Date_State (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Date_State", Target);
      end case;
   end Check_Kind_For_Date_State;

   function Get_Date_State (Unit : Iir_Design_Unit) return Date_State_Type is
   begin
      Check_Kind_For_Date_State (Unit);
      return Date_State_Type'Val (Get_State1 (Unit));
   end Get_Date_State;

   procedure Set_Date_State (Unit : Iir_Design_Unit; State : Date_State_Type)
      is
   begin
      Check_Kind_For_Date_State (Unit);
      Set_State1 (Unit, Date_State_Type'Pos (State));
   end Set_Date_State;

   procedure Check_Kind_For_Guarded_Target_State (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Signal_Assignment_Statement =>
            null;
         when others =>
            Failed ("Guarded_Target_State", Target);
      end case;
   end Check_Kind_For_Guarded_Target_State;

   function Get_Guarded_Target_State (Stmt : Iir) return Tri_State_Type is
   begin
      Check_Kind_For_Guarded_Target_State (Stmt);
      return Tri_State_Type'Val (Get_State3 (Stmt));
   end Get_Guarded_Target_State;

   procedure Set_Guarded_Target_State (Stmt : Iir; State : Tri_State_Type) is
   begin
      Check_Kind_For_Guarded_Target_State (Stmt);
      Set_State3 (Stmt, Tri_State_Type'Pos (State));
   end Set_Guarded_Target_State;

   procedure Check_Kind_For_Library_Unit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Library_Unit", Target);
      end case;
   end Check_Kind_For_Library_Unit;

   function Get_Library_Unit (Design_Unit : Iir_Design_Unit) return Iir is
   begin
      Check_Kind_For_Library_Unit (Design_Unit);
      return Get_Field5 (Design_Unit);
   end Get_Library_Unit;

   procedure Set_Library_Unit (Design_Unit : Iir_Design_Unit; Lib_Unit : Iir)
      is
   begin
      Check_Kind_For_Library_Unit (Design_Unit);
      Set_Field5 (Design_Unit, Lib_Unit);
   end Set_Library_Unit;

   procedure Check_Kind_For_Hash_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Hash_Chain", Target);
      end case;
   end Check_Kind_For_Hash_Chain;

   function Get_Hash_Chain (Design_Unit : Iir_Design_Unit) return Iir is
   begin
      Check_Kind_For_Hash_Chain (Design_Unit);
      return Get_Field7 (Design_Unit);
   end Get_Hash_Chain;

   procedure Set_Hash_Chain (Design_Unit : Iir_Design_Unit; Chain : Iir) is
   begin
      Check_Kind_For_Hash_Chain (Design_Unit);
      Set_Field7 (Design_Unit, Chain);
   end Set_Hash_Chain;

   procedure Check_Kind_For_Value (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Integer_Literal
           | Iir_Kind_Physical_Int_Literal =>
            null;
         when others =>
            Failed ("Value", Target);
      end case;
   end Check_Kind_For_Value;

   function Get_Value (Lit : Iir) return Iir_Int64 is
   begin
      Check_Kind_For_Value (Lit);
      return Get_Int64 (Lit);
   end Get_Value;

   procedure Set_Value (Lit : Iir; Val : Iir_Int64) is
   begin
      Check_Kind_For_Value (Lit);
      Set_Int64 (Lit, Val);
   end Set_Value;

   procedure Check_Kind_For_Enum_Pos (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Enumeration_Literal =>
            null;
         when others =>
            Failed ("Enum_Pos", Target);
      end case;
   end Check_Kind_For_Enum_Pos;

   function Get_Enum_Pos (Lit : Iir) return Iir_Int32 is
   begin
      Check_Kind_For_Enum_Pos (Lit);
      return Iir_Int32'Val (Get_Field10 (Lit));
   end Get_Enum_Pos;

   procedure Set_Enum_Pos (Lit : Iir; Val : Iir_Int32) is
   begin
      Check_Kind_For_Enum_Pos (Lit);
      Set_Field10 (Lit, Iir_Int32'Pos (Val));
   end Set_Enum_Pos;

   procedure Check_Kind_For_Physical_Literal (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Unit_Declaration =>
            null;
         when others =>
            Failed ("Physical_Literal", Target);
      end case;
   end Check_Kind_For_Physical_Literal;

   function Get_Physical_Literal (Unit : Iir) return Iir is
   begin
      Check_Kind_For_Physical_Literal (Unit);
      return Get_Field6 (Unit);
   end Get_Physical_Literal;

   procedure Set_Physical_Literal (Unit : Iir; Lit : Iir) is
   begin
      Check_Kind_For_Physical_Literal (Unit);
      Set_Field6 (Unit, Lit);
   end Set_Physical_Literal;

   procedure Check_Kind_For_Physical_Unit_Value (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Unit_Declaration =>
            null;
         when others =>
            Failed ("Physical_Unit_Value", Target);
      end case;
   end Check_Kind_For_Physical_Unit_Value;

   function Get_Physical_Unit_Value (Unit : Iir) return Iir is
   begin
      Check_Kind_For_Physical_Unit_Value (Unit);
      return Get_Field7 (Unit);
   end Get_Physical_Unit_Value;

   procedure Set_Physical_Unit_Value (Unit : Iir; Lit : Iir) is
   begin
      Check_Kind_For_Physical_Unit_Value (Unit);
      Set_Field7 (Unit, Lit);
   end Set_Physical_Unit_Value;

   procedure Check_Kind_For_Fp_Value (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            null;
         when others =>
            Failed ("Fp_Value", Target);
      end case;
   end Check_Kind_For_Fp_Value;

   function Get_Fp_Value (Lit : Iir) return Iir_Fp64 is
   begin
      Check_Kind_For_Fp_Value (Lit);
      return Get_Fp64 (Lit);
   end Get_Fp_Value;

   procedure Set_Fp_Value (Lit : Iir; Val : Iir_Fp64) is
   begin
      Check_Kind_For_Fp_Value (Lit);
      Set_Fp64 (Lit, Val);
   end Set_Fp_Value;

   procedure Check_Kind_For_Enumeration_Decl (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Enumeration_Literal =>
            null;
         when others =>
            Failed ("Enumeration_Decl", Target);
      end case;
   end Check_Kind_For_Enumeration_Decl;

   function Get_Enumeration_Decl (Target : Iir) return Iir is
   begin
      Check_Kind_For_Enumeration_Decl (Target);
      return Get_Field6 (Target);
   end Get_Enumeration_Decl;

   procedure Set_Enumeration_Decl (Target : Iir; Lit : Iir) is
   begin
      Check_Kind_For_Enumeration_Decl (Target);
      Set_Field6 (Target, Lit);
   end Set_Enumeration_Decl;

   procedure Check_Kind_For_Simple_Aggregate_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Simple_Aggregate =>
            null;
         when others =>
            Failed ("Simple_Aggregate_List", Target);
      end case;
   end Check_Kind_For_Simple_Aggregate_List;

   function Get_Simple_Aggregate_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Simple_Aggregate_List (Target);
      return Iir_To_Iir_List (Get_Field3 (Target));
   end Get_Simple_Aggregate_List;

   procedure Set_Simple_Aggregate_List (Target : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Simple_Aggregate_List (Target);
      Set_Field3 (Target, Iir_List_To_Iir (List));
   end Set_Simple_Aggregate_List;

   procedure Check_Kind_For_Bit_String_Base (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Bit_String_Literal =>
            null;
         when others =>
            Failed ("Bit_String_Base", Target);
      end case;
   end Check_Kind_For_Bit_String_Base;

   function Get_Bit_String_Base (Lit : Iir) return Base_Type is
   begin
      Check_Kind_For_Bit_String_Base (Lit);
      return Base_Type'Val (Get_Field11 (Lit));
   end Get_Bit_String_Base;

   procedure Set_Bit_String_Base (Lit : Iir; Base : Base_Type) is
   begin
      Check_Kind_For_Bit_String_Base (Lit);
      Set_Field11 (Lit, Base_Type'Pos (Base));
   end Set_Bit_String_Base;

   procedure Check_Kind_For_Bit_String_0 (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Bit_String_Literal =>
            null;
         when others =>
            Failed ("Bit_String_0", Target);
      end case;
   end Check_Kind_For_Bit_String_0;

   function Get_Bit_String_0 (Lit : Iir) return Iir_Enumeration_Literal is
   begin
      Check_Kind_For_Bit_String_0 (Lit);
      return Get_Field4 (Lit);
   end Get_Bit_String_0;

   procedure Set_Bit_String_0 (Lit : Iir; El : Iir_Enumeration_Literal) is
   begin
      Check_Kind_For_Bit_String_0 (Lit);
      Set_Field4 (Lit, El);
   end Set_Bit_String_0;

   procedure Check_Kind_For_Bit_String_1 (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Bit_String_Literal =>
            null;
         when others =>
            Failed ("Bit_String_1", Target);
      end case;
   end Check_Kind_For_Bit_String_1;

   function Get_Bit_String_1 (Lit : Iir) return Iir_Enumeration_Literal is
   begin
      Check_Kind_For_Bit_String_1 (Lit);
      return Get_Field5 (Lit);
   end Get_Bit_String_1;

   procedure Set_Bit_String_1 (Lit : Iir; El : Iir_Enumeration_Literal) is
   begin
      Check_Kind_For_Bit_String_1 (Lit);
      Set_Field5 (Lit, El);
   end Set_Bit_String_1;

   procedure Check_Kind_For_Literal_Origin (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_String_Literal
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Bit_String_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Enumeration_Literal =>
            null;
         when others =>
            Failed ("Literal_Origin", Target);
      end case;
   end Check_Kind_For_Literal_Origin;

   function Get_Literal_Origin (Lit : Iir) return Iir is
   begin
      Check_Kind_For_Literal_Origin (Lit);
      return Get_Field2 (Lit);
   end Get_Literal_Origin;

   procedure Set_Literal_Origin (Lit : Iir; Orig : Iir) is
   begin
      Check_Kind_For_Literal_Origin (Lit);
      Set_Field2 (Lit, Orig);
   end Set_Literal_Origin;

   procedure Check_Kind_For_Proxy (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Proxy =>
            null;
         when others =>
            Failed ("Proxy", Target);
      end case;
   end Check_Kind_For_Proxy;

   function Get_Proxy (Target : Iir_Proxy) return Iir is
   begin
      Check_Kind_For_Proxy (Target);
      return Get_Field1 (Target);
   end Get_Proxy;

   procedure Set_Proxy (Target : Iir_Proxy; Proxy : Iir) is
   begin
      Check_Kind_For_Proxy (Target);
      Set_Field1 (Target, Proxy);
   end Set_Proxy;

   procedure Check_Kind_For_Entity_Class (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Entity_Class
           | Iir_Kind_Attribute_Specification =>
            null;
         when others =>
            Failed ("Entity_Class", Target);
      end case;
   end Check_Kind_For_Entity_Class;

   function Get_Entity_Class (Target : Iir) return Token_Type is
   begin
      Check_Kind_For_Entity_Class (Target);
      return Iir_To_Token_Type (Get_Field3 (Target));
   end Get_Entity_Class;

   procedure Set_Entity_Class (Target : Iir; Kind : Token_Type) is
   begin
      Check_Kind_For_Entity_Class (Target);
      Set_Field3 (Target, Token_Type_To_Iir (Kind));
   end Set_Entity_Class;

   procedure Check_Kind_For_Entity_Name_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Specification =>
            null;
         when others =>
            Failed ("Entity_Name_List", Target);
      end case;
   end Check_Kind_For_Entity_Name_List;

   function Get_Entity_Name_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Entity_Name_List (Target);
      return Iir_To_Iir_List (Get_Field1 (Target));
   end Get_Entity_Name_List;

   procedure Set_Entity_Name_List (Target : Iir; Names : Iir_List) is
   begin
      Check_Kind_For_Entity_Name_List (Target);
      Set_Field1 (Target, Iir_List_To_Iir (Names));
   end Set_Entity_Name_List;

   procedure Check_Kind_For_Attribute_Designator (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Specification =>
            null;
         when others =>
            Failed ("Attribute_Designator", Target);
      end case;
   end Check_Kind_For_Attribute_Designator;

   function Get_Attribute_Designator (Target : Iir) return Iir is
   begin
      Check_Kind_For_Attribute_Designator (Target);
      return Get_Field6 (Target);
   end Get_Attribute_Designator;

   procedure Set_Attribute_Designator (Target : Iir; Designator : Iir) is
   begin
      Check_Kind_For_Attribute_Designator (Target);
      Set_Field6 (Target, Designator);
   end Set_Attribute_Designator;

   procedure Check_Kind_For_Attribute_Specification_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Specification =>
            null;
         when others =>
            Failed ("Attribute_Specification_Chain", Target);
      end case;
   end Check_Kind_For_Attribute_Specification_Chain;

   function Get_Attribute_Specification_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Attribute_Specification_Chain (Target);
      return Get_Field7 (Target);
   end Get_Attribute_Specification_Chain;

   procedure Set_Attribute_Specification_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Attribute_Specification_Chain (Target);
      Set_Field7 (Target, Chain);
   end Set_Attribute_Specification_Chain;

   procedure Check_Kind_For_Attribute_Specification (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Value =>
            null;
         when others =>
            Failed ("Attribute_Specification", Target);
      end case;
   end Check_Kind_For_Attribute_Specification;

   function Get_Attribute_Specification (Val : Iir) return Iir is
   begin
      Check_Kind_For_Attribute_Specification (Val);
      return Get_Field4 (Val);
   end Get_Attribute_Specification;

   procedure Set_Attribute_Specification (Val : Iir; Attr : Iir) is
   begin
      Check_Kind_For_Attribute_Specification (Val);
      Set_Field4 (Val, Attr);
   end Set_Attribute_Specification;

   procedure Check_Kind_For_Signal_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Disconnection_Specification =>
            null;
         when others =>
            Failed ("Signal_List", Target);
      end case;
   end Check_Kind_For_Signal_List;

   function Get_Signal_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Signal_List (Target);
      return Iir_To_Iir_List (Get_Field4 (Target));
   end Get_Signal_List;

   procedure Set_Signal_List (Target : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Signal_List (Target);
      Set_Field4 (Target, Iir_List_To_Iir (List));
   end Set_Signal_List;

   procedure Check_Kind_For_Designated_Entity (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Value =>
            null;
         when others =>
            Failed ("Designated_Entity", Target);
      end case;
   end Check_Kind_For_Designated_Entity;

   function Get_Designated_Entity (Val : Iir_Attribute_Value) return Iir is
   begin
      Check_Kind_For_Designated_Entity (Val);
      return Get_Field3 (Val);
   end Get_Designated_Entity;

   procedure Set_Designated_Entity (Val : Iir_Attribute_Value; Entity : Iir)
      is
   begin
      Check_Kind_For_Designated_Entity (Val);
      Set_Field3 (Val, Entity);
   end Set_Designated_Entity;

   procedure Check_Kind_For_Formal (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open =>
            null;
         when others =>
            Failed ("Formal", Target);
      end case;
   end Check_Kind_For_Formal;

   function Get_Formal (Target : Iir) return Iir is
   begin
      Check_Kind_For_Formal (Target);
      return Get_Field1 (Target);
   end Get_Formal;

   procedure Set_Formal (Target : Iir; Formal : Iir) is
   begin
      Check_Kind_For_Formal (Target);
      Set_Field1 (Target, Formal);
   end Set_Formal;

   procedure Check_Kind_For_Actual (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Expression =>
            null;
         when others =>
            Failed ("Actual", Target);
      end case;
   end Check_Kind_For_Actual;

   function Get_Actual (Target : Iir) return Iir is
   begin
      Check_Kind_For_Actual (Target);
      return Get_Field3 (Target);
   end Get_Actual;

   procedure Set_Actual (Target : Iir; Actual : Iir) is
   begin
      Check_Kind_For_Actual (Target);
      Set_Field3 (Target, Actual);
   end Set_Actual;

   procedure Check_Kind_For_In_Conversion (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Expression =>
            null;
         when others =>
            Failed ("In_Conversion", Target);
      end case;
   end Check_Kind_For_In_Conversion;

   function Get_In_Conversion (Target : Iir) return Iir is
   begin
      Check_Kind_For_In_Conversion (Target);
      return Get_Field4 (Target);
   end Get_In_Conversion;

   procedure Set_In_Conversion (Target : Iir; Conv : Iir) is
   begin
      Check_Kind_For_In_Conversion (Target);
      Set_Field4 (Target, Conv);
   end Set_In_Conversion;

   procedure Check_Kind_For_Out_Conversion (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Expression =>
            null;
         when others =>
            Failed ("Out_Conversion", Target);
      end case;
   end Check_Kind_For_Out_Conversion;

   function Get_Out_Conversion (Target : Iir) return Iir is
   begin
      Check_Kind_For_Out_Conversion (Target);
      return Get_Field5 (Target);
   end Get_Out_Conversion;

   procedure Set_Out_Conversion (Target : Iir; Conv : Iir) is
   begin
      Check_Kind_For_Out_Conversion (Target);
      Set_Field5 (Target, Conv);
   end Set_Out_Conversion;

   procedure Check_Kind_For_Whole_Association_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open =>
            null;
         when others =>
            Failed ("Whole_Association_Flag", Target);
      end case;
   end Check_Kind_For_Whole_Association_Flag;

   function Get_Whole_Association_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Whole_Association_Flag (Target);
      return Get_Flag1 (Target);
   end Get_Whole_Association_Flag;

   procedure Set_Whole_Association_Flag (Target : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Whole_Association_Flag (Target);
      Set_Flag1 (Target, Flag);
   end Set_Whole_Association_Flag;

   procedure Check_Kind_For_Collapse_Signal_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open =>
            null;
         when others =>
            Failed ("Collapse_Signal_Flag", Target);
      end case;
   end Check_Kind_For_Collapse_Signal_Flag;

   function Get_Collapse_Signal_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Collapse_Signal_Flag (Target);
      return Get_Flag2 (Target);
   end Get_Collapse_Signal_Flag;

   procedure Set_Collapse_Signal_Flag (Target : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Collapse_Signal_Flag (Target);
      Set_Flag2 (Target, Flag);
   end Set_Collapse_Signal_Flag;

   procedure Check_Kind_For_Artificial_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_Open =>
            null;
         when others =>
            Failed ("Artificial_Flag", Target);
      end case;
   end Check_Kind_For_Artificial_Flag;

   function Get_Artificial_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Artificial_Flag (Target);
      return Get_Flag3 (Target);
   end Get_Artificial_Flag;

   procedure Set_Artificial_Flag (Target : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Artificial_Flag (Target);
      Set_Flag3 (Target, Flag);
   end Set_Artificial_Flag;

   procedure Check_Kind_For_Open_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Signal_Interface_Declaration =>
            null;
         when others =>
            Failed ("Open_Flag", Target);
      end case;
   end Check_Kind_For_Open_Flag;

   function Get_Open_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Open_Flag (Target);
      return Get_Flag3 (Target);
   end Get_Open_Flag;

   procedure Set_Open_Flag (Target : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Open_Flag (Target);
      Set_Flag3 (Target, Flag);
   end Set_Open_Flag;

   procedure Check_Kind_For_After_Drivers_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            null;
         when others =>
            Failed ("After_Drivers_Flag", Target);
      end case;
   end Check_Kind_For_After_Drivers_Flag;

   function Get_After_Drivers_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_After_Drivers_Flag (Target);
      return Get_Flag5 (Target);
   end Get_After_Drivers_Flag;

   procedure Set_After_Drivers_Flag (Target : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_After_Drivers_Flag (Target);
      Set_Flag5 (Target, Flag);
   end Set_After_Drivers_Flag;

   procedure Check_Kind_For_We_Value (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Waveform_Element =>
            null;
         when others =>
            Failed ("We_Value", Target);
      end case;
   end Check_Kind_For_We_Value;

   function Get_We_Value (We : Iir_Waveform_Element) return Iir is
   begin
      Check_Kind_For_We_Value (We);
      return Get_Field1 (We);
   end Get_We_Value;

   procedure Set_We_Value (We : Iir_Waveform_Element; An_Iir : Iir) is
   begin
      Check_Kind_For_We_Value (We);
      Set_Field1 (We, An_Iir);
   end Set_We_Value;

   procedure Check_Kind_For_Time (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Waveform_Element =>
            null;
         when others =>
            Failed ("Time", Target);
      end case;
   end Check_Kind_For_Time;

   function Get_Time (We : Iir_Waveform_Element) return Iir is
   begin
      Check_Kind_For_Time (We);
      return Get_Field3 (We);
   end Get_Time;

   procedure Set_Time (We : Iir_Waveform_Element; An_Iir : Iir) is
   begin
      Check_Kind_For_Time (We);
      Set_Field3 (We, An_Iir);
   end Set_Time;

   procedure Check_Kind_For_Associated (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name =>
            null;
         when others =>
            Failed ("Associated", Target);
      end case;
   end Check_Kind_For_Associated;

   function Get_Associated (Target : Iir) return Iir is
   begin
      Check_Kind_For_Associated (Target);
      return Get_Field1 (Target);
   end Get_Associated;

   procedure Set_Associated (Target : Iir; Associated : Iir) is
   begin
      Check_Kind_For_Associated (Target);
      Set_Field1 (Target, Associated);
   end Set_Associated;

   procedure Check_Kind_For_Same_Alternative_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name =>
            null;
         when others =>
            Failed ("Same_Alternative_Flag", Target);
      end case;
   end Check_Kind_For_Same_Alternative_Flag;

   function Get_Same_Alternative_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Same_Alternative_Flag (Target);
      return Get_Flag1 (Target);
   end Get_Same_Alternative_Flag;

   procedure Set_Same_Alternative_Flag (Target : Iir; Val : Boolean) is
   begin
      Check_Kind_For_Same_Alternative_Flag (Target);
      Set_Flag1 (Target, Val);
   end Set_Same_Alternative_Flag;

   procedure Check_Kind_For_Architecture (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Entity_Aspect_Entity =>
            null;
         when others =>
            Failed ("Architecture", Target);
      end case;
   end Check_Kind_For_Architecture;

   function Get_Architecture (Target : Iir_Entity_Aspect_Entity) return Iir is
   begin
      Check_Kind_For_Architecture (Target);
      return Get_Field2 (Target);
   end Get_Architecture;

   procedure Set_Architecture (Target : Iir_Entity_Aspect_Entity; Arch : Iir)
      is
   begin
      Check_Kind_For_Architecture (Target);
      Set_Field2 (Target, Arch);
   end Set_Architecture;

   procedure Check_Kind_For_Block_Specification (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Configuration =>
            null;
         when others =>
            Failed ("Block_Specification", Target);
      end case;
   end Check_Kind_For_Block_Specification;

   function Get_Block_Specification (Target : Iir) return Iir is
   begin
      Check_Kind_For_Block_Specification (Target);
      return Get_Field5 (Target);
   end Get_Block_Specification;

   procedure Set_Block_Specification (Target : Iir; Block : Iir) is
   begin
      Check_Kind_For_Block_Specification (Target);
      Set_Field5 (Target, Block);
   end Set_Block_Specification;

   procedure Check_Kind_For_Prev_Block_Configuration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Configuration =>
            null;
         when others =>
            Failed ("Prev_Block_Configuration", Target);
      end case;
   end Check_Kind_For_Prev_Block_Configuration;

   function Get_Prev_Block_Configuration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Prev_Block_Configuration (Target);
      return Get_Field4 (Target);
   end Get_Prev_Block_Configuration;

   procedure Set_Prev_Block_Configuration (Target : Iir; Block : Iir) is
   begin
      Check_Kind_For_Prev_Block_Configuration (Target);
      Set_Field4 (Target, Block);
   end Set_Prev_Block_Configuration;

   procedure Check_Kind_For_Configuration_Item_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Configuration =>
            null;
         when others =>
            Failed ("Configuration_Item_Chain", Target);
      end case;
   end Check_Kind_For_Configuration_Item_Chain;

   function Get_Configuration_Item_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Configuration_Item_Chain (Target);
      return Get_Field3 (Target);
   end Get_Configuration_Item_Chain;

   procedure Set_Configuration_Item_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Configuration_Item_Chain (Target);
      Set_Field3 (Target, Chain);
   end Set_Configuration_Item_Chain;

   procedure Check_Kind_For_Attribute_Value_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement =>
            null;
         when others =>
            Failed ("Attribute_Value_Chain", Target);
      end case;
   end Check_Kind_For_Attribute_Value_Chain;

   function Get_Attribute_Value_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Attribute_Value_Chain (Target);
      return Get_Field4 (Target);
   end Get_Attribute_Value_Chain;

   procedure Set_Attribute_Value_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Attribute_Value_Chain (Target);
      Set_Field4 (Target, Chain);
   end Set_Attribute_Value_Chain;

   procedure Check_Kind_For_Spec_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Value =>
            null;
         when others =>
            Failed ("Spec_Chain", Target);
      end case;
   end Check_Kind_For_Spec_Chain;

   function Get_Spec_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Spec_Chain (Target);
      return Get_Field0 (Target);
   end Get_Spec_Chain;

   procedure Set_Spec_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Spec_Chain (Target);
      Set_Field0 (Target, Chain);
   end Set_Spec_Chain;

   procedure Check_Kind_For_Attribute_Value_Spec_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Specification =>
            null;
         when others =>
            Failed ("Attribute_Value_Spec_Chain", Target);
      end case;
   end Check_Kind_For_Attribute_Value_Spec_Chain;

   function Get_Attribute_Value_Spec_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Attribute_Value_Spec_Chain (Target);
      return Get_Field4 (Target);
   end Get_Attribute_Value_Spec_Chain;

   procedure Set_Attribute_Value_Spec_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Attribute_Value_Spec_Chain (Target);
      Set_Field4 (Target, Chain);
   end Set_Attribute_Value_Spec_Chain;

   procedure Check_Kind_For_Entity (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Entity_Aspect_Entity
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Architecture_Declaration =>
            null;
         when others =>
            Failed ("Entity", Target);
      end case;
   end Check_Kind_For_Entity;

   function Get_Entity (Decl : Iir) return Iir is
   begin
      Check_Kind_For_Entity (Decl);
      return Get_Field4 (Decl);
   end Get_Entity;

   procedure Set_Entity (Decl : Iir; Entity : Iir) is
   begin
      Check_Kind_For_Entity (Decl);
      Set_Field4 (Decl, Entity);
   end Set_Entity;

   procedure Check_Kind_For_Package (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Package_Body =>
            null;
         when others =>
            Failed ("Package", Target);
      end case;
   end Check_Kind_For_Package;

   function Get_Package (Package_Body : Iir) return Iir_Package_Declaration is
   begin
      Check_Kind_For_Package (Package_Body);
      return Get_Field4 (Package_Body);
   end Get_Package;

   procedure Set_Package (Package_Body : Iir; Decl : Iir_Package_Declaration)
      is
   begin
      Check_Kind_For_Package (Package_Body);
      Set_Field4 (Package_Body, Decl);
   end Set_Package;

   procedure Check_Kind_For_Package_Body (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Package_Declaration =>
            null;
         when others =>
            Failed ("Package_Body", Target);
      end case;
   end Check_Kind_For_Package_Body;

   function Get_Package_Body (Pkg : Iir) return Iir_Package_Body is
   begin
      Check_Kind_For_Package_Body (Pkg);
      return Get_Field4 (Pkg);
   end Get_Package_Body;

   procedure Set_Package_Body (Pkg : Iir; Decl : Iir_Package_Body) is
   begin
      Check_Kind_For_Package_Body (Pkg);
      Set_Field4 (Pkg, Decl);
   end Set_Package_Body;

   procedure Check_Kind_For_Need_Body (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Package_Declaration =>
            null;
         when others =>
            Failed ("Need_Body", Target);
      end case;
   end Check_Kind_For_Need_Body;

   function Get_Need_Body (Decl : Iir_Package_Declaration) return Boolean is
   begin
      Check_Kind_For_Need_Body (Decl);
      return Get_Flag1 (Decl);
   end Get_Need_Body;

   procedure Set_Need_Body (Decl : Iir_Package_Declaration; Flag : Boolean) is
   begin
      Check_Kind_For_Need_Body (Decl);
      Set_Flag1 (Decl, Flag);
   end Set_Need_Body;

   procedure Check_Kind_For_Block_Configuration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Declaration =>
            null;
         when others =>
            Failed ("Block_Configuration", Target);
      end case;
   end Check_Kind_For_Block_Configuration;

   function Get_Block_Configuration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Block_Configuration (Target);
      return Get_Field5 (Target);
   end Get_Block_Configuration;

   procedure Set_Block_Configuration (Target : Iir; Block : Iir) is
   begin
      Check_Kind_For_Block_Configuration (Target);
      Set_Field5 (Target, Block);
   end Set_Block_Configuration;

   procedure Check_Kind_For_Concurrent_Statement_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement =>
            null;
         when others =>
            Failed ("Concurrent_Statement_Chain", Target);
      end case;
   end Check_Kind_For_Concurrent_Statement_Chain;

   function Get_Concurrent_Statement_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Concurrent_Statement_Chain (Target);
      return Get_Field5 (Target);
   end Get_Concurrent_Statement_Chain;

   procedure Set_Concurrent_Statement_Chain (Target : Iir; First : Iir) is
   begin
      Check_Kind_For_Concurrent_Statement_Chain (Target);
      Set_Field5 (Target, First);
   end Set_Concurrent_Statement_Chain;

   procedure Check_Kind_For_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Library_Clause
           | Iir_Kind_Use_Clause
           | Iir_Kind_Waveform_Element
           | Iir_Kind_Conditional_Waveform
           | Iir_Kind_Association_Element_By_Expression
           | Iir_Kind_Association_Element_By_Individual
           | Iir_Kind_Association_Element_Open
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Entity_Class
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            null;
         when others =>
            Failed ("Chain", Target);
      end case;
   end Check_Kind_For_Chain;

   function Get_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Chain (Target);
      return Get_Field2 (Target);
   end Get_Chain;

   procedure Set_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Chain (Target);
      Set_Field2 (Target, Chain);
   end Set_Chain;

   procedure Check_Kind_For_Port_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Header
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Component_Declaration =>
            null;
         when others =>
            Failed ("Port_Chain", Target);
      end case;
   end Check_Kind_For_Port_Chain;

   function Get_Port_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Port_Chain (Target);
      return Get_Field7 (Target);
   end Get_Port_Chain;

   procedure Set_Port_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Port_Chain (Target);
      Set_Field7 (Target, Chain);
   end Set_Port_Chain;

   procedure Check_Kind_For_Generic_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Header
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Component_Declaration =>
            null;
         when others =>
            Failed ("Generic_Chain", Target);
      end case;
   end Check_Kind_For_Generic_Chain;

   function Get_Generic_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Generic_Chain (Target);
      return Get_Field6 (Target);
   end Get_Generic_Chain;

   procedure Set_Generic_Chain (Target : Iir; Generics : Iir) is
   begin
      Check_Kind_For_Generic_Chain (Target);
      Set_Field6 (Target, Generics);
   end Set_Generic_Chain;

   procedure Check_Kind_For_Type (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Character_Literal
           | Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Null_Literal
           | Iir_Kind_String_Literal
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Bit_String_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Range_Expression
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call
           | Iir_Kind_Aggregate
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Psl_Expression
           | Iir_Kind_Return_Statement
           | Iir_Kind_Simple_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            null;
         when others =>
            Failed ("Type", Target);
      end case;
   end Check_Kind_For_Type;

   function Get_Type (Target : Iir) return Iir is
   begin
      Check_Kind_For_Type (Target);
      return Get_Field1 (Target);
   end Get_Type;

   procedure Set_Type (Target : Iir; Atype : Iir) is
   begin
      Check_Kind_For_Type (Target);
      Set_Field1 (Target, Atype);
   end Set_Type;

   procedure Check_Kind_For_Subtype_Definition (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Anonymous_Type_Declaration =>
            null;
         when others =>
            Failed ("Subtype_Definition", Target);
      end case;
   end Check_Kind_For_Subtype_Definition;

   function Get_Subtype_Definition (Target : Iir) return Iir is
   begin
      Check_Kind_For_Subtype_Definition (Target);
      return Get_Field4 (Target);
   end Get_Subtype_Definition;

   procedure Set_Subtype_Definition (Target : Iir; Def : Iir) is
   begin
      Check_Kind_For_Subtype_Definition (Target);
      Set_Field4 (Target, Def);
   end Set_Subtype_Definition;

   procedure Check_Kind_For_Mode (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_File_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            null;
         when others =>
            Failed ("Mode", Target);
      end case;
   end Check_Kind_For_Mode;

   function Get_Mode (Target : Iir) return Iir_Mode is
   begin
      Check_Kind_For_Mode (Target);
      return Iir_Mode'Val (Get_Odigit1 (Target));
   end Get_Mode;

   procedure Set_Mode (Target : Iir; Mode : Iir_Mode) is
   begin
      Check_Kind_For_Mode (Target);
      Set_Odigit1 (Target, Iir_Mode'Pos (Mode));
   end Set_Mode;

   procedure Check_Kind_For_Signal_Kind (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Signal_Interface_Declaration =>
            null;
         when others =>
            Failed ("Signal_Kind", Target);
      end case;
   end Check_Kind_For_Signal_Kind;

   function Get_Signal_Kind (Target : Iir) return Iir_Signal_Kind is
   begin
      Check_Kind_For_Signal_Kind (Target);
      return Iir_Signal_Kind'Val (Get_State3 (Target));
   end Get_Signal_Kind;

   procedure Set_Signal_Kind (Target : Iir; Signal_Kind : Iir_Signal_Kind) is
   begin
      Check_Kind_For_Signal_Kind (Target);
      Set_State3 (Target, Iir_Signal_Kind'Pos (Signal_Kind));
   end Set_Signal_Kind;

   procedure Check_Kind_For_Base_Name (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Value
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Function_Call
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Simple_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            null;
         when others =>
            Failed ("Base_Name", Target);
      end case;
   end Check_Kind_For_Base_Name;

   function Get_Base_Name (Target : Iir) return Iir is
   begin
      Check_Kind_For_Base_Name (Target);
      return Get_Field5 (Target);
   end Get_Base_Name;

   procedure Set_Base_Name (Target : Iir; Name : Iir) is
   begin
      Check_Kind_For_Base_Name (Target);
      Set_Field5 (Target, Name);
   end Set_Base_Name;

   procedure Check_Kind_For_Interface_Declaration_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Interface_Declaration_Chain", Target);
      end case;
   end Check_Kind_For_Interface_Declaration_Chain;

   function Get_Interface_Declaration_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Interface_Declaration_Chain (Target);
      return Get_Field5 (Target);
   end Get_Interface_Declaration_Chain;

   procedure Set_Interface_Declaration_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Interface_Declaration_Chain (Target);
      Set_Field5 (Target, Chain);
   end Set_Interface_Declaration_Chain;

   procedure Check_Kind_For_Subprogram_Specification (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            null;
         when others =>
            Failed ("Subprogram_Specification", Target);
      end case;
   end Check_Kind_For_Subprogram_Specification;

   function Get_Subprogram_Specification (Target : Iir) return Iir is
   begin
      Check_Kind_For_Subprogram_Specification (Target);
      return Get_Field4 (Target);
   end Get_Subprogram_Specification;

   procedure Set_Subprogram_Specification (Target : Iir; Spec : Iir) is
   begin
      Check_Kind_For_Subprogram_Specification (Target);
      Set_Field4 (Target, Spec);
   end Set_Subprogram_Specification;

   procedure Check_Kind_For_Sequential_Statement_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            null;
         when others =>
            Failed ("Sequential_Statement_Chain", Target);
      end case;
   end Check_Kind_For_Sequential_Statement_Chain;

   function Get_Sequential_Statement_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Sequential_Statement_Chain (Target);
      return Get_Field5 (Target);
   end Get_Sequential_Statement_Chain;

   procedure Set_Sequential_Statement_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Sequential_Statement_Chain (Target);
      Set_Field5 (Target, Chain);
   end Set_Sequential_Statement_Chain;

   procedure Check_Kind_For_Subprogram_Body (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Subprogram_Body", Target);
      end case;
   end Check_Kind_For_Subprogram_Body;

   function Get_Subprogram_Body (Target : Iir) return Iir is
   begin
      Check_Kind_For_Subprogram_Body (Target);
      return Get_Field6 (Target);
   end Get_Subprogram_Body;

   procedure Set_Subprogram_Body (Target : Iir; A_Body : Iir) is
   begin
      Check_Kind_For_Subprogram_Body (Target);
      Set_Field6 (Target, A_Body);
   end Set_Subprogram_Body;

   procedure Check_Kind_For_Overload_Number (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Overload_Number", Target);
      end case;
   end Check_Kind_For_Overload_Number;

   function Get_Overload_Number (Target : Iir) return Iir_Int32 is
   begin
      Check_Kind_For_Overload_Number (Target);
      return Iir_Int32'Val (Get_Field9 (Target));
   end Get_Overload_Number;

   procedure Set_Overload_Number (Target : Iir; Val : Iir_Int32) is
   begin
      Check_Kind_For_Overload_Number (Target);
      Set_Field9 (Target, Iir_Int32'Pos (Val));
   end Set_Overload_Number;

   procedure Check_Kind_For_Subprogram_Depth (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Subprogram_Depth", Target);
      end case;
   end Check_Kind_For_Subprogram_Depth;

   function Get_Subprogram_Depth (Target : Iir) return Iir_Int32 is
   begin
      Check_Kind_For_Subprogram_Depth (Target);
      return Iir_Int32'Val (Get_Field10 (Target));
   end Get_Subprogram_Depth;

   procedure Set_Subprogram_Depth (Target : Iir; Depth : Iir_Int32) is
   begin
      Check_Kind_For_Subprogram_Depth (Target);
      Set_Field10 (Target, Iir_Int32'Pos (Depth));
   end Set_Subprogram_Depth;

   procedure Check_Kind_For_Subprogram_Hash (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal =>
            null;
         when others =>
            Failed ("Subprogram_Hash", Target);
      end case;
   end Check_Kind_For_Subprogram_Hash;

   function Get_Subprogram_Hash (Target : Iir) return Iir_Int32 is
   begin
      Check_Kind_For_Subprogram_Hash (Target);
      return Iir_Int32'Val (Get_Field11 (Target));
   end Get_Subprogram_Hash;

   procedure Set_Subprogram_Hash (Target : Iir; Val : Iir_Int32) is
   begin
      Check_Kind_For_Subprogram_Hash (Target);
      Set_Field11 (Target, Iir_Int32'Pos (Val));
   end Set_Subprogram_Hash;

   procedure Check_Kind_For_Extra_Info (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            null;
         when others =>
            Failed ("Extra_Info", Target);
      end case;
   end Check_Kind_For_Extra_Info;

   function Get_Extra_Info (Target : Iir) return Iir_Int32 is
   begin
      Check_Kind_For_Extra_Info (Target);
      return Iir_Int32'Val (Get_Field8 (Target));
   end Get_Extra_Info;

   procedure Set_Extra_Info (Target : Iir; Info : Iir_Int32) is
   begin
      Check_Kind_For_Extra_Info (Target);
      Set_Field8 (Target, Iir_Int32'Pos (Info));
   end Set_Extra_Info;

   procedure Check_Kind_For_Impure_Depth (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            null;
         when others =>
            Failed ("Impure_Depth", Target);
      end case;
   end Check_Kind_For_Impure_Depth;

   function Get_Impure_Depth (Target : Iir) return Iir_Int32 is
   begin
      Check_Kind_For_Impure_Depth (Target);
      return Iir_To_Iir_Int32 (Get_Field3 (Target));
   end Get_Impure_Depth;

   procedure Set_Impure_Depth (Target : Iir; Depth : Iir_Int32) is
   begin
      Check_Kind_For_Impure_Depth (Target);
      Set_Field3 (Target, Iir_Int32_To_Iir (Depth));
   end Set_Impure_Depth;

   procedure Check_Kind_For_Return_Type (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Signature
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Enumeration_Literal =>
            null;
         when others =>
            Failed ("Return_Type", Target);
      end case;
   end Check_Kind_For_Return_Type;

   function Get_Return_Type (Target : Iir) return Iir is
   begin
      Check_Kind_For_Return_Type (Target);
      return Get_Field1 (Target);
   end Get_Return_Type;

   procedure Set_Return_Type (Target : Iir; Decl : Iir) is
   begin
      Check_Kind_For_Return_Type (Target);
      Set_Field1 (Target, Decl);
   end Set_Return_Type;

   procedure Check_Kind_For_Implicit_Definition (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Implicit_Definition", Target);
      end case;
   end Check_Kind_For_Implicit_Definition;

   function Get_Implicit_Definition (D : Iir) return Iir_Predefined_Functions
      is
   begin
      Check_Kind_For_Implicit_Definition (D);
      return Iir_Predefined_Functions'Val (Get_Field6 (D));
   end Get_Implicit_Definition;

   procedure Set_Implicit_Definition (D : Iir; Def : Iir_Predefined_Functions)
      is
   begin
      Check_Kind_For_Implicit_Definition (D);
      Set_Field6 (D, Iir_Predefined_Functions'Pos (Def));
   end Set_Implicit_Definition;

   procedure Check_Kind_For_Type_Reference (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Type_Reference", Target);
      end case;
   end Check_Kind_For_Type_Reference;

   function Get_Type_Reference (Target : Iir) return Iir is
   begin
      Check_Kind_For_Type_Reference (Target);
      return Get_Field10 (Target);
   end Get_Type_Reference;

   procedure Set_Type_Reference (Target : Iir; Decl : Iir) is
   begin
      Check_Kind_For_Type_Reference (Target);
      Set_Field10 (Target, Decl);
   end Set_Type_Reference;

   procedure Check_Kind_For_Default_Value (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            null;
         when others =>
            Failed ("Default_Value", Target);
      end case;
   end Check_Kind_For_Default_Value;

   function Get_Default_Value (Target : Iir) return Iir is
   begin
      Check_Kind_For_Default_Value (Target);
      return Get_Field6 (Target);
   end Get_Default_Value;

   procedure Set_Default_Value (Target : Iir; Value : Iir) is
   begin
      Check_Kind_For_Default_Value (Target);
      Set_Field6 (Target, Value);
   end Set_Default_Value;

   procedure Check_Kind_For_Deferred_Declaration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Constant_Declaration =>
            null;
         when others =>
            Failed ("Deferred_Declaration", Target);
      end case;
   end Check_Kind_For_Deferred_Declaration;

   function Get_Deferred_Declaration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Deferred_Declaration (Target);
      return Get_Field7 (Target);
   end Get_Deferred_Declaration;

   procedure Set_Deferred_Declaration (Target : Iir; Decl : Iir) is
   begin
      Check_Kind_For_Deferred_Declaration (Target);
      Set_Field7 (Target, Decl);
   end Set_Deferred_Declaration;

   procedure Check_Kind_For_Deferred_Declaration_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Constant_Declaration =>
            null;
         when others =>
            Failed ("Deferred_Declaration_Flag", Target);
      end case;
   end Check_Kind_For_Deferred_Declaration_Flag;

   function Get_Deferred_Declaration_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Deferred_Declaration_Flag (Target);
      return Get_Flag1 (Target);
   end Get_Deferred_Declaration_Flag;

   procedure Set_Deferred_Declaration_Flag (Target : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Deferred_Declaration_Flag (Target);
      Set_Flag1 (Target, Flag);
   end Set_Deferred_Declaration_Flag;

   procedure Check_Kind_For_Shared_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Variable_Declaration =>
            null;
         when others =>
            Failed ("Shared_Flag", Target);
      end case;
   end Check_Kind_For_Shared_Flag;

   function Get_Shared_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Shared_Flag (Target);
      return Get_Flag2 (Target);
   end Get_Shared_Flag;

   procedure Set_Shared_Flag (Target : Iir; Shared : Boolean) is
   begin
      Check_Kind_For_Shared_Flag (Target);
      Set_Flag2 (Target, Shared);
   end Set_Shared_Flag;

   procedure Check_Kind_For_Design_Unit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Configuration_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Declaration =>
            null;
         when others =>
            Failed ("Design_Unit", Target);
      end case;
   end Check_Kind_For_Design_Unit;

   function Get_Design_Unit (Target : Iir) return Iir_Design_Unit is
   begin
      Check_Kind_For_Design_Unit (Target);
      return Get_Field0 (Target);
   end Get_Design_Unit;

   procedure Set_Design_Unit (Target : Iir; Unit : Iir_Design_Unit) is
   begin
      Check_Kind_For_Design_Unit (Target);
      Set_Field0 (Target, Unit);
   end Set_Design_Unit;

   procedure Check_Kind_For_Block_Statement (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Guard_Signal_Declaration =>
            null;
         when others =>
            Failed ("Block_Statement", Target);
      end case;
   end Check_Kind_For_Block_Statement;

   function Get_Block_Statement (Target : Iir) return Iir_Block_Statement is
   begin
      Check_Kind_For_Block_Statement (Target);
      return Get_Field7 (Target);
   end Get_Block_Statement;

   procedure Set_Block_Statement (Target : Iir; Block : Iir_Block_Statement)
      is
   begin
      Check_Kind_For_Block_Statement (Target);
      Set_Field7 (Target, Block);
   end Set_Block_Statement;

   procedure Check_Kind_For_Signal_Driver (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Signal_Declaration =>
            null;
         when others =>
            Failed ("Signal_Driver", Target);
      end case;
   end Check_Kind_For_Signal_Driver;

   function Get_Signal_Driver (Target : Iir_Signal_Declaration) return Iir is
   begin
      Check_Kind_For_Signal_Driver (Target);
      return Get_Field7 (Target);
   end Get_Signal_Driver;

   procedure Set_Signal_Driver (Target : Iir_Signal_Declaration; Driver : Iir)
      is
   begin
      Check_Kind_For_Signal_Driver (Target);
      Set_Field7 (Target, Driver);
   end Set_Signal_Driver;

   procedure Check_Kind_For_Declaration_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Configuration
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement =>
            null;
         when others =>
            Failed ("Declaration_Chain", Target);
      end case;
   end Check_Kind_For_Declaration_Chain;

   function Get_Declaration_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Declaration_Chain (Target);
      return Get_Field1 (Target);
   end Get_Declaration_Chain;

   procedure Set_Declaration_Chain (Target : Iir; Decls : Iir) is
   begin
      Check_Kind_For_Declaration_Chain (Target);
      Set_Field1 (Target, Decls);
   end Set_Declaration_Chain;

   procedure Check_Kind_For_File_Logical_Name (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_File_Declaration =>
            null;
         when others =>
            Failed ("File_Logical_Name", Target);
      end case;
   end Check_Kind_For_File_Logical_Name;

   function Get_File_Logical_Name (Target : Iir_File_Declaration) return Iir
      is
   begin
      Check_Kind_For_File_Logical_Name (Target);
      return Get_Field6 (Target);
   end Get_File_Logical_Name;

   procedure Set_File_Logical_Name (Target : Iir_File_Declaration; Name : Iir)
      is
   begin
      Check_Kind_For_File_Logical_Name (Target);
      Set_Field6 (Target, Name);
   end Set_File_Logical_Name;

   procedure Check_Kind_For_File_Open_Kind (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_File_Declaration =>
            null;
         when others =>
            Failed ("File_Open_Kind", Target);
      end case;
   end Check_Kind_For_File_Open_Kind;

   function Get_File_Open_Kind (Target : Iir_File_Declaration) return Iir is
   begin
      Check_Kind_For_File_Open_Kind (Target);
      return Get_Field7 (Target);
   end Get_File_Open_Kind;

   procedure Set_File_Open_Kind (Target : Iir_File_Declaration; Kind : Iir) is
   begin
      Check_Kind_For_File_Open_Kind (Target);
      Set_Field7 (Target, Kind);
   end Set_File_Open_Kind;

   procedure Check_Kind_For_Element_Position (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Element_Declaration =>
            null;
         when others =>
            Failed ("Element_Position", Target);
      end case;
   end Check_Kind_For_Element_Position;

   function Get_Element_Position (Target : Iir) return Iir_Index32 is
   begin
      Check_Kind_For_Element_Position (Target);
      return Iir_Index32'Val (Get_Field4 (Target));
   end Get_Element_Position;

   procedure Set_Element_Position (Target : Iir; Pos : Iir_Index32) is
   begin
      Check_Kind_For_Element_Position (Target);
      Set_Field4 (Target, Iir_Index32'Pos (Pos));
   end Set_Element_Position;

   procedure Check_Kind_For_Element_Declaration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Record_Element_Constraint =>
            null;
         when others =>
            Failed ("Element_Declaration", Target);
      end case;
   end Check_Kind_For_Element_Declaration;

   function Get_Element_Declaration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Element_Declaration (Target);
      return Get_Field2 (Target);
   end Get_Element_Declaration;

   procedure Set_Element_Declaration (Target : Iir; El : Iir) is
   begin
      Check_Kind_For_Element_Declaration (Target);
      Set_Field2 (Target, El);
   end Set_Element_Declaration;

   procedure Check_Kind_For_Selected_Element (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Selected_Element =>
            null;
         when others =>
            Failed ("Selected_Element", Target);
      end case;
   end Check_Kind_For_Selected_Element;

   function Get_Selected_Element (Target : Iir) return Iir is
   begin
      Check_Kind_For_Selected_Element (Target);
      return Get_Field2 (Target);
   end Get_Selected_Element;

   procedure Set_Selected_Element (Target : Iir; El : Iir) is
   begin
      Check_Kind_For_Selected_Element (Target);
      Set_Field2 (Target, El);
   end Set_Selected_Element;

   procedure Check_Kind_For_Suffix_Identifier (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Selected_Name =>
            null;
         when others =>
            Failed ("Suffix_Identifier", Target);
      end case;
   end Check_Kind_For_Suffix_Identifier;

   function Get_Suffix_Identifier (Target : Iir) return Name_Id is
   begin
      Check_Kind_For_Suffix_Identifier (Target);
      return Iir_To_Name_Id (Get_Field2 (Target));
   end Get_Suffix_Identifier;

   procedure Set_Suffix_Identifier (Target : Iir; Ident : Name_Id) is
   begin
      Check_Kind_For_Suffix_Identifier (Target);
      Set_Field2 (Target, Name_Id_To_Iir (Ident));
   end Set_Suffix_Identifier;

   procedure Check_Kind_For_Attribute_Identifier (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Name =>
            null;
         when others =>
            Failed ("Attribute_Identifier", Target);
      end case;
   end Check_Kind_For_Attribute_Identifier;

   function Get_Attribute_Identifier (Target : Iir) return Name_Id is
   begin
      Check_Kind_For_Attribute_Identifier (Target);
      return Iir_To_Name_Id (Get_Field2 (Target));
   end Get_Attribute_Identifier;

   procedure Set_Attribute_Identifier (Target : Iir; Ident : Name_Id) is
   begin
      Check_Kind_For_Attribute_Identifier (Target);
      Set_Field2 (Target, Name_Id_To_Iir (Ident));
   end Set_Attribute_Identifier;

   procedure Check_Kind_For_Use_Clause_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Use_Clause =>
            null;
         when others =>
            Failed ("Use_Clause_Chain", Target);
      end case;
   end Check_Kind_For_Use_Clause_Chain;

   function Get_Use_Clause_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Use_Clause_Chain (Target);
      return Get_Field3 (Target);
   end Get_Use_Clause_Chain;

   procedure Set_Use_Clause_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Use_Clause_Chain (Target);
      Set_Field3 (Target, Chain);
   end Set_Use_Clause_Chain;

   procedure Check_Kind_For_Selected_Name (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Use_Clause =>
            null;
         when others =>
            Failed ("Selected_Name", Target);
      end case;
   end Check_Kind_For_Selected_Name;

   function Get_Selected_Name (Target : Iir_Use_Clause) return Iir is
   begin
      Check_Kind_For_Selected_Name (Target);
      return Get_Field1 (Target);
   end Get_Selected_Name;

   procedure Set_Selected_Name (Target : Iir_Use_Clause; Name : Iir) is
   begin
      Check_Kind_For_Selected_Name (Target);
      Set_Field1 (Target, Name);
   end Set_Selected_Name;

   procedure Check_Kind_For_Type_Declarator (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Type_Declarator", Target);
      end case;
   end Check_Kind_For_Type_Declarator;

   function Get_Type_Declarator (Target : Iir) return Iir is
   begin
      Check_Kind_For_Type_Declarator (Target);
      return Get_Field3 (Target);
   end Get_Type_Declarator;

   procedure Set_Type_Declarator (Target : Iir; Decl : Iir) is
   begin
      Check_Kind_For_Type_Declarator (Target);
      Set_Field3 (Target, Decl);
   end Set_Type_Declarator;

   procedure Check_Kind_For_Enumeration_Literal_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Enumeration_Type_Definition =>
            null;
         when others =>
            Failed ("Enumeration_Literal_List", Target);
      end case;
   end Check_Kind_For_Enumeration_Literal_List;

   function Get_Enumeration_Literal_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Enumeration_Literal_List (Target);
      return Iir_To_Iir_List (Get_Field2 (Target));
   end Get_Enumeration_Literal_List;

   procedure Set_Enumeration_Literal_List (Target : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Enumeration_Literal_List (Target);
      Set_Field2 (Target, Iir_List_To_Iir (List));
   end Set_Enumeration_Literal_List;

   procedure Check_Kind_For_Entity_Class_Entry_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Group_Template_Declaration =>
            null;
         when others =>
            Failed ("Entity_Class_Entry_Chain", Target);
      end case;
   end Check_Kind_For_Entity_Class_Entry_Chain;

   function Get_Entity_Class_Entry_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Entity_Class_Entry_Chain (Target);
      return Get_Field1 (Target);
   end Get_Entity_Class_Entry_Chain;

   procedure Set_Entity_Class_Entry_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Entity_Class_Entry_Chain (Target);
      Set_Field1 (Target, Chain);
   end Set_Entity_Class_Entry_Chain;

   procedure Check_Kind_For_Group_Constituent_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Group_Declaration =>
            null;
         when others =>
            Failed ("Group_Constituent_List", Target);
      end case;
   end Check_Kind_For_Group_Constituent_List;

   function Get_Group_Constituent_List (Group : Iir) return Iir_List is
   begin
      Check_Kind_For_Group_Constituent_List (Group);
      return Iir_To_Iir_List (Get_Field1 (Group));
   end Get_Group_Constituent_List;

   procedure Set_Group_Constituent_List (Group : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Group_Constituent_List (Group);
      Set_Field1 (Group, Iir_List_To_Iir (List));
   end Set_Group_Constituent_List;

   procedure Check_Kind_For_Unit_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Unit_Chain", Target);
      end case;
   end Check_Kind_For_Unit_Chain;

   function Get_Unit_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Unit_Chain (Target);
      return Get_Field1 (Target);
   end Get_Unit_Chain;

   procedure Set_Unit_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Unit_Chain (Target);
      Set_Field1 (Target, Chain);
   end Set_Unit_Chain;

   procedure Check_Kind_For_Primary_Unit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Primary_Unit", Target);
      end case;
   end Check_Kind_For_Primary_Unit;

   function Get_Primary_Unit (Target : Iir) return Iir is
   begin
      Check_Kind_For_Primary_Unit (Target);
      return Get_Field1 (Target);
   end Get_Primary_Unit;

   procedure Check_Kind_For_Identifier (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Library_Clause
           | Iir_Kind_Character_Literal
           | Iir_Kind_Operator_Symbol
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Simple_Name =>
            null;
         when others =>
            Failed ("Identifier", Target);
      end case;
   end Check_Kind_For_Identifier;

   function Get_Identifier (Target : Iir) return Name_Id is
   begin
      Check_Kind_For_Identifier (Target);
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Identifier;

   procedure Set_Identifier (Target : Iir; Identifier : Name_Id) is
   begin
      Check_Kind_For_Identifier (Target);
      Set_Field3 (Target, Name_Id_To_Iir (Identifier));
   end Set_Identifier;

   procedure Check_Kind_For_Label (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement =>
            null;
         when others =>
            Failed ("Label", Target);
      end case;
   end Check_Kind_For_Label;

   function Get_Label (Target : Iir) return Name_Id is
   begin
      Check_Kind_For_Label (Target);
      return Iir_To_Name_Id (Get_Field3 (Target));
   end Get_Label;

   procedure Set_Label (Target : Iir; Label : Name_Id) is
   begin
      Check_Kind_For_Label (Target);
      Set_Field3 (Target, Name_Id_To_Iir (Label));
   end Set_Label;

   procedure Check_Kind_For_Visible_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Library_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Element_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement =>
            null;
         when others =>
            Failed ("Visible_Flag", Target);
      end case;
   end Check_Kind_For_Visible_Flag;

   function Get_Visible_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Visible_Flag (Target);
      return Get_Flag4 (Target);
   end Get_Visible_Flag;

   procedure Set_Visible_Flag (Target : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Visible_Flag (Target);
      Set_Flag4 (Target, Flag);
   end Set_Visible_Flag;

   procedure Check_Kind_For_Range_Constraint (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Subtype_Definition =>
            null;
         when others =>
            Failed ("Range_Constraint", Target);
      end case;
   end Check_Kind_For_Range_Constraint;

   function Get_Range_Constraint (Target : Iir) return Iir is
   begin
      Check_Kind_For_Range_Constraint (Target);
      return Get_Field1 (Target);
   end Get_Range_Constraint;

   procedure Set_Range_Constraint (Target : Iir; Constraint : Iir) is
   begin
      Check_Kind_For_Range_Constraint (Target);
      Set_Field1 (Target, Constraint);
   end Set_Range_Constraint;

   procedure Check_Kind_For_Direction (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Range_Expression =>
            null;
         when others =>
            Failed ("Direction", Target);
      end case;
   end Check_Kind_For_Direction;

   function Get_Direction (Decl : Iir) return Iir_Direction is
   begin
      Check_Kind_For_Direction (Decl);
      return Iir_Direction'Val (Get_State2 (Decl));
   end Get_Direction;

   procedure Set_Direction (Decl : Iir; Dir : Iir_Direction) is
   begin
      Check_Kind_For_Direction (Decl);
      Set_State2 (Decl, Iir_Direction'Pos (Dir));
   end Set_Direction;

   procedure Check_Kind_For_Left_Limit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Range_Expression =>
            null;
         when others =>
            Failed ("Left_Limit", Target);
      end case;
   end Check_Kind_For_Left_Limit;

   function Get_Left_Limit (Decl : Iir_Range_Expression) return Iir is
   begin
      Check_Kind_For_Left_Limit (Decl);
      return Get_Field2 (Decl);
   end Get_Left_Limit;

   procedure Set_Left_Limit (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      Check_Kind_For_Left_Limit (Decl);
      Set_Field2 (Decl, Limit);
   end Set_Left_Limit;

   procedure Check_Kind_For_Right_Limit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Range_Expression =>
            null;
         when others =>
            Failed ("Right_Limit", Target);
      end case;
   end Check_Kind_For_Right_Limit;

   function Get_Right_Limit (Decl : Iir_Range_Expression) return Iir is
   begin
      Check_Kind_For_Right_Limit (Decl);
      return Get_Field3 (Decl);
   end Get_Right_Limit;

   procedure Set_Right_Limit (Decl : Iir_Range_Expression; Limit : Iir) is
   begin
      Check_Kind_For_Right_Limit (Decl);
      Set_Field3 (Decl, Limit);
   end Set_Right_Limit;

   procedure Check_Kind_For_Base_Type (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Base_Type", Target);
      end case;
   end Check_Kind_For_Base_Type;

   function Get_Base_Type (Decl : Iir) return Iir is
   begin
      Check_Kind_For_Base_Type (Decl);
      return Get_Field4 (Decl);
   end Get_Base_Type;

   procedure Set_Base_Type (Decl : Iir; Base_Type : Iir) is
   begin
      Check_Kind_For_Base_Type (Decl);
      Set_Field4 (Decl, Base_Type);
   end Set_Base_Type;

   procedure Check_Kind_For_Resolution_Function (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Subtype_Definition =>
            null;
         when others =>
            Failed ("Resolution_Function", Target);
      end case;
   end Check_Kind_For_Resolution_Function;

   function Get_Resolution_Function (Decl : Iir) return Iir is
   begin
      Check_Kind_For_Resolution_Function (Decl);
      return Get_Field5 (Decl);
   end Get_Resolution_Function;

   procedure Set_Resolution_Function (Decl : Iir; Func : Iir) is
   begin
      Check_Kind_For_Resolution_Function (Decl);
      Set_Field5 (Decl, Func);
   end Set_Resolution_Function;

   procedure Check_Kind_For_Text_File_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_File_Type_Definition =>
            null;
         when others =>
            Failed ("Text_File_Flag", Target);
      end case;
   end Check_Kind_For_Text_File_Flag;

   function Get_Text_File_Flag (Atype : Iir) return Boolean is
   begin
      Check_Kind_For_Text_File_Flag (Atype);
      return Get_Flag4 (Atype);
   end Get_Text_File_Flag;

   procedure Set_Text_File_Flag (Atype : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Text_File_Flag (Atype);
      Set_Flag4 (Atype, Flag);
   end Set_Text_File_Flag;

   procedure Check_Kind_For_Only_Characters_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Enumeration_Type_Definition =>
            null;
         when others =>
            Failed ("Only_Characters_Flag", Target);
      end case;
   end Check_Kind_For_Only_Characters_Flag;

   function Get_Only_Characters_Flag (Atype : Iir) return Boolean is
   begin
      Check_Kind_For_Only_Characters_Flag (Atype);
      return Get_Flag4 (Atype);
   end Get_Only_Characters_Flag;

   procedure Set_Only_Characters_Flag (Atype : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Only_Characters_Flag (Atype);
      Set_Flag4 (Atype, Flag);
   end Set_Only_Characters_Flag;

   procedure Check_Kind_For_Type_Staticness (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Type_Staticness", Target);
      end case;
   end Check_Kind_For_Type_Staticness;

   function Get_Type_Staticness (Atype : Iir) return Iir_Staticness is
   begin
      Check_Kind_For_Type_Staticness (Atype);
      return Iir_Staticness'Val (Get_State1 (Atype));
   end Get_Type_Staticness;

   procedure Set_Type_Staticness (Atype : Iir; Static : Iir_Staticness) is
   begin
      Check_Kind_For_Type_Staticness (Atype);
      Set_State1 (Atype, Iir_Staticness'Pos (Static));
   end Set_Type_Staticness;

   procedure Check_Kind_For_Constraint_State (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            null;
         when others =>
            Failed ("Constraint_State", Target);
      end case;
   end Check_Kind_For_Constraint_State;

   function Get_Constraint_State (Atype : Iir) return Iir_Constraint is
   begin
      Check_Kind_For_Constraint_State (Atype);
      return Iir_Constraint'Val (Get_State2 (Atype));
   end Get_Constraint_State;

   procedure Set_Constraint_State (Atype : Iir; State : Iir_Constraint) is
   begin
      Check_Kind_For_Constraint_State (Atype);
      Set_State2 (Atype, Iir_Constraint'Pos (State));
   end Set_Constraint_State;

   procedure Check_Kind_For_Index_Subtype_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            null;
         when others =>
            Failed ("Index_Subtype_List", Target);
      end case;
   end Check_Kind_For_Index_Subtype_List;

   function Get_Index_Subtype_List (Decl : Iir) return Iir_List is
   begin
      Check_Kind_For_Index_Subtype_List (Decl);
      return Iir_To_Iir_List (Get_Field6 (Decl));
   end Get_Index_Subtype_List;

   procedure Set_Index_Subtype_List (Decl : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Index_Subtype_List (Decl);
      Set_Field6 (Decl, Iir_List_To_Iir (List));
   end Set_Index_Subtype_List;

   procedure Check_Kind_For_Index_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Indexed_Name =>
            null;
         when others =>
            Failed ("Index_List", Target);
      end case;
   end Check_Kind_For_Index_List;

   function Get_Index_List (Decl : Iir) return Iir_List is
   begin
      Check_Kind_For_Index_List (Decl);
      return Iir_To_Iir_List (Get_Field2 (Decl));
   end Get_Index_List;

   procedure Set_Index_List (Decl : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Index_List (Decl);
      Set_Field2 (Decl, Iir_List_To_Iir (List));
   end Set_Index_List;

   procedure Check_Kind_For_Element_Subtype (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            null;
         when others =>
            Failed ("Element_Subtype", Target);
      end case;
   end Check_Kind_For_Element_Subtype;

   function Get_Element_Subtype (Decl : Iir) return Iir is
   begin
      Check_Kind_For_Element_Subtype (Decl);
      return Get_Field1 (Decl);
   end Get_Element_Subtype;

   procedure Set_Element_Subtype (Decl : Iir; Sub_Type : Iir) is
   begin
      Check_Kind_For_Element_Subtype (Decl);
      Set_Field1 (Decl, Sub_Type);
   end Set_Element_Subtype;

   procedure Check_Kind_For_Elements_Declaration_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            null;
         when others =>
            Failed ("Elements_Declaration_List", Target);
      end case;
   end Check_Kind_For_Elements_Declaration_List;

   function Get_Elements_Declaration_List (Decl : Iir) return Iir_List is
   begin
      Check_Kind_For_Elements_Declaration_List (Decl);
      return Iir_To_Iir_List (Get_Field1 (Decl));
   end Get_Elements_Declaration_List;

   procedure Set_Elements_Declaration_List (Decl : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Elements_Declaration_List (Decl);
      Set_Field1 (Decl, Iir_List_To_Iir (List));
   end Set_Elements_Declaration_List;

   procedure Check_Kind_For_Designated_Type (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Access_Type_Definition =>
            null;
         when others =>
            Failed ("Designated_Type", Target);
      end case;
   end Check_Kind_For_Designated_Type;

   function Get_Designated_Type (Target : Iir) return Iir is
   begin
      Check_Kind_For_Designated_Type (Target);
      return Get_Field2 (Target);
   end Get_Designated_Type;

   procedure Set_Designated_Type (Target : Iir; Dtype : Iir) is
   begin
      Check_Kind_For_Designated_Type (Target);
      Set_Field2 (Target, Dtype);
   end Set_Designated_Type;

   procedure Check_Kind_For_Target (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Variable_Assignment_Statement =>
            null;
         when others =>
            Failed ("Target", Target);
      end case;
   end Check_Kind_For_Target;

   function Get_Target (Target : Iir) return Iir is
   begin
      Check_Kind_For_Target (Target);
      return Get_Field1 (Target);
   end Get_Target;

   procedure Set_Target (Target : Iir; Atarget : Iir) is
   begin
      Check_Kind_For_Target (Target);
      Set_Field1 (Target, Atarget);
   end Set_Target;

   procedure Check_Kind_For_Waveform_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Conditional_Waveform
           | Iir_Kind_Signal_Assignment_Statement =>
            null;
         when others =>
            Failed ("Waveform_Chain", Target);
      end case;
   end Check_Kind_For_Waveform_Chain;

   function Get_Waveform_Chain (Target : Iir) return Iir_Waveform_Element is
   begin
      Check_Kind_For_Waveform_Chain (Target);
      return Get_Field5 (Target);
   end Get_Waveform_Chain;

   procedure Set_Waveform_Chain (Target : Iir; Chain : Iir_Waveform_Element)
      is
   begin
      Check_Kind_For_Waveform_Chain (Target);
      Set_Field5 (Target, Chain);
   end Set_Waveform_Chain;

   procedure Check_Kind_For_Guard (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            null;
         when others =>
            Failed ("Guard", Target);
      end case;
   end Check_Kind_For_Guard;

   function Get_Guard (Target : Iir) return Iir is
   begin
      Check_Kind_For_Guard (Target);
      return Get_Field8 (Target);
   end Get_Guard;

   procedure Set_Guard (Target : Iir; Guard : Iir) is
   begin
      Check_Kind_For_Guard (Target);
      Set_Field8 (Target, Guard);
   end Set_Guard;

   procedure Check_Kind_For_Delay_Mechanism (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Signal_Assignment_Statement =>
            null;
         when others =>
            Failed ("Delay_Mechanism", Target);
      end case;
   end Check_Kind_For_Delay_Mechanism;

   function Get_Delay_Mechanism (Target : Iir) return Iir_Delay_Mechanism is
   begin
      Check_Kind_For_Delay_Mechanism (Target);
      return Iir_Delay_Mechanism'Val (Get_Field12 (Target));
   end Get_Delay_Mechanism;

   procedure Set_Delay_Mechanism (Target : Iir; Kind : Iir_Delay_Mechanism) is
   begin
      Check_Kind_For_Delay_Mechanism (Target);
      Set_Field12 (Target, Iir_Delay_Mechanism'Pos (Kind));
   end Set_Delay_Mechanism;

   procedure Check_Kind_For_Reject_Time_Expression (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Signal_Assignment_Statement =>
            null;
         when others =>
            Failed ("Reject_Time_Expression", Target);
      end case;
   end Check_Kind_For_Reject_Time_Expression;

   function Get_Reject_Time_Expression (Target : Iir) return Iir is
   begin
      Check_Kind_For_Reject_Time_Expression (Target);
      return Get_Field6 (Target);
   end Get_Reject_Time_Expression;

   procedure Set_Reject_Time_Expression (Target : Iir; Expr : Iir) is
   begin
      Check_Kind_For_Reject_Time_Expression (Target);
      Set_Field6 (Target, Expr);
   end Set_Reject_Time_Expression;

   procedure Check_Kind_For_Sensitivity_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Wait_Statement =>
            null;
         when others =>
            Failed ("Sensitivity_List", Target);
      end case;
   end Check_Kind_For_Sensitivity_List;

   function Get_Sensitivity_List (Wait : Iir) return Iir_List is
   begin
      Check_Kind_For_Sensitivity_List (Wait);
      return Iir_To_Iir_List (Get_Field6 (Wait));
   end Get_Sensitivity_List;

   procedure Set_Sensitivity_List (Wait : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Sensitivity_List (Wait);
      Set_Field6 (Wait, Iir_List_To_Iir (List));
   end Set_Sensitivity_List;

   procedure Check_Kind_For_Condition_Clause (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Wait_Statement =>
            null;
         when others =>
            Failed ("Condition_Clause", Target);
      end case;
   end Check_Kind_For_Condition_Clause;

   function Get_Condition_Clause (Wait : Iir_Wait_Statement) return Iir is
   begin
      Check_Kind_For_Condition_Clause (Wait);
      return Get_Field5 (Wait);
   end Get_Condition_Clause;

   procedure Set_Condition_Clause (Wait : Iir_Wait_Statement; Cond : Iir) is
   begin
      Check_Kind_For_Condition_Clause (Wait);
      Set_Field5 (Wait, Cond);
   end Set_Condition_Clause;

   procedure Check_Kind_For_Timeout_Clause (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Wait_Statement =>
            null;
         when others =>
            Failed ("Timeout_Clause", Target);
      end case;
   end Check_Kind_For_Timeout_Clause;

   function Get_Timeout_Clause (Wait : Iir_Wait_Statement) return Iir is
   begin
      Check_Kind_For_Timeout_Clause (Wait);
      return Get_Field1 (Wait);
   end Get_Timeout_Clause;

   procedure Set_Timeout_Clause (Wait : Iir_Wait_Statement; Timeout : Iir) is
   begin
      Check_Kind_For_Timeout_Clause (Wait);
      Set_Field1 (Wait, Timeout);
   end Set_Timeout_Clause;

   procedure Check_Kind_For_Postponed_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement =>
            null;
         when others =>
            Failed ("Postponed_Flag", Target);
      end case;
   end Check_Kind_For_Postponed_Flag;

   function Get_Postponed_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Postponed_Flag (Target);
      return Get_Flag3 (Target);
   end Get_Postponed_Flag;

   procedure Set_Postponed_Flag (Target : Iir; Value : Boolean) is
   begin
      Check_Kind_For_Postponed_Flag (Target);
      Set_Flag3 (Target, Value);
   end Set_Postponed_Flag;

   procedure Check_Kind_For_Callees_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            null;
         when others =>
            Failed ("Callees_List", Target);
      end case;
   end Check_Kind_For_Callees_List;

   function Get_Callees_List (Proc : Iir) return Iir_List is
   begin
      Check_Kind_For_Callees_List (Proc);
      return Iir_To_Iir_List (Get_Field7 (Proc));
   end Get_Callees_List;

   procedure Set_Callees_List (Proc : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Callees_List (Proc);
      Set_Field7 (Proc, Iir_List_To_Iir (List));
   end Set_Callees_List;

   procedure Check_Kind_For_Passive_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            null;
         when others =>
            Failed ("Passive_Flag", Target);
      end case;
   end Check_Kind_For_Passive_Flag;

   function Get_Passive_Flag (Proc : Iir) return Boolean is
   begin
      Check_Kind_For_Passive_Flag (Proc);
      return Get_Flag2 (Proc);
   end Get_Passive_Flag;

   procedure Set_Passive_Flag (Proc : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Passive_Flag (Proc);
      Set_Flag2 (Proc, Flag);
   end Set_Passive_Flag;

   procedure Check_Kind_For_Resolution_Function_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration =>
            null;
         when others =>
            Failed ("Resolution_Function_Flag", Target);
      end case;
   end Check_Kind_For_Resolution_Function_Flag;

   function Get_Resolution_Function_Flag (Func : Iir) return Boolean is
   begin
      Check_Kind_For_Resolution_Function_Flag (Func);
      return Get_Flag7 (Func);
   end Get_Resolution_Function_Flag;

   procedure Set_Resolution_Function_Flag (Func : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Resolution_Function_Flag (Func);
      Set_Flag7 (Func, Flag);
   end Set_Resolution_Function_Flag;

   procedure Check_Kind_For_Wait_State (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            null;
         when others =>
            Failed ("Wait_State", Target);
      end case;
   end Check_Kind_For_Wait_State;

   function Get_Wait_State (Proc : Iir) return Tri_State_Type is
   begin
      Check_Kind_For_Wait_State (Proc);
      return Tri_State_Type'Val (Get_State1 (Proc));
   end Get_Wait_State;

   procedure Set_Wait_State (Proc : Iir; State : Tri_State_Type) is
   begin
      Check_Kind_For_Wait_State (Proc);
      Set_State1 (Proc, Tri_State_Type'Pos (State));
   end Set_Wait_State;

   procedure Check_Kind_For_All_Sensitized_State (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            null;
         when others =>
            Failed ("All_Sensitized_State", Target);
      end case;
   end Check_Kind_For_All_Sensitized_State;

   function Get_All_Sensitized_State (Proc : Iir) return Iir_All_Sensitized is
   begin
      Check_Kind_For_All_Sensitized_State (Proc);
      return Iir_All_Sensitized'Val (Get_State3 (Proc));
   end Get_All_Sensitized_State;

   procedure Set_All_Sensitized_State (Proc : Iir; State : Iir_All_Sensitized)
      is
   begin
      Check_Kind_For_All_Sensitized_State (Proc);
      Set_State3 (Proc, Iir_All_Sensitized'Pos (State));
   end Set_All_Sensitized_State;

   procedure Check_Kind_For_Seen_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement =>
            null;
         when others =>
            Failed ("Seen_Flag", Target);
      end case;
   end Check_Kind_For_Seen_Flag;

   function Get_Seen_Flag (Proc : Iir) return Boolean is
   begin
      Check_Kind_For_Seen_Flag (Proc);
      return Get_Flag1 (Proc);
   end Get_Seen_Flag;

   procedure Set_Seen_Flag (Proc : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Seen_Flag (Proc);
      Set_Flag1 (Proc, Flag);
   end Set_Seen_Flag;

   procedure Check_Kind_For_Pure_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration =>
            null;
         when others =>
            Failed ("Pure_Flag", Target);
      end case;
   end Check_Kind_For_Pure_Flag;

   function Get_Pure_Flag (Func : Iir) return Boolean is
   begin
      Check_Kind_For_Pure_Flag (Func);
      return Get_Flag2 (Func);
   end Get_Pure_Flag;

   procedure Set_Pure_Flag (Func : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Pure_Flag (Func);
      Set_Flag2 (Func, Flag);
   end Set_Pure_Flag;

   procedure Check_Kind_For_Foreign_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Architecture_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Foreign_Flag", Target);
      end case;
   end Check_Kind_For_Foreign_Flag;

   function Get_Foreign_Flag (Decl : Iir) return Boolean is
   begin
      Check_Kind_For_Foreign_Flag (Decl);
      return Get_Flag3 (Decl);
   end Get_Foreign_Flag;

   procedure Set_Foreign_Flag (Decl : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Foreign_Flag (Decl);
      Set_Flag3 (Decl, Flag);
   end Set_Foreign_Flag;

   procedure Check_Kind_For_Resolved_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Resolved_Flag", Target);
      end case;
   end Check_Kind_For_Resolved_Flag;

   function Get_Resolved_Flag (Atype : Iir) return Boolean is
   begin
      Check_Kind_For_Resolved_Flag (Atype);
      return Get_Flag1 (Atype);
   end Get_Resolved_Flag;

   procedure Set_Resolved_Flag (Atype : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Resolved_Flag (Atype);
      Set_Flag1 (Atype, Flag);
   end Set_Resolved_Flag;

   procedure Check_Kind_For_Signal_Type_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Protected_Type_Declaration
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Signal_Type_Flag", Target);
      end case;
   end Check_Kind_For_Signal_Type_Flag;

   function Get_Signal_Type_Flag (Atype : Iir) return Boolean is
   begin
      Check_Kind_For_Signal_Type_Flag (Atype);
      return Get_Flag2 (Atype);
   end Get_Signal_Type_Flag;

   procedure Set_Signal_Type_Flag (Atype : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Signal_Type_Flag (Atype);
      Set_Flag2 (Atype, Flag);
   end Set_Signal_Type_Flag;

   procedure Check_Kind_For_Has_Signal_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Incomplete_Type_Definition
           | Iir_Kind_Record_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            null;
         when others =>
            Failed ("Has_Signal_Flag", Target);
      end case;
   end Check_Kind_For_Has_Signal_Flag;

   function Get_Has_Signal_Flag (Atype : Iir) return Boolean is
   begin
      Check_Kind_For_Has_Signal_Flag (Atype);
      return Get_Flag3 (Atype);
   end Get_Has_Signal_Flag;

   procedure Set_Has_Signal_Flag (Atype : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Has_Signal_Flag (Atype);
      Set_Flag3 (Atype, Flag);
   end Set_Has_Signal_Flag;

   procedure Check_Kind_For_Purity_State (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Procedure_Declaration =>
            null;
         when others =>
            Failed ("Purity_State", Target);
      end case;
   end Check_Kind_For_Purity_State;

   function Get_Purity_State (Proc : Iir) return Iir_Pure_State is
   begin
      Check_Kind_For_Purity_State (Proc);
      return Iir_Pure_State'Val (Get_State2 (Proc));
   end Get_Purity_State;

   procedure Set_Purity_State (Proc : Iir; State : Iir_Pure_State) is
   begin
      Check_Kind_For_Purity_State (Proc);
      Set_State2 (Proc, Iir_Pure_State'Pos (State));
   end Set_Purity_State;

   procedure Check_Kind_For_Elab_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("Elab_Flag", Target);
      end case;
   end Check_Kind_For_Elab_Flag;

   function Get_Elab_Flag (Design : Iir) return Boolean is
   begin
      Check_Kind_For_Elab_Flag (Design);
      return Get_Flag3 (Design);
   end Get_Elab_Flag;

   procedure Set_Elab_Flag (Design : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Elab_Flag (Design);
      Set_Flag3 (Design, Flag);
   end Set_Elab_Flag;

   procedure Check_Kind_For_Index_Constraint_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Array_Type_Definition
           | Iir_Kind_Array_Subtype_Definition =>
            null;
         when others =>
            Failed ("Index_Constraint_Flag", Target);
      end case;
   end Check_Kind_For_Index_Constraint_Flag;

   function Get_Index_Constraint_Flag (Atype : Iir) return Boolean is
   begin
      Check_Kind_For_Index_Constraint_Flag (Atype);
      return Get_Flag4 (Atype);
   end Get_Index_Constraint_Flag;

   procedure Set_Index_Constraint_Flag (Atype : Iir; Flag : Boolean) is
   begin
      Check_Kind_For_Index_Constraint_Flag (Atype);
      Set_Flag4 (Atype, Flag);
   end Set_Index_Constraint_Flag;

   procedure Check_Kind_For_Assertion_Condition (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Assertion_Statement =>
            null;
         when others =>
            Failed ("Assertion_Condition", Target);
      end case;
   end Check_Kind_For_Assertion_Condition;

   function Get_Assertion_Condition (Target : Iir) return Iir is
   begin
      Check_Kind_For_Assertion_Condition (Target);
      return Get_Field1 (Target);
   end Get_Assertion_Condition;

   procedure Set_Assertion_Condition (Target : Iir; Cond : Iir) is
   begin
      Check_Kind_For_Assertion_Condition (Target);
      Set_Field1 (Target, Cond);
   end Set_Assertion_Condition;

   procedure Check_Kind_For_Report_Expression (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement =>
            null;
         when others =>
            Failed ("Report_Expression", Target);
      end case;
   end Check_Kind_For_Report_Expression;

   function Get_Report_Expression (Target : Iir) return Iir is
   begin
      Check_Kind_For_Report_Expression (Target);
      return Get_Field6 (Target);
   end Get_Report_Expression;

   procedure Set_Report_Expression (Target : Iir; Expr : Iir) is
   begin
      Check_Kind_For_Report_Expression (Target);
      Set_Field6 (Target, Expr);
   end Set_Report_Expression;

   procedure Check_Kind_For_Severity_Expression (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement =>
            null;
         when others =>
            Failed ("Severity_Expression", Target);
      end case;
   end Check_Kind_For_Severity_Expression;

   function Get_Severity_Expression (Target : Iir) return Iir is
   begin
      Check_Kind_For_Severity_Expression (Target);
      return Get_Field5 (Target);
   end Get_Severity_Expression;

   procedure Set_Severity_Expression (Target : Iir; Expr : Iir) is
   begin
      Check_Kind_For_Severity_Expression (Target);
      Set_Field5 (Target, Expr);
   end Set_Severity_Expression;

   procedure Check_Kind_For_Instantiated_Unit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Instantiation_Statement =>
            null;
         when others =>
            Failed ("Instantiated_Unit", Target);
      end case;
   end Check_Kind_For_Instantiated_Unit;

   function Get_Instantiated_Unit (Target : Iir) return Iir is
   begin
      Check_Kind_For_Instantiated_Unit (Target);
      return Get_Field1 (Target);
   end Get_Instantiated_Unit;

   procedure Set_Instantiated_Unit (Target : Iir; Unit : Iir) is
   begin
      Check_Kind_For_Instantiated_Unit (Target);
      Set_Field1 (Target, Unit);
   end Set_Instantiated_Unit;

   procedure Check_Kind_For_Generic_Map_Aspect_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Header
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Component_Instantiation_Statement =>
            null;
         when others =>
            Failed ("Generic_Map_Aspect_Chain", Target);
      end case;
   end Check_Kind_For_Generic_Map_Aspect_Chain;

   function Get_Generic_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Generic_Map_Aspect_Chain (Target);
      return Get_Field8 (Target);
   end Get_Generic_Map_Aspect_Chain;

   procedure Set_Generic_Map_Aspect_Chain (Target : Iir; Generics : Iir) is
   begin
      Check_Kind_For_Generic_Map_Aspect_Chain (Target);
      Set_Field8 (Target, Generics);
   end Set_Generic_Map_Aspect_Chain;

   procedure Check_Kind_For_Port_Map_Aspect_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Header
           | Iir_Kind_Binding_Indication
           | Iir_Kind_Component_Instantiation_Statement =>
            null;
         when others =>
            Failed ("Port_Map_Aspect_Chain", Target);
      end case;
   end Check_Kind_For_Port_Map_Aspect_Chain;

   function Get_Port_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Port_Map_Aspect_Chain (Target);
      return Get_Field9 (Target);
   end Get_Port_Map_Aspect_Chain;

   procedure Set_Port_Map_Aspect_Chain (Target : Iir; Port : Iir) is
   begin
      Check_Kind_For_Port_Map_Aspect_Chain (Target);
      Set_Field9 (Target, Port);
   end Set_Port_Map_Aspect_Chain;

   procedure Check_Kind_For_Configuration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Entity_Aspect_Configuration =>
            null;
         when others =>
            Failed ("Configuration", Target);
      end case;
   end Check_Kind_For_Configuration;

   function Get_Configuration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Configuration (Target);
      return Get_Field1 (Target);
   end Get_Configuration;

   procedure Set_Configuration (Target : Iir; Conf : Iir) is
   begin
      Check_Kind_For_Configuration (Target);
      Set_Field1 (Target, Conf);
   end Set_Configuration;

   procedure Check_Kind_For_Component_Configuration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Instantiation_Statement =>
            null;
         when others =>
            Failed ("Component_Configuration", Target);
      end case;
   end Check_Kind_For_Component_Configuration;

   function Get_Component_Configuration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Component_Configuration (Target);
      return Get_Field6 (Target);
   end Get_Component_Configuration;

   procedure Set_Component_Configuration (Target : Iir; Conf : Iir) is
   begin
      Check_Kind_For_Component_Configuration (Target);
      Set_Field6 (Target, Conf);
   end Set_Component_Configuration;

   procedure Check_Kind_For_Configuration_Specification (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Instantiation_Statement =>
            null;
         when others =>
            Failed ("Configuration_Specification", Target);
      end case;
   end Check_Kind_For_Configuration_Specification;

   function Get_Configuration_Specification (Target : Iir) return Iir is
   begin
      Check_Kind_For_Configuration_Specification (Target);
      return Get_Field7 (Target);
   end Get_Configuration_Specification;

   procedure Set_Configuration_Specification (Target : Iir; Conf : Iir) is
   begin
      Check_Kind_For_Configuration_Specification (Target);
      Set_Field7 (Target, Conf);
   end Set_Configuration_Specification;

   procedure Check_Kind_For_Default_Binding_Indication (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Instantiation_Statement =>
            null;
         when others =>
            Failed ("Default_Binding_Indication", Target);
      end case;
   end Check_Kind_For_Default_Binding_Indication;

   function Get_Default_Binding_Indication (Target : Iir) return Iir is
   begin
      Check_Kind_For_Default_Binding_Indication (Target);
      return Get_Field5 (Target);
   end Get_Default_Binding_Indication;

   procedure Set_Default_Binding_Indication (Target : Iir; Conf : Iir) is
   begin
      Check_Kind_For_Default_Binding_Indication (Target);
      Set_Field5 (Target, Conf);
   end Set_Default_Binding_Indication;

   procedure Check_Kind_For_Default_Configuration_Declaration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Architecture_Declaration =>
            null;
         when others =>
            Failed ("Default_Configuration_Declaration", Target);
      end case;
   end Check_Kind_For_Default_Configuration_Declaration;

   function Get_Default_Configuration_Declaration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Default_Configuration_Declaration (Target);
      return Get_Field6 (Target);
   end Get_Default_Configuration_Declaration;

   procedure Set_Default_Configuration_Declaration (Target : Iir; Conf : Iir)
      is
   begin
      Check_Kind_For_Default_Configuration_Declaration (Target);
      Set_Field6 (Target, Conf);
   end Set_Default_Configuration_Declaration;

   procedure Check_Kind_For_Expression (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_Case_Statement =>
            null;
         when others =>
            Failed ("Expression", Target);
      end case;
   end Check_Kind_For_Expression;

   function Get_Expression (Target : Iir) return Iir is
   begin
      Check_Kind_For_Expression (Target);
      return Get_Field5 (Target);
   end Get_Expression;

   procedure Set_Expression (Target : Iir; Expr : Iir) is
   begin
      Check_Kind_For_Expression (Target);
      Set_Field5 (Target, Expr);
   end Set_Expression;

   procedure Check_Kind_For_Selected_Waveform_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            null;
         when others =>
            Failed ("Selected_Waveform_Chain", Target);
      end case;
   end Check_Kind_For_Selected_Waveform_Chain;

   function Get_Selected_Waveform_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Selected_Waveform_Chain (Target);
      return Get_Field7 (Target);
   end Get_Selected_Waveform_Chain;

   procedure Set_Selected_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Selected_Waveform_Chain (Target);
      Set_Field7 (Target, Chain);
   end Set_Selected_Waveform_Chain;

   procedure Check_Kind_For_Conditional_Waveform_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            null;
         when others =>
            Failed ("Conditional_Waveform_Chain", Target);
      end case;
   end Check_Kind_For_Conditional_Waveform_Chain;

   function Get_Conditional_Waveform_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Conditional_Waveform_Chain (Target);
      return Get_Field7 (Target);
   end Get_Conditional_Waveform_Chain;

   procedure Set_Conditional_Waveform_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Conditional_Waveform_Chain (Target);
      Set_Field7 (Target, Chain);
   end Set_Conditional_Waveform_Chain;

   procedure Check_Kind_For_Guard_Expression (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Guard_Signal_Declaration =>
            null;
         when others =>
            Failed ("Guard_Expression", Target);
      end case;
   end Check_Kind_For_Guard_Expression;

   function Get_Guard_Expression (Target : Iir) return Iir is
   begin
      Check_Kind_For_Guard_Expression (Target);
      return Get_Field2 (Target);
   end Get_Guard_Expression;

   procedure Set_Guard_Expression (Target : Iir; Expr : Iir) is
   begin
      Check_Kind_For_Guard_Expression (Target);
      Set_Field2 (Target, Expr);
   end Set_Guard_Expression;

   procedure Check_Kind_For_Guard_Decl (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Statement =>
            null;
         when others =>
            Failed ("Guard_Decl", Target);
      end case;
   end Check_Kind_For_Guard_Decl;

   function Get_Guard_Decl (Target : Iir_Block_Statement) return Iir is
   begin
      Check_Kind_For_Guard_Decl (Target);
      return Get_Field8 (Target);
   end Get_Guard_Decl;

   procedure Set_Guard_Decl (Target : Iir_Block_Statement; Decl : Iir) is
   begin
      Check_Kind_For_Guard_Decl (Target);
      Set_Field8 (Target, Decl);
   end Set_Guard_Decl;

   procedure Check_Kind_For_Guard_Sensitivity_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Guard_Signal_Declaration =>
            null;
         when others =>
            Failed ("Guard_Sensitivity_List", Target);
      end case;
   end Check_Kind_For_Guard_Sensitivity_List;

   function Get_Guard_Sensitivity_List (Guard : Iir) return Iir_List is
   begin
      Check_Kind_For_Guard_Sensitivity_List (Guard);
      return Iir_To_Iir_List (Get_Field6 (Guard));
   end Get_Guard_Sensitivity_List;

   procedure Set_Guard_Sensitivity_List (Guard : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Guard_Sensitivity_List (Guard);
      Set_Field6 (Guard, Iir_List_To_Iir (List));
   end Set_Guard_Sensitivity_List;

   procedure Check_Kind_For_Block_Block_Configuration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Statement =>
            null;
         when others =>
            Failed ("Block_Block_Configuration", Target);
      end case;
   end Check_Kind_For_Block_Block_Configuration;

   function Get_Block_Block_Configuration (Block : Iir) return Iir is
   begin
      Check_Kind_For_Block_Block_Configuration (Block);
      return Get_Field6 (Block);
   end Get_Block_Block_Configuration;

   procedure Set_Block_Block_Configuration (Block : Iir; Conf : Iir) is
   begin
      Check_Kind_For_Block_Block_Configuration (Block);
      Set_Field6 (Block, Conf);
   end Set_Block_Block_Configuration;

   procedure Check_Kind_For_Block_Header (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Block_Statement =>
            null;
         when others =>
            Failed ("Block_Header", Target);
      end case;
   end Check_Kind_For_Block_Header;

   function Get_Block_Header (Target : Iir) return Iir is
   begin
      Check_Kind_For_Block_Header (Target);
      return Get_Field7 (Target);
   end Get_Block_Header;

   procedure Set_Block_Header (Target : Iir; Header : Iir) is
   begin
      Check_Kind_For_Block_Header (Target);
      Set_Field7 (Target, Header);
   end Set_Block_Header;

   procedure Check_Kind_For_Generate_Block_Configuration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Generate_Statement =>
            null;
         when others =>
            Failed ("Generate_Block_Configuration", Target);
      end case;
   end Check_Kind_For_Generate_Block_Configuration;

   function Get_Generate_Block_Configuration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Generate_Block_Configuration (Target);
      return Get_Field7 (Target);
   end Get_Generate_Block_Configuration;

   procedure Set_Generate_Block_Configuration (Target : Iir; Conf : Iir) is
   begin
      Check_Kind_For_Generate_Block_Configuration (Target);
      Set_Field7 (Target, Conf);
   end Set_Generate_Block_Configuration;

   procedure Check_Kind_For_Generation_Scheme (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Generate_Statement =>
            null;
         when others =>
            Failed ("Generation_Scheme", Target);
      end case;
   end Check_Kind_For_Generation_Scheme;

   function Get_Generation_Scheme (Target : Iir) return Iir is
   begin
      Check_Kind_For_Generation_Scheme (Target);
      return Get_Field6 (Target);
   end Get_Generation_Scheme;

   procedure Set_Generation_Scheme (Target : Iir; Scheme : Iir) is
   begin
      Check_Kind_For_Generation_Scheme (Target);
      Set_Field6 (Target, Scheme);
   end Set_Generation_Scheme;

   procedure Check_Kind_For_Condition (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Conditional_Waveform
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            null;
         when others =>
            Failed ("Condition", Target);
      end case;
   end Check_Kind_For_Condition;

   function Get_Condition (Target : Iir) return Iir is
   begin
      Check_Kind_For_Condition (Target);
      return Get_Field1 (Target);
   end Get_Condition;

   procedure Set_Condition (Target : Iir; Condition : Iir) is
   begin
      Check_Kind_For_Condition (Target);
      Set_Field1 (Target, Condition);
   end Set_Condition;

   procedure Check_Kind_For_Else_Clause (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            null;
         when others =>
            Failed ("Else_Clause", Target);
      end case;
   end Check_Kind_For_Else_Clause;

   function Get_Else_Clause (Target : Iir) return Iir_Elsif is
   begin
      Check_Kind_For_Else_Clause (Target);
      return Get_Field6 (Target);
   end Get_Else_Clause;

   procedure Set_Else_Clause (Target : Iir; Clause : Iir_Elsif) is
   begin
      Check_Kind_For_Else_Clause (Target);
      Set_Field6 (Target, Clause);
   end Set_Else_Clause;

   procedure Check_Kind_For_Iterator_Scheme (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_For_Loop_Statement =>
            null;
         when others =>
            Failed ("Iterator_Scheme", Target);
      end case;
   end Check_Kind_For_Iterator_Scheme;

   function Get_Iterator_Scheme (Target : Iir) return Iir is
   begin
      Check_Kind_For_Iterator_Scheme (Target);
      return Get_Field1 (Target);
   end Get_Iterator_Scheme;

   procedure Set_Iterator_Scheme (Target : Iir; Iterator : Iir) is
   begin
      Check_Kind_For_Iterator_Scheme (Target);
      Set_Field1 (Target, Iterator);
   end Set_Iterator_Scheme;

   procedure Check_Kind_For_Parent (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_File
           | Iir_Kind_Design_Unit
           | Iir_Kind_Library_Clause
           | Iir_Kind_Use_Clause
           | Iir_Kind_Choice_By_Others
           | Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range
           | Iir_Kind_Choice_By_None
           | Iir_Kind_Choice_By_Name
           | Iir_Kind_Block_Configuration
           | Iir_Kind_Component_Configuration
           | Iir_Kind_Procedure_Call
           | Iir_Kind_Record_Element_Constraint
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Disconnection_Specification
           | Iir_Kind_Configuration_Specification
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Configuration_Declaration
           | Iir_Kind_Entity_Declaration
           | Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Body
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Assert_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_Generate_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Elsif =>
            null;
         when others =>
            Failed ("Parent", Target);
      end case;
   end Check_Kind_For_Parent;

   function Get_Parent (Target : Iir) return Iir is
   begin
      Check_Kind_For_Parent (Target);
      return Get_Field0 (Target);
   end Get_Parent;

   procedure Set_Parent (Target : Iir; Parent : Iir) is
   begin
      Check_Kind_For_Parent (Target);
      Set_Field0 (Target, Parent);
   end Set_Parent;

   procedure Check_Kind_For_Loop (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement =>
            null;
         when others =>
            Failed ("Loop", Target);
      end case;
   end Check_Kind_For_Loop;

   function Get_Loop (Target : Iir) return Iir is
   begin
      Check_Kind_For_Loop (Target);
      return Get_Field5 (Target);
   end Get_Loop;

   procedure Set_Loop (Target : Iir; Stmt : Iir) is
   begin
      Check_Kind_For_Loop (Target);
      Set_Field5 (Target, Stmt);
   end Set_Loop;

   procedure Check_Kind_For_Component_Name (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Specification =>
            null;
         when others =>
            Failed ("Component_Name", Target);
      end case;
   end Check_Kind_For_Component_Name;

   function Get_Component_Name (Target : Iir) return Iir is
   begin
      Check_Kind_For_Component_Name (Target);
      return Get_Field4 (Target);
   end Get_Component_Name;

   procedure Set_Component_Name (Target : Iir; Name : Iir) is
   begin
      Check_Kind_For_Component_Name (Target);
      Set_Field4 (Target, Name);
   end Set_Component_Name;

   procedure Check_Kind_For_Instantiation_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Specification =>
            null;
         when others =>
            Failed ("Instantiation_List", Target);
      end case;
   end Check_Kind_For_Instantiation_List;

   function Get_Instantiation_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Instantiation_List (Target);
      return Iir_To_Iir_List (Get_Field1 (Target));
   end Get_Instantiation_List;

   procedure Set_Instantiation_List (Target : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Instantiation_List (Target);
      Set_Field1 (Target, Iir_List_To_Iir (List));
   end Set_Instantiation_List;

   procedure Check_Kind_For_Entity_Aspect (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Binding_Indication =>
            null;
         when others =>
            Failed ("Entity_Aspect", Target);
      end case;
   end Check_Kind_For_Entity_Aspect;

   function Get_Entity_Aspect (Target : Iir_Binding_Indication) return Iir is
   begin
      Check_Kind_For_Entity_Aspect (Target);
      return Get_Field3 (Target);
   end Get_Entity_Aspect;

   procedure Set_Entity_Aspect (Target : Iir_Binding_Indication; Entity : Iir)
      is
   begin
      Check_Kind_For_Entity_Aspect (Target);
      Set_Field3 (Target, Entity);
   end Set_Entity_Aspect;

   procedure Check_Kind_For_Default_Entity_Aspect (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Binding_Indication =>
            null;
         when others =>
            Failed ("Default_Entity_Aspect", Target);
      end case;
   end Check_Kind_For_Default_Entity_Aspect;

   function Get_Default_Entity_Aspect (Target : Iir) return Iir is
   begin
      Check_Kind_For_Default_Entity_Aspect (Target);
      return Get_Field1 (Target);
   end Get_Default_Entity_Aspect;

   procedure Set_Default_Entity_Aspect (Target : Iir; Aspect : Iir) is
   begin
      Check_Kind_For_Default_Entity_Aspect (Target);
      Set_Field1 (Target, Aspect);
   end Set_Default_Entity_Aspect;

   procedure Check_Kind_For_Default_Generic_Map_Aspect_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Binding_Indication =>
            null;
         when others =>
            Failed ("Default_Generic_Map_Aspect_Chain", Target);
      end case;
   end Check_Kind_For_Default_Generic_Map_Aspect_Chain;

   function Get_Default_Generic_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Default_Generic_Map_Aspect_Chain (Target);
      return Get_Field6 (Target);
   end Get_Default_Generic_Map_Aspect_Chain;

   procedure Set_Default_Generic_Map_Aspect_Chain (Target : Iir; Chain : Iir)
      is
   begin
      Check_Kind_For_Default_Generic_Map_Aspect_Chain (Target);
      Set_Field6 (Target, Chain);
   end Set_Default_Generic_Map_Aspect_Chain;

   procedure Check_Kind_For_Default_Port_Map_Aspect_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Binding_Indication =>
            null;
         when others =>
            Failed ("Default_Port_Map_Aspect_Chain", Target);
      end case;
   end Check_Kind_For_Default_Port_Map_Aspect_Chain;

   function Get_Default_Port_Map_Aspect_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Default_Port_Map_Aspect_Chain (Target);
      return Get_Field7 (Target);
   end Get_Default_Port_Map_Aspect_Chain;

   procedure Set_Default_Port_Map_Aspect_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Default_Port_Map_Aspect_Chain (Target);
      Set_Field7 (Target, Chain);
   end Set_Default_Port_Map_Aspect_Chain;

   procedure Check_Kind_For_Binding_Indication (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Component_Configuration
           | Iir_Kind_Configuration_Specification =>
            null;
         when others =>
            Failed ("Binding_Indication", Target);
      end case;
   end Check_Kind_For_Binding_Indication;

   function Get_Binding_Indication (Target : Iir) return Iir is
   begin
      Check_Kind_For_Binding_Indication (Target);
      return Get_Field3 (Target);
   end Get_Binding_Indication;

   procedure Set_Binding_Indication (Target : Iir; Binding : Iir) is
   begin
      Check_Kind_For_Binding_Indication (Target);
      Set_Field3 (Target, Binding);
   end Set_Binding_Indication;

   procedure Check_Kind_For_Named_Entity (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Operator_Symbol
           | Iir_Kind_Simple_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Attribute_Name =>
            null;
         when others =>
            Failed ("Named_Entity", Target);
      end case;
   end Check_Kind_For_Named_Entity;

   function Get_Named_Entity (Target : Iir) return Iir is
   begin
      Check_Kind_For_Named_Entity (Target);
      return Get_Field4 (Target);
   end Get_Named_Entity;

   procedure Set_Named_Entity (Target : Iir; Val : Iir) is
   begin
      Check_Kind_For_Named_Entity (Target);
      Set_Field4 (Target, Val);
   end Set_Named_Entity;

   procedure Check_Kind_For_Expr_Staticness (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error
           | Iir_Kind_Integer_Literal
           | Iir_Kind_Floating_Point_Literal
           | Iir_Kind_Null_Literal
           | Iir_Kind_String_Literal
           | Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal
           | Iir_Kind_Bit_String_Literal
           | Iir_Kind_Simple_Aggregate
           | Iir_Kind_Attribute_Value
           | Iir_Kind_Range_Expression
           | Iir_Kind_Unit_Declaration
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call
           | Iir_Kind_Aggregate
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion
           | Iir_Kind_Allocator_By_Expression
           | Iir_Kind_Allocator_By_Subtype
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Simple_Name
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            null;
         when others =>
            Failed ("Expr_Staticness", Target);
      end case;
   end Check_Kind_For_Expr_Staticness;

   function Get_Expr_Staticness (Target : Iir) return Iir_Staticness is
   begin
      Check_Kind_For_Expr_Staticness (Target);
      return Iir_Staticness'Val (Get_State1 (Target));
   end Get_Expr_Staticness;

   procedure Set_Expr_Staticness (Target : Iir; Static : Iir_Staticness) is
   begin
      Check_Kind_For_Expr_Staticness (Target);
      Set_State1 (Target, Iir_Staticness'Pos (Static));
   end Set_Expr_Staticness;

   procedure Check_Kind_For_Error_Origin (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Error =>
            null;
         when others =>
            Failed ("Error_Origin", Target);
      end case;
   end Check_Kind_For_Error_Origin;

   function Get_Error_Origin (Target : Iir) return Iir is
   begin
      Check_Kind_For_Error_Origin (Target);
      return Get_Field2 (Target);
   end Get_Error_Origin;

   procedure Set_Error_Origin (Target : Iir; Origin : Iir) is
   begin
      Check_Kind_For_Error_Origin (Target);
      Set_Field2 (Target, Origin);
   end Set_Error_Origin;

   procedure Check_Kind_For_Operand (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator =>
            null;
         when others =>
            Failed ("Operand", Target);
      end case;
   end Check_Kind_For_Operand;

   function Get_Operand (Target : Iir) return Iir is
   begin
      Check_Kind_For_Operand (Target);
      return Get_Field2 (Target);
   end Get_Operand;

   procedure Set_Operand (Target : Iir; An_Iir : Iir) is
   begin
      Check_Kind_For_Operand (Target);
      Set_Field2 (Target, An_Iir);
   end Set_Operand;

   procedure Check_Kind_For_Left (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator =>
            null;
         when others =>
            Failed ("Left", Target);
      end case;
   end Check_Kind_For_Left;

   function Get_Left (Target : Iir) return Iir is
   begin
      Check_Kind_For_Left (Target);
      return Get_Field2 (Target);
   end Get_Left;

   procedure Set_Left (Target : Iir; An_Iir : Iir) is
   begin
      Check_Kind_For_Left (Target);
      Set_Field2 (Target, An_Iir);
   end Set_Left;

   procedure Check_Kind_For_Right (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator =>
            null;
         when others =>
            Failed ("Right", Target);
      end case;
   end Check_Kind_For_Right;

   function Get_Right (Target : Iir) return Iir is
   begin
      Check_Kind_For_Right (Target);
      return Get_Field4 (Target);
   end Get_Right;

   procedure Set_Right (Target : Iir; An_Iir : Iir) is
   begin
      Check_Kind_For_Right (Target);
      Set_Field4 (Target, An_Iir);
   end Set_Right;

   procedure Check_Kind_For_Unit_Name (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Physical_Int_Literal
           | Iir_Kind_Physical_Fp_Literal =>
            null;
         when others =>
            Failed ("Unit_Name", Target);
      end case;
   end Check_Kind_For_Unit_Name;

   function Get_Unit_Name (Target : Iir) return Iir is
   begin
      Check_Kind_For_Unit_Name (Target);
      return Get_Field3 (Target);
   end Get_Unit_Name;

   procedure Set_Unit_Name (Target : Iir; Name : Iir) is
   begin
      Check_Kind_For_Unit_Name (Target);
      Set_Field3 (Target, Name);
   end Set_Unit_Name;

   procedure Check_Kind_For_Name (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Choice_By_Name
           | Iir_Kind_Signature
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            null;
         when others =>
            Failed ("Name", Target);
      end case;
   end Check_Kind_For_Name;

   function Get_Name (Target : Iir) return Iir is
   begin
      Check_Kind_For_Name (Target);
      return Get_Field4 (Target);
   end Get_Name;

   procedure Set_Name (Target : Iir; Name : Iir) is
   begin
      Check_Kind_For_Name (Target);
      Set_Field4 (Target, Name);
   end Set_Name;

   procedure Check_Kind_For_Group_Template_Name (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Group_Declaration =>
            null;
         when others =>
            Failed ("Group_Template_Name", Target);
      end case;
   end Check_Kind_For_Group_Template_Name;

   function Get_Group_Template_Name (Target : Iir) return Iir is
   begin
      Check_Kind_For_Group_Template_Name (Target);
      return Get_Field5 (Target);
   end Get_Group_Template_Name;

   procedure Set_Group_Template_Name (Target : Iir; Name : Iir) is
   begin
      Check_Kind_For_Group_Template_Name (Target);
      Set_Field5 (Target, Name);
   end Set_Group_Template_Name;

   procedure Check_Kind_For_Name_Staticness (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Attribute_Value
           | Iir_Kind_Enumeration_Literal
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration
           | Iir_Kind_Function_Call
           | Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute =>
            null;
         when others =>
            Failed ("Name_Staticness", Target);
      end case;
   end Check_Kind_For_Name_Staticness;

   function Get_Name_Staticness (Target : Iir) return Iir_Staticness is
   begin
      Check_Kind_For_Name_Staticness (Target);
      return Iir_Staticness'Val (Get_State2 (Target));
   end Get_Name_Staticness;

   procedure Set_Name_Staticness (Target : Iir; Static : Iir_Staticness) is
   begin
      Check_Kind_For_Name_Staticness (Target);
      Set_State2 (Target, Iir_Staticness'Pos (Static));
   end Set_Name_Staticness;

   procedure Check_Kind_For_Prefix (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Selected_Element
           | Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference
           | Iir_Kind_Slice_Name
           | Iir_Kind_Indexed_Name
           | Iir_Kind_Selected_Name
           | Iir_Kind_Selected_By_All_Name
           | Iir_Kind_Parenthesis_Name
           | Iir_Kind_Base_Attribute
           | Iir_Kind_Left_Type_Attribute
           | Iir_Kind_Right_Type_Attribute
           | Iir_Kind_High_Type_Attribute
           | Iir_Kind_Low_Type_Attribute
           | Iir_Kind_Ascending_Type_Attribute
           | Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Event_Attribute
           | Iir_Kind_Active_Attribute
           | Iir_Kind_Last_Event_Attribute
           | Iir_Kind_Last_Active_Attribute
           | Iir_Kind_Last_Value_Attribute
           | Iir_Kind_Driving_Attribute
           | Iir_Kind_Driving_Value_Attribute
           | Iir_Kind_Simple_Name_Attribute
           | Iir_Kind_Instance_Name_Attribute
           | Iir_Kind_Path_Name_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute
           | Iir_Kind_Attribute_Name =>
            null;
         when others =>
            Failed ("Prefix", Target);
      end case;
   end Check_Kind_For_Prefix;

   function Get_Prefix (Target : Iir) return Iir is
   begin
      Check_Kind_For_Prefix (Target);
      return Get_Field3 (Target);
   end Get_Prefix;

   procedure Set_Prefix (Target : Iir; Prefix : Iir) is
   begin
      Check_Kind_For_Prefix (Target);
      Set_Field3 (Target, Prefix);
   end Set_Prefix;

   procedure Check_Kind_For_Suffix (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Slice_Name =>
            null;
         when others =>
            Failed ("Suffix", Target);
      end case;
   end Check_Kind_For_Suffix;

   function Get_Suffix (Target : Iir) return Iir is
   begin
      Check_Kind_For_Suffix (Target);
      return Get_Field2 (Target);
   end Get_Suffix;

   procedure Set_Suffix (Target : Iir; Suffix : Iir) is
   begin
      Check_Kind_For_Suffix (Target);
      Set_Field2 (Target, Suffix);
   end Set_Suffix;

   procedure Check_Kind_For_Index_Subtype (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            null;
         when others =>
            Failed ("Index_Subtype", Target);
      end case;
   end Check_Kind_For_Index_Subtype;

   function Get_Index_Subtype (Attr : Iir) return Iir is
   begin
      Check_Kind_For_Index_Subtype (Attr);
      return Get_Field2 (Attr);
   end Get_Index_Subtype;

   procedure Set_Index_Subtype (Attr : Iir; St : Iir) is
   begin
      Check_Kind_For_Index_Subtype (Attr);
      Set_Field2 (Attr, St);
   end Set_Index_Subtype;

   procedure Check_Kind_For_Parameter (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Image_Attribute
           | Iir_Kind_Value_Attribute
           | Iir_Kind_Pos_Attribute
           | Iir_Kind_Val_Attribute
           | Iir_Kind_Succ_Attribute
           | Iir_Kind_Pred_Attribute
           | Iir_Kind_Leftof_Attribute
           | Iir_Kind_Rightof_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Left_Array_Attribute
           | Iir_Kind_Right_Array_Attribute
           | Iir_Kind_High_Array_Attribute
           | Iir_Kind_Low_Array_Attribute
           | Iir_Kind_Length_Array_Attribute
           | Iir_Kind_Ascending_Array_Attribute
           | Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            null;
         when others =>
            Failed ("Parameter", Target);
      end case;
   end Check_Kind_For_Parameter;

   function Get_Parameter (Target : Iir) return Iir is
   begin
      Check_Kind_For_Parameter (Target);
      return Get_Field4 (Target);
   end Get_Parameter;

   procedure Set_Parameter (Target : Iir; Param : Iir) is
   begin
      Check_Kind_For_Parameter (Target);
      Set_Field4 (Target, Param);
   end Set_Parameter;

   procedure Check_Kind_For_Actual_Type (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Individual =>
            null;
         when others =>
            Failed ("Actual_Type", Target);
      end case;
   end Check_Kind_For_Actual_Type;

   function Get_Actual_Type (Target : Iir) return Iir is
   begin
      Check_Kind_For_Actual_Type (Target);
      return Get_Field3 (Target);
   end Get_Actual_Type;

   procedure Set_Actual_Type (Target : Iir; Atype : Iir) is
   begin
      Check_Kind_For_Actual_Type (Target);
      Set_Field3 (Target, Atype);
   end Set_Actual_Type;

   procedure Check_Kind_For_Association_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Parenthesis_Name =>
            null;
         when others =>
            Failed ("Association_Chain", Target);
      end case;
   end Check_Kind_For_Association_Chain;

   function Get_Association_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Association_Chain (Target);
      return Get_Field2 (Target);
   end Get_Association_Chain;

   procedure Set_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Association_Chain (Target);
      Set_Field2 (Target, Chain);
   end Set_Association_Chain;

   procedure Check_Kind_For_Individual_Association_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Association_Element_By_Individual =>
            null;
         when others =>
            Failed ("Individual_Association_Chain", Target);
      end case;
   end Check_Kind_For_Individual_Association_Chain;

   function Get_Individual_Association_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Individual_Association_Chain (Target);
      return Get_Field4 (Target);
   end Get_Individual_Association_Chain;

   procedure Set_Individual_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Individual_Association_Chain (Target);
      Set_Field4 (Target, Chain);
   end Set_Individual_Association_Chain;

   procedure Check_Kind_For_Aggregate_Info (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate =>
            null;
         when others =>
            Failed ("Aggregate_Info", Target);
      end case;
   end Check_Kind_For_Aggregate_Info;

   function Get_Aggregate_Info (Target : Iir) return Iir_Aggregate_Info is
   begin
      Check_Kind_For_Aggregate_Info (Target);
      return Get_Field2 (Target);
   end Get_Aggregate_Info;

   procedure Set_Aggregate_Info (Target : Iir; Info : Iir_Aggregate_Info) is
   begin
      Check_Kind_For_Aggregate_Info (Target);
      Set_Field2 (Target, Info);
   end Set_Aggregate_Info;

   procedure Check_Kind_For_Sub_Aggregate_Info (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate_Info =>
            null;
         when others =>
            Failed ("Sub_Aggregate_Info", Target);
      end case;
   end Check_Kind_For_Sub_Aggregate_Info;

   function Get_Sub_Aggregate_Info (Target : Iir) return Iir_Aggregate_Info is
   begin
      Check_Kind_For_Sub_Aggregate_Info (Target);
      return Get_Field1 (Target);
   end Get_Sub_Aggregate_Info;

   procedure Set_Sub_Aggregate_Info (Target : Iir; Info : Iir_Aggregate_Info)
      is
   begin
      Check_Kind_For_Sub_Aggregate_Info (Target);
      Set_Field1 (Target, Info);
   end Set_Sub_Aggregate_Info;

   procedure Check_Kind_For_Aggr_Dynamic_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate_Info =>
            null;
         when others =>
            Failed ("Aggr_Dynamic_Flag", Target);
      end case;
   end Check_Kind_For_Aggr_Dynamic_Flag;

   function Get_Aggr_Dynamic_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Aggr_Dynamic_Flag (Target);
      return Get_Flag3 (Target);
   end Get_Aggr_Dynamic_Flag;

   procedure Set_Aggr_Dynamic_Flag (Target : Iir; Val : Boolean) is
   begin
      Check_Kind_For_Aggr_Dynamic_Flag (Target);
      Set_Flag3 (Target, Val);
   end Set_Aggr_Dynamic_Flag;

   procedure Check_Kind_For_Aggr_Max_Length (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate_Info =>
            null;
         when others =>
            Failed ("Aggr_Max_Length", Target);
      end case;
   end Check_Kind_For_Aggr_Max_Length;

   function Get_Aggr_Max_Length (Info : Iir_Aggregate_Info) return Iir_Int32
      is
   begin
      Check_Kind_For_Aggr_Max_Length (Info);
      return Iir_To_Iir_Int32 (Get_Field4 (Info));
   end Get_Aggr_Max_Length;

   procedure Set_Aggr_Max_Length (Info : Iir_Aggregate_Info; Nbr : Iir_Int32)
      is
   begin
      Check_Kind_For_Aggr_Max_Length (Info);
      Set_Field4 (Info, Iir_Int32_To_Iir (Nbr));
   end Set_Aggr_Max_Length;

   procedure Check_Kind_For_Aggr_Low_Limit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate_Info =>
            null;
         when others =>
            Failed ("Aggr_Low_Limit", Target);
      end case;
   end Check_Kind_For_Aggr_Low_Limit;

   function Get_Aggr_Low_Limit (Target : Iir_Aggregate_Info) return Iir is
   begin
      Check_Kind_For_Aggr_Low_Limit (Target);
      return Get_Field2 (Target);
   end Get_Aggr_Low_Limit;

   procedure Set_Aggr_Low_Limit (Target : Iir_Aggregate_Info; Limit : Iir) is
   begin
      Check_Kind_For_Aggr_Low_Limit (Target);
      Set_Field2 (Target, Limit);
   end Set_Aggr_Low_Limit;

   procedure Check_Kind_For_Aggr_High_Limit (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate_Info =>
            null;
         when others =>
            Failed ("Aggr_High_Limit", Target);
      end case;
   end Check_Kind_For_Aggr_High_Limit;

   function Get_Aggr_High_Limit (Target : Iir_Aggregate_Info) return Iir is
   begin
      Check_Kind_For_Aggr_High_Limit (Target);
      return Get_Field3 (Target);
   end Get_Aggr_High_Limit;

   procedure Set_Aggr_High_Limit (Target : Iir_Aggregate_Info; Limit : Iir) is
   begin
      Check_Kind_For_Aggr_High_Limit (Target);
      Set_Field3 (Target, Limit);
   end Set_Aggr_High_Limit;

   procedure Check_Kind_For_Aggr_Others_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate_Info =>
            null;
         when others =>
            Failed ("Aggr_Others_Flag", Target);
      end case;
   end Check_Kind_For_Aggr_Others_Flag;

   function Get_Aggr_Others_Flag (Target : Iir_Aggregate_Info) return Boolean
      is
   begin
      Check_Kind_For_Aggr_Others_Flag (Target);
      return Get_Flag2 (Target);
   end Get_Aggr_Others_Flag;

   procedure Set_Aggr_Others_Flag (Target : Iir_Aggregate_Info; Val : Boolean)
      is
   begin
      Check_Kind_For_Aggr_Others_Flag (Target);
      Set_Flag2 (Target, Val);
   end Set_Aggr_Others_Flag;

   procedure Check_Kind_For_Aggr_Named_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate_Info =>
            null;
         when others =>
            Failed ("Aggr_Named_Flag", Target);
      end case;
   end Check_Kind_For_Aggr_Named_Flag;

   function Get_Aggr_Named_Flag (Target : Iir_Aggregate_Info) return Boolean
      is
   begin
      Check_Kind_For_Aggr_Named_Flag (Target);
      return Get_Flag4 (Target);
   end Get_Aggr_Named_Flag;

   procedure Set_Aggr_Named_Flag (Target : Iir_Aggregate_Info; Val : Boolean)
      is
   begin
      Check_Kind_For_Aggr_Named_Flag (Target);
      Set_Flag4 (Target, Val);
   end Set_Aggr_Named_Flag;

   procedure Check_Kind_For_Value_Staticness (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate =>
            null;
         when others =>
            Failed ("Value_Staticness", Target);
      end case;
   end Check_Kind_For_Value_Staticness;

   function Get_Value_Staticness (Target : Iir) return Iir_Staticness is
   begin
      Check_Kind_For_Value_Staticness (Target);
      return Iir_Staticness'Val (Get_State2 (Target));
   end Get_Value_Staticness;

   procedure Set_Value_Staticness (Target : Iir; Staticness : Iir_Staticness)
      is
   begin
      Check_Kind_For_Value_Staticness (Target);
      Set_State2 (Target, Iir_Staticness'Pos (Staticness));
   end Set_Value_Staticness;

   procedure Check_Kind_For_Association_Choices_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate =>
            null;
         when others =>
            Failed ("Association_Choices_Chain", Target);
      end case;
   end Check_Kind_For_Association_Choices_Chain;

   function Get_Association_Choices_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Association_Choices_Chain (Target);
      return Get_Field4 (Target);
   end Get_Association_Choices_Chain;

   procedure Set_Association_Choices_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Association_Choices_Chain (Target);
      Set_Field4 (Target, Chain);
   end Set_Association_Choices_Chain;

   procedure Check_Kind_For_Case_Statement_Alternative_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Case_Statement =>
            null;
         when others =>
            Failed ("Case_Statement_Alternative_Chain", Target);
      end case;
   end Check_Kind_For_Case_Statement_Alternative_Chain;

   function Get_Case_Statement_Alternative_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Case_Statement_Alternative_Chain (Target);
      return Get_Field1 (Target);
   end Get_Case_Statement_Alternative_Chain;

   procedure Set_Case_Statement_Alternative_Chain (Target : Iir; Chain : Iir)
      is
   begin
      Check_Kind_For_Case_Statement_Alternative_Chain (Target);
      Set_Field1 (Target, Chain);
   end Set_Case_Statement_Alternative_Chain;

   procedure Check_Kind_For_Choice_Staticness (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Choice_By_Expression
           | Iir_Kind_Choice_By_Range =>
            null;
         when others =>
            Failed ("Choice_Staticness", Target);
      end case;
   end Check_Kind_For_Choice_Staticness;

   function Get_Choice_Staticness (Target : Iir) return Iir_Staticness is
   begin
      Check_Kind_For_Choice_Staticness (Target);
      return Iir_Staticness'Val (Get_State2 (Target));
   end Get_Choice_Staticness;

   procedure Set_Choice_Staticness (Target : Iir; Staticness : Iir_Staticness)
      is
   begin
      Check_Kind_For_Choice_Staticness (Target);
      Set_State2 (Target, Iir_Staticness'Pos (Staticness));
   end Set_Choice_Staticness;

   procedure Check_Kind_For_Procedure_Call (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Procedure_Call_Statement =>
            null;
         when others =>
            Failed ("Procedure_Call", Target);
      end case;
   end Check_Kind_For_Procedure_Call;

   function Get_Procedure_Call (Stmt : Iir) return Iir is
   begin
      Check_Kind_For_Procedure_Call (Stmt);
      return Get_Field1 (Stmt);
   end Get_Procedure_Call;

   procedure Set_Procedure_Call (Stmt : Iir; Call : Iir) is
   begin
      Check_Kind_For_Procedure_Call (Stmt);
      Set_Field1 (Stmt, Call);
   end Set_Procedure_Call;

   procedure Check_Kind_For_Implementation (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Procedure_Call
           | Iir_Kind_Identity_Operator
           | Iir_Kind_Negation_Operator
           | Iir_Kind_Absolute_Operator
           | Iir_Kind_Not_Operator
           | Iir_Kind_And_Operator
           | Iir_Kind_Or_Operator
           | Iir_Kind_Nand_Operator
           | Iir_Kind_Nor_Operator
           | Iir_Kind_Xor_Operator
           | Iir_Kind_Xnor_Operator
           | Iir_Kind_Equality_Operator
           | Iir_Kind_Inequality_Operator
           | Iir_Kind_Less_Than_Operator
           | Iir_Kind_Less_Than_Or_Equal_Operator
           | Iir_Kind_Greater_Than_Operator
           | Iir_Kind_Greater_Than_Or_Equal_Operator
           | Iir_Kind_Sll_Operator
           | Iir_Kind_Sla_Operator
           | Iir_Kind_Srl_Operator
           | Iir_Kind_Sra_Operator
           | Iir_Kind_Rol_Operator
           | Iir_Kind_Ror_Operator
           | Iir_Kind_Addition_Operator
           | Iir_Kind_Substraction_Operator
           | Iir_Kind_Concatenation_Operator
           | Iir_Kind_Multiplication_Operator
           | Iir_Kind_Division_Operator
           | Iir_Kind_Modulus_Operator
           | Iir_Kind_Remainder_Operator
           | Iir_Kind_Exponentiation_Operator
           | Iir_Kind_Function_Call =>
            null;
         when others =>
            Failed ("Implementation", Target);
      end case;
   end Check_Kind_For_Implementation;

   function Get_Implementation (Target : Iir) return Iir is
   begin
      Check_Kind_For_Implementation (Target);
      return Get_Field3 (Target);
   end Get_Implementation;

   procedure Set_Implementation (Target : Iir; Decl : Iir) is
   begin
      Check_Kind_For_Implementation (Target);
      Set_Field3 (Target, Decl);
   end Set_Implementation;

   procedure Check_Kind_For_Parameter_Association_Chain (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Procedure_Call
           | Iir_Kind_Function_Call =>
            null;
         when others =>
            Failed ("Parameter_Association_Chain", Target);
      end case;
   end Check_Kind_For_Parameter_Association_Chain;

   function Get_Parameter_Association_Chain (Target : Iir) return Iir is
   begin
      Check_Kind_For_Parameter_Association_Chain (Target);
      return Get_Field2 (Target);
   end Get_Parameter_Association_Chain;

   procedure Set_Parameter_Association_Chain (Target : Iir; Chain : Iir) is
   begin
      Check_Kind_For_Parameter_Association_Chain (Target);
      Set_Field2 (Target, Chain);
   end Set_Parameter_Association_Chain;

   procedure Check_Kind_For_Method_Object (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Procedure_Call
           | Iir_Kind_Function_Call =>
            null;
         when others =>
            Failed ("Method_Object", Target);
      end case;
   end Check_Kind_For_Method_Object;

   function Get_Method_Object (Target : Iir) return Iir is
   begin
      Check_Kind_For_Method_Object (Target);
      return Get_Field4 (Target);
   end Get_Method_Object;

   procedure Set_Method_Object (Target : Iir; Object : Iir) is
   begin
      Check_Kind_For_Method_Object (Target);
      Set_Field4 (Target, Object);
   end Set_Method_Object;

   procedure Check_Kind_For_Type_Mark (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_File_Type_Definition
           | Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Record_Subtype_Definition
           | Iir_Kind_Access_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Subtype_Definition
           | Iir_Kind_Qualified_Expression
           | Iir_Kind_Type_Conversion =>
            null;
         when others =>
            Failed ("Type_Mark", Target);
      end case;
   end Check_Kind_For_Type_Mark;

   function Get_Type_Mark (Target : Iir) return Iir is
   begin
      Check_Kind_For_Type_Mark (Target);
      return Get_Field2 (Target);
   end Get_Type_Mark;

   procedure Set_Type_Mark (Target : Iir; Mark : Iir) is
   begin
      Check_Kind_For_Type_Mark (Target);
      Set_Field2 (Target, Mark);
   end Set_Type_Mark;

   procedure Check_Kind_For_Lexical_Layout (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            null;
         when others =>
            Failed ("Lexical_Layout", Target);
      end case;
   end Check_Kind_For_Lexical_Layout;

   function Get_Lexical_Layout (Decl : Iir) return Iir_Lexical_Layout_Type is
   begin
      Check_Kind_For_Lexical_Layout (Decl);
      return Iir_Lexical_Layout_Type'Val (Get_Odigit2 (Decl));
   end Get_Lexical_Layout;

   procedure Set_Lexical_Layout (Decl : Iir; Lay : Iir_Lexical_Layout_Type) is
   begin
      Check_Kind_For_Lexical_Layout (Decl);
      Set_Odigit2 (Decl, Iir_Lexical_Layout_Type'Pos (Lay));
   end Set_Lexical_Layout;

   procedure Check_Kind_For_Incomplete_Type_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Incomplete_Type_Definition =>
            null;
         when others =>
            Failed ("Incomplete_Type_List", Target);
      end case;
   end Check_Kind_For_Incomplete_Type_List;

   function Get_Incomplete_Type_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Incomplete_Type_List (Target);
      return Iir_To_Iir_List (Get_Field2 (Target));
   end Get_Incomplete_Type_List;

   procedure Set_Incomplete_Type_List (Target : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Incomplete_Type_List (Target);
      Set_Field2 (Target, Iir_List_To_Iir (List));
   end Set_Incomplete_Type_List;

   procedure Check_Kind_For_Has_Disconnect_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Signal_Interface_Declaration =>
            null;
         when others =>
            Failed ("Has_Disconnect_Flag", Target);
      end case;
   end Check_Kind_For_Has_Disconnect_Flag;

   function Get_Has_Disconnect_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Has_Disconnect_Flag (Target);
      return Get_Flag1 (Target);
   end Get_Has_Disconnect_Flag;

   procedure Set_Has_Disconnect_Flag (Target : Iir; Val : Boolean) is
   begin
      Check_Kind_For_Has_Disconnect_Flag (Target);
      Set_Flag1 (Target, Val);
   end Set_Has_Disconnect_Flag;

   procedure Check_Kind_For_Has_Active_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Transaction_Attribute =>
            null;
         when others =>
            Failed ("Has_Active_Flag", Target);
      end case;
   end Check_Kind_For_Has_Active_Flag;

   function Get_Has_Active_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Has_Active_Flag (Target);
      return Get_Flag2 (Target);
   end Get_Has_Active_Flag;

   procedure Set_Has_Active_Flag (Target : Iir; Val : Boolean) is
   begin
      Check_Kind_For_Has_Active_Flag (Target);
      Set_Flag2 (Target, Val);
   end Set_Has_Active_Flag;

   procedure Check_Kind_For_Is_Within_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Architecture_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Sensitized_Process_Statement
           | Iir_Kind_Process_Statement
           | Iir_Kind_Block_Statement
           | Iir_Kind_For_Loop_Statement =>
            null;
         when others =>
            Failed ("Is_Within_Flag", Target);
      end case;
   end Check_Kind_For_Is_Within_Flag;

   function Get_Is_Within_Flag (Target : Iir) return Boolean is
   begin
      Check_Kind_For_Is_Within_Flag (Target);
      return Get_Flag5 (Target);
   end Get_Is_Within_Flag;

   procedure Set_Is_Within_Flag (Target : Iir; Val : Boolean) is
   begin
      Check_Kind_For_Is_Within_Flag (Target);
      Set_Flag5 (Target, Val);
   end Set_Is_Within_Flag;

   procedure Check_Kind_For_Type_Marks_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Signature =>
            null;
         when others =>
            Failed ("Type_Marks_List", Target);
      end case;
   end Check_Kind_For_Type_Marks_List;

   function Get_Type_Marks_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Type_Marks_List (Target);
      return Iir_To_Iir_List (Get_Field2 (Target));
   end Get_Type_Marks_List;

   procedure Set_Type_Marks_List (Target : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Type_Marks_List (Target);
      Set_Field2 (Target, Iir_List_To_Iir (List));
   end Set_Type_Marks_List;

   procedure Check_Kind_For_Signature (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Attribute_Name =>
            null;
         when others =>
            Failed ("Signature", Target);
      end case;
   end Check_Kind_For_Signature;

   function Get_Signature (Target : Iir) return Iir is
   begin
      Check_Kind_For_Signature (Target);
      return Get_Field5 (Target);
   end Get_Signature;

   procedure Set_Signature (Target : Iir; Value : Iir) is
   begin
      Check_Kind_For_Signature (Target);
      Set_Field5 (Target, Value);
   end Set_Signature;

   procedure Check_Kind_For_Overload_List (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Overload_List =>
            null;
         when others =>
            Failed ("Overload_List", Target);
      end case;
   end Check_Kind_For_Overload_List;

   function Get_Overload_List (Target : Iir) return Iir_List is
   begin
      Check_Kind_For_Overload_List (Target);
      return Iir_To_Iir_List (Get_Field1 (Target));
   end Get_Overload_List;

   procedure Set_Overload_List (Target : Iir; List : Iir_List) is
   begin
      Check_Kind_For_Overload_List (Target);
      Set_Field1 (Target, Iir_List_To_Iir (List));
   end Set_Overload_List;

   procedure Check_Kind_For_Simple_Name_Identifier (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Simple_Name_Attribute =>
            null;
         when others =>
            Failed ("Simple_Name_Identifier", Target);
      end case;
   end Check_Kind_For_Simple_Name_Identifier;

   function Get_Simple_Name_Identifier (Target : Iir) return Name_Id is
   begin
      Check_Kind_For_Simple_Name_Identifier (Target);
      return Iir_To_Name_Id (Get_Field2 (Target));
   end Get_Simple_Name_Identifier;

   procedure Set_Simple_Name_Identifier (Target : Iir; Ident : Name_Id) is
   begin
      Check_Kind_For_Simple_Name_Identifier (Target);
      Set_Field2 (Target, Name_Id_To_Iir (Ident));
   end Set_Simple_Name_Identifier;

   procedure Check_Kind_For_Protected_Type_Body (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Protected_Type_Declaration =>
            null;
         when others =>
            Failed ("Protected_Type_Body", Target);
      end case;
   end Check_Kind_For_Protected_Type_Body;

   function Get_Protected_Type_Body (Target : Iir) return Iir is
   begin
      Check_Kind_For_Protected_Type_Body (Target);
      return Get_Field2 (Target);
   end Get_Protected_Type_Body;

   procedure Set_Protected_Type_Body (Target : Iir; Bod : Iir) is
   begin
      Check_Kind_For_Protected_Type_Body (Target);
      Set_Field2 (Target, Bod);
   end Set_Protected_Type_Body;

   procedure Check_Kind_For_Protected_Type_Declaration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Protected_Type_Body =>
            null;
         when others =>
            Failed ("Protected_Type_Declaration", Target);
      end case;
   end Check_Kind_For_Protected_Type_Declaration;

   function Get_Protected_Type_Declaration (Target : Iir) return Iir is
   begin
      Check_Kind_For_Protected_Type_Declaration (Target);
      return Get_Field4 (Target);
   end Get_Protected_Type_Declaration;

   procedure Set_Protected_Type_Declaration (Target : Iir; Decl : Iir) is
   begin
      Check_Kind_For_Protected_Type_Declaration (Target);
      Set_Field4 (Target, Decl);
   end Set_Protected_Type_Declaration;

   procedure Check_Kind_For_End_Location (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Design_Unit =>
            null;
         when others =>
            Failed ("End_Location", Target);
      end case;
   end Check_Kind_For_End_Location;

   function Get_End_Location (Target : Iir) return Location_Type is
   begin
      Check_Kind_For_End_Location (Target);
      return Iir_To_Location_Type (Get_Field6 (Target));
   end Get_End_Location;

   procedure Set_End_Location (Target : Iir; Loc : Location_Type) is
   begin
      Check_Kind_For_End_Location (Target);
      Set_Field6 (Target, Location_Type_To_Iir (Loc));
   end Set_End_Location;

   procedure Check_Kind_For_String_Id (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_String_Literal
           | Iir_Kind_Bit_String_Literal =>
            null;
         when others =>
            Failed ("String_Id", Target);
      end case;
   end Check_Kind_For_String_Id;

   function Get_String_Id (Lit : Iir) return String_Id is
   begin
      Check_Kind_For_String_Id (Lit);
      return Iir_To_String_Id (Get_Field3 (Lit));
   end Get_String_Id;

   procedure Set_String_Id (Lit : Iir; Id : String_Id) is
   begin
      Check_Kind_For_String_Id (Lit);
      Set_Field3 (Lit, String_Id_To_Iir (Id));
   end Set_String_Id;

   procedure Check_Kind_For_String_Length (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_String_Literal
           | Iir_Kind_Bit_String_Literal =>
            null;
         when others =>
            Failed ("String_Length", Target);
      end case;
   end Check_Kind_For_String_Length;

   function Get_String_Length (Lit : Iir) return Int32 is
   begin
      Check_Kind_For_String_Length (Lit);
      return Iir_To_Int32 (Get_Field0 (Lit));
   end Get_String_Length;

   procedure Set_String_Length (Lit : Iir; Len : Int32) is
   begin
      Check_Kind_For_String_Length (Lit);
      Set_Field0 (Lit, Int32_To_Iir (Len));
   end Set_String_Length;

   procedure Check_Kind_For_Use_Flag (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Component_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Implicit_Function_Declaration
           | Iir_Kind_Implicit_Procedure_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Object_Alias_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Signal_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_Constant_Declaration
           | Iir_Kind_Iterator_Declaration
           | Iir_Kind_Constant_Interface_Declaration
           | Iir_Kind_Variable_Interface_Declaration
           | Iir_Kind_Signal_Interface_Declaration
           | Iir_Kind_File_Interface_Declaration =>
            null;
         when others =>
            Failed ("Use_Flag", Target);
      end case;
   end Check_Kind_For_Use_Flag;

   function Get_Use_Flag (Decl : Iir) return Boolean is
   begin
      Check_Kind_For_Use_Flag (Decl);
      return Get_Flag6 (Decl);
   end Get_Use_Flag;

   procedure Set_Use_Flag (Decl : Iir; Val : Boolean) is
   begin
      Check_Kind_For_Use_Flag (Decl);
      Set_Flag6 (Decl, Val);
   end Set_Use_Flag;

   procedure Check_Kind_For_Psl_Property (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Psl_Assert_Statement =>
            null;
         when others =>
            Failed ("Psl_Property", Target);
      end case;
   end Check_Kind_For_Psl_Property;

   function Get_Psl_Property (Decl : Iir) return PSL_Node is
   begin
      Check_Kind_For_Psl_Property (Decl);
      return Iir_To_PSL_Node (Get_Field1 (Decl));
   end Get_Psl_Property;

   procedure Set_Psl_Property (Decl : Iir; Prop : PSL_Node) is
   begin
      Check_Kind_For_Psl_Property (Decl);
      Set_Field1 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Property;

   procedure Check_Kind_For_Psl_Declaration (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Psl_Declaration =>
            null;
         when others =>
            Failed ("Psl_Declaration", Target);
      end case;
   end Check_Kind_For_Psl_Declaration;

   function Get_Psl_Declaration (Decl : Iir) return PSL_Node is
   begin
      Check_Kind_For_Psl_Declaration (Decl);
      return Iir_To_PSL_Node (Get_Field1 (Decl));
   end Get_Psl_Declaration;

   procedure Set_Psl_Declaration (Decl : Iir; Prop : PSL_Node) is
   begin
      Check_Kind_For_Psl_Declaration (Decl);
      Set_Field1 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Declaration;

   procedure Check_Kind_For_Psl_Expression (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Psl_Expression =>
            null;
         when others =>
            Failed ("Psl_Expression", Target);
      end case;
   end Check_Kind_For_Psl_Expression;

   function Get_Psl_Expression (Decl : Iir) return PSL_Node is
   begin
      Check_Kind_For_Psl_Expression (Decl);
      return Iir_To_PSL_Node (Get_Field3 (Decl));
   end Get_Psl_Expression;

   procedure Set_Psl_Expression (Decl : Iir; Prop : PSL_Node) is
   begin
      Check_Kind_For_Psl_Expression (Decl);
      Set_Field3 (Decl, PSL_Node_To_Iir (Prop));
   end Set_Psl_Expression;

   procedure Check_Kind_For_Psl_Boolean (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Psl_Default_Clock =>
            null;
         when others =>
            Failed ("Psl_Boolean", Target);
      end case;
   end Check_Kind_For_Psl_Boolean;

   function Get_Psl_Boolean (N : Iir) return PSL_Node is
   begin
      Check_Kind_For_Psl_Boolean (N);
      return Iir_To_PSL_Node (Get_Field1 (N));
   end Get_Psl_Boolean;

   procedure Set_Psl_Boolean (N : Iir; Bool : PSL_Node) is
   begin
      Check_Kind_For_Psl_Boolean (N);
      Set_Field1 (N, PSL_Node_To_Iir (Bool));
   end Set_Psl_Boolean;

   procedure Check_Kind_For_PSL_Clock (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Assert_Statement =>
            null;
         when others =>
            Failed ("PSL_Clock", Target);
      end case;
   end Check_Kind_For_PSL_Clock;

   function Get_PSL_Clock (N : Iir) return PSL_Node is
   begin
      Check_Kind_For_PSL_Clock (N);
      return Iir_To_PSL_Node (Get_Field7 (N));
   end Get_PSL_Clock;

   procedure Set_PSL_Clock (N : Iir; Clock : PSL_Node) is
   begin
      Check_Kind_For_PSL_Clock (N);
      Set_Field7 (N, PSL_Node_To_Iir (Clock));
   end Set_PSL_Clock;

   procedure Check_Kind_For_PSL_NFA (Target : Iir) is
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Assert_Statement =>
            null;
         when others =>
            Failed ("PSL_NFA", Target);
      end case;
   end Check_Kind_For_PSL_NFA;

   function Get_PSL_NFA (N : Iir) return PSL_NFA is
   begin
      Check_Kind_For_PSL_NFA (N);
      return Iir_To_PSL_NFA (Get_Field8 (N));
   end Get_PSL_NFA;

   procedure Set_PSL_NFA (N : Iir; Fa : PSL_NFA) is
   begin
      Check_Kind_For_PSL_NFA (N);
      Set_Field8 (N, PSL_NFA_To_Iir (Fa));
   end Set_PSL_NFA;

end Iirs;
