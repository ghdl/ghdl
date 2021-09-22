--  Nodes recognizer for ieee.vital_timing.
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
with Std_Names;
with Flags; use Flags;
with Errorout; use Errorout;
with Name_Table;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Tokens; use Vhdl.Tokens;
with Vhdl.Ieee.Std_Logic_1164; use Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Sem_Scopes;
with Vhdl.Sem_Specs;
with Vhdl.Evaluation;
with Vhdl.Sem;
with Vhdl.Utils; use Vhdl.Utils;

package body Vhdl.Ieee.Vital_Timing is
   --  This package is based on IEEE 1076.4 1995.

   --  Control generics identifier.
   InstancePath_Id : Name_Id;
   TimingChecksOn_Id : Name_Id;
   XOn_Id : Name_Id;
   MsgOn_Id : Name_Id;

   --  Extract declarations from package IEEE.VITAL_Timing.
   procedure Extract_Declarations (Pkg : Iir_Package_Declaration)
   is
      Ill_Formed : exception;

      function Try_Get_Identifier (Str : String) return Name_Id
      is
         Id : Name_Id;
      begin
         Id := Name_Table.Get_Identifier_No_Create (Str);
         if Id = Null_Identifier then
            raise Ill_Formed;
         end if;
         return Id;
      end Try_Get_Identifier;

      use Name_Table;

      Decl : Iir;
      Id : Name_Id;

      VitalDelayType_Id : Name_Id;
      VitalDelayType01_Id   : Name_Id;
      VitalDelayType01Z_Id  : Name_Id;
      VitalDelayType01ZX_Id : Name_Id;

      VitalDelayArrayType_Id     : Name_Id;
      VitalDelayArrayType01_Id   : Name_Id;
      VitalDelayArrayType01Z_Id  : Name_Id;
      VitalDelayArrayType01ZX_Id : Name_Id;
   begin
      --  Get Vital delay type identifiers.
      VitalDelayType_Id     := Try_Get_Identifier ("vitaldelaytype");
      VitalDelayType01_Id   := Try_Get_Identifier ("vitaldelaytype01");
      VitalDelayType01Z_Id  := Try_Get_Identifier ("vitaldelaytype01z");
      VitalDelayType01ZX_Id := Try_Get_Identifier ("vitaldelaytype01zx");

      VitalDelayArrayType_Id    :=
        Try_Get_Identifier ("vitaldelayarraytype");
      VitalDelayArrayType01_Id  :=
        Try_Get_Identifier ("vitaldelayarraytype01");
      VitalDelayArrayType01Z_Id :=
        Try_Get_Identifier ("vitaldelayarraytype01z");
      VitalDelayArrayType01ZX_Id :=
        Try_Get_Identifier ("vitaldelayarraytype01zx");

      --  Iterate on every declaration.
      --  Do name-matching.
      Decl := Get_Declaration_Chain (Pkg);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Attribute_Declaration =>
               Id := Get_Identifier (Decl);
               if Id = Std_Names.Name_VITAL_Level0 then
                  Vital_Level0_Attribute := Decl;
               elsif Id = Std_Names.Name_VITAL_Level1 then
                  Vital_Level1_Attribute := Decl;
               end if;
            when Iir_Kind_Subtype_Declaration =>
               Id := Get_Identifier (Decl);
               if Id = VitalDelayType_Id then
                  VitalDelayType := Get_Type (Decl);
               end if;
            when Iir_Kind_Type_Declaration =>
               Id := Get_Identifier (Decl);
               if Id = VitalDelayArrayType_Id then
                  VitalDelayArrayType := Get_Type_Definition (Decl);
               elsif Id = VitalDelayArrayType01_Id then
                  VitalDelayArrayType01 := Get_Type_Definition (Decl);
               elsif Id = VitalDelayArrayType01Z_Id then
                  VitalDelayArrayType01Z := Get_Type_Definition (Decl);
               elsif Id = VitalDelayArrayType01ZX_Id then
                  VitalDelayArrayType01ZX := Get_Type_Definition (Decl);
               end if;
            when Iir_Kind_Anonymous_Type_Declaration =>
               Id := Get_Identifier (Decl);
               if Id = VitalDelayType01_Id then
                  VitalDelayType01 := Get_Type_Definition (Decl);
               elsif Id = VitalDelayType01Z_Id then
                  VitalDelayType01Z := Get_Type_Definition (Decl);
               elsif Id = VitalDelayType01ZX_Id then
                  VitalDelayType01ZX := Get_Type_Definition (Decl);
               end if;
            when others =>
               null;
         end case;
         Decl := Get_Chain (Decl);
      end loop;

      --  If a declaration was not found, then the package is not the expected
      --  one.
      if Vital_Level0_Attribute = Null_Iir
        or Vital_Level1_Attribute = Null_Iir
        or VitalDelayType = Null_Iir
        or VitalDelayType01 = Null_Iir
        or VitalDelayType01Z = Null_Iir
        or VitalDelayType01ZX = Null_Iir
        or VitalDelayArrayType = Null_Iir
        or VitalDelayArrayType01 = Null_Iir
        or VitalDelayArrayType01Z = Null_Iir
        or VitalDelayArrayType01ZX = Null_Iir
      then
         raise Ill_Formed;
      end if;

      --  Create identifier for control generics.
      InstancePath_Id := Get_Identifier ("instancepath");
      TimingChecksOn_Id := Get_Identifier ("timingcheckson");
      XOn_Id := Get_Identifier ("xon");
      MsgOn_Id := Get_Identifier ("msgon");

      exception
         when Ill_Formed =>
            Error_Msg_Sem (+Pkg, "package ieee.vital_timing is ill-formed");

            Vital_Level0_Attribute := Null_Iir;
            Vital_Level1_Attribute := Null_Iir;

            VitalDelayType := Null_Iir;
            VitalDelayType01 := Null_Iir;
            VitalDelayType01Z := Null_Iir;
            VitalDelayType01ZX := Null_Iir;

            VitalDelayArrayType := Null_Iir;
            VitalDelayArrayType01 := Null_Iir;
            VitalDelayArrayType01Z := Null_Iir;
            VitalDelayArrayType01ZX := Null_Iir;
   end Extract_Declarations;

   procedure Error_Vital
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Error_Msg_Sem (Loc, Msg, Args);
   end Error_Vital;

   procedure Warning_Vital
     (Loc : Iir; Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Warning_Msg_Sem (Warnid_Vital_Generic, +Loc, Msg, Args);
   end Warning_Vital;

   --  Check DECL is the VITAL level 0 attribute specification.
   procedure Check_Level0_Attribute_Specification (Decl : Iir)
   is
      Expr : Iir;
   begin
      if Get_Kind (Decl) /= Iir_Kind_Attribute_Specification
        or else (Get_Named_Entity (Get_Attribute_Designator (Decl))
                   /= Vital_Level0_Attribute)
      then
         Error_Vital
           (+Decl,
            "first declaration must be the VITAL attribute specification");
         return;
      end if;

      --  IEEE 1076.4 4.1
      --  The expression in the VITAL_Level0 attribute specification shall be
      --  the Boolean literal TRUE.
      Expr := Get_Expression (Decl);
      if Get_Kind (Expr) not in Iir_Kinds_Denoting_Name
        or else Get_Named_Entity (Expr) /= Boolean_True
      then
         Error_Vital
           (+Decl, "the expression in the VITAL_Level0 attribute "
              & "specification shall be the Boolean literal TRUE");
      end if;

      --  IEEE 1076.4 4.1
      --  The entity specification of the decorating attribute specification
      --  shall be such that the enclosing entity or architecture inherits the
      --  VITAL_Level0 attribute.
      case Get_Entity_Class (Decl) is
         when Tok_Entity
           | Tok_Architecture =>
            null;
         when others =>
            Error_Vital (+Decl, "VITAL attribute specification does not "
                           & "decorate the enclosing entity or architecture");
      end case;
   end Check_Level0_Attribute_Specification;

   function Is_Slv_Subtype (Base_Type : Iir) return Boolean is
   begin
      if Vhdl_Std >= Vhdl_08 then
         return Base_Type = Std_Ulogic_Vector_Type;
      else
         return Base_Type = Std_Logic_Vector_Type;
      end if;
   end Is_Slv_Subtype;

   procedure Check_Entity_Port_Declaration
     (Decl : Iir_Interface_Signal_Declaration)
   is
      use Name_Table;

      Name : constant String := Image (Get_Identifier (Decl));
      Atype : Iir;
      Base_Type : Iir;
      Type_Decl : Iir;
   begin
      --  IEEE 1076.4 4.3.1
      --  The identifiers in an entity port declaration shall not contain
      --  underscore characters.
      pragma Assert (Name'First = 1);
      if Name (1) = '/' then
         Error_Vital
           (+Decl, "VITAL entity port shall not be an extended identifier");
      end if;
      for I in Name'Range loop
         if Name (I) = '_' then
            Error_Vital
              (+Decl, "VITAL entity port shall not contain underscore");
            exit;
         end if;
      end loop;

      --  IEEE 1076.4 4.3.1
      --  A port that is declared in an entity port declaration shall not be
      --  of mode LINKAGE.
      if Get_Mode (Decl) = Iir_Linkage_Mode then
         Error_Vital (+Decl, "VITAL entity port shall not be of mode LINKAGE");
      end if;

      --  IEEE 1076.4 4.3.1
      --  The type mark in an entity port declaration shall denote a type or
      --  a subtype that is declared in package Std_Logic_1164.  The type
      --  mark in the declaration of a scalar port shall denote the subtype
      --  Std_Ulogic or a subtype of Std_Ulogic.  The type mark in the
      --  declaration of an array port shall denote the type Std_Logic_Vector.
      Atype := Get_Type (Decl);
      Base_Type := Get_Base_Type (Atype);
      Type_Decl := Get_Type_Declarator (Atype);
      if Is_Slv_Subtype (Base_Type) then
         if Get_Resolution_Indication (Atype) /= Null_Iir then
            Error_Vital
              (+Decl,
               "VITAL array port type cannot override resolution function");
         end if;
         --  FIXME: is an unconstrained array port allowed ?
         --  FIXME: what about staticness of the index_constraint ?
      elsif Base_Type = Std_Ulogic_Type then
         if Type_Decl = Null_Iir
           or else Get_Parent (Type_Decl) /= Std_Logic_1164_Pkg
         then
            Error_Vital
              (+Decl,
               "VITAL entity port type mark shall be one of Std_Logic_1164");
         end if;
      else
         Error_Vital
           (+Decl, "VITAL port type must be Std_Logic_Vector or Std_Ulogic");
      end if;

      if Get_Guarded_Signal_Flag (Decl) then
         Error_Vital (+Decl, "VITAL entity port cannot be guarded");
      end if;
   end Check_Entity_Port_Declaration;

   procedure Check_Entity_Generic_Declaration
     (Decl : Iir_Interface_Constant_Declaration; Gen_Chain : Iir)
   is
      Id : constant Name_Id := Get_Identifier (Decl);
      Name : String := Name_Table.Image (Id);
      Len : constant Natural := Name'Last;

      --  Current position in the generic name, stored into Name.
      Gen_Name_Pos : Natural;

      --  Length of the generic name.
      Gen_Name_Length : Natural;

      --  The generic being analyzed.
      Gen_Decl : Iir;

      Port_Length : Natural;

      procedure Error_Vital_Name (Str : String)
      is
         Loc : Location_Type;
      begin
         Loc := Get_Location (Gen_Decl);
         Error_Vital (Loc + Location_Type (Gen_Name_Pos - 1), Str);
      end Error_Vital_Name;

      --  Check the next sub-string in the generic name is a port.
      --  Returns the port.
      function Check_Port return Iir
      is
         use Vhdl.Sem_Scopes;
         use Name_Table;

         C : Character;
         Res : Iir;
         Id : Name_Id;
         Inter : Name_Interpretation_Type;
      begin
         Port_Length := 0;
         while Gen_Name_Pos <= Gen_Name_Length loop
            C := Name (Gen_Name_Pos);
            Gen_Name_Pos := Gen_Name_Pos + 1;
            exit when C = '_';
            Port_Length := Port_Length + 1;
            Name (Port_Length) := C;
         end loop;

         if Port_Length = 0 then
            Error_Vital_Name ("port expected in VITAL generic name");
            return Null_Iir;
         end if;

         Id := Get_Identifier_No_Create (Name (1 .. Port_Length));
         Res := Null_Iir;
         if Id /= Null_Identifier then
            Inter := Get_Interpretation (Id);
            if Valid_Interpretation (Inter) then
               Res := Get_Declaration (Inter);
            end if;
         end if;
         if Res = Null_Iir then
            Warning_Vital (Gen_Decl, "'" & Name (1 .. Port_Length)
                             & "' is not a port name (in VITAL generic name)");
         end if;
         return Res;
      end Check_Port;

      --  Checks the port is an input port.
      function Check_Input_Port return Iir
      is
         Res : Iir;
      begin
         Res := Check_Port;
         if Res /= Null_Iir then
            --  IEEE 1076.4 4.3.2.1.3
            --  an input port is a VHDL port of mode IN or INOUT.
            case Get_Mode (Res) is
               when Iir_In_Mode
                 | Iir_Inout_Mode =>
                  null;
               when others =>
                  Error_Vital
                    (+Gen_Decl, "%i must be an input port", (1 => +Res));
            end case;
         end if;
         return Res;
      end Check_Input_Port;

      --  Checks the port is an output port.
      function Check_Output_Port return Iir
      is
         Res : Iir;
      begin
         Res := Check_Port;
         if Res /= Null_Iir then
            --  IEEE 1076.4 4.3.2.1.3
            --  An output port is a VHDL port of mode OUT, INOUT or BUFFER.
            case Get_Mode (Res) is
               when Iir_Out_Mode
                 | Iir_Inout_Mode
                 | Iir_Buffer_Mode =>
                  null;
               when others =>
                  Error_Vital
                    (+Gen_Decl, "%i must be an output port", (1 => +Res));
            end case;
         end if;
         return Res;
      end Check_Output_Port;

      --  Extract a suffix from the generic name.
      type Suffixes_Kind is
        (
         Suffix_Name,     --  [a-z]*
         Suffix_Num_Name,  --  [0-9]*
         Suffix_Edge,     --  posedge, negedge, 01, 10, 0z, z1, 1z, z0
         Suffix_Noedge,   --  noedge
         Suffix_Eon       --  End of name
        );

      function Get_Next_Suffix_Kind return Suffixes_Kind
      is
         Len : Natural;
         P : constant Natural := Gen_Name_Pos;
         C : Character;
      begin
         Len := 0;
         while Gen_Name_Pos <= Gen_Name_Length loop
            C := Name (Gen_Name_Pos);
            Gen_Name_Pos := Gen_Name_Pos + 1;
            exit when C = '_';
            Len := Len + 1;
         end loop;
         if Len = 0 then
            return Suffix_Eon;
         end if;

         case Name (P) is
            when '0' =>
               if Len = 2 and then (Name (P + 1) = '1' or Name (P + 1) = 'z')
               then
                  return Suffix_Edge;
               else
                  return Suffix_Num_Name;
               end if;
            when '1' =>
               if Len = 2 and then (Name (P + 1) = '0' or Name (P + 1) = 'z')
               then
                  return Suffix_Edge;
               else
                  return Suffix_Num_Name;
               end if;
            when '2' .. '9' =>
               return Suffix_Num_Name;
            when 'z' =>
               if Len = 2 and then (Name (P + 1) = '0' or Name (P + 1) = '1')
               then
                  return Suffix_Edge;
               else
                  return Suffix_Name;
               end if;
            when 'p' =>
               if Len = 7 and then Name (P .. P + 6) = "posedge" then
                  return Suffix_Edge;
               else
                  return Suffix_Name;
               end if;
            when 'n' =>
               if Len = 7 and then Name (P .. P + 6) = "negedge" then
                  return Suffix_Edge;
               elsif Len = 6 and then Name (P .. P + 5) = "noedge" then
                  return Suffix_Edge;
               else
                  return Suffix_Name;
               end if;
            when 'a' .. 'm'
              | 'o'
              | 'q' .. 'y' =>
               return Suffix_Name;
            when others =>
               raise Internal_Error;
         end case;
      end Get_Next_Suffix_Kind;

      --  <SDFSimpleConditionAndOrEdge> ::=
      --     <ConditionName>
      --   | <Edge>
      --   | <ConditionName>_<Edge>
      procedure Check_Simple_Condition_And_Or_Edge
      is
         First : Boolean := True;
      begin
         loop
            case Get_Next_Suffix_Kind is
               when Suffix_Eon =>
                  --  Simple condition is optional.
                  return;
               when Suffix_Edge =>
                  if Get_Next_Suffix_Kind /= Suffix_Eon then
                     Error_Vital_Name ("garbage after edge");
                  end if;
                  return;
               when Suffix_Num_Name =>
                  if First then
                     Error_Vital_Name ("condition is a simple name");
                  end if;
               when Suffix_Noedge =>
                  Error_Vital_Name
                    ("'noedge' not allowed in simple condition");
               when Suffix_Name =>
                  null;
            end case;
            First := False;
         end loop;
      end Check_Simple_Condition_And_Or_Edge;

      --  <SDFFullConditionAndOrEdge> ::=
      --    <ConditionNameEdge>[_<SDFSimpleConditionAndOrEdge>]
      --
      --  <ConditionNameEdge> ::=
      --      [<ConditionName>_]<Edge>
      --    | [<ConditionName>_]noedge
      procedure Check_Full_Condition_And_Or_Edge is
      begin
         case Get_Next_Suffix_Kind is
            when Suffix_Eon =>
               --  FullCondition is always optional.
               return;
            when Suffix_Edge
              | Suffix_Noedge =>
               Check_Simple_Condition_And_Or_Edge;
               return;
            when Suffix_Num_Name =>
               Error_Vital_Name ("condition is a simple name");
            when Suffix_Name =>
               null;
         end case;

         loop
            case Get_Next_Suffix_Kind is
               when Suffix_Eon =>
                  Error_Vital_Name ("missing edge or noedge");
                  return;
               when Suffix_Edge
                 | Suffix_Noedge =>
                  Check_Simple_Condition_And_Or_Edge;
                  return;
               when Suffix_Num_Name
                 | Suffix_Name =>
                  null;
            end case;
         end loop;
      end Check_Full_Condition_And_Or_Edge;

      procedure Check_End is
      begin
         if Get_Next_Suffix_Kind /= Suffix_Eon then
            Error_Vital_Name ("garbage at end of name");
         end if;
      end Check_End;

      --  Return the length of a port P.
      --  If P is a scalar port, return PORT_LENGTH_SCALAR
      --  If P is a vector, return the length of the vector (>= 0)
      --  Otherwise, return PORT_LENGTH_ERROR.
      Port_Length_Unknown : constant Int64 := -1;
      Port_Length_Scalar  : constant Int64 := -2;
      Port_Length_Error   : constant Int64 := -3;
      function Get_Port_Length (P : Iir) return Int64
      is
         Ptype : constant Iir := Get_Type (P);
         Itype : Iir;
      begin
         if Get_Base_Type (Ptype) = Std_Ulogic_Type then
            return Port_Length_Scalar;
         elsif Get_Kind (Ptype) = Iir_Kind_Array_Subtype_Definition
           and then Is_Slv_Subtype (Get_Base_Type (Ptype))
         then
            Itype := Get_Nth_Element (Get_Index_Subtype_List (Ptype), 0);
            if Get_Type_Staticness (Itype) /= Locally then
               return Port_Length_Unknown;
            end if;
            return Vhdl.Evaluation.Eval_Discrete_Type_Length (Itype);
         else
            return Port_Length_Error;
         end if;
      end Get_Port_Length;

      --  IEEE 1076.4  9.1  VITAL delay types and subtypes.
      --  The transition dependent delay types are
      --  VitalDelayType01, VitalDelayType01Z, VitalDelayType01ZX,
      --  VitalDelayArrayType01, VitalDelayArrayType01Z,
      --  VitalDelayArrayType01ZX.
      --  The first three are scalar forms, the last three are vector forms.
      --
      --  The simple delay types and subtypes include
      --  Time, VitalDelayType, and VitalDelayArrayType.
      --  The first two are scalar forms, and the latter is the vector form.
      type Timing_Generic_Type_Kind is
        (
         Timing_Type_Simple_Scalar,
         Timing_Type_Simple_Vector,
         Timing_Type_Trans_Scalar,
         Timing_Type_Trans_Vector,
         Timing_Type_Bad
        );

      function Get_Timing_Generic_Type_Kind return Timing_Generic_Type_Kind
      is
         Gtype : constant Iir := Get_Type (Gen_Decl);
         Btype : constant Iir := Get_Base_Type (Gtype);
      begin
         case Get_Kind (Gtype) is
            when Iir_Kind_Array_Subtype_Definition =>
               if Btype = VitalDelayArrayType then
                  return Timing_Type_Simple_Vector;
               end if;
               if Btype = VitalDelayType01
                 or Btype = VitalDelayType01Z
                 or Btype = VitalDelayType01ZX
               then
                  return Timing_Type_Trans_Scalar;
               end if;
               if Btype = VitalDelayArrayType01
                 or Btype = VitalDelayArrayType01Z
                 or Btype = VitalDelayArrayType01ZX
               then
                  return Timing_Type_Trans_Vector;
               end if;
            when Iir_Kind_Physical_Subtype_Definition =>
               if Gtype = Time_Subtype_Definition
                 or else Gtype = VitalDelayType
               then
                  return Timing_Type_Simple_Scalar;
               end if;
            when others =>
               null;
         end case;
         Error_Vital (+Gen_Decl,
                      "type of timing generic is not a VITAL delay type");
         return Timing_Type_Bad;
      end Get_Timing_Generic_Type_Kind;

      function Get_Timing_Generic_Type_Length return Int64
      is
         Itype : Iir;
      begin
         Itype := Get_Nth_Element
           (Get_Index_Subtype_List (Get_Type (Gen_Decl)), 0);
         if Get_Type_Staticness (Itype) /= Locally then
            return Port_Length_Unknown;
         else
            return Vhdl.Evaluation.Eval_Discrete_Type_Length (Itype);
         end if;
      end Get_Timing_Generic_Type_Length;

      --  IEEE 1076.4  4.3.2.1.2  Timing generic subtypes
      --  *  If the timing generic is associated with a single port and that
      --     port is a scalar, then the type of the timing generic shall be a
      --     scalar form of delay type.
      --  *  If such a timing generic is associated with a single port and that
      --     port is a vector, then the type of the timing generic shall be a
      --     vector form of delay type, and the constraint on the generic shall
      --     match that on the associated port.
      procedure Check_Vital_Delay_Type (P : Iir;
                                        Is_Simple : Boolean := False;
                                        Is_Scalar : Boolean := False)
      is
         Kind : Timing_Generic_Type_Kind;
         Len : Int64;
         Len1 : Int64;
      begin
         Kind := Get_Timing_Generic_Type_Kind;
         if P = Null_Iir or Kind = Timing_Type_Bad then
            return;
         end if;
         Len := Get_Port_Length (P);
         if Len = Port_Length_Scalar then
            case Kind is
               when Timing_Type_Simple_Scalar =>
                  null;
               when Timing_Type_Trans_Scalar =>
                  if Is_Simple then
                     Error_Vital
                       (+Gen_Decl, "VITAL simple scalar timing type expected");
                     return;
                  end if;
               when others =>
                  Error_Vital (+Gen_Decl, "VITAL scalar timing type expected");
                  return;
            end case;
         elsif Len >= Port_Length_Unknown then
            if Is_Scalar then
               Error_Vital (+Gen_Decl, "VITAL scalar timing type expected");
               return;
            end if;

            case Kind is
               when Timing_Type_Simple_Vector =>
                  null;
               when Timing_Type_Trans_Vector =>
                  if Is_Simple then
                     Error_Vital
                       (+Gen_Decl, "VITAL simple vector timing type expected");
                     return;
                  end if;
               when others =>
                  Error_Vital (+Gen_Decl, "VITAL vector timing type expected");
                  return;
            end case;
            Len1 := Get_Timing_Generic_Type_Length;
            if Len1 /= Len then
               Error_Vital
                 (+Gen_Decl, "length of port and VITAL vector timing "
                    & "subtype does not match");
            end if;
         end if;
      end Check_Vital_Delay_Type;

      --  IEEE 1076.4  4.3.2.1.2  Timing generic subtypes
      --  * If the timing generic is associated with two scalar ports, then the
      --    type of the timing generic shall be a scalar form of delay type.
      --  * If the timing generic is associated with two ports, one or more of
      --    which is a vector, then the type of the timing generic shall be a
      --    vector form of delay type, and the length of the index range of the
      --    generic shall be equal to the product of the number of scalar
      --    subelements in the first port and the number of scalar subelements
      --    in the second port.
      procedure Check_Vital_Delay_Type
        (P1, P2 : Iir;
         Is_Simple : Boolean := False;
         Is_Scalar : Boolean := False)
      is
         Kind : Timing_Generic_Type_Kind;
         Len1 : Int64;
         Len2 : Int64;
         Lenp : Int64;
      begin
         Kind := Get_Timing_Generic_Type_Kind;
         if P1 = Null_Iir or P2 = Null_Iir or Kind = Timing_Type_Bad then
            return;
         end if;
         Len1 := Get_Port_Length (P1);
         Len2 := Get_Port_Length (P2);
         if Len1 = Port_Length_Scalar and Len2 = Port_Length_Scalar then
            case Kind is
               when Timing_Type_Simple_Scalar =>
                  null;
               when Timing_Type_Trans_Scalar =>
                  if Is_Simple then
                     Error_Vital
                       (+Gen_Decl, "VITAL simple scalar timing type expected");
                     return;
                  end if;
               when others =>
                  Error_Vital (+Gen_Decl, "VITAL scalar timing type expected");
                  return;
            end case;
         elsif Len1 >= Port_Length_Unknown or Len2 >= Port_Length_Unknown then
            if Is_Scalar then
               Error_Vital (+Gen_Decl, "VITAL scalar timing type expected");
               return;
            end if;
            case Kind is
               when Timing_Type_Simple_Vector =>
                  null;
               when Timing_Type_Trans_Vector =>
                  if Is_Simple then
                     Error_Vital
                       (+Gen_Decl, "VITAL simple vector timing type expected");
                     return;
                  end if;
               when others =>
                  Error_Vital (+Gen_Decl, "VITAL vector timing type expected");
                  return;
            end case;
            if Len1 = Port_Length_Scalar then
               Len1 := 1;
            elsif Len1 = Port_Length_Error then
               return;
            end if;
            if Len2 = Port_Length_Scalar then
               Len2 := 1;
            elsif Len2 = Port_Length_Error then
               return;
            end if;
            Lenp := Get_Timing_Generic_Type_Length;
            if Lenp /= Len1 * Len2 then
               Error_Vital
                 (+Gen_Decl, "length of port and VITAL vector timing "
                    & "subtype does not match");
            end if;
         end if;
      end Check_Vital_Delay_Type;

      function Check_Timing_Generic_Prefix
        (Decl : Iir_Interface_Constant_Declaration; Prefix_Length : Natural)
        return Boolean
      is
      begin
         --  IEEE 1076.4 4.3.1
         --  It is an error for a model to use a timing generic prefix to begin
         --  the simple name of an entity generic that is not a timing generic.
         if Len < Prefix_Length or else Name (Prefix_Length) /= '_' then
            Error_Vital
              (+Decl, "invalid use of a VITAL timing generic prefix");
            return False;
         end if;
         Gen_Name_Pos := Prefix_Length + 1;
         Gen_Name_Length := Len;
         Gen_Decl := Decl;
         return True;
      end Check_Timing_Generic_Prefix;

      --  IEEE 1076.4 4.3.2.1.3.1 Propagation Delay
      --  <VITALPropagationDelayName> ::=
      --     TPD_<InputPort>_<OutputPort>[_<SDFSimpleConditionAndOrEdge>]
      procedure Check_Propagation_Delay_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Iport : Iir;
         Oport : Iir;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 4) then
            return;
         end if;
         Iport := Check_Input_Port;
         Oport := Check_Output_Port;
         Check_Simple_Condition_And_Or_Edge;
         Check_Vital_Delay_Type (Iport, Oport);
      end Check_Propagation_Delay_Name;

      procedure Check_Test_Reference
      is
         Tport : Iir;
         Rport : Iir;
      begin
         Tport := Check_Input_Port;
         Rport := Check_Input_Port;
         Check_Full_Condition_And_Or_Edge;
         Check_Vital_Delay_Type (Tport, Rport, Is_Simple => True);
      end Check_Test_Reference;

      --  tsetup
      procedure Check_Input_Setup_Time_Name
        (Decl : Iir_Interface_Constant_Declaration) is
      begin
         if not Check_Timing_Generic_Prefix (Decl, 7) then
            return;
         end if;
         Check_Test_Reference;
      end Check_Input_Setup_Time_Name;

      --  thold
      procedure Check_Input_Hold_Time_Name
        (Decl : Iir_Interface_Constant_Declaration) is
      begin
         if not Check_Timing_Generic_Prefix (Decl, 6) then
            return;
         end if;
         Check_Test_Reference;
      end Check_Input_Hold_Time_Name;

      --  trecovery
      procedure Check_Input_Recovery_Time_Name
        (Decl : Iir_Interface_Constant_Declaration) is
      begin
         if not Check_Timing_Generic_Prefix (Decl, 10) then
            return;
         end if;
         Check_Test_Reference;
      end Check_Input_Recovery_Time_Name;

      --  tremoval
      procedure Check_Input_Removal_Time_Name
        (Decl : Iir_Interface_Constant_Declaration) is
      begin
         if not Check_Timing_Generic_Prefix (Decl, 9) then
            return;
         end if;
         Check_Test_Reference;
      end Check_Input_Removal_Time_Name;

      --  tperiod
      procedure Check_Input_Period_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Iport : Iir;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 8) then
            return;
         end if;
         Iport := Check_Input_Port;
         Check_Simple_Condition_And_Or_Edge;
         Check_Vital_Delay_Type (Iport, Is_Simple => True);
      end Check_Input_Period_Name;

      --  tpw
      procedure Check_Pulse_Width_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Iport : Iir;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 4) then
            return;
         end if;
         Iport := Check_Input_Port;
         Check_Simple_Condition_And_Or_Edge;
         Check_Vital_Delay_Type (Iport, Is_Simple => True);
      end Check_Pulse_Width_Name;

      --  tskew
      procedure Check_Input_Skew_Time_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Fport : Iir;
         Sport : Iir;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 6) then
            return;
         end if;
         Fport := Check_Port;
         Sport := Check_Port;
         Check_Full_Condition_And_Or_Edge;
         Check_Vital_Delay_Type (Fport, Sport, Is_Simple => True);
      end Check_Input_Skew_Time_Name;

      --  tncsetup
      procedure Check_No_Change_Setup_Time_Name
        (Decl : Iir_Interface_Constant_Declaration) is
      begin
         if not Check_Timing_Generic_Prefix (Decl, 9) then
            return;
         end if;
         Check_Test_Reference;
      end Check_No_Change_Setup_Time_Name;

      --  tnchold
      procedure Check_No_Change_Hold_Time_Name
        (Decl : Iir_Interface_Constant_Declaration) is
      begin
         if not Check_Timing_Generic_Prefix (Decl, 8) then
            return;
         end if;
         Check_Test_Reference;
      end Check_No_Change_Hold_Time_Name;

      --  tipd
      procedure Check_Interconnect_Path_Delay_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Iport : Iir;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 5) then
            return;
         end if;
         Iport := Check_Input_Port;
         Check_End;
         Check_Vital_Delay_Type (Iport);
      end Check_Interconnect_Path_Delay_Name;

      --  tdevice
      procedure Check_Device_Delay_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Oport : Iir;
         pragma Unreferenced (Oport);
         Pos : Natural;
         Kind : Timing_Generic_Type_Kind;
         pragma Unreferenced (Kind);
      begin
         if not Check_Timing_Generic_Prefix (Decl, 8) then
            return;
         end if;
         if Get_Next_Suffix_Kind /= Suffix_Name then
            Error_Vital_Name ("instance_name expected in VITAL generic name");
            return;
         end if;
         Pos := Gen_Name_Pos;
         if Get_Next_Suffix_Kind /= Suffix_Eon then
            Gen_Name_Pos := Pos;
            Oport := Check_Output_Port;
            Check_End;
         end if;
         Kind := Get_Timing_Generic_Type_Kind;
      end Check_Device_Delay_Name;

      --  tisd
      procedure Check_Internal_Signal_Delay_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Iport : Iir;
         Cport : Iir;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 5) then
            return;
         end if;
         Iport := Check_Input_Port;
         Cport := Check_Input_Port;
         Check_End;
         Check_Vital_Delay_Type (Iport, Cport,
                                 Is_Simple => True, Is_Scalar => True);
      end Check_Internal_Signal_Delay_Name;

      --  tbpd
      procedure Check_Biased_Propagation_Delay_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Iport : Iir;
         Oport : Iir;
         Cport : Iir;
         pragma Unreferenced (Cport);
         Clock_Start : Natural;
         Clock_End : Natural;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 5) then
            return;
         end if;
         Iport := Check_Input_Port;
         Oport := Check_Output_Port;
         Clock_Start := Gen_Name_Pos - 1; -- At the '_'.
         Cport := Check_Input_Port;
         Clock_End := Gen_Name_Pos;
         Check_Simple_Condition_And_Or_Edge;
         Check_Vital_Delay_Type (Iport, Oport);

         --  IEEE 1076.4  4.3.2.1.3.14  Biased propagation delay
         --  There shall exist, in the same entity generic clause, a
         --  corresponding propagation delay generic denoting the same ports,
         --  condition name, and edge.
         declare
            use Name_Table;

            Decl_Name : constant String := Image (Get_Identifier (Decl));

            --  '-1' is for the missing 'b' in 'tpd'.
            Tpd_Name : String
              (1 .. Gen_Name_Length - 1 - (Clock_End - Clock_Start));
            Tpd_Decl : Iir;
            Tpd_Id : Name_Id;
         begin
            Tpd_Name (1) := 't';
            --  The part before '_<ClockPort>'.
            Tpd_Name (2 .. Clock_Start - 2) :=
              Decl_Name (3 .. Clock_Start - 1);
            Tpd_Name (Clock_Start - 1 .. Tpd_Name'Last) :=
              Decl_Name (Clock_End .. Decl_Name'Last);

            Tpd_Id := Get_Identifier_No_Create (Tpd_Name);
            Tpd_Decl := Gen_Chain;
            loop
               exit when Tpd_Decl = Null_Iir;
               exit when Get_Identifier (Tpd_Decl) = Tpd_Id;
               Tpd_Decl := Get_Chain (Tpd_Decl);
            end loop;

            if Tpd_Decl = Null_Iir then
               Error_Vital
                 (+Decl,
                  "no matching 'tpd' generic for VITAL 'tbpd' timing generic");
            else
               --  IEEE 1076.4  4.3.2.1.3.14  Biased propagation delay
               --  Furthermore, the type of the biased propagation generic
               --  shall be the same as the type of the corresponding delay
               --  generic.
               if not Vhdl.Sem.Are_Trees_Equal (Get_Type (Decl),
                                                Get_Type (Tpd_Decl))
               then
                  Error_Vital
                    (+Decl, "type of VITAL 'tbpd' generic mismatch type of "
                       & "'tpd' generic");
                  Error_Vital
                    (+Tpd_Decl, "(corresponding 'tpd' timing generic)");
               end if;
            end if;
         end;
      end Check_Biased_Propagation_Delay_Name;

      --  ticd
      procedure Check_Internal_Clock_Delay_Generic_Name
        (Decl : Iir_Interface_Constant_Declaration)
      is
         Cport : Iir;
         P_End : Natural;
      begin
         if not Check_Timing_Generic_Prefix (Decl, 5) then
            return;
         end if;
         Cport := Check_Input_Port;
         P_End := Gen_Name_Pos;
         Check_End;
         Check_Vital_Delay_Type (Cport, Is_Simple => True, Is_Scalar => True);

         --  IEEE 1076.4  4.3.2.1.3.15  Internal clock delay
         --  It is an error for a clocks signal name to appear as one of the
         --  following elements in the name of a timing generic:
         --  * As either the input port in the name of a biased propagation
         --    delay generic.
         --  * As the input signal name in an internal delay timing generic.
         --  * As the test port in a timing check or recovery removal timing
         --    generic.
         --  FIXME: recovery OR removal ?

         if P_End - 1 /= Gen_Name_Length then
            --  Do not check in case of error.
            return;
         end if;
         declare
            use Name_Table;
            Port : constant String := Image (Get_Identifier (Cport));
            El : Iir;
         begin
            El := Gen_Chain;
            while El /= Null_Iir loop
               declare
                  Gen_Name : constant String := Image (Get_Identifier (El));
                  pragma Assert (Gen_Name'First = 1);
                  Offset : Natural;

                  procedure Check_Not_Clock
                  is
                     S : Natural;
                  begin
                     S := Offset;
                     loop
                        Offset := Offset + 1;
                        exit when Offset > Gen_Name'Last
                          or else Gen_Name (Offset) = '_';
                     end loop;
                     if Offset - S = Port'Length
                       and then Gen_Name (S .. Offset - 1) = Port
                     then
                        Error_Vital
                          (+El, "clock port name of 'ticd' VITAL generic must"
                             & " not appear here");
                     end if;
                  end Check_Not_Clock;
               begin
                  if Gen_Name'Last > 5
                    and then Gen_Name (1) = 't'
                  then
                     if Gen_Name (2 .. 5) = "bpd_" then
                        Offset := 6;
                        Check_Not_Clock; -- input
                        Check_Not_Clock; -- output
                     elsif Gen_Name (2 .. 5) = "isd_" then
                        Offset := 6;
                        Check_Not_Clock; -- input
                     elsif Gen_Name'Last > 10
                       and then Gen_Name (2 .. 10) = "recovery_"
                     then
                        Offset := 11;
                        Check_Not_Clock; -- test port
                     elsif Gen_Name'Last > 9
                       and then Gen_Name (2 .. 9) = "removal_"
                     then
                        Offset := 10;
                        Check_Not_Clock;
                     end if;
                  end if;
               end;
               El := Get_Chain (El);
            end loop;
         end;
      end Check_Internal_Clock_Delay_Generic_Name;

   begin
      pragma Assert (Name'First = 1);

      --  Extract prefix.
      if Name (1) = 't' and Len >= 3 then
         --  Timing generic names.
         if Name (2) = 'p' then
            if Name (3) = 'd' then
               Check_Propagation_Delay_Name (Decl); --  tpd
               return;
            elsif Name (3) = 'w' then
               Check_Pulse_Width_Name (Decl); -- tpw
               return;
            elsif Len >= 7
              and then Name (3 .. 7) = "eriod"
            then
               Check_Input_Period_Name (Decl); --  tperiod
               return;
            end if;
         elsif Name (2) = 'i'
           and then Len >= 4
           and then Name (4) = 'd'
         then
            if Name (3) = 'p' then
               Check_Interconnect_Path_Delay_Name (Decl); --  tipd
               return;
            elsif Name (3) = 's' then
               Check_Internal_Signal_Delay_Name (Decl); --  tisd
               return;
            elsif Name (3) = 'c' then
               Check_Internal_Clock_Delay_Generic_Name (Decl); --  ticd
               return;
            end if;
         elsif Len >= 6 and then Name (2 .. 6) = "setup" then
            Check_Input_Setup_Time_Name (Decl); --  tsetup
            return;
         elsif Len >= 5 and then Name (2 .. 5) = "hold" then
            Check_Input_Hold_Time_Name (Decl); -- thold
            return;
         elsif Len >= 9 and then Name (2 .. 9) = "recovery" then
            Check_Input_Recovery_Time_Name (Decl); -- trecovery
            return;
         elsif Len >= 8 and then Name (2 .. 8) = "removal" then
            Check_Input_Removal_Time_Name (Decl); -- tremoval
            return;
         elsif Len >= 5 and then Name (2 .. 5) = "skew" then
            Check_Input_Skew_Time_Name (Decl); -- tskew
            return;
         elsif Len >= 8 and then Name (2 .. 8) = "ncsetup" then
            Check_No_Change_Setup_Time_Name (Decl); -- tncsetup
            return;
         elsif Len >= 7 and then Name (2 .. 7) = "nchold" then
            Check_No_Change_Hold_Time_Name (Decl); -- tnchold
            return;
         elsif Len >= 7 and then Name (2 .. 7) = "device" then
            Check_Device_Delay_Name (Decl); -- tdevice
            return;
         elsif Len >= 4 and then Name (2 .. 4) = "bpd" then
            Check_Biased_Propagation_Delay_Name (Decl); -- tbpd
            return;
         end if;
      end if;

      if Id = InstancePath_Id then
         if Get_Base_Type (Get_Type (Decl)) /= String_Type_Definition then
            Error_Vital
              (+Decl, "InstancePath VITAL generic must be of type String");
         end if;
         return;
      elsif Id = TimingChecksOn_Id
        or Id = XOn_Id
        or Id = MsgOn_Id
      then
         if Get_Type (Decl) /= Boolean_Type_Definition then
            Error_Vital
              (+Decl, "%i VITAL generic must be of type Boolean", (1 => +Id));
         end if;
         return;
      end if;

      if Is_Warning_Enabled (Warnid_Vital_Generic) then
         Warning_Vital (Decl, "%n is not a VITAL generic", (1 => +Decl));
      end if;
   end Check_Entity_Generic_Declaration;

   --  Checks rules for a VITAL level 0 entity.
   procedure Check_Vital_Level0_Entity (Ent : Iir_Entity_Declaration)
   is
      use Vhdl.Sem_Scopes;
      Decl : Iir;
      Gen_Chain : Iir;
   begin
      --  IEEE 1076.4 4.3.1
      --  The only form of declaration allowed in the entity declarative part
      --  is the specification of the VITAL_Level0 attribute.
      Decl := Get_Declaration_Chain (Ent);
      if Decl = Null_Iir then
         --  Cannot happen, since there is at least the attribute spec.
         raise Internal_Error;
      end if;
      Check_Level0_Attribute_Specification (Decl);
      Decl := Get_Chain (Decl);
      if Decl /= Null_Iir then
         Error_Vital (+Decl, "VITAL entity declarative part must only contain "
                      & "the attribute specification");
      end if;

      --  IEEE 1076.4 4.3.1
      --  No statements are allowed in the entity statement part.
      Decl := Get_Concurrent_Statement_Chain (Ent);
      if Decl /= Null_Iir then
         Error_Vital
           (+Decl, "VITAL entity must not have concurrent statement");
      end if;

      --  Check ports.
      Push_Interpretations;
      Open_Declarative_Region;
      Decl := Get_Port_Chain (Ent);
      while Decl /= Null_Iir loop
         Check_Entity_Port_Declaration (Decl);
         Add_Name (Decl);
         Decl := Get_Chain (Decl);
      end loop;

      --  Check generics.
      Gen_Chain := Get_Generic_Chain (Ent);
      Decl := Gen_Chain;
      while Decl /= Null_Iir loop
         Check_Entity_Generic_Declaration (Decl, Gen_Chain);
         Decl := Get_Chain (Decl);
      end loop;
      Close_Declarative_Region;
      Pop_Interpretations;
   end Check_Vital_Level0_Entity;

   --  Return TRUE if UNIT was decorated with attribute VITAL_Level0.
   function Is_Vital_Level0 (Unit : Iir_Entity_Declaration) return Boolean
   is
      Value : Iir_Attribute_Value;
      Spec : Iir_Attribute_Specification;
   begin
      Value := Vhdl.Sem_Specs.Find_Attribute_Value
        (Unit, Std_Names.Name_VITAL_Level0);
      if Value = Null_Iir then
         return False;
      end if;
      Spec := Get_Attribute_Specification (Value);
      return Get_Named_Entity (Get_Attribute_Designator (Spec))
        = Vital_Level0_Attribute;
   end Is_Vital_Level0;

   procedure Check_Vital_Level0_Architecture (Arch : Iir_Architecture_Body)
   is
      Decl : Iir;
   begin
      --  IEEE 1076.4 4.1
      --  The entity associated with a Level 0 architecture shall be a VITAL
      --  Level 0 entity.
      if not Is_Vital_Level0 (Utils.Get_Entity (Arch)) then
         Error_Vital (+Arch, "entity associated with a VITAL level 0 "
                        & "architecture shall be a VITAL level 0 entity");
      end if;

      --  VITAL_Level_0_architecture_declarative_part ::=
      --    VITAL_Level0_attribute_specification { block_declarative_item }
      Decl := Get_Declaration_Chain (Arch);
      Check_Level0_Attribute_Specification (Decl);
   end Check_Vital_Level0_Architecture;

   --  Check a VITAL level 0 decorated design unit.
   procedure Check_Vital_Level0 (Unit : Iir_Design_Unit)
   is
      Lib_Unit : Iir;
   begin
      Lib_Unit := Get_Library_Unit (Unit);
      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Entity_Declaration =>
            Check_Vital_Level0_Entity (Lib_Unit);
         when Iir_Kind_Architecture_Body =>
            Check_Vital_Level0_Architecture (Lib_Unit);
         when others =>
            Error_Vital
              (+Lib_Unit, "only entity or architecture can be VITAL_Level0");
      end case;
   end Check_Vital_Level0;

   procedure Check_Vital_Level1 (Unit : Iir_Design_Unit)
   is
      Arch : Iir;
   begin
      Arch := Get_Library_Unit (Unit);
      if Get_Kind (Arch) /= Iir_Kind_Architecture_Body then
         Error_Vital (+Arch, "only architecture can be VITAL_Level1");
         return;
      end if;
      --  FIXME: todo
   end Check_Vital_Level1;

end Vhdl.Ieee.Vital_Timing;
