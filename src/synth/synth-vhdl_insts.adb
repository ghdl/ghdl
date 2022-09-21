--  Instantiation synthesis.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
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

with GNAT.SHA1;

with Types_Utils; use Types_Utils;
with Name_Table;
with Std_Names;
with Hash; use Hash;
with Dyn_Tables;
with Interning;
with Areapools;
with Synthesis; use Synthesis;

with Grt.Algos;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Concats;
with Netlists.Folds;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors;
with Vhdl.Evaluation;
with Vhdl.Ieee.Math_Real;
with Vhdl.Std_Package;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Files;
with Elab.Debugger;
with Elab.Vhdl_Errors;

with Synth.Vhdl_Environment; use Synth.Vhdl_Environment.Env;
with Synth.Vhdl_Stmts; use Synth.Vhdl_Stmts;
with Synth.Vhdl_Decls; use Synth.Vhdl_Decls;
with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Source; use Synth.Source;
with Synth.Errors;
with Synth.Vhdl_Context; use Synth.Vhdl_Context;

package body Synth.Vhdl_Insts is
   Global_Base_Instance : Base_Instance_Acc;

   function Mode_To_Port_Kind (Mode : Iir_Mode) return Port_Kind is
   begin
      case Mode is
         when Iir_In_Mode =>
            return Port_In;
         when Iir_Buffer_Mode
           | Iir_Out_Mode =>
            return Port_Out;
         when Iir_Inout_Mode =>
            return Port_Inout;
         when Iir_Linkage_Mode
           | Iir_Unknown_Mode =>
            raise Synth_Error;
      end case;
   end Mode_To_Port_Kind;

   --  Parameters that define an instance.
   type Inst_Params is record
      --  Declaration: either the entity or the component.
      Decl : Node;
      --  Implementation: the architecture or Null_Node for black boxes.
      Arch : Node;
      --  Configuration (Null_Node for black boxes).
      Config : Node;
      --  Values of generics.
      Syn_Inst : Synth_Instance_Acc;
      --  Encoding if the instance name.
      Encoding : Name_Encoding;
   end record;

   type Inst_Object is record
      Decl : Node;
      Arch : Node;
      Config : Node;
      Syn_Inst : Synth_Instance_Acc;
      M : Module;
      --  Encoding if the instance name.
      Encoding : Name_Encoding;
   end record;

   function Hash (Params : Inst_Params) return Hash_Value_Type
   is
      Res : Hash_Value_Type;
   begin
      Res := Hash_Value_Type (Params.Decl);
      Res := Res xor Hash_Value_Type (Params.Arch);
      Res := Res xor Hash_Value_Type (Params.Config);
      --  TODO: hash generics
      return Res;
   end Hash;

   function Equal (Obj : Inst_Object; Params : Inst_Params) return Boolean
   is
      Inter : Node;
   begin
      if Obj.Decl /= Params.Decl
        or else Obj.Arch /= Params.Arch
        or else Obj.Config /= Params.Config
      then
         return False;
      end if;
      Inter := Get_Generic_Chain (Params.Decl);
      while Inter /= Null_Node loop
         pragma Assert (Get_Kind (Inter)
                          = Iir_Kind_Interface_Constant_Declaration);
         if not Is_Equal (Get_Value (Obj.Syn_Inst, Inter),
                          Get_Value (Params.Syn_Inst, Inter))
         then
            return False;
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      Inter := Get_Port_Chain (Params.Decl);
      while Inter /= Null_Node loop
         pragma Assert (Get_Kind (Inter)
                          = Iir_Kind_Interface_Signal_Declaration);
         if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
            if not Are_Types_Equal (Get_Value (Obj.Syn_Inst, Inter).Typ,
                                    Get_Value (Params.Syn_Inst, Inter).Typ)
            then
               return False;
            end if;
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      return True;
   end Equal;

   procedure Hash_Uns64 (C : in out GNAT.SHA1.Context; Val : Uns64)
   is
      V : Uns64;
      S : String (1 .. 8);
   begin
      --  Store to S using little endianness.
      V := Val;
      for I in S'Range loop
         S (I) := Character'Val (V and 16#ff#);
         V := Shift_Right (V, 8);
      end loop;

      GNAT.SHA1.Update (C, S);
   end Hash_Uns64;

   procedure Hash_Memory (C : in out GNAT.SHA1.Context;
                          M : Memory_Ptr;
                          Typ : Type_Acc)
   is
      S : String (1 .. Natural (Typ.Sz));
      for S'Address use M (0)'Address;
      pragma Import (Ada, S);
   begin
      GNAT.SHA1.Update (C, S);
   end Hash_Memory;

   procedure Hash_Bound (C : in out GNAT.SHA1.Context; B : Bound_Type) is
   begin
      Hash_Uns64 (C, Direction_Type'Pos (B.Dir));
      Hash_Uns64 (C, To_Uns64 (Int64 (B.Left)));
      Hash_Uns64 (C, To_Uns64 (Int64 (B.Right)));
   end Hash_Bound;

   procedure Hash_Bounds (C : in out GNAT.SHA1.Context; Typ : Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Vector =>
            Hash_Bound (C, Typ.Abound);
         when Type_Array =>
            declare
               T : Type_Acc;
            begin
               T := Typ;
               loop
                  Hash_Bound (C, T.Abound);
                  exit when T.Alast;
                  T := T.Arr_El;
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Hash_Bounds (C, Typ.Rec.E (I).Typ);
            end loop;
         when Type_Bit
           | Type_Logic =>
            null;
         when others =>
            raise Internal_Error;
      end case;
   end Hash_Bounds;

   procedure Hash_Const (C : in out GNAT.SHA1.Context;
                         Val : Value_Acc;
                         Typ : Type_Acc) is
   begin
      case Val.Kind is
         when Value_Memory =>
            Hash_Memory (C, Val.Mem, Typ);
         when Value_Const =>
            Hash_Const (C, Val.C_Val, Typ);
         when Value_Alias =>
            if Val.A_Off /= (0, 0) then
               raise Internal_Error;
            end if;
            Hash_Const (C, Val.A_Obj, Typ);
         when Value_Net
           | Value_Wire
           | Value_Signal
           | Value_File
           | Value_Quantity
           | Value_Terminal
           | Value_Dyn_Alias =>
            raise Internal_Error;
      end case;
   end Hash_Const;

   function Create_Module_Name (Params : Inst_Params) return Sname
   is
      use GNAT.SHA1;
      Decl : constant Node := Params.Decl;
      Id : constant Name_Id := Get_Identifier (Decl);
      Generics : constant Node := Get_Generic_Chain (Decl);
      Ports : constant Node := Get_Port_Chain (Decl);
      Ctxt : GNAT.SHA1.Context;
      Has_Hash : Boolean;

      --  Create a buffer, store the entity name.
      --  For each generic:
      --  * write the value for integers.
      --  * write the identifier for enumerated type with only non-extended
      --    identifiers.
      --  * hash all other values
      --  Append the hash if any.
      use Name_Table;
      Id_Len : constant Natural := Get_Name_Length (Id);
      Str_Len : constant Natural := Id_Len + 512;

      --  True in practice (and used to set the length of STR, but doesn't work
      --  anymore with gcc/gnat 11.
      --  pragma Assert (GNAT.SHA1.Hash_Length = 20);
      Str : String (1 .. Str_Len + 41);
      Len : Natural;

      Gen_Decl : Node;
      Gen : Valtyp;
   begin
      Len := Id_Len;
      Str (1 .. Len) := Get_Name_Ptr (Id) (1 .. Len);

      Has_Hash := False;

      case Params.Encoding is
         when Name_Hash =>
            Ctxt := GNAT.SHA1.Initial_Context;

            Gen_Decl := Generics;
            while Gen_Decl /= Null_Node loop
               if Get_Kind (Gen_Decl) = Iir_Kind_Interface_Constant_Declaration
               then
                  Gen := Get_Value (Params.Syn_Inst, Gen_Decl);
                  Strip_Const (Gen);
                  case Gen.Typ.Kind is
                     when Type_Discrete =>
                        declare
                           S : constant String :=
                             Uns64'Image (To_Uns64 (Read_Discrete (Gen)));
                        begin
                           if Len + S'Length > Str_Len then
                              Has_Hash := True;
                              Hash_Const (Ctxt, Gen.Val, Gen.Typ);
                           else
                              Str (Len + 1 .. Len + S'Length) := S;
                              pragma Assert (Str (Len + 1) = ' ');
                              Str (Len + 1) := '_';  --  Overwrite the space.
                              Len := Len + S'Length;
                           end if;
                        end;
                     when others =>
                        Has_Hash := True;
                        Hash_Const (Ctxt, Gen.Val, Gen.Typ);
                  end case;
               else
                  --  TODO: add a unique number (index)
                  null;
               end if;
               Gen_Decl := Get_Chain (Gen_Decl);
            end loop;

            declare
               Port_Decl : Node;
               Port_Typ : Type_Acc;
            begin
               Port_Decl := Ports;
               while Port_Decl /= Null_Node loop
                  if not Is_Fully_Constrained_Type (Get_Type (Port_Decl)) then
                     Port_Typ := Get_Value (Params.Syn_Inst, Port_Decl).Typ;
                     Has_Hash := True;
                     Hash_Bounds (Ctxt, Port_Typ);
                  end if;
                  Port_Decl := Get_Chain (Port_Decl);
               end loop;
            end;
            if not Has_Hash and then Generics = Null_Node then
               --  Simple case: same name.
               --  TODO: what about two entities with the same identifier but
               --   declared in two different libraries ?
               --  TODO: what about extended identifiers ?
               return New_Sname_User (Id, No_Sname);
            end if;

            if Has_Hash then
               Str (Len + 1) := '_';
               Len := Len + 1;
               Str (Len + 1 .. Len + 40) := GNAT.SHA1.Digest (Ctxt);
               Len := Len + 40;
            end if;

         when Name_Asis
           | Name_Parameters =>
            return New_Sname_User (Get_Source_Identifier (Decl), No_Sname);

         when Name_Index =>
            --  TODO.
            raise Internal_Error;
      end case;


      return New_Sname_User (Get_Identifier (Str (1 .. Len)), No_Sname);
   end Create_Module_Name;

   --  Create the name of an interface.
   function Get_Encoded_Name_Id (Decl : Node; Enc : Name_Encoding)
                                return Name_Id is
   begin
      case Enc is
         when Name_Asis
           | Name_Parameters =>
            return Get_Source_Identifier (Decl);
         when others =>
            return Get_Identifier (Decl);
      end case;
   end Get_Encoded_Name_Id;

   --  Create the name of an interface.
   function Create_Inter_Name (Decl : Node; Enc : Name_Encoding)
                              return Sname is
   begin
      return New_Sname_User (Get_Encoded_Name_Id (Decl, Enc), No_Sname);
   end Create_Inter_Name;

   --  Return the number of ports for a type.  A record type create one
   --  port per immediate subelement.  Sub-records are not expanded.
   function Count_Nbr_Ports (Typ : Type_Acc) return Port_Nbr is
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float
           | Type_Vector
           | Type_Unbounded_Vector
           | Type_Array
           | Type_Unbounded_Array =>
            return 1;
         when Type_Record
           | Type_Unbounded_Record =>
            return Port_Nbr (Typ.Rec.Len);
         when Type_Slice
           | Type_Access
           | Type_File
           | Type_Protected =>
            raise Internal_Error;
      end case;
   end Count_Nbr_Ports;

   function Get_Type2 (N : Node) return Node
   is
      Res : Node;
   begin
      Res := Get_Type (N);
      if Get_Kind (Res) = Iir_Kind_Interface_Type_Definition then
         Res := Get_Associated_Type (Res);
      end if;
      return Res;
   end Get_Type2;

   procedure Build_Ports_Desc (Descs : in out Port_Desc_Array;
                               Idx : in out Port_Nbr;
                               Pkind : Port_Kind;
                               Encoding : Name_Encoding;
                               Typ : Type_Acc;
                               Inter : Node)
   is
      Port_Sname : Sname;
   begin
      Port_Sname := Create_Inter_Name (Inter, Encoding);

      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float
           | Type_Vector
           | Type_Unbounded_Vector
           | Type_Array
           | Type_Unbounded_Array =>
            Idx := Idx + 1;
            Descs (Idx) := (Name => Port_Sname,
                            Dir => Pkind,
                            W => Get_Type_Width (Typ));
         when Type_Record
           | Type_Unbounded_Record =>
            declare
               Els : constant Node_Flist := Get_Elements_Declaration_List
                 (Get_Type2 (Inter));
               El : Node;
            begin
               for I in Typ.Rec.E'Range loop
                  El := Get_Nth_Element (Els, Natural (I - 1));
                  Idx := Idx + 1;
                  Descs (Idx) :=
                    (Name => New_Sname_User
                       (Get_Encoded_Name_Id (El, Encoding), Port_Sname),
                     Dir => Pkind,
                     W => Get_Type_Width (Typ.Rec.E (I).Typ));
               end loop;
            end;
         when Type_Slice
           | Type_Access
           | Type_File
           | Type_Protected =>
            raise Internal_Error;
      end case;
   end Build_Ports_Desc;

   function Build (Params : Inst_Params) return Inst_Object
   is
      Decl : constant Node := Params.Decl;
      Arch : constant Node := Params.Arch;
      Inter : Node;
      Inter_Typ : Type_Acc;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Nbr_Params : Param_Nbr;
      Cur_Module : Module;
      Val : Valtyp;
      Id : Module_Id;
   begin
      --  Copy values for generics.
      Inter := Get_Generic_Chain (Decl);
      Nbr_Params := 0;
      while Inter /= Null_Node loop
         Nbr_Params := Nbr_Params + 1;
         Inter := Get_Chain (Inter);
      end loop;

      --  Allocate values and count inputs and outputs
      Inter := Get_Port_Chain (Decl);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      Current_Pool := Global_Pool'Access;
      while Is_Valid (Inter) loop
         Inter_Typ := Get_Value (Params.Syn_Inst, Inter).Typ;

         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Val := Create_Value_Net (No_Net, Inter_Typ);
               Nbr_Inputs := Nbr_Inputs + Count_Nbr_Ports (Inter_Typ);
            when Port_Out
              | Port_Inout =>
               Val := Create_Value_Wire
                 (No_Wire_Id, Inter_Typ, Current_Pool);
               Nbr_Outputs := Nbr_Outputs + Count_Nbr_Ports (Inter_Typ);
         end case;
         Replace_Signal (Params.Syn_Inst, Inter, Val);
         Inter := Get_Chain (Inter);
      end loop;
      Current_Pool := Expr_Pool'Access;

      --  Declare module.
      --  Build it now because it may be referenced for instantiations before
      --  being synthetized.
      if Params.Encoding = Name_Parameters
        and then Nbr_Params > 0
      then
         Id := Id_User_Parameters;
      else
         Id := Id_User_None;
         Nbr_Params := 0;
      end if;
      Cur_Module := New_User_Module (Get_Top_Module (Root_Instance),
                                     Create_Module_Name (Params), Id,
                                     Nbr_Inputs, Nbr_Outputs, Nbr_Params);

      if Id = Id_User_Parameters then
         declare
            Descs : Param_Desc_Array (1 .. Nbr_Params);
            Ptype : Param_Type;
         begin
            Inter := Get_Generic_Chain (Decl);
            Nbr_Params := 0;
            while Inter /= Null_Node loop
               --  Bounds or range of the type.
               Ptype := Type_To_Param_Type (Get_Type (Inter));
               Nbr_Params := Nbr_Params + 1;
               Descs (Nbr_Params) :=
                 (Name => Create_Inter_Name (Inter, Params.Encoding),
                  Typ => Ptype);
               Inter := Get_Chain (Inter);
            end loop;
            Set_Params_Desc (Cur_Module, Descs);
         end;
      end if;

      --  Add ports to module.
      declare
         Inports : Port_Desc_Array (1 .. Nbr_Inputs);
         Outports : Port_Desc_Array (1 .. Nbr_Outputs);
         Pkind : Port_Kind;
         Vt : Valtyp;
      begin
         Inter := Get_Port_Chain (Decl);
         Nbr_Inputs := 0;
         Nbr_Outputs := 0;
         while Is_Valid (Inter) loop
            Pkind := Mode_To_Port_Kind (Get_Mode (Inter));
            Vt := Get_Value (Params.Syn_Inst, Inter);

            case Pkind is
               when Port_In =>
                  Build_Ports_Desc (Inports, Nbr_Inputs,
                                    Pkind, Params.Encoding,
                                    Vt.Typ, Inter);
               when Port_Out
                 | Port_Inout =>
                  Build_Ports_Desc (Outports, Nbr_Outputs,
                                    Pkind, Params.Encoding,
                                    Vt.Typ, Inter);
            end case;
            Inter := Get_Chain (Inter);
         end loop;
         pragma Assert (Nbr_Inputs = Inports'Last);
         pragma Assert (Nbr_Outputs = Outports'Last);
         Set_Ports_Desc (Cur_Module, Inports, Outports);
      end;

      Set_Extra (Params.Syn_Inst, Global_Base_Instance, No_Sname);

      return Inst_Object'(Decl => Decl,
                          Arch => Arch,
                          Config => Params.Config,
                          Syn_Inst => Params.Syn_Inst,
                          M => Cur_Module,
                          Encoding => Params.Encoding);
   end Build;

   package Insts_Interning is new Interning
     (Params_Type => Inst_Params,
      Object_Type => Inst_Object,
      Hash => Hash,
      Build => Build,
      Equal => Equal);

   function Is_Arch_Black_Box (Arch : Node) return Boolean
   is
      use Vhdl.Std_Package;
      use Elab.Vhdl_Errors;
      Value : Node;
      Spec : Node;
      Attr_Decl : Node;
      Val : Node;
   begin
      if Arch = Null_Node then
         return True;
      end if;

      --  A little bit like Find_Attribute_Value, but recoded to handle
      --  many attributes.
      Value := Get_Attribute_Value_Chain (Arch);
      while Value /= Null_Node loop
         if Get_Designated_Entity (Value) = Arch then
            Spec := Get_Attribute_Specification (Value);
            Attr_Decl := Get_Named_Entity (Get_Attribute_Designator (Spec));
            case Get_Identifier (Attr_Decl) is
               when Std_Names.Name_Syn_Black_Box =>
                  if Get_Type (Attr_Decl) /= Boolean_Type_Definition then
                     Error_Msg_Elab
                       (+Attr_Decl,
                        "type of syn_black_box attribute must be boolean");
                     return True;
                  end if;
                  Val := Get_Expression (Spec);
                  if Get_Expr_Staticness (Val) /= Locally then
                     --  Do we really require the value to be static if the
                     --  architecture has been elaborated ?
                     Error_Msg_Elab
                       (+Spec, "value of syn_black_box must be static");
                     return True;
                  end if;
                  if Vhdl.Evaluation.Eval_Pos (Val) /= 0 then
                     return True;
                  end if;
               when others =>
                  null;
            end case;
         end if;
         Value := Get_Value_Chain (Value);
      end loop;
      return False;
   end Is_Arch_Black_Box;

   function Interning_Get (Param : Inst_Params) return Inst_Object is
   begin
      if Is_Arch_Black_Box (Param.Arch) then
         return Insts_Interning.Get ((Decl => Param.Decl,
                                      Arch => Null_Node,
                                      Config => Null_Node,
                                      Syn_Inst => Param.Syn_Inst,
                                      Encoding => Name_Parameters));
      else
         return Insts_Interning.Get (Param);
      end if;
   end Interning_Get;

   function Synth_Single_Input_Assoc (Syn_Inst : Synth_Instance_Acc;
                                      Inter_Typ : Type_Acc;
                                      Act_Inst : Synth_Instance_Acc;
                                      Actual : Node;
                                      Assoc : Node) return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Conv : Node;
      Act : Valtyp;
   begin
      if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Name then
         Conv := Get_Actual_Conversion (Assoc);
      else
         Conv := Null_Node;
      end if;
      if Conv /= Null_Node then
         case Get_Kind (Conv) is
            when Iir_Kind_Function_Call =>
               pragma Assert (Act_Inst = Syn_Inst);
               --  This is an abuse, but it works like a user operator.
               Act := Synth_User_Operator (Syn_Inst, Actual, Null_Node, Conv);
            when Iir_Kind_Type_Conversion =>
               Act := Synth_Type_Conversion (Syn_Inst, Conv);
            when others =>
               Vhdl.Errors.Error_Kind ("synth_single_input_assoc", Conv);
         end case;
      elsif Actual = Null_Node then
         --  No actual, no default value.
         Act := Create_Value_Net
           (Build_Const_X (Ctxt, Inter_Typ.W), Inter_Typ);
      else
         Act := Synth_Expression_With_Type (Act_Inst, Actual, Inter_Typ);
      end if;

      Act := Synth_Subtype_Conversion (Act_Inst, Act, Inter_Typ, False, Assoc);
      return Act;
   end Synth_Single_Input_Assoc;

   type Value_Offset_Record is record
      Off : Uns32;
      Val : Valtyp;
   end record;

   package Value_Offset_Tables is new Dyn_Tables
     (Table_Component_Type => Value_Offset_Record,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1);

   procedure Sort_Value_Offset (Els : Value_Offset_Tables.Instance)
   is
      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Els.Table (Op1).Off < Els.Table (Op2).Off;
      end Lt;

      procedure Swap (From : Natural; To : Natural)
      is
         T : constant Value_Offset_Record := Els.Table (From);
      begin
         Els.Table (From) := Els.Table (To);
         Els.Table (To) := T;
      end Swap;

      procedure Heap_Sort is new Grt.Algos.Heap_Sort (Lt => Lt, Swap => Swap);
   begin
      Heap_Sort (Value_Offset_Tables.Last (Els));
   end Sort_Value_Offset;

   function Synth_Individual_Input_Assoc (Syn_Inst : Synth_Instance_Acc;
                                          Assoc : Node;
                                          Inter_Inst : Synth_Instance_Acc)
                                         return Net
   is
      use Netlists.Concats;
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Iassoc : Node;
      V : Valtyp;
      Typ : Type_Acc;
      Els : Value_Offset_Tables.Instance;
      Concat : Concat_Type;
      N_Off : Uns32;
      N : Net;
      Base : Valtyp;
      Offs : Value_Offsets;
      Dyn : Dyn_Name;
   begin
      Value_Offset_Tables.Init (Els, 16);

      Iassoc := Get_Chain (Assoc);
      while Iassoc /= Null_Node
        and then not Get_Whole_Association_Flag (Iassoc)
      loop
         --  For each individual assoc:
         --   1. compute type and offset
         Synth_Assignment_Prefix
           (Syn_Inst, Inter_Inst, Get_Formal (Iassoc), Base, Typ, Offs, Dyn);
         pragma Assert (Dyn = No_Dyn_Name);

         --   2. synth expression
         V := Synth_Single_Input_Assoc
           (Syn_Inst, Typ, Syn_Inst, Get_Actual (Iassoc), Iassoc);

         --   3. save in a table
         Value_Offset_Tables.Append (Els, (Offs.Net_Off, V));

         Iassoc := Get_Chain (Iassoc);
      end loop;

      pragma Unreferenced (Base);

      --  Then:
      --   1. sort table by offset
      Sort_Value_Offset (Els);

      --   2. concat
      N_Off := 0;
      for I in Value_Offset_Tables.First .. Value_Offset_Tables.Last (Els)
      loop
         pragma Assert (N_Off = Els.Table (I).Off);
         V := Els.Table (I).Val;
         N_Off := N_Off + V.Typ.W;
         Append (Concat, Get_Net (Ctxt, V));
      end loop;
      Value_Offset_Tables.Free (Els);

      --   3. connect
      Build (Ctxt, Concat, N);
      return N;
   end Synth_Individual_Input_Assoc;

   function Synth_Input_Assoc (Syn_Inst : Synth_Instance_Acc;
                               Assoc : Node;
                               Inter_Inst : Synth_Instance_Acc;
                               Inter : Node;
                               Inter_Typ : Type_Acc) return Net
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Marker : Mark_Type;
      Res : Valtyp;
      Res_Net : Net;
   begin
      Mark_Expr_Pool (Marker);
      case Iir_Kinds_Association_Element_Parameters (Get_Kind (Assoc)) is
         when Iir_Kind_Association_Element_Open =>
            Res := Synth_Single_Input_Assoc
              (Syn_Inst, Inter_Typ, Inter_Inst,
               Get_Default_Value (Inter), Assoc);
         when Iir_Kind_Association_Element_By_Expression
            | Iir_Kind_Association_Element_By_Name =>
            Res := Synth_Single_Input_Assoc
              (Syn_Inst, Inter_Typ, Syn_Inst, Get_Actual (Assoc), Assoc);
         when Iir_Kind_Association_Element_By_Individual =>
            Res_Net := Synth_Individual_Input_Assoc
              (Syn_Inst, Assoc, Inter_Inst);
            Release_Expr_Pool (Marker);
            return Res_Net;
      end case;

      if Res = No_Valtyp then
         return No_Net;
      end if;
      Res_Net := Get_Net (Ctxt, Res);
      Release_Expr_Pool (Marker);

      return Res_Net;
   end Synth_Input_Assoc;

   procedure Synth_Individual_Output_Assoc (Outp : Net;
                                            Syn_Inst : Synth_Instance_Acc;
                                            Assoc : Node;
                                            Inter_Inst : Synth_Instance_Acc)
   is
      Marker : Mark_Type;
      Iassoc : Node;
      V : Valtyp;
      Typ : Type_Acc;
      O : Net;
      Port : Net;
      Base : Valtyp;
      Dyn : Dyn_Name;
      Offs : Value_Offsets;
   begin
      Mark_Expr_Pool (Marker);

      Port := Builders.Build_Port (Get_Build (Syn_Inst), Outp);
      Set_Location (Port, Assoc);

      Iassoc := Get_Chain (Assoc);
      while Iassoc /= Null_Node
        and then not Get_Whole_Association_Flag (Iassoc)
      loop
         --  For each individual assoc:
         --   1. compute type and offset
         Synth_Assignment_Prefix
           (Syn_Inst, Inter_Inst, Get_Formal (Iassoc), Base, Typ, Offs, Dyn);
         pragma Assert (Dyn = No_Dyn_Name);

         --   2. Extract the value.
         O := Build_Extract (Get_Build (Syn_Inst), Port, Offs.Net_Off, Typ.W);
         V := Create_Value_Net (O, Typ);

         --   3. Assign.
         Synth_Assignment (Syn_Inst, Get_Actual (Iassoc), V, Iassoc);

         Release_Expr_Pool (Marker);

         Iassoc := Get_Chain (Iassoc);
      end loop;
   end Synth_Individual_Output_Assoc;

   procedure Synth_Output_Assoc (Outp : Net;
                                 Syn_Inst : Synth_Instance_Acc;
                                 Assoc : Node;
                                 Inter_Inst : Synth_Instance_Acc;
                                 Inter : Node)
   is
      Marker : Mark_Type;
      Actual : Node;
      Formal_Typ : Type_Acc;
      Port : Net;
      O : Valtyp;
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Association_Element_Open =>
            --  Not connected.
            return;
         when Iir_Kinds_Association_Element_By_Actual =>
            Actual := Get_Actual (Assoc);
         when others =>
            Synth_Individual_Output_Assoc
              (Outp, Syn_Inst, Assoc, Inter_Inst);
            return;
      end case;

      Formal_Typ := Get_Value (Inter_Inst, Inter).Typ;

      Mark_Expr_Pool (Marker);
      --  Create a port gate (so that is has a name).
      Port := Builders.Build_Port (Get_Build (Syn_Inst), Outp);
      Set_Location (Port, Assoc);
      O := Create_Value_Net (Port, Formal_Typ);
      --  Assign the port output to the actual (a net).
      Synth_Assignment (Syn_Inst, Actual, O, Assoc);
      Release_Expr_Pool (Marker);
   end Synth_Output_Assoc;

   procedure Inst_Input_Connect (Syn_Inst : Synth_Instance_Acc;
                                 Inst : Instance;
                                 Port : in out Port_Idx;
                                 Inter_Typ : Type_Acc;
                                 N : Net) is
   begin
      case Inter_Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float
           | Type_Vector
           | Type_Unbounded_Vector
           | Type_Array
           | Type_Unbounded_Array =>
            if N /= No_Net then
               Connect (Get_Input (Inst, Port), N);
            end if;
            Port := Port + 1;
         when Type_Record
           | Type_Unbounded_Record =>
            for I in Inter_Typ.Rec.E'Range loop
               if N /= No_Net then
                  Connect (Get_Input (Inst, Port),
                           Build_Extract (Get_Build (Syn_Inst), N,
                                          Inter_Typ.Rec.E (I).Offs.Net_Off,
                                          Inter_Typ.Rec.E (I).Typ.W));
               end if;
               Port := Port + 1;
            end loop;
         when Type_Slice
           | Type_Access
           | Type_File
           | Type_Protected =>
            raise Internal_Error;
      end case;
   end Inst_Input_Connect;

   procedure Inst_Output_Connect (Syn_Inst : Synth_Instance_Acc;
                                  Inst : Instance;
                                  Idx : in out Port_Idx;
                                  Inter_Typ : Type_Acc;
                                  N : out Net) is
   begin
      case Inter_Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float
           | Type_Vector
           | Type_Unbounded_Vector
           | Type_Array
           | Type_Unbounded_Array =>
            N := Get_Output (Inst, Idx);
            Idx := Idx + 1;
         when Type_Record
           | Type_Unbounded_Record =>
            declare
               Nets : Net_Array (1 .. Nat32 (Inter_Typ.Rec.Len));
            begin
               for I in Inter_Typ.Rec.E'Range loop
                  Nets (Nat32 (I)) := Get_Output (Inst, Idx);
                  Idx := Idx + 1;
               end loop;
               N := Folds.Build2_Concat (Get_Build (Syn_Inst), Nets);
            end;
         when Type_Slice
           | Type_Access
           | Type_File
           | Type_Protected =>
            raise Internal_Error;
      end case;
   end Inst_Output_Connect;

   --  Subprogram used for instantiation (direct or by component).
   --  PORTS_ASSOC belong to SYN_INST.
   procedure Synth_Instantiate_Module_Ports (Syn_Inst : Synth_Instance_Acc;
                                             Inst : Instance;
                                             Ent_Inst : Synth_Instance_Acc;
                                             Ent : Node;
                                             Ports_Assoc : Node)
   is
      --  Instantiate the module
      --  Elaborate ports + map aspect for the inputs (component then entity)
      --  Elaborate ports + map aspect for the outputs (entity then component)
      Marker : Mark_Type;

      Assoc : Node;
      Assoc_Inter : Node;
      Inter : Node;
      Inter_Typ : Type_Acc;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      N : Net;
   begin
      Mark_Expr_Pool (Marker);

      Assoc := Ports_Assoc;
      Assoc_Inter := Get_Port_Chain (Ent);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Assoc) loop
         if Get_Whole_Association_Flag (Assoc) then
            Inter := Get_Association_Interface (Assoc, Assoc_Inter);
            Inter_Typ := Get_Subtype_Object (Ent_Inst, Get_Type (Inter));

            case Mode_To_Port_Kind (Get_Mode (Inter)) is
               when Port_In =>
                  --  Connect the net to the input.
                  N := Synth_Input_Assoc
                    (Syn_Inst, Assoc, Ent_Inst, Inter, Inter_Typ);
                  Inst_Input_Connect
                    (Syn_Inst, Inst, Nbr_Inputs, Inter_Typ, N);

               when Port_Out
                 | Port_Inout =>
                  Inst_Output_Connect
                    (Syn_Inst, Inst, Nbr_Outputs, Inter_Typ, N);

                  Synth_Output_Assoc
                    (N, Syn_Inst, Assoc, Ent_Inst, Inter);

            end case;
            pragma Assert (Areapools.Is_At_Mark (Expr_Pool, Marker));
         end if;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Instantiate_Module_Ports;

   procedure Synth_Instantiate_Module_Generics (Inst : Instance;
                                                Inst_Obj : Inst_Object) is
   begin
      if Inst_Obj.Encoding = Name_Parameters then
         --  Copy values of the generics to module parameters.
         declare
            Inter : Node;
            Vt : Valtyp;
            Pv : Pval;
            Idx : Param_Idx;
         begin
            Idx := 0;
            Inter := Get_Generic_Chain (Inst_Obj.Decl);
            while Inter /= Null_Node loop
               Vt := Get_Value (Inst_Obj.Syn_Inst, Inter);
               if Vt /= No_Valtyp then
                  --  Avoid errors
                  Pv := Memtyp_To_Pval (Get_Memtyp (Vt));
                  Set_Param_Pval (Inst, Idx, Pv);
               end if;
               Inter := Get_Chain (Inter);
               Idx := Idx + 1;
            end loop;
         end;
      end if;
   end Synth_Instantiate_Module_Generics;

   procedure Synth_Direct_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Sub_Inst : Synth_Instance_Acc;
      Ent : Node;
      Arch : Node;
      Config : Node)
   is
      Inst_Obj : Inst_Object;
      Inst : Instance;
      Enc : Name_Encoding;
   begin
      if Arch /= Null_Node then
         --  For whiteboxes: append parameters or/and hash.
         Enc := Name_Hash;
      else
         --  For blackboxes: define the parameters.
         Enc := Name_Parameters;
      end if;

      --  Search if corresponding module has already been used.
      --  If not create a new module
      --   * create a name from the generics and the library
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.
      Inst_Obj := Interning_Get ((Decl => Ent,
                                  Arch => Arch,
                                  Config => Config,
                                  Syn_Inst => Sub_Inst,
                                  Encoding => Enc));

      pragma Assert (Is_Expr_Pool_Empty);

      --  Do the instantiation.
      Inst := New_Instance
        (Get_Instance_Module (Syn_Inst),
         Inst_Obj.M,
         New_Sname_User (Get_Identifier (Stmt), Get_Sname (Syn_Inst)));
      Set_Location (Inst, Stmt);

      pragma Assert (Is_Expr_Pool_Empty);

      Push_Phi;

      Synth_Instantiate_Module_Ports
        (Syn_Inst, Inst, Inst_Obj.Syn_Inst, Inst_Obj.Decl,
         Get_Port_Map_Aspect_Chain (Stmt));
      pragma Assert (Is_Expr_Pool_Empty);

      Synth_Instantiate_Module_Generics (Inst, Inst_Obj);
      pragma Assert (Is_Expr_Pool_Empty);

      Pop_And_Merge_Phi (Get_Build (Syn_Inst), Get_Location (Stmt));

      pragma Assert (Is_Expr_Pool_Empty);
   end Synth_Direct_Instantiation_Statement;

   procedure Synth_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Sub_Inst : constant Synth_Instance_Acc :=
        Get_Sub_Instance (Syn_Inst, Stmt);
      Arch : constant Node := Get_Source_Scope (Sub_Inst);
      Ent : constant Node := Get_Entity (Arch);
      Config : constant Node := Get_Instance_Config (Sub_Inst);
   begin
      Synth_Direct_Instantiation_Statement
        (Syn_Inst, Stmt, Sub_Inst, Ent, Arch, Config);
   end Synth_Design_Instantiation_Statement;

   procedure Synth_Blackbox_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Sub_Inst : constant Synth_Instance_Acc :=
        Get_Sub_Instance (Syn_Inst, Stmt);
      Comp : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
   begin
      Synth_Direct_Instantiation_Statement
        (Syn_Inst, Stmt, Sub_Inst, Comp, Null_Node, Null_Node);
   end Synth_Blackbox_Instantiation_Statement;

   procedure Create_Component_Wire (Ctxt : Context_Acc;
                                    Inter : Node;
                                    Val : Valtyp;
                                    Pfx_Name : Sname;
                                    Loc : Source.Syn_Src)
   is
      Value : Net;
      W : Width;
   begin
      case Val.Val.Kind is
         when Value_Wire =>
            --  Create a gate for the output, so that it could be read.
            Set_Value_Wire
              (Val.Val, Alloc_Wire (Wire_Output, (Inter, Bit_Type)));
            W := Get_Type_Width (Val.Typ);
            Value := Build_Signal
              (Ctxt, New_Internal_Name (Ctxt, Pfx_Name), W);
            Set_Location (Value, Loc);
            Set_Wire_Gate (Get_Value_Wire (Val.Val), Value);
         when others =>
            raise Internal_Error;
      end case;
   end Create_Component_Wire;

   procedure Synth_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Syn_Inst);
      Comp_Inst : constant Synth_Instance_Acc :=
        Get_Sub_Instance (Syn_Inst, Stmt);
      Config : constant Node := Get_Instance_Config (Comp_Inst);
      Component : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
      Bind : constant Node := Get_Binding_Indication (Config);
      Aspect : constant Node := Get_Entity_Aspect (Bind);

      Marker : Mark_Type;
      Ent : Node;
      Arch : Node;
      Sub_Config : Node;
      Sub_Inst : Synth_Instance_Acc;
      Inst_Obj : Inst_Object;
      Inst : Instance;
      Inst_Name : Sname;

      M : Module;
   begin
      Mark_Expr_Pool (Marker);
      pragma Assert (Is_Expr_Pool_Empty);
      pragma Assert (Get_Kind (Aspect) = Iir_Kind_Entity_Aspect_Entity);

      Push_Phi;

      Inst_Name := New_Sname_User (Get_Identifier (Stmt),
                                   Get_Sname (Syn_Inst));

      Set_Extra (Comp_Inst, Syn_Inst, Inst_Name);

      --  Create objects for the inputs and the outputs of the component,
      --  assign inputs (that's nets) and create wires for outputs.
      declare
         Assoc : Node;
         Assoc_Inter : Node;
         Inter : Node;
         Inter_Typ : Type_Acc;
         Val : Valtyp;
         N : Net;
      begin
         Assoc := Get_Port_Map_Aspect_Chain (Stmt);
         Assoc_Inter := Get_Port_Chain (Component);
         while Is_Valid (Assoc) loop
            if Get_Whole_Association_Flag (Assoc) then
               Inter := Get_Association_Interface (Assoc, Assoc_Inter);
               Val := Get_Value (Comp_Inst, Inter);
               Inter_Typ := Val.Typ;

               case Mode_To_Port_Kind (Get_Mode (Inter)) is
                  when Port_In =>
                     N := Synth_Input_Assoc
                       (Syn_Inst, Assoc, Comp_Inst, Inter, Inter_Typ);
                     Val := Create_Value_Net (N, Inter_Typ);
                  when Port_Out
                    | Port_Inout =>
                     Val := Create_Value_Wire
                       (No_Wire_Id, Inter_Typ, Instance_Pool);
                     Create_Component_Wire
                       (Get_Build (Syn_Inst), Assoc_Inter, Val, Inst_Name,
                        Assoc);
               end case;
               Replace_Signal (Comp_Inst, Assoc_Inter, Val);
            end if;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;

      Sub_Inst := Get_Component_Instance (Comp_Inst);
      Arch := Get_Source_Scope (Sub_Inst);
      Sub_Config := Get_Instance_Config (Sub_Inst);
      if Get_Kind (Arch) = Iir_Kind_Foreign_Module then
         M := Synth_Foreign_Module
           (Global_Base_Instance, Get_Instance_Foreign (Sub_Inst),
            Sub_Inst, Arch);

         Inst := New_Instance (Get_Instance_Module (Syn_Inst), M, Inst_Name);
         Set_Location (Inst, Stmt);

         Synth_Instantiate_Module_Ports
           (Comp_Inst, Inst, Sub_Inst, Arch,
            Get_Port_Map_Aspect_Chain (Bind));
      else
         Ent := Get_Entity (Arch);

         --  Elaborate generic + map aspect for the entity instance.
         Set_Extra (Sub_Inst, Comp_Inst,
                    New_Sname_User (Get_Identifier (Ent), No_Sname));

         --  Search if corresponding module has already been used.
         --  If not create a new module
         --   * create a name from the generics and the library
         --   * create inputs/outputs
         --   * add it to the list of module to be synthesized.
         Inst_Obj := Interning_Get ((Decl => Ent,
                                     Arch => Arch,
                                     Config => Sub_Config,
                                     Syn_Inst => Sub_Inst,
                                     Encoding => Name_Hash));

         --  TODO: free sub_inst.

         Inst := New_Instance (Get_Instance_Module (Syn_Inst),
                               Inst_Obj.M, Inst_Name);
         Set_Location (Inst, Stmt);

         Synth_Instantiate_Module_Ports
           (Comp_Inst, Inst, Inst_Obj.Syn_Inst, Inst_Obj.Decl,
            Get_Port_Map_Aspect_Chain (Bind));
         Synth_Instantiate_Module_Generics (Inst, Inst_Obj);
      end if;

      pragma Unreferenced (M);

      --  Connect out from component to instance.
      --  Instantiate the module
      --  Elaborate ports + map aspect for the inputs (component then entity)
      --  Elaborate ports + map aspect for the outputs (entity then component)
      declare
         Assoc : Node;
         Assoc_Inter : Node;
         Inter : Node;
         Port : Net;
         O : Valtyp;
         Nbr_Outputs : Port_Nbr;
      begin
         Assoc := Get_Port_Map_Aspect_Chain (Stmt);
         Assoc_Inter := Get_Port_Chain (Component);
         Nbr_Outputs := 0;
         while Is_Valid (Assoc) loop
            if Get_Whole_Association_Flag (Assoc) then
               Inter := Get_Association_Interface (Assoc, Assoc_Inter);

               if Mode_To_Port_Kind (Get_Mode (Inter)) = Port_Out then
                  O := Get_Value (Comp_Inst, Inter);
                  Port := Get_Net (Ctxt, O);
                  Synth_Output_Assoc (Port, Syn_Inst, Assoc, Comp_Inst, Inter);
                  Nbr_Outputs := Nbr_Outputs + 1;
               end if;
            end if;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;

      Pop_And_Merge_Phi (Ctxt, Get_Location (Stmt));

      Finalize_Declarations (Comp_Inst, Get_Port_Chain (Component));

      Release_Expr_Pool (Marker);
   end Synth_Component_Instantiation_Statement;

   procedure Synth_Dependencies (Parent_Inst : Synth_Instance_Acc; Unit : Node)
   is
      Dep_List : constant Node_List := Get_Dependence_List (Unit);
      Dep_It : List_Iterator;
      Dep : Node;
      Dep_Unit : Node;
   begin
      Dep_It := List_Iterate (Dep_List);
      while Is_Valid (Dep_It) loop
         Dep := Get_Element (Dep_It);
         if Get_Kind (Dep) = Iir_Kind_Design_Unit
           and then not Get_Elab_Flag (Dep)
         then
            Set_Elab_Flag (Dep, True);
            Synth_Dependencies (Parent_Inst, Dep);
            Dep_Unit := Get_Library_Unit (Dep);
            case Iir_Kinds_Library_Unit (Get_Kind (Dep_Unit)) is
               when Iir_Kind_Entity_Declaration =>
                  null;
               when Iir_Kind_Configuration_Declaration =>
                  null;
               when Iir_Kind_Context_Declaration =>
                  null;
               when Iir_Kind_Package_Declaration =>
                  declare
                     Bod : constant Node := Get_Package_Body (Dep_Unit);
                     Bod_Unit : Node;
                  begin
                     Synth_Package_Declaration (Parent_Inst, Dep_Unit);
                     --  Do not try to elaborate math_real body: there are
                     --  functions with loop.  Currently, try create signals,
                     --  which is not possible during package elaboration.
                     if Bod /= Null_Node
                       and then Dep_Unit /= Vhdl.Ieee.Math_Real.Math_Real_Pkg
                     then
                        Bod_Unit := Get_Design_Unit (Bod);
                        Synth_Dependencies (Parent_Inst, Bod_Unit);
                        Synth_Package_Body (Parent_Inst, Dep_Unit, Bod);
                     end if;
                  end;
               when Iir_Kind_Package_Instantiation_Declaration =>
                  Synth_Package_Instantiation (Parent_Inst, Dep_Unit);
               when Iir_Kind_Package_Body =>
                  null;
               when Iir_Kind_Architecture_Body =>
                  null;
               when Iir_Kinds_Verification_Unit =>
                  null;
               when Iir_Kind_Foreign_Module =>
                  raise Internal_Error;
            end case;
         end if;
         Next (Dep_It);
      end loop;
   end Synth_Dependencies;

   procedure Synth_Top_Entity (Base : Base_Instance_Acc;
                               Design_Unit : Node;
                               Encoding : Name_Encoding;
                               Syn_Inst : Synth_Instance_Acc)
   is
      Lib_Unit : constant Node := Get_Library_Unit (Design_Unit);
      Arch : Node;
      Entity : Node;
      Config : Node;
      Inst_Obj : Inst_Object;
   begin
      --  Extract architecture from design.
      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Architecture_Body =>
            Arch := Lib_Unit;
            Config := Get_Library_Unit
              (Get_Default_Configuration_Declaration (Arch));
         when Iir_Kind_Configuration_Declaration =>
            Config := Lib_Unit;
            Arch := Get_Named_Entity
              (Get_Block_Specification (Get_Block_Configuration (Lib_Unit)));
         when others =>
            raise Internal_Error;
      end case;
      Entity := Get_Entity (Arch);

      Make_Base_Instance (Base);

      Global_Base_Instance := Base;

      Insts_Interning.Init;

      if Flags.Flag_Debug_Init then
         Elab.Debugger.Debug_Init (Arch);
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Dependencies first.
      Synth_Dependencies (Root_Instance, Get_Design_Unit (Entity));
      Synth_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      Set_Extra
        (Syn_Inst, Base, New_Sname_User (Get_Identifier (Entity), No_Sname));

      --  Search if corresponding module has already been used.
      --  If not create a new module
      --   * create a name from the generics and the library
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.
      Inst_Obj := Insts_Interning.Get
        ((Decl => Entity,
          Arch => Arch,
          Config => Get_Block_Configuration (Config),
          Syn_Inst => Syn_Inst,
          Encoding => Encoding));
      pragma Unreferenced (Inst_Obj);

      pragma Assert (Is_Expr_Pool_Empty);
   end Synth_Top_Entity;

   procedure Create_Input_Wire (Syn_Inst : Synth_Instance_Acc;
                                Self_Inst : Instance;
                                Idx : in out Port_Idx;
                                Val : Valtyp)
   is
      N : Net;
   begin
      pragma Assert (Val.Val.Kind = Value_Net);
      N := Get_Value_Net (Val.Val);
      Inst_Output_Connect (Syn_Inst, Self_Inst, Idx, Val.Typ, N);
      Set_Value_Net (Val.Val, N);
   end Create_Input_Wire;

   procedure Create_Output_Wire (Syn_Inst : Synth_Instance_Acc;
                                 Self_Inst : Instance;
                                 Inter : Node;
                                 Idx : in out Port_Idx;
                                 Val : Valtyp)
   is
      Ctxt      : constant Context_Acc := Get_Build (Syn_Inst);
      Default   : constant Node := Get_Default_Value (Inter);
      Desc      : constant Port_Desc :=
        Get_Output_Desc (Get_Module (Self_Inst), Idx);
      Marker : Mark_Type;
      Inter_Typ : Type_Acc;
      Value     : Net;
      Vout      : Net;
      Init      : Valtyp;
      Init_Net  : Net;
   begin
      pragma Assert (Val.Val.Kind = Value_Wire);

      --  Create a gate for the output, so that it could be read.
      Set_Value_Wire (Val.Val, Alloc_Wire (Wire_Output, (Inter, Val.Typ)));
      --  pragma Assert (Desc.W = Get_Type_Width (Val.Typ));

      if Default /= Null_Node then
         Mark_Expr_Pool (Marker);
         Inter_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Inter));
         Init := Synth_Expression_With_Type (Syn_Inst, Default, Inter_Typ);
         Init := Synth_Subtype_Conversion
           (Syn_Inst, Init, Inter_Typ, False, Inter);
         Init_Net := Get_Net (Ctxt, Init);
         Release_Expr_Pool (Marker);
      else
         Init_Net := No_Net;
      end if;

      if Desc.Dir = Port_Inout then
         declare
            Io_Inst : Instance;
         begin
            if Init_Net /= No_Net then
               Io_Inst := Builders.Build_Iinout (Ctxt, Val.Typ.W);
               Connect (Get_Input (Io_Inst, 1), Init_Net);
            else
               Io_Inst := Builders.Build_Inout (Ctxt, Val.Typ.W);
            end if;
            --  Connect port1 of gate inout to the pin.
            Vout := Get_Output (Io_Inst, 1);
            --  And port0 of the gate will be use to read from the pin.
            Value := Get_Output (Io_Inst, 0);
         end;
      else
         if Init_Net /= No_Net then
            Value := Builders.Build_Ioutput (Ctxt, Init_Net);
         else
            Value := Builders.Build_Output (Ctxt, Val.Typ.W);
         end if;
         Vout := Value;
      end if;
      Set_Location (Value, Inter);
      Set_Wire_Gate (Get_Value_Wire (Val.Val), Value);

      Inst_Input_Connect (Syn_Inst, Self_Inst, Idx, Val.Typ, Vout);
   end Create_Output_Wire;

   procedure Synth_Verification_Units (Syn_Inst : Synth_Instance_Acc)
   is
      Extra : Synth_Instance_Acc;
      Unit : Node;
   begin
      Extra := Get_First_Extra_Instance (Syn_Inst);
      while Extra /= null loop
         Unit := Get_Source_Scope (Extra);
         Synth_Verification_Unit (Extra, Unit, Syn_Inst);
         Extra := Get_Next_Extra_Instance (Syn_Inst);
      end loop;
   end Synth_Verification_Units;

   procedure Synth_Instance (Inst : Inst_Object)
   is
      Entity : constant Node := Inst.Decl;
      Arch : constant Node := Inst.Arch;
      Syn_Inst : constant Synth_Instance_Acc := Inst.Syn_Inst;
      Marker : Mark_Type;
      Self_Inst : Instance;
      Inter : Node;
      Vt : Valtyp;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      if Arch = Null_Node then
         --  Black box.
         return;
      end if;

      if Flag_Verbose then
         Errors.Info_Msg_Synth (+Entity, "synthesizing %n", (1 => +Entity));
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Save the current architecture, so that files can be open using a
      --  path relative to the architecture filename.
      Elab.Vhdl_Files.Set_Design_Unit (Arch);

      Synth_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      Set_Instance_Module (Syn_Inst, Inst.M);
      Self_Inst := Get_Self_Instance (Inst.M);
      Set_Location (Self_Inst, Entity);

      pragma Assert (Is_Expr_Pool_Empty);

      Areapools.Mark (Marker, Process_Pool);
      Instance_Pool := Process_Pool'Access;

      --  Create wires for inputs and outputs.
      Inter := Get_Port_Chain (Entity);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         Vt := Get_Value (Syn_Inst, Inter);
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Create_Input_Wire (Syn_Inst, Self_Inst, Nbr_Inputs, Vt);
            when Port_Out
              | Port_Inout =>
               Create_Output_Wire
                 (Syn_Inst, Self_Inst, Inter, Nbr_Outputs, Vt);
         end case;
         pragma Assert (Is_Expr_Pool_Empty);
         Inter := Get_Chain (Inter);
      end loop;

      --  Apply configuration.
      --  FIXME: what about inner block configuration ?
      pragma Assert (Get_Kind (Inst.Config) = Iir_Kind_Block_Configuration);

      --  Entity
      Synth_Concurrent_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      if not Is_Error (Syn_Inst) then
         Synth_Concurrent_Statements
           (Syn_Inst, Get_Concurrent_Statement_Chain (Entity));
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      if not Is_Error (Syn_Inst) then
         Synth_Attribute_Values (Syn_Inst, Entity);
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Architecture
      if not Is_Error (Syn_Inst) then
         Synth_Concurrent_Declarations
           (Syn_Inst, Get_Declaration_Chain (Arch));
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      if not Is_Error (Syn_Inst) then
         Synth_Concurrent_Statements
           (Syn_Inst, Get_Concurrent_Statement_Chain (Arch));
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      if not Is_Error (Syn_Inst) then
         Synth_Attribute_Values (Syn_Inst, Arch);
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Vunits
      if not Is_Error (Syn_Inst) then
         Synth_Verification_Units (Syn_Inst);
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Finalize
      Finalize_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));
      Finalize_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      Finalize_Declarations (Syn_Inst, Get_Port_Chain (Entity));

      Finalize_Wires;

      Areapools.Release (Marker, Process_Pool);

      Synthesis.Instance_Passes (Get_Build (Syn_Inst), Inst.M);

      pragma Assert (Is_Expr_Pool_Empty);
   end Synth_Instance;

   procedure Synth_All_Instances
   is
      use Insts_Interning;
      Idx : Index_Type;
   begin
      Idx := First_Index;
      while Idx <= Last_Index loop
         Synth_Instance (Get_By_Index (Idx));
         Idx := Idx + 1;
      end loop;
   end Synth_All_Instances;
end Synth.Vhdl_Insts;
