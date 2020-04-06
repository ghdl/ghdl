--  Instantiation synthesis.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with GNAT.SHA1;

with Types; use Types;
with Types_Utils; use Types_Utils;
with Files_Map;
with Name_Table;
with Libraries;
with Hash; use Hash;
with Dyn_Tables;
with Interning;
with Synthesis; use Synthesis;

with Grt.Algos;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Cleanup;
with Netlists.Memories;
with Netlists.Expands;
with Netlists.Concats;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors;
with Vhdl.Ieee.Math_Real;
with Vhdl.Std_Package;

with Synth.Values; use Synth.Values;
with Synth.Environment; use Synth.Environment;
with Synth.Stmts; use Synth.Stmts;
with Synth.Decls; use Synth.Decls;
with Synth.Expr; use Synth.Expr;
with Synth.Source; use Synth.Source;
with Synth.Debugger;

package body Synth.Insts is
   Root_Instance : Synth_Instance_Acc;

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
         if not Is_Equal (Get_Value (Obj.Syn_Inst, Inter),
                          Get_Value (Params.Syn_Inst, Inter))
         then
            return False;
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      Inter := Get_Port_Chain (Params.Decl);
      while Inter /= Null_Node loop
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
      Hash_Uns64 (C, Iir_Direction'Pos (B.Dir));
      Hash_Uns64 (C, To_Uns64 (Int64 (B.Left)));
      Hash_Uns64 (C, To_Uns64 (Int64 (B.Right)));
   end Hash_Bound;

   procedure Hash_Bounds (C : in out GNAT.SHA1.Context; Typ : Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Vector =>
            Hash_Bound (C, Typ.Vbound);
         when Type_Array =>
            for I in Typ.Abounds.D'Range loop
               Hash_Bound (C, Typ.Abounds.D (I));
            end loop;
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
           | Value_File =>
            raise Internal_Error;
      end case;
   end Hash_Const;

   function Get_Source_Identifier (Decl : Node) return Name_Id
   is
      use Files_Map;
      use Name_Table;
      Loc : constant Location_Type := Get_Location (Decl);
      Len : constant Natural := Get_Name_Length (Get_Identifier (Decl));
      subtype Ident_Str is String (1 .. Len);
      File : Source_File_Entry;
      Pos : Source_Ptr;
      Buf : File_Buffer_Acc;
   begin
      Location_To_File_Pos (Loc, File, Pos);
      Buf := Get_File_Source (File);
      return Get_Identifier
        (Ident_Str (Buf (Pos .. Pos + Source_Ptr (Len - 1))));
   end Get_Source_Identifier;

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
      pragma Assert (GNAT.SHA1.Hash_Length = 20);
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
   function Create_Inter_Name (Decl : Node; Enc : Name_Encoding) return Sname
   is
      Id : Name_Id;
   begin
      case Enc is
         when Name_Asis
           | Name_Parameters =>
            Id := Get_Source_Identifier (Decl);
         when others =>
            Id := Get_Identifier (Decl);
      end case;

      return New_Sname_User (Id, No_Sname);
   end Create_Inter_Name;

   function Build (Params : Inst_Params) return Inst_Object
   is
      Decl : constant Node := Params.Decl;
      Arch : constant Node := Params.Arch;
      Imp : Node;
      Syn_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inter_Type : Node;
      Inter_Typ : Type_Acc;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Nbr_Params : Param_Nbr;
      Cur_Module : Module;
      Val : Valtyp;
      Id : Module_Id;
   begin
      if Get_Kind (Params.Decl) = Iir_Kind_Component_Declaration then
         pragma Assert (Params.Arch = Null_Node);
         pragma Assert (Params.Config = Null_Node);
         Imp := Params.Decl;
      else
         pragma Assert
           (Get_Kind (Params.Config) = Iir_Kind_Block_Configuration);
         Imp := Params.Arch;
      end if;

      --  Create the instance.
      Syn_Inst := Make_Instance (Root_Instance, Imp, No_Sname);

      --  Copy values for generics.
      Inter := Get_Generic_Chain (Decl);
      Nbr_Params := 0;
      while Inter /= Null_Node loop
         --  Bounds or range of the type.
         Inter_Type := Get_Subtype_Indication (Inter);
         if Inter_Type /= Null_Node then
            case Get_Kind (Inter_Type) is
               when Iir_Kind_Array_Subtype_Definition
                 | Iir_Kind_Integer_Subtype_Definition =>
                  Create_Subtype_Object
                    (Syn_Inst, Inter_Type,
                     Get_Subtype_Object (Params.Syn_Inst, Inter_Type));
               when others =>
                  null;
            end case;
            Nbr_Params := Nbr_Params + 1;
         end if;

         --  Object.
         Create_Object (Syn_Inst, Inter, Get_Value (Params.Syn_Inst, Inter));
         Inter := Get_Chain (Inter);
      end loop;

      --  Allocate values and count inputs and outputs
      Inter := Get_Port_Chain (Decl);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         --  Elaborate the type...
         Synth_Declaration_Type (Syn_Inst, Inter);
         Inter_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Inter));
         if not Is_Bounded_Type (Inter_Typ) then
            --  ... but get it from the template (so that unbounded types
            --  are bounded).
            Inter_Typ := Get_Value (Params.Syn_Inst, Inter).Typ;
         end if;
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Val := Create_Value_Net (No_Net, Inter_Typ);
               Nbr_Inputs := Nbr_Inputs + 1;
            when Port_Out
              | Port_Inout =>
               Val := Create_Value_Wire (No_Wire_Id, Inter_Typ);
               Nbr_Outputs := Nbr_Outputs + 1;
         end case;
         Create_Object (Syn_Inst, Inter, Val);
         Inter := Get_Chain (Inter);
      end loop;

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
            use Vhdl.Std_Package;
            Descs : Param_Desc_Array (1 .. Nbr_Params);
            Ptype : Param_Type;
         begin
            Inter := Get_Generic_Chain (Decl);
            Nbr_Params := 0;
            while Inter /= Null_Node loop
               --  Bounds or range of the type.
               Inter_Type := Get_Type (Inter);
               Inter_Type := Get_Base_Type (Inter_Type);
               if Inter_Type = String_Type_Definition then
                  Ptype := Param_Pval_String;
               elsif Inter_Type = Time_Type_Definition then
                  Ptype := Param_Pval_Time_Ps;
               else
                  case Get_Kind (Inter_Type) is
                     when Iir_Kind_Integer_Type_Definition =>
                        Ptype := Param_Pval_Integer;
                     when Iir_Kind_Floating_Type_Definition =>
                        Ptype := Param_Pval_Real;
                     when others =>
                        Ptype := Param_Pval_Vector;
                  end case;
               end if;
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
         Desc : Port_Desc;
         Vt : Valtyp;
      begin
         Inter := Get_Port_Chain (Decl);
         Nbr_Inputs := 0;
         Nbr_Outputs := 0;
         while Is_Valid (Inter) loop
            Pkind := Mode_To_Port_Kind (Get_Mode (Inter));
            Vt := Get_Value (Syn_Inst, Inter);

            Desc := (Name => Create_Inter_Name (Inter, Params.Encoding),
                     Is_Inout => Pkind = Port_Inout,
                     W => Get_Type_Width (Vt.Typ));

            case Pkind is
               when Port_In =>
                  Nbr_Inputs := Nbr_Inputs + 1;
                  Inports (Nbr_Inputs) := Desc;
               when Port_Out
                 | Port_Inout =>
                  Nbr_Outputs := Nbr_Outputs + 1;
                  Outports (Nbr_Outputs) := Desc;
            end case;
            Inter := Get_Chain (Inter);
         end loop;
         pragma Assert (Nbr_Inputs = Inports'Last);
         pragma Assert (Nbr_Outputs = Outports'Last);
         Set_Ports_Desc (Cur_Module, Inports, Outports);
      end;

      return Inst_Object'(Decl => Decl,
                          Arch => Arch,
                          Config => Params.Config,
                          Syn_Inst => Syn_Inst,
                          M => Cur_Module,
                          Encoding => Params.Encoding);
   end Build;

   package Insts_Interning is new Interning
     (Params_Type => Inst_Params,
      Object_Type => Inst_Object,
      Hash => Hash,
      Build => Build,
      Equal => Equal);

   procedure Synth_Individual_Prefix (Syn_Inst : Synth_Instance_Acc;
                                      Inter_Inst : Synth_Instance_Acc;
                                      Formal : Node;
                                      Off : out Uns32;
                                      Typ : out Type_Acc) is
   begin
      case Get_Kind (Formal) is
         when Iir_Kind_Interface_Signal_Declaration =>
            Off := 0;
            Typ := Get_Subtype_Object (Inter_Inst, Get_Type (Formal));
         when Iir_Kind_Simple_Name =>
            Synth_Individual_Prefix
              (Syn_Inst, Inter_Inst, Get_Named_Entity (Formal), Off, Typ);
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Formal));
            begin
               Synth_Individual_Prefix
                 (Syn_Inst, Inter_Inst, Get_Prefix (Formal), Off, Typ);
               Off := Off + Typ.Rec.E (Idx + 1).Boff;
               Typ := Typ.Rec.E (Idx + 1).Typ;
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Voff : Net;
               Arr_Off : Value_Offsets;
            begin
               Synth_Individual_Prefix
                 (Syn_Inst, Inter_Inst, Get_Prefix (Formal), Off, Typ);
               Synth_Indexed_Name (Syn_Inst, Formal, Typ, Voff, Arr_Off);
               if Voff /= No_Net then
                  raise Internal_Error;
               end if;
               Off := Off + Arr_Off.Net_Off;
               Typ := Get_Array_Element (Typ);
            end;
         when Iir_Kind_Slice_Name =>
            declare
               Pfx_Bnd : Bound_Type;
               El_Typ : Type_Acc;
               Res_Bnd : Bound_Type;
               Sl_Voff : Net;
               Sl_Off : Value_Offsets;
            begin
               Synth_Individual_Prefix
                 (Syn_Inst, Inter_Inst, Get_Prefix (Formal), Off, Typ);

               Get_Onedimensional_Array_Bounds (Typ, Pfx_Bnd, El_Typ);
               Synth_Slice_Suffix (Syn_Inst, Formal, Pfx_Bnd, El_Typ,
                                   Res_Bnd, Sl_Voff, Sl_Off);
               if Sl_Voff /= No_Net then
                  raise Internal_Error;
               end if;
               Off := Off + Sl_Off.Net_Off;
               Typ := Create_Onedimensional_Array_Subtype (Typ, Res_Bnd);
            end;
         when others =>
            Vhdl.Errors.Error_Kind ("synth_individual_prefix", Formal);
      end case;
   end Synth_Individual_Prefix;

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
      Iassoc : Node;
      V : Valtyp;
      Off : Uns32;
      Typ : Type_Acc;
      Els : Value_Offset_Tables.Instance;
      Concat : Concat_Type;
      N_Off : Uns32;
      N : Net;
   begin
      Value_Offset_Tables.Init (Els, 16);

      Iassoc := Get_Chain (Assoc);
      while Iassoc /= Null_Node
        and then not Get_Whole_Association_Flag (Iassoc)
      loop
         --  For each individual assoc:
         --   1. compute type and offset
         Synth_Individual_Prefix
           (Syn_Inst, Inter_Inst, Get_Formal (Iassoc), Off, Typ);

         --   2. synth expression
         V := Synth_Expression_With_Type (Syn_Inst, Get_Actual (Iassoc), Typ);

         --   3. save in a table
         Value_Offset_Tables.Append (Els, (Off, V));

         Iassoc := Get_Chain (Iassoc);
      end loop;

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
         Append (Concat, Get_Net (V));
      end loop;
      Value_Offset_Tables.Free (Els);

      --   3. connect
      Build (Get_Build (Syn_Inst), Concat, N);
      return N;
   end Synth_Individual_Input_Assoc;

   function Synth_Input_Assoc (Syn_Inst : Synth_Instance_Acc;
                               Assoc : Node;
                               Inter_Inst : Synth_Instance_Acc;
                               Inter : Node)
                              return Net
   is
      Actual : Node;
      Formal_Typ : Type_Acc;
      Act_Inst : Synth_Instance_Acc;
      Act : Valtyp;
   begin
      case Iir_Kinds_Association_Element_Parameters (Get_Kind (Assoc)) is
         when Iir_Kind_Association_Element_Open =>
            Actual := Get_Default_Value (Inter);
            Act_Inst := Inter_Inst;
         when Iir_Kind_Association_Element_By_Expression =>
            Actual := Get_Actual (Assoc);
            if Get_Kind (Actual) = Iir_Kind_Reference_Name then
               --  Skip inserted anonymous signal declaration.
               --  FIXME: simply do not insert it ?
               Actual := Get_Named_Entity (Actual);
               pragma Assert
                 (Get_Kind (Actual) = Iir_Kind_Anonymous_Signal_Declaration);
               Actual := Get_Expression (Actual);
            end if;
            Act_Inst := Syn_Inst;
         when Iir_Kind_Association_Element_By_Individual =>
            return Synth_Individual_Input_Assoc (Syn_Inst, Assoc, Inter_Inst);
      end case;

      Formal_Typ := Get_Subtype_Object (Inter_Inst, Get_Type (Inter));

      Act := Synth_Expression_With_Type (Act_Inst, Actual, Formal_Typ);
      return Get_Net (Act);
   end Synth_Input_Assoc;

   procedure Synth_Individual_Output_Assoc (Outp : Net;
                                            Syn_Inst : Synth_Instance_Acc;
                                            Assoc : Node;
                                            Inter_Inst : Synth_Instance_Acc)
   is
      Iassoc : Node;
      V : Valtyp;
      Off : Uns32;
      Typ : Type_Acc;
      O : Net;
      Port : Net;
   begin
      Port := Builders.Build_Port (Get_Build (Syn_Inst), Outp);

      Iassoc := Get_Chain (Assoc);
      while Iassoc /= Null_Node
        and then not Get_Whole_Association_Flag (Iassoc)
      loop
         --  For each individual assoc:
         --   1. compute type and offset
         Synth_Individual_Prefix
           (Syn_Inst, Inter_Inst, Get_Formal (Iassoc), Off, Typ);

         --   2. Extract the value.
         O := Build_Extract (Get_Build (Syn_Inst), Port, Off, Typ.W);
         V := Create_Value_Net (O, Typ);

         --   3. Assign.
         Synth_Assignment (Syn_Inst, Get_Actual (Iassoc), V, Iassoc);

         Iassoc := Get_Chain (Iassoc);
      end loop;
   end Synth_Individual_Output_Assoc;

   procedure Synth_Output_Assoc (Outp : Net;
                                 Syn_Inst : Synth_Instance_Acc;
                                 Assoc : Node;
                                 Inter_Inst : Synth_Instance_Acc;
                                 Inter : Node)
   is
      Actual : Node;
      Formal_Typ : Type_Acc;
      Port : Net;
      O : Valtyp;
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Association_Element_Open =>
            --  Not connected.
            return;
         when Iir_Kind_Association_Element_By_Expression =>
            Actual := Get_Actual (Assoc);
         when others =>
            Synth_Individual_Output_Assoc
              (Outp, Syn_Inst, Assoc, Inter_Inst);
            return;
      end case;

      Formal_Typ := Get_Value (Inter_Inst, Inter).Typ;

      --  Create a port gate (so that is has a name).
      Port := Builders.Build_Port (Get_Build (Syn_Inst), Outp);
      O := Create_Value_Net (Port, Formal_Typ);
      --  Assign the port output to the actual (a net).
      Synth_Assignment (Syn_Inst, Actual, O, Assoc);
   end Synth_Output_Assoc;

   --  Subprogram used for instantiation (direct or by component).
   --  PORTS_ASSOC belong to SYN_INST.
   procedure Synth_Instantiate_Module (Syn_Inst : Synth_Instance_Acc;
                                       Inst : Instance;
                                       Inst_Obj : Inst_Object;
                                       Ports_Assoc : Node)
   is
      --  Instantiate the module
      --  Elaborate ports + map aspect for the inputs (component then entity)
      --  Elaborate ports + map aspect for the outputs (entity then component)

      Assoc : Node;
      Assoc_Inter : Node;
      Inter : Node;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      Assoc := Ports_Assoc;
      Assoc_Inter := Get_Port_Chain (Inst_Obj.Decl);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Assoc) loop
         if Get_Whole_Association_Flag (Assoc) then
            Inter := Get_Association_Interface (Assoc, Assoc_Inter);

            case Mode_To_Port_Kind (Get_Mode (Inter)) is
               when Port_In =>
                  --  Connect the net to the input.
                  Connect (Get_Input (Inst, Nbr_Inputs),
                           Synth_Input_Assoc
                             (Syn_Inst, Assoc, Inst_Obj.Syn_Inst, Inter));
                  Nbr_Inputs := Nbr_Inputs + 1;
               when Port_Out
                 | Port_Inout =>
                  Synth_Output_Assoc
                    (Get_Output (Inst, Nbr_Outputs),
                     Syn_Inst, Assoc, Inst_Obj.Syn_Inst, Inter);
                  Nbr_Outputs := Nbr_Outputs + 1;
            end case;
         end if;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;

      if Inst_Obj.Encoding = Name_Parameters then
         declare
            Inter : Node;
            Vt : Valtyp;
            Vec : Logvec_Array_Acc;
            Len : Uns32;
            Off : Uns32;
            Has_Zx : Boolean;
            Pv : Pval;
            Idx : Param_Idx;
         begin
            Idx := 0;
            Inter := Get_Generic_Chain (Inst_Obj.Decl);
            while Inter /= Null_Node loop
               Vt := Get_Value (Inst_Obj.Syn_Inst, Inter);
               Len := (Vt.Typ.W + 31) / 32;
               pragma Assert (Len > 0);
               Vec := new Logvec_Array'(0 .. Digit_Index (Len - 1) => (0, 0));
               Off := 0;
               Has_Zx := False;
               Value2logvec (Vt, Vec.all, Off, Has_Zx);
               if Has_Zx then
                  Pv := Create_Pval4 (Vt.Typ.W);
               else
                  Pv := Create_Pval2 (Vt.Typ.W);
               end if;
               for I in 0 .. Len - 1 loop
                  Write_Pval (Pv, I, Vec (Digit_Index (I)));
               end loop;
               Set_Param_Pval (Inst, Idx, Pv);

               Inter := Get_Chain (Inter);
               Idx := Idx + 1;
            end loop;
         end;
      end if;
   end Synth_Instantiate_Module;

   function Synth_Port_Association_Type (Sub_Inst : Synth_Instance_Acc;
                                         Syn_Inst : Synth_Instance_Acc;
                                         Inter : Node;
                                         Assoc : Node) return Type_Acc is
   begin
      if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
         --  TODO
         --  Find the association for this interface
         --  * if individual assoc: get type
         --  * if whole assoc: get type from object.
         if Assoc = Null_Node then
            raise Internal_Error;
         end if;
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Expression =>
               return Synth_Type_Of_Object (Syn_Inst, Get_Actual (Assoc));
            when others =>
               raise Internal_Error;
         end case;
      else
         Synth_Declaration_Type (Sub_Inst, Inter);
         return Get_Subtype_Object (Sub_Inst, Get_Type (Inter));
      end if;
   end Synth_Port_Association_Type;

   procedure Synth_Ports_Association_Type (Sub_Inst : Synth_Instance_Acc;
                                           Syn_Inst : Synth_Instance_Acc;
                                           Inter_Chain : Node;
                                           Assoc_Chain : Node)
   is
      Inter : Node;
      Assoc : Node;
      Assoc_Inter : Node;
      Val : Valtyp;
      Inter_Typ : Type_Acc;
   begin
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         if Get_Whole_Association_Flag (Assoc) then
            Inter_Typ :=  Synth_Port_Association_Type
              (Sub_Inst, Syn_Inst, Inter, Assoc);
            case Mode_To_Port_Kind (Get_Mode (Inter)) is
               when Port_In =>
                  Val := Create_Value_Net (No_Net, Inter_Typ);
               when Port_Out
                 | Port_Inout =>
                  Val := Create_Value_Wire (No_Wire_Id, Inter_Typ);
            end case;
            Create_Object (Sub_Inst, Inter, Val);
         end if;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Ports_Association_Type;

   procedure Synth_Direct_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Ent : Node;
      Arch : Node;
      Config : Node)
   is
      Sub_Inst : Synth_Instance_Acc;
      Inst_Obj : Inst_Object;
      Inst : Instance;
      Enc : Name_Encoding;
   begin
      --  Elaborate generic + map aspect
      Sub_Inst := Make_Instance
        (Syn_Inst, Ent, New_Sname_User (Get_Identifier (Ent), No_Sname));

      Synth_Generics_Association (Sub_Inst, Syn_Inst,
                                  Get_Generic_Chain (Ent),
                                  Get_Generic_Map_Aspect_Chain (Stmt));

      --  Elaborate port types.
      Synth_Ports_Association_Type (Sub_Inst, Syn_Inst,
                                    Get_Port_Chain (Ent),
                                    Get_Port_Map_Aspect_Chain (Stmt));

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
      Inst_Obj := Insts_Interning.Get ((Decl => Ent,
                                        Arch => Arch,
                                        Config => Config,
                                        Syn_Inst => Sub_Inst,
                                        Encoding => Enc));

      --  TODO: free sub_inst.

      Inst := New_Instance
        (Get_Instance_Module (Syn_Inst),
         Inst_Obj.M,
         New_Sname_User (Get_Identifier (Stmt), Get_Sname (Syn_Inst)));
      Set_Location (Inst, Stmt);

      Synth_Instantiate_Module
        (Syn_Inst, Inst, Inst_Obj, Get_Port_Map_Aspect_Chain (Stmt));
   end Synth_Direct_Instantiation_Statement;

   procedure Synth_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Aspect : constant Iir := Get_Instantiated_Unit (Stmt);
      Arch : Node;
      Ent : Node;
      Config : Node;
   begin
      --  Load configured entity + architecture
      case Iir_Kinds_Entity_Aspect (Get_Kind (Aspect)) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Arch := Get_Architecture (Aspect);
            if Arch = Null_Node then
               Arch := Libraries.Get_Latest_Architecture (Get_Entity (Aspect));
            else
               Arch := Strip_Denoting_Name (Arch);
            end if;
            Config := Get_Library_Unit
              (Get_Default_Configuration_Declaration (Arch));
         when Iir_Kind_Entity_Aspect_Configuration =>
            Config := Get_Configuration (Aspect);
            Arch := Get_Block_Specification (Get_Block_Configuration (Config));
         when Iir_Kind_Entity_Aspect_Open =>
            return;
      end case;
      Config := Get_Block_Configuration (Config);
      Ent := Get_Entity (Arch);

      Synth_Direct_Instantiation_Statement
        (Syn_Inst, Stmt, Ent, Arch, Config);
   end Synth_Design_Instantiation_Statement;

   procedure Synth_Blackbox_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Comp : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
   begin
      Synth_Direct_Instantiation_Statement
        (Syn_Inst, Stmt, Comp, Null_Node, Null_Node);
   end Synth_Blackbox_Instantiation_Statement;

   procedure Create_Component_Wire
     (Ctxt : Context_Acc; Inter : Node; Val : Valtyp; Pfx_Name : Sname)
   is
      Value : Net;
      W : Width;
   begin
      case Val.Val.Kind is
         when Value_Wire =>
            --  Create a gate for the output, so that it could be read.
            Val.Val.W := Alloc_Wire (Wire_Output, Inter);
            W := Get_Type_Width (Val.Typ);
            Value := Build_Signal
              (Ctxt, New_Internal_Name (Ctxt, Pfx_Name), W);
            Set_Wire_Gate (Val.Val.W, Value);
         when others =>
            raise Internal_Error;
      end case;
   end Create_Component_Wire;

   procedure Synth_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Component : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
      Config : constant Node := Get_Component_Configuration (Stmt);
      Bind : constant Node := Get_Binding_Indication (Config);
      Aspect : constant Node := Get_Entity_Aspect (Bind);
      Comp_Inst : Synth_Instance_Acc;

      Ent : Node;
      Arch : Node;
      Sub_Config : Node;
      Sub_Inst : Synth_Instance_Acc;
      Inst_Obj : Inst_Object;
      Inst : Instance;
      Inst_Name : Sname;
   begin
      pragma Assert (Get_Component_Configuration (Stmt) /= Null_Node);
      pragma Assert (Get_Kind (Aspect) = Iir_Kind_Entity_Aspect_Entity);

      Inst_Name := New_Sname_User (Get_Identifier (Stmt),
                                   Get_Sname (Syn_Inst));

      --  Create the sub-instance for the component
      --  Elaborate generic + map aspect
      Comp_Inst := Make_Instance
        (Syn_Inst, Component,
         New_Sname_User (Get_Identifier (Component), No_Sname));

      Synth_Generics_Association (Comp_Inst, Syn_Inst,
                                  Get_Generic_Chain (Component),
                                  Get_Generic_Map_Aspect_Chain (Stmt));

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

               Inter_Typ := Synth_Port_Association_Type
                 (Comp_Inst, Syn_Inst, Inter, Assoc);

               case Mode_To_Port_Kind (Get_Mode (Inter)) is
                  when Port_In =>
                     N := Synth_Input_Assoc
                       (Syn_Inst, Assoc, Comp_Inst, Inter);
                     Val := Create_Value_Net (N, Inter_Typ);
                  when Port_Out
                    | Port_Inout =>
                     Val := Create_Value_Wire (No_Wire_Id, Inter_Typ);
                     Create_Component_Wire
                       (Get_Build (Syn_Inst), Assoc_Inter, Val, Inst_Name);
               end case;
               Create_Object (Comp_Inst, Assoc_Inter, Val);
            end if;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;

      --  Extract entity/architecture instantiated by the component.
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Ent := Get_Entity (Aspect);
            Arch := Get_Architecture (Aspect);
         when others =>
            Vhdl.Errors.Error_Kind
              ("Synth_Component_Instantiation_Statement(2)", Aspect);
      end case;

      if Arch = Null_Node then
         Arch := Libraries.Get_Latest_Architecture (Ent);
      else
         Arch := Get_Named_Entity (Arch);
      end if;
      Sub_Config := Get_Library_Unit
        (Get_Default_Configuration_Declaration (Arch));
      Sub_Config := Get_Block_Configuration (Sub_Config);

      --  Elaborate generic + map aspect for the entity instance.
      Sub_Inst := Make_Instance
        (Comp_Inst, Ent, New_Sname_User (Get_Identifier (Ent), No_Sname));
      Synth_Generics_Association (Sub_Inst, Comp_Inst,
                                  Get_Generic_Chain (Ent),
                                  Get_Generic_Map_Aspect_Chain (Bind));

      Synth_Ports_Association_Type (Sub_Inst, Comp_Inst,
                                    Get_Port_Chain (Ent),
                                    Get_Port_Map_Aspect_Chain (Bind));

      --  Search if corresponding module has already been used.
      --  If not create a new module
      --   * create a name from the generics and the library
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.
      Inst_Obj := Insts_Interning.Get ((Decl => Ent,
                                        Arch => Arch,
                                        Config => Sub_Config,
                                        Syn_Inst => Sub_Inst,
                                        Encoding => Name_Hash));

      --  TODO: free sub_inst.

      Inst := New_Instance (Get_Instance_Module (Syn_Inst),
                            Inst_Obj.M, Inst_Name);

      Synth_Instantiate_Module
        (Comp_Inst, Inst, Inst_Obj, Get_Port_Map_Aspect_Chain (Bind));

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
                  Port := Get_Net (O);
                  Synth_Output_Assoc (Port, Syn_Inst, Assoc, Comp_Inst, Inter);
                  Nbr_Outputs := Nbr_Outputs + 1;
               end if;
            end if;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;
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
            end case;
         end if;
         Next (Dep_It);
      end loop;
   end Synth_Dependencies;

   procedure Synth_Top_Entity (Global_Instance : Synth_Instance_Acc;
                               Arch : Node;
                               Config : Node;
                               Encoding : Name_Encoding;
                               Inst : out Synth_Instance_Acc)
   is
      Entity : constant Node := Get_Entity (Arch);
      Syn_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inter_Typ : Type_Acc;
      Inst_Obj : Inst_Object;
      Val : Valtyp;
   begin
      Root_Instance := Global_Instance;

      Insts_Interning.Init;

      if Flags.Flag_Debug_Init then
         Synth.Debugger.Debug_Init (Arch);
      end if;

      --  Dependencies first.
      Synth_Dependencies (Global_Instance, Get_Design_Unit (Entity));
      Synth_Dependencies (Global_Instance, Get_Design_Unit (Arch));

      Syn_Inst := Make_Instance
        (Global_Instance, Arch,
         New_Sname_User (Get_Identifier (Entity), No_Sname));

      --  Compute generics.
      Inter := Get_Generic_Chain (Entity);
      while Is_Valid (Inter) loop
         Synth_Declaration_Type (Syn_Inst, Inter);
         declare
            Val : Valtyp;
            Inter_Typ : Type_Acc;
         begin
            Inter_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Inter));
            Val := Synth_Expression_With_Type
              (Syn_Inst, Get_Default_Value (Inter), Inter_Typ);
            pragma Assert (Is_Static (Val.Val));
            Create_Object (Syn_Inst, Inter, Val);
         end;
         Inter := Get_Chain (Inter);
      end loop;

      --  Elaborate port types.
      --  FIXME: what about unconstrained ports ?  Get the type from the
      --    association.
      Inter := Get_Port_Chain (Entity);
      while Is_Valid (Inter) loop
         if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
            --  TODO
            raise Internal_Error;
         end if;
         Synth_Declaration_Type (Syn_Inst, Inter);
         Inter_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Inter));
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Val := Create_Value_Net (No_Net, Inter_Typ);
            when Port_Out
              | Port_Inout =>
               Val := Create_Value_Wire (No_Wire_Id, Inter_Typ);
         end case;
         Create_Object (Syn_Inst, Inter, Val);
         Inter := Get_Chain (Inter);
      end loop;

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
      Inst := Inst_Obj.Syn_Inst;
   end Synth_Top_Entity;

   procedure Create_Input_Wire (Self_Inst : Instance;
                                Idx : Port_Idx;
                                Val : Value_Acc) is
   begin
      pragma Assert (Val.Kind = Value_Net);
      Val.N := Get_Output (Self_Inst, Idx);
   end Create_Input_Wire;

   procedure Create_Output_Wire (Syn_Inst : Synth_Instance_Acc;
                                 Self_Inst : Instance;
                                 Inter : Node;
                                 Idx : Port_Idx;
                                 Val : Value_Acc)
   is
      Default : constant Node := Get_Default_Value (Inter);
      Desc : constant Port_Desc :=
        Get_Output_Desc (Get_Module (Self_Inst), Idx);
      Inter_Typ : Type_Acc;
      Value : Net;
      Init : Valtyp;
      Inp : Input;
   begin
      pragma Assert (Val.Kind = Value_Wire);

      --  Create a gate for the output, so that it could be read.
      Val.W := Alloc_Wire (Wire_Output, Inter);
      --  pragma Assert (Desc.W = Get_Type_Width (Val.Typ));

      Inp := Get_Input (Self_Inst, Idx);

      if Desc.Is_Inout then
         if Default /= Null_Node then
            --  TODO: initialized inout.
            raise Internal_Error;
         end if;
         declare
            Io_Inst : Instance;
         begin
            Io_Inst := Builders.Build_Inout (Build_Context, Desc.W);
            --  Connect port1 of gate inout to the pin.
            Connect (Inp, Get_Output (Io_Inst, 1));
            --  And port0 of the gate will be use to read from the pin.
            Value := Get_Output (Io_Inst, 0);
         end;
      else
         if Default /= Null_Node then
            Inter_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Inter));
            Init := Synth_Expression_With_Type
              (Syn_Inst, Default, Inter_Typ);
            Init := Synth_Subtype_Conversion
              (Init, Inter_Typ, False, Inter);
            Value := Builders.Build_Ioutput (Build_Context, Get_Net (Init));
         else
            Value := Builders.Build_Output (Build_Context, Desc.W);
         end if;
         Connect (Inp, Value);
      end if;
      Set_Location (Value, Inter);
      Set_Wire_Gate (Val.W, Value);
   end Create_Output_Wire;

   procedure Apply_Block_Configuration (Cfg : Node; Blk : Node)
   is
      Item : Node;
   begin
      --  Be sure CFG applies to BLK.
      pragma Assert (Get_Block_From_Block_Specification
                       (Get_Block_Specification (Cfg)) = Blk);

      Clear_Instantiation_Configuration (Blk);

      Item := Get_Configuration_Item_Chain (Cfg);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Component_Configuration =>
               declare
                  List : constant Iir_Flist :=
                    Get_Instantiation_List (Item);
                  El : Node;
                  Inst : Node;
               begin
                  for I in Flist_First .. Flist_Last (List) loop
                     El := Get_Nth_Element (List, I);
                     Inst := Get_Named_Entity (El);
                     pragma Assert
                       (Get_Kind (Inst)
                          = Iir_Kind_Component_Instantiation_Statement);
                     pragma Assert
                       (Get_Component_Configuration (Inst) = Null_Node);
                     Set_Component_Configuration (Inst, Item);
                  end loop;
               end;
            when Iir_Kind_Block_Configuration =>
               declare
                  Sub_Blk : constant Node := Get_Block_From_Block_Specification
                    (Get_Block_Specification (Item));
               begin
                  case Get_Kind (Sub_Blk) is
                     when Iir_Kind_Generate_Statement_Body =>
                        -- Linked chain.
                        Set_Prev_Block_Configuration
                          (Item, Get_Generate_Block_Configuration (Sub_Blk));
                        Set_Generate_Block_Configuration (Sub_Blk, Item);
                     when Iir_Kind_Block_Statement =>
                        Set_Block_Block_Configuration (Sub_Blk, Item);
                     when others =>
                        Vhdl.Errors.Error_Kind
                          ("apply_block_configuration(blk)", Sub_Blk);
                  end case;
               end;
            when others =>
               Vhdl.Errors.Error_Kind ("apply_block_configuration", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Apply_Block_Configuration;

   procedure Synth_Verification_Units
     (Syn_Inst : Synth_Instance_Acc; Parent : Node)
   is
      Unit : Node;
   begin
      Unit := Get_Bound_Vunit_Chain (Parent);
      while Unit /= Null_Node loop
         Synth_Verification_Unit (Syn_Inst, Unit);
         Unit := Get_Bound_Vunit_Chain (Unit);
      end loop;
   end Synth_Verification_Units;

   procedure Synth_Instance (Inst : Inst_Object)
   is
      Entity : constant Node := Inst.Decl;
      Arch : constant Node := Inst.Arch;
      Syn_Inst : constant Synth_Instance_Acc := Inst.Syn_Inst;
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

      Synth_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      Set_Instance_Module (Syn_Inst, Inst.M);
      Self_Inst := Get_Self_Instance (Inst.M);
      Set_Location (Self_Inst, Entity);

      --  Create wires for inputs and outputs.
      Inter := Get_Port_Chain (Entity);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         Vt := Get_Value (Syn_Inst, Inter);
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Create_Input_Wire (Self_Inst, Nbr_Inputs, Vt.Val);
               Nbr_Inputs := Nbr_Inputs + 1;
            when Port_Out
              | Port_Inout =>
               Create_Output_Wire
                 (Syn_Inst, Self_Inst, Inter, Nbr_Outputs, Vt.Val);
               Nbr_Outputs := Nbr_Outputs + 1;
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      --  Apply configuration.
      --  FIXME: what about inner block configuration ?
      pragma Assert (Get_Kind (Inst.Config) = Iir_Kind_Block_Configuration);
      Apply_Block_Configuration (Inst.Config, Arch);

      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      if not Is_Error (Syn_Inst) then
         Synth_Concurrent_Statements
           (Syn_Inst, Get_Concurrent_Statement_Chain (Entity));
      end if;

      if not Is_Error (Syn_Inst) then
         Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));
      end if;
      if not Is_Error (Syn_Inst) then
         Synth_Concurrent_Statements
           (Syn_Inst, Get_Concurrent_Statement_Chain (Arch));
      end if;

      if not Is_Error (Syn_Inst) then
         Synth_Verification_Units (Syn_Inst, Entity);
      end if;
      if not Is_Error (Syn_Inst) then
         Synth_Verification_Units (Syn_Inst, Arch);
      end if;

      Finalize_Assignments (Get_Build (Syn_Inst));

      Finalize_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));

      --  Remove unused gates.  This is not only an optimization but also
      --  a correctness point: there might be some unsynthesizable gates, like
      --  the one created for 'rising_egde (clk) and not rst'.
      if not Synth.Flags.Flag_Debug_Nocleanup then
         --  Netlists.Cleanup.Remove_Unconnected_Instances (Inst.M);
         Netlists.Cleanup.Mark_And_Sweep (Inst.M);
         Netlists.Cleanup.Remove_Output_Gates (Inst.M);
      end if;

      if not Synth.Flags.Flag_Debug_Nomemory then
         Netlists.Memories.Extract_Memories2 (Get_Build (Syn_Inst), Inst.M);
         --  Remove remaining clock edge gates.
         Netlists.Cleanup.Mark_And_Sweep (Inst.M);
      end if;

      if not Synth.Flags.Flag_Debug_Noexpand then
         Netlists.Expands.Expand_Gates (Get_Build (Syn_Inst), Inst.M);
      end if;
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
end Synth.Insts;
