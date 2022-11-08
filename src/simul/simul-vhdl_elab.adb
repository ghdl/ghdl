--  Elaboration for VHDL simulation
--  Copyright (C) 2022 Tristan Gingold
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

with Areapools;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Canon;

with Synth.Vhdl_Stmts;
with Synth.Vhdl_Decls;
with Synth.Vhdl_Expr;
with Trans_Analyzes;

with Simul.Vhdl_Debug;

package body Simul.Vhdl_Elab is
   procedure Gather_Processes_1 (Inst : Synth_Instance_Acc);

   procedure Convert_Type_Width (T : Type_Acc) is
   begin
      if T.Wkind = Wkind_Sim then
         return;
      end if;
      case T.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            T.W := 1;
            T.Wkind := Wkind_Sim;
         when Type_Vector
           | Type_Array =>
            Convert_Type_Width (T.Arr_El);
            T.W := T.Abound.Len * T.Arr_El.W;
            T.Wkind := Wkind_Sim;
         when Type_Record =>
            T.W := 0;
            for I in T.Rec.E'Range loop
               T.Rec.E (I).Offs.Net_Off := T.W;
               Convert_Type_Width (T.Rec.E (I).Typ);
               T.W := T.W + T.Rec.E (I).Typ.W;
            end loop;
            T.Wkind := Wkind_Sim;
         when others =>
            raise Internal_Error;
      end case;
   end Convert_Type_Width;

   --  For each scalar element, set Vec (off).Total to 1 if the signal is
   --  resolved.
   procedure Mark_Resolved_Signals (Sig_Off : Uns32;
                                    Sig_Type: Iir;
                                    Typ : Type_Acc;
                                    Vec : in out Nbr_Sources_Array;
                                    Already_Resolved : Boolean)
   is
      Sub_Resolved : Boolean;
   begin
      if not Already_Resolved
        and then Get_Kind (Sig_Type) in Iir_Kinds_Subtype_Definition
      then
         Sub_Resolved := Get_Resolution_Indication (Sig_Type) /= Null_Iir;
      else
         Sub_Resolved := Already_Resolved;
      end if;

      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Float
           | Type_Discrete =>
            if Sub_Resolved then
               Vec (Sig_Off).Total := 1;
            end if;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
               El_Type : Node;
            begin
               if Typ.Alast then
                  El_Type := Get_Element_Subtype (Sig_Type);
               else
                  El_Type := Sig_Type;
               end if;
               for I in 1 .. Len loop
                  Mark_Resolved_Signals
                    (Sig_Off + (Len - I) * Typ.Arr_El.W,
                     El_Type, Typ.Arr_El,
                     Vec, Already_Resolved);
               end loop;
            end;
         when Type_Record =>
            declare
               List : constant Node_Flist := Get_Elements_Declaration_List
                 (Sig_Type);
               El : Iir_Element_Declaration;
            begin
               for I in Typ.Rec.E'Range loop
                  El := Get_Nth_Element (List, Natural (I - 1));
                  Mark_Resolved_Signals
                    (Sig_Off + Typ.Rec.E (I).Offs.Net_Off,
                     Get_Type (El), Typ.Rec.E (I).Typ,
                     Vec, Sub_Resolved);
               end loop;
            end;

         when Type_Slice
           | Type_Access
           | Type_Unbounded_Vector
           | Type_Unbounded_Array
           | Type_Unbounded_Record
           | Type_File
           | Type_Protected =>
            raise Internal_Error;
      end case;
   end Mark_Resolved_Signals;

   procedure Gather_Signal (Proto_E : Signal_Entry)
   is
      Val : constant Valtyp := Get_Value (Proto_E.Inst, Proto_E.Decl);
      E : Signal_Entry;
   begin
      E := Proto_E;
      E.Typ := Val.Typ;

      --  Be sure the width is correct for a signal.
      Convert_Type_Width (E.Typ);

      --  Allocate the value in global pool.
      E.Val := Alloc_Memory (E.Typ, Global_Pool'Access);

      --  Set it to the default value.
      if Val.Val.Init /= null then
         Copy_Memory (E.Val, Get_Memory (Val.Val.Init), E.Typ.Sz);
      else
         Write_Value_Default (E.Val, E.Typ);
      end if;
      E.Sig := null;

      if E.Kind in Mode_Signal_User then
         if E.Typ.W > 0 then
            E.Nbr_Sources :=
              new Nbr_Sources_Array'(0 .. E.Typ.W - 1 =>
                                       (Nbr_Drivers => 0,
                                        Nbr_Conns => 0,
                                        Total => 0,
                                        Last_Proc => No_Process_Index));

            Mark_Resolved_Signals
              (0, Get_Type (E.Decl), E.Typ, E.Nbr_Sources.all, False);
         else
            E.Nbr_Sources := new Nbr_Sources_Array (1 .. 0);
         end if;
      end if;

      pragma Assert (E.Kind /= Mode_End);
      pragma Assert (Signals_Table.Table (Val.Val.S).Kind = Mode_End);
      Signals_Table.Table (Val.Val.S) := E;
   end Gather_Signal;

   function Compute_Sub_Signal (Inst : Synth_Instance_Acc; Name : Node)
                               return Sub_Signal_Type
   is
      Marker : Mark_Type;
      Base : Valtyp;
      Typ : Type_Acc;
      Off : Value_Offsets;
      Res : Sub_Signal_Type;
   begin
      Mark_Expr_Pool (Marker);

      Synth.Vhdl_Stmts.Synth_Assignment_Prefix (Inst, Name, Base, Typ, Off);
      Res := (Base => Base.Val.S,
              Typ => Unshare (Typ, Global_Pool'Access),
              Offs => Off);

      Release_Expr_Pool (Marker);
      return Res;
   end Compute_Sub_Signal;

   procedure Gather_Disconnection (Inst : Synth_Instance_Acc; Decl : Node)
   is
      List : constant Node_Flist := Get_Signal_List (Decl);
      Marker : Mark_Type;
      Name : Node;
      Sig : Sub_Signal_Type;
      Tval : Valtyp;
      T : Std_Time;
   begin
      Mark_Expr_Pool (Marker);

      Tval := Synth.Vhdl_Expr.Synth_Expression (Inst, Get_Expression (Decl));
      T := Std_Time (Read_Discrete (Tval));

      Release_Expr_Pool (Marker);

      for I in Flist_First .. Flist_Last (List) loop
         Name := Get_Nth_Element (List, I);
         Sig := Compute_Sub_Signal (Inst, Name);
         Disconnect_Table.Append
           ((Sig => Sig,
             Val => T,
             Prev => Signals_Table.Table (Sig.Base).Disconnect));
         Signals_Table.Table (Sig.Base).Disconnect := Disconnect_Table.Last;
      end loop;

   end Gather_Disconnection;

   procedure Gather_Quantity (Inst : Synth_Instance_Acc; Decl : Node)
   is
      Val : constant Valtyp := Get_Value (Inst, Decl);
   begin
      Convert_Type_Width (Val.Typ);
      pragma Assert (Val.Val.Q = No_Quantity_Index);
      Quantity_Table.Append ((Decl, Inst, Val.Typ, null, No_Scalar_Quantity));
      Val.Val.Q := Quantity_Table.Last;
   end Gather_Quantity;

   procedure Gather_Terminal (Inst : Synth_Instance_Acc; Decl : Node)
   is
      Val : constant Valtyp := Get_Value (Inst, Decl);
      Def : constant Node := Get_Nature (Decl);
      Across_Typ : Type_Acc;
      Through_Typ : Type_Acc;
   begin
      Across_Typ := Get_Subtype_Object (Inst, Get_Across_Type (Def));
      Through_Typ := Get_Subtype_Object (Inst, Get_Through_Type (Def));
      pragma Assert (Val.Val.T = No_Terminal_Index);
      Terminal_Table.Append ((Decl, Inst, Across_Typ, Through_Typ, null,
                              No_Scalar_Quantity, No_Scalar_Terminal));
      Val.Val.T := Terminal_Table.Last;
   end Gather_Terminal;

   function Compute_Attribute_Time (Inst : Synth_Instance_Acc; Attr : Node)
                                   return Std_Time
   is
      Param : constant Node := Get_Parameter (Attr);
      Marker : Mark_Type;
      Val : Valtyp;
      Res : Std_Time;
   begin
      if Param = Null_Node then
         return 0;
      end if;
      Mark_Expr_Pool (Marker);
      Val := Synth.Vhdl_Expr.Synth_Expression (Inst, Param);
      Res := Std_Time (Read_Discrete (Val));
      Release_Expr_Pool (Marker);
      return Res;
   end Compute_Attribute_Time;

   procedure Gather_Processes_Decl (Inst : Synth_Instance_Acc; Decl : Node) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Interface_Signal_Declaration =>
            --  Driver.
            case Get_Mode (Decl) is
               when Iir_Unknown_Mode =>
                  raise Internal_Error;
               when Iir_Linkage_Mode =>
                  Gather_Signal ((Mode_Linkage, Decl, Inst, null, null, null,
                                  No_Sensitivity_Index, No_Signal_Index,
                                  No_Connect_Index, No_Driver_Index,
                                  No_Disconnect_Index, null));
               when Iir_Buffer_Mode =>
                  Gather_Signal ((Mode_Buffer, Decl, Inst, null, null, null,
                                  No_Sensitivity_Index, No_Signal_Index,
                                  No_Connect_Index, No_Driver_Index,
                                  No_Disconnect_Index, null));
               when Iir_Out_Mode =>
                  Gather_Signal ((Mode_Out, Decl, Inst, null, null, null,
                                  No_Sensitivity_Index, No_Signal_Index,
                                  No_Connect_Index, No_Driver_Index,
                                  No_Disconnect_Index, null));
               when Iir_Inout_Mode =>
                  Gather_Signal ((Mode_Inout, Decl, Inst, null, null, null,
                                  No_Sensitivity_Index, No_Signal_Index,
                                  No_Connect_Index, No_Driver_Index,
                                  No_Disconnect_Index, null));
               when Iir_In_Mode =>
                  Gather_Signal ((Mode_In, Decl, Inst, null, null, null,
                                  No_Sensitivity_Index, No_Signal_Index,
                                  No_Connect_Index, No_Driver_Index,
                                  No_Disconnect_Index, null));
            end case;
         when Iir_Kind_Signal_Declaration =>
            Gather_Signal ((Mode_Signal, Decl, Inst, null, null, null,
                            No_Sensitivity_Index, No_Signal_Index,
                            No_Connect_Index, No_Driver_Index,
                            No_Disconnect_Index, null));
         when Iir_Kind_Configuration_Specification =>
            null;
         when Iir_Kind_Free_Quantity_Declaration
           | Iir_Kinds_Branch_Quantity_Declaration
           | Iir_Kind_Dot_Attribute =>
            Gather_Quantity (Inst, Decl);
         when Iir_Kind_Terminal_Declaration =>
            Gather_Terminal (Inst, Decl);
         when Iir_Kind_Nature_Declaration =>
            declare
               Def : constant Node := Get_Nature (Decl);
               Across_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Inst, Get_Across_Type (Def));
               Through_Typ : constant Type_Acc :=
                 Get_Subtype_Object (Inst, Get_Through_Type (Def));
            begin
               Convert_Type_Width (Across_Typ);
               Convert_Type_Width (Through_Typ);
            end;
         when Iir_Kind_Attribute_Implicit_Declaration =>
            declare
               Sig : Node;
            begin
               Sig := Get_Attribute_Implicit_Chain (Decl);
               while Sig /= Null_Node loop
                  Gather_Processes_Decl (Inst, Sig);
                  Sig := Get_Attr_Chain (Sig);
               end loop;
            end;
         when Iir_Kind_Above_Attribute =>
            Gather_Signal ((Mode_Above, Decl, Inst, null, null, null,
                            No_Sensitivity_Index, No_Signal_Index,
                            No_Connect_Index));
         when Iir_Kind_Quiet_Attribute =>
            declare
               T : Std_Time;
               Pfx : Sub_Signal_Type;
            begin
               T := Compute_Attribute_Time (Inst, Decl);
               Pfx := Compute_Sub_Signal (Inst, Get_Prefix (Decl));
               Gather_Signal ((Mode_Quiet, Decl, Inst, null, null, null,
                               No_Sensitivity_Index, No_Signal_Index,
                               No_Connect_Index, T, Pfx));
            end;
         when Iir_Kind_Stable_Attribute =>
            declare
               T : Std_Time;
               Pfx : Sub_Signal_Type;
            begin
               T := Compute_Attribute_Time (Inst, Decl);
               Pfx := Compute_Sub_Signal (Inst, Get_Prefix (Decl));
               Gather_Signal ((Mode_Stable, Decl, Inst, null, null, null,
                               No_Sensitivity_Index, No_Signal_Index,
                               No_Connect_Index, T, Pfx));
            end;
         when Iir_Kind_Delayed_Attribute =>
            declare
               T : Std_Time;
               Pfx : Sub_Signal_Type;
            begin
               T := Compute_Attribute_Time (Inst, Decl);
               Pfx := Compute_Sub_Signal (Inst, Get_Prefix (Decl));
               Gather_Signal ((Mode_Delayed, Decl, Inst, null, null, null,
                               No_Sensitivity_Index, No_Signal_Index,
                               No_Connect_Index, T, Pfx));
            end;
         when Iir_Kind_Object_Alias_Declaration =>
            --  In case it aliases a signal.
            declare
               Marker : Mark_Type;
               V : Valtyp;
               Base : Valtyp;
               Typ : Type_Acc;
               Off : Value_Offsets;
            begin
               V := Get_Value (Inst, Decl);
               Convert_Type_Width (V.Typ);

               --  Recompute alias offsets.
               Mark_Expr_Pool (Marker);
               Synth.Vhdl_Stmts.Synth_Assignment_Prefix
                 (Inst, Get_Name (Decl), Base, Typ, Off);
               V.Val.A_Off := Off;
               pragma Assert (Base.Val = V.Val.A_Obj);
               pragma Unreferenced (Typ);
               Release_Expr_Pool (Marker);
            end;
         when Iir_Kind_Disconnection_Specification =>
            Gather_Disconnection (Inst, Decl);
         when Iir_Kind_Variable_Declaration =>
            pragma Assert (Get_Shared_Flag (Decl));
            if Get_Default_Value (Decl) = Null_Node then
               --  Elab doesn't set a value to variables with no default
               --  value.
               declare
                  V : Valtyp;
               begin
                  V := Get_Value (Inst, Decl);
                  pragma Assert (V.Val = null);
                  Current_Pool := Global_Pool'Access;
                  if V.Typ.Kind = Type_Protected then
                     V := Synth.Vhdl_Decls.Create_Protected_Object
                       (Inst, Decl, V.Typ);
                  else
                     V := Create_Value_Default (V.Typ);
                  end if;
                  Current_Pool := Expr_Pool'Access;
                  Mutate_Object (Inst, Decl, V);
               end;
            end if;

         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Non_Object_Alias_Declaration
           | Iir_Kind_Attribute_Declaration
           | Iir_Kind_Attribute_Specification
           | Iir_Kind_Type_Declaration
           | Iir_Kind_Anonymous_Type_Declaration
           | Iir_Kind_Subtype_Declaration
           | Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body
           | Iir_Kind_Component_Declaration
           | Iir_Kind_File_Declaration
           | Iir_Kind_Protected_Type_Body
           | Iir_Kind_Use_Clause
           | Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration =>
            null;
         when others =>
            Error_Kind ("gather_processes_decl", Decl);
      end case;
      pragma Assert (Areapools.Is_Empty (Expr_Pool));
   end Gather_Processes_Decl;

   procedure Gather_Processes_Decls
     (Inst : Synth_Instance_Acc; Decls : Node)
   is
      Decl : Node;
   begin
      Decl := Decls;
      while Decl /= Null_Node loop
         Gather_Processes_Decl (Inst, Decl);
         Decl := Get_Chain (Decl);
      end loop;
   end Gather_Processes_Decls;

   --  Add a driver for process PROC_IDX on signal SIG at OFF/TYP.
   procedure Add_Process_Driver (Proc_Idx : Process_Index_Type;
                                 Sig : Sub_Signal_Type;
                                 Loc : Node)
   is
      S : Signal_Entry renames Signals_Table.Table (Sig.Base);
      Resolved : constant Boolean := Get_Resolved_Flag (Get_Type (S.Decl));
      Need_It : Boolean;
   begin
      pragma Assert (Sig.Typ.Wkind = Wkind_Sim);

      if Sig.Typ.W = 0 then
         --  Be safe: no signal, then no driver.
         return;
      end if;

      --  Increment the number of driver for each scalar element.
      Need_It := False;
      for I in Sig.Offs.Net_Off .. Sig.Offs.Net_Off + Sig.Typ.W - 1 loop
         declare
            Ns : Nbr_Sources_Type renames S.Nbr_Sources (I);
         begin
            if Ns.Last_Proc /= Proc_Idx then
               --  New driver.
               if not Need_It
                 and then Ns.Nbr_Drivers > 0
                 and then Ns.Total = 0
                 and then not Resolved
               then
                  Error_Msg_Elab (Loc, "too many drivers for %n", +S.Decl);
               end if;
               Ns.Nbr_Drivers := Ns.Nbr_Drivers + 1;
               Ns.Last_Proc := Proc_Idx;
               Need_It := True;
            end if;
         end;
      end loop;

      if not Need_It then
         --  The driver has already been added.
         return;
      end if;

      Drivers_Table.Append
        ((Sig => Sig,
          Prev_Sig => S.Drivers,

          Proc => Proc_Idx,
          Prev_Proc => Processes_Table.Table (Proc_Idx).Drivers));

      S.Drivers := Drivers_Table.Last;
      Processes_Table.Table (Proc_Idx).Drivers := Drivers_Table.Last;
   end Add_Process_Driver;

   --  Add drivers for process PROC.
   procedure Gather_Process_Drivers
     (Inst : Synth_Instance_Acc; Proc : Node; Proc_Idx : Process_Index_Type)
   is
      Driver_List: Iir_List;
      It : List_Iterator;
      El : Node;
      Sig : Sub_Signal_Type;
   begin
      Instance_Pool := Process_Pool'Access;

      Driver_List := Trans_Analyzes.Extract_Drivers (Proc);
      It := List_Iterate_Safe (Driver_List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         exit when El = Null_Node;
         Sig := Compute_Sub_Signal (Inst, El);

         Add_Process_Driver (Proc_Idx, Sig, El);

         Next (It);
      end loop;
      Instance_Pool := null;
      Trans_Analyzes.Free_Drivers_List (Driver_List);
   end Gather_Process_Drivers;

   procedure Gather_Sensitivity (Inst : Synth_Instance_Acc;
                                 Proc_Idx : Process_Index_Type;
                                 List : Iir_List)
   is
      It : List_Iterator;
      El : Node;
      Sig : Sub_Signal_Type;
   begin
      It := List_Iterate_Safe (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         exit when El = Null_Node;
         Sig := Compute_Sub_Signal (Inst, El);

         Sensitivity_Table.Append
           ((Sig => Sig,
             Prev_Sig => Signals_Table.Table (Sig.Base).Sensitivity,

             Proc => Proc_Idx,
             Prev_Proc => Processes_Table.Table (Proc_Idx).Sensitivity));

         Signals_Table.Table (Sig.Base).Sensitivity := Sensitivity_Table.Last;
         Processes_Table.Table (Proc_Idx).Sensitivity :=
           Sensitivity_Table.Last;

         Next (It);
      end loop;
   end Gather_Sensitivity;

   procedure Gather_Process_Sensitivity
     (Inst : Synth_Instance_Acc; Proc : Node; Proc_Idx : Process_Index_Type)
   is
      List : Iir_List;
   begin
      case Get_Kind (Proc) is
         when Iir_Kind_Process_Statement =>
            --  No sensitivity list.
            --  TODO: extract potential list from wait statements ?
            return;
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            List := Create_Iir_List;
            Vhdl.Canon.Canon_Extract_Sensitivity_Simple_Signal_Assignment
              (Proc, List);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            List := Create_Iir_List;
            Vhdl.Canon.Canon_Extract_Sensitivity_Conditional_Signal_Assignment
              (Proc, List);
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            List := Create_Iir_List;
            Vhdl.Canon.Canon_Extract_Sensitivity_Selected_Signal_Assignment
              (Proc, List);
         when Iir_Kind_Concurrent_Assertion_Statement =>
            List := Create_Iir_List;
            Vhdl.Canon.Canon_Extract_Sensitivity_Assertion_Statement
              (Proc, List);
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            List := Create_Iir_List;
            Vhdl.Canon.Canon_Extract_Sensitivity_Procedure_Call
              (Get_Procedure_Call (Proc), List);
         when Iir_Kind_Sensitized_Process_Statement =>
            List := Get_Sensitivity_List (Proc);
            if List = Iir_List_All then
               List := Vhdl.Canon.Canon_Extract_Sensitivity_Process (Proc);
            else
               Gather_Sensitivity (Inst, Proc_Idx, List);
               return;
            end if;
         when Iir_Kind_Psl_Assert_Directive =>
            List := Get_PSL_Clock_Sensitivity (Proc);
            Gather_Sensitivity (Inst, Proc_Idx, List);
            return;
         when Iir_Kind_Concurrent_Break_Statement =>
            List := Get_Sensitivity_List (Proc);
            if List /= Null_Iir_List then
               Gather_Sensitivity (Inst, Proc_Idx, List);
               return;
            else
               List := Create_Iir_List;
               Vhdl.Canon.Canon_Extract_Sensitivity_Break_Statement
                 (Proc, List);
            end if;
         when others =>
            Error_Kind ("gather_process_sensitivity", Proc);
      end case;
      Gather_Sensitivity (Inst, Proc_Idx, List);
      Destroy_Iir_List (List);
   end Gather_Process_Sensitivity;

   --  Increment the number of sources for EP.
   procedure Increment_Nbr_Sources (Ep : Sub_Signal_Type) is
   begin
      if Ep.Typ.W = 0 then
         return;
      end if;
      for I in Ep.Offs.Net_Off .. Ep.Offs.Net_Off + Ep.Typ.W - 1 loop
         declare
            N : Uns32 renames
              Signals_Table.Table (Ep.Base).Nbr_Sources (I).Nbr_Conns;
         begin
            N := N + 1;
         end;
      end loop;
   end Increment_Nbr_Sources;

   procedure Gather_Connections (Port_Inst : Synth_Instance_Acc;
                                 Ports : Node;
                                 Assoc_Inst : Synth_Instance_Acc;
                                 Assocs : Node)
   is
      use Synth.Vhdl_Stmts;
      Marker : Mark_Type;
      Assoc_Inter : Node;
      Assoc : Node;
      Inter : Node;
      Formal : Node;
      Formal_Base : Valtyp;
      Actual_Base : Valtyp;
      Formal_Sig : Signal_Index_Type;
      Actual_Sig : Signal_Index_Type;
      Typ : Type_Acc;
      Off : Value_Offsets;
      Conn : Connect_Entry;
      List : Iir_List;
      Formal_Ep, Actual_Ep : Sub_Signal_Type;
   begin
      Mark_Expr_Pool (Marker);
      Assoc := Assocs;
      Assoc_Inter := Ports;
      while Is_Valid (Assoc) loop
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Name =>
               Inter := Get_Association_Interface (Assoc, Assoc_Inter);
               Formal := Get_Formal (Assoc);
               if Formal = Null_Iir then
                  Formal := Inter;
               end if;
               Synth_Assignment_Prefix
                 (Port_Inst, Formal, Formal_Base, Typ, Off);
               Typ := Unshare (Typ, Global_Pool'Access);
               Formal_Sig := Formal_Base.Val.S;
               Formal_Ep := (Formal_Sig, Off, Typ);

               Synth_Assignment_Prefix
                 (Assoc_Inst, Get_Actual (Assoc), Actual_Base, Typ, Off);
               Typ := Unshare (Typ, Global_Pool'Access);
               Actual_Sig := Actual_Base.Val.S;
               Actual_Ep := (Actual_Sig, Off, Typ);

               Conn :=
                 (Formal => Formal_Ep,
                  Formal_Link => Signals_Table.Table (Formal_Sig).Connect,
                  Actual => Actual_Ep,
                  Actual_Link => Signals_Table.Table (Actual_Sig).Connect,
                  Drive_Formal => False,
                  Drive_Actual => False,
                  Collapsed => False,
                  Assoc => Assoc,
                  Assoc_Inst => Assoc_Inst);

               --  LRM08 6.4.2.3 Signal declarations
               --  [...], each source is either a driver or an OUT, INOUT,
               --  BUFFER, or LINKAGE port [...]
               case Get_Mode (Inter) is
                  when Iir_In_Mode =>
                     Conn.Drive_Formal := True;
                     Conn.Drive_Actual := False;
                  when Iir_Out_Mode
                    | Iir_Buffer_Mode =>
                     Conn.Drive_Formal := False;
                     Conn.Drive_Actual := True;
                     Increment_Nbr_Sources (Actual_Ep);
                  when Iir_Inout_Mode
                    | Iir_Linkage_Mode =>
                     Conn.Drive_Formal := True;
                     Conn.Drive_Actual := True;
                     Increment_Nbr_Sources (Actual_Ep);
                  when Iir_Unknown_Mode =>
                     raise Internal_Error;
               end case;

               Connect_Table.Append (Conn);

               Signals_Table.Table (Formal_Sig).Connect := Connect_Table.Last;
               Signals_Table.Table (Actual_Sig).Connect := Connect_Table.Last;

               --  Collapse
               if Get_Collapse_Signal_Flag (Assoc)
                 and then Formal_Ep.Offs.Mem_Off = 0
                 and then Actual_Ep.Offs.Mem_Off = 0
                 and then Actual_Base.Typ.W = Actual_Ep.Typ.W
                 and then Formal_Base.Typ.W = Formal_Ep.Typ.W
               then
                  --  Full collapse.
                  pragma Assert (Signals_Table.Table (Formal_Sig).Collapsed_By
                                   = No_Signal_Index);
                  pragma Assert (Formal_Sig > Actual_Sig);
                  Signals_Table.Table (Formal_Sig).Collapsed_By := Actual_Sig;
                  Connect_Table.Table (Connect_Table.Last).Collapsed := True;
               end if;
            when Iir_Kind_Association_Element_Open
              | Iir_Kind_Association_Element_By_Individual =>
               null;
            when Iir_Kind_Association_Element_By_Expression =>
               Inter := Get_Association_Interface (Assoc, Assoc_Inter);
               Formal := Get_Formal (Assoc);
               if Formal = Null_Iir then
                  Formal := Inter;
               end if;
               Formal_Ep := Compute_Sub_Signal (Port_Inst, Formal);

               Actual_Ep := (No_Signal_Index, No_Value_Offsets, null);

               Conn :=
                 (Formal => Formal_Ep,
                  Formal_Link => Signals_Table.Table (Formal_Ep.Base).Connect,
                  Actual => Actual_Ep,
                  Actual_Link => No_Connect_Index,
                  Drive_Formal => True, --  Always an IN interface
                  Drive_Actual => False,
                  Collapsed => False,
                  Assoc => Assoc,
                  Assoc_Inst => Assoc_Inst);

               Connect_Table.Append (Conn);

               Signals_Table.Table (Formal_Ep.Base).Connect :=
                 Connect_Table.Last;

               if Get_Expr_Staticness (Get_Actual (Assoc)) < Globally then
                  --  Create a process to assign the expression to the port.
                  Processes_Table.Append
                    ((Proc => Assoc,
                      Inst => Assoc_Inst,
                      Drivers => No_Driver_Index,
                      Sensitivity => No_Sensitivity_Index));

                  Add_Process_Driver
                    (Processes_Table.Last, Formal_Ep, Assoc);

                  List := Create_Iir_List;
                  Vhdl.Canon.Canon_Extract_Sensitivity_Expression
                    (Get_Actual (Assoc), List, False);
                  Gather_Sensitivity (Assoc_Inst, Processes_Table.Last, List);
                  Destroy_Iir_List (List);
               end if;
            when others =>
               Error_Kind ("gather_connections", Assoc);
         end case;
         Release_Expr_Pool (Marker);
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Gather_Connections;

   procedure Gather_Connections_Instantiation_Statement
     (Inst : Synth_Instance_Acc; Stmt : Node; Sub_Inst : Synth_Instance_Acc)
   is
      Sub_Scope : constant Node := Get_Source_Scope (Sub_Inst);
      Comp_Inst : Synth_Instance_Acc;
      Arch : Node;
      Ent : Node;
      Config : Node;
      Bind : Node;
   begin
      if Get_Kind (Sub_Scope) = Iir_Kind_Component_Declaration then
         --  Connections with the components.
         Gather_Connections (Sub_Inst, Get_Port_Chain (Sub_Scope),
                             Inst, Get_Port_Map_Aspect_Chain (Stmt));
         --  Connections with the entity
         Comp_Inst := Get_Component_Instance (Sub_Inst);
         if Comp_Inst = null then
            --  Unbounded.
            return;
         end if;
         Arch := Get_Source_Scope (Comp_Inst);
         Ent := Get_Entity (Arch);
         Config := Get_Instance_Config (Sub_Inst);
         Bind := Get_Binding_Indication (Config);
         --  Connections of the entity with the component.
         Gather_Connections (Comp_Inst, Get_Port_Chain (Ent),
                             Sub_Inst, Get_Port_Map_Aspect_Chain (Bind));
      else
         pragma Assert (Get_Kind (Sub_Scope) = Iir_Kind_Architecture_Body);
         Gather_Connections
           (Sub_Inst, Get_Port_Chain (Get_Entity (Sub_Scope)),
            Inst, Get_Port_Map_Aspect_Chain (Stmt));
      end if;
      pragma Assert (Areapools.Is_Empty (Expr_Pool));
   end Gather_Connections_Instantiation_Statement;

   procedure Gather_Processes_Stmt
     (Inst : Synth_Instance_Acc; Stmt : Node) is
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Component_Instantiation_Statement =>
            declare
               Sub_Inst : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Inst, Stmt);
            begin
               Gather_Processes_1 (Sub_Inst);
               pragma Assert (Areapools.Is_Empty (Expr_Pool));
               Gather_Connections_Instantiation_Statement
                 (Inst, Stmt, Sub_Inst);
            end;
         when Iir_Kind_If_Generate_Statement =>
            declare
               Sub : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Inst, Stmt);
            begin
               if Sub /= null then
                  Gather_Processes_1 (Sub);
               end if;
            end;
         when Iir_Kind_For_Generate_Statement =>
            declare
               It : constant Node := Get_Parameter_Specification (Stmt);
               It_Rng : Type_Acc;
               It_Len : Natural;
               Gen_Inst : Synth_Instance_Acc;
            begin
               It_Rng := Get_Subtype_Object (Inst, Get_Type (It));
               It_Len := Natural (Get_Range_Length (It_Rng.Drange));
               Gen_Inst := Get_Sub_Instance (Inst, Stmt);
               for I in 1 .. It_Len loop
                  Gather_Processes_1
                    (Get_Generate_Sub_Instance (Gen_Inst, I));
               end loop;
            end;
         when Iir_Kind_Block_Statement =>
            declare
               Sub : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Inst, Stmt);
               Hdr : constant Node := Get_Block_Header (Stmt);
            begin
               Gather_Processes_1 (Sub);
               if Hdr /= Null_Node then
                  Gather_Connections (Sub, Get_Port_Chain (Hdr),
                                      Inst, Get_Port_Map_Aspect_Chain (Hdr));
               end if;
            end;
         when Iir_Kinds_Concurrent_Signal_Assignment
           | Iir_Kind_Concurrent_Assertion_Statement
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kinds_Process_Statement =>
            Processes_Table.Append ((Proc => Stmt,
                                     Inst => Inst,
                                     Drivers => No_Driver_Index,
                                     Sensitivity => No_Sensitivity_Index));
            pragma Assert (Is_Expr_Pool_Empty);
            Gather_Process_Drivers (Inst, Stmt, Processes_Table.Last);
            pragma Assert (Is_Expr_Pool_Empty);
            Gather_Process_Sensitivity (Inst, Stmt, Processes_Table.Last);
         when Iir_Kind_Psl_Default_Clock =>
            null;
         when Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Concurrent_Break_Statement =>
            Processes_Table.Append ((Proc => Stmt,
                                     Inst => Inst,
                                     Drivers => No_Driver_Index,
                                     Sensitivity => No_Sensitivity_Index));
            Gather_Process_Sensitivity (Inst, Stmt, Processes_Table.Last);
         when Iir_Kind_Simple_Simultaneous_Statement =>
            Simultaneous_Table.Append ((Stmt => Stmt, Inst => Inst));
         when others =>
            Vhdl.Errors.Error_Kind ("gather_processes_stmt", Stmt);
      end case;
      pragma Assert (Is_Expr_Pool_Empty);
   end Gather_Processes_Stmt;

   procedure Gather_Processes_Stmts (Inst : Synth_Instance_Acc; Stmts : Node)
   is
      Stmt : Node;
   begin
      Stmt := Stmts;
      while Stmt /= Null_Node loop
         Gather_Processes_Stmt (Inst, Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Gather_Processes_Stmts;

   procedure Gather_Processes_1 (Inst : Synth_Instance_Acc)
   is
      N : constant Node := Get_Source_Scope (Inst);
   begin
      case Get_Kind (N) is
         when Iir_Kind_Architecture_Body =>
            declare
               Ent : constant Node := Get_Entity (N);
            begin
               Gather_Processes_Decls
                 (Inst, Get_Port_Chain (Ent));
               Gather_Processes_Decls
                 (Inst, Get_Declaration_Chain (Ent));
               Gather_Processes_Stmts
                 (Inst, Get_Concurrent_Statement_Chain (Ent));
               Gather_Processes_Decls
                 (Inst, Get_Declaration_Chain (N));
               Gather_Processes_Stmts
                 (Inst, Get_Concurrent_Statement_Chain (N));
            end;
         when Iir_Kind_Component_Declaration =>
            declare
               Comp_Inst : constant Synth_Instance_Acc :=
                 Get_Component_Instance (Inst);
            begin
               Gather_Processes_Decls (Inst, Get_Port_Chain (N));
               if Comp_Inst /= null then
                  Gather_Processes_1 (Comp_Inst);
               end if;
            end;
         when Iir_Kind_Block_Statement =>
            declare
               Hdr : constant Node := Get_Block_Header (N);
               Guard : constant Node := Get_Guard_Decl (N);
            begin
               if Guard /= Null_Node then
                  Gather_Signal ((Mode_Guard, Guard, Inst, null, null, null,
                                  No_Sensitivity_Index, No_Signal_Index,
                                  No_Connect_Index));
               end if;
               if Hdr /= Null_Node then
                  Gather_Processes_Decls (Inst, Get_Port_Chain (Hdr));
               end if;
               Gather_Processes_Decls
                 (Inst, Get_Declaration_Chain (N));
               Gather_Processes_Stmts
                 (Inst, Get_Concurrent_Statement_Chain (N));
            end;
         when Iir_Kind_Generate_Statement_Body =>
            Gather_Processes_Decls
              (Inst, Get_Declaration_Chain (N));
            Gather_Processes_Stmts
              (Inst, Get_Concurrent_Statement_Chain (N));
         when Iir_Kind_Package_Declaration =>
            Gather_Processes_Decls
              (Inst, Get_Declaration_Chain (N));
         when Iir_Kind_Configuration_Declaration =>
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("gather_processes_1", N);
      end case;

      pragma Assert (Areapools.Is_Empty (Expr_Pool));
   end Gather_Processes_1;

   procedure Gather_Processes (Top : Synth_Instance_Acc) is
   begin
      pragma Assert (Areapools.Is_Empty (Expr_Pool));

      Processes_Table.Init;
      Signals_Table.Init;
      Drivers_Table.Init;

      Simul.Vhdl_Debug.Init;

      --  Init Signals_Table.
      Signals_Table.Set_Last (Get_Nbr_Signal);
      for I in Signals_Table.First .. Signals_Table.Last loop
         Signals_Table.Table (I) :=
           (Mode_End, Null_Node, null, null, null, null,
            No_Sensitivity_Index, No_Signal_Index, No_Connect_Index);
      end loop;

      --  Gather declarations of top-level packages.
      declare
         It : Iterator_Top_Level_Type;
         Inst : Synth_Instance_Acc;
      begin
         It := Iterator_Top_Level_Init;
         loop
            Iterate_Top_Level (It, Inst);
            exit when Inst = null;
            pragma Assert (Inst /= Top);
            Gather_Processes_1 (Inst);
         end loop;
      end;

      --  Gather declarations in the hierarchy.
      Gather_Processes_1 (Top);

      --  For the debugger.
      Top_Instance := Top;

      --  Compute total number of sources.
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            E : Signal_Entry renames Signals_Table.Table (I);
            Is_Out : constant Boolean :=
              Get_Kind (E.Decl) = Iir_Kind_Interface_Signal_Declaration
              and then Get_Mode (E.Decl) in Iir_Out_Modes;
         begin
            if E.Kind in Mode_Signal_User then
               for J in 1 .. E.Typ.W loop
                  declare
                     Ns : Nbr_Sources_Type renames E.Nbr_Sources (J - 1);
                  begin
                     Ns.Total := Ns.Nbr_Drivers + Ns.Nbr_Conns;
                     if Ns.Total = 0 and then Is_Out then
                        Ns.Total := 1;
                     end if;
                     if E.Collapsed_By /= No_Signal_Index
                       and then (Signals_Table.Table (E.Collapsed_By).Kind
                                   in Mode_Signal_User)
                     then
                        --  Add to the parent.
                        declare
                           C_Ns : Nbr_Sources_Type renames
                             Signals_Table.Table (E.Collapsed_By)
                             .Nbr_Sources (J - 1);
                        begin
                           --  Remove 1 for the connection.
                           C_Ns.Total := C_Ns.Total + Ns.Total - 1;
                        end;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
   end Gather_Processes;

   procedure Elab_Processes
   is
      Proc : Node;
      Proc_Inst : Synth_Instance_Acc;
   begin
      pragma Assert (Areapools.Is_Empty (Expr_Pool));

      Instance_Pool := Global_Pool'Access;
      for I in Processes_Table.First .. Processes_Table.Last loop
         Proc := Processes_Table.Table (I).Proc;
         if Get_Kind (Proc) in Iir_Kinds_Process_Statement then
            Proc_Inst := Make_Elab_Instance (Processes_Table.Table (I).Inst,
                                             Proc, Null_Node);
            Processes_Table.Table (I).Inst := Proc_Inst;
            Set_Instance_Const (Proc_Inst, True);
            Synth.Vhdl_Decls.Synth_Declarations
              (Proc_Inst, Get_Declaration_Chain (Proc), True);
            exit when Is_Error (Proc_Inst);
         end if;
      end loop;
      Instance_Pool := null;
   end Elab_Processes;

   procedure Elab_Drivers is
   begin
      null;
   end Elab_Drivers;
end Simul.Vhdl_Elab;
