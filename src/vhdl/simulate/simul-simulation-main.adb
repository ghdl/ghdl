--  Interpreted simulation
--  Copyright (C) 2014-2017 Tristan Gingold
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
with Simple_IO; use Simple_IO;
with Types; use Types;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors; use Vhdl.Errors;
with PSL.Types; use PSL.Types;
with PSL.Nodes;
with PSL.NFAs;
with PSL.NFAs.Utils;
with PSL.Errors; use PSL.Errors;
with Vhdl.Std_Package;
with Trans_Analyzes;
with Simul.Elaboration; use Simul.Elaboration;
with Simul.Execution; use Simul.Execution;
with Vhdl.Annotations; use Vhdl.Annotations;
with Vhdl.Ieee.Std_Logic_1164;
with Grt.Main;
with Simul.Debugger; use Simul.Debugger;
with Simul.Debugger.AMS;
with Grt.Errors;
with Grt.Processes;
with Grt.Signals;
with Areapools; use Areapools;

package body Simul.Simulation.Main is
   --  Configuration for the whole design
   Top_Config : Iir_Design_Unit;

   --  Elaborate the design
   procedure Ghdl_Elaborate;
   pragma Export (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   function To_Instance_Acc is new Ada.Unchecked_Conversion
     (System.Address, Grt.Processes.Instance_Acc);

   procedure Process_Executer (Self : Grt.Processes.Instance_Acc);
   pragma Convention (C, Process_Executer);

   procedure Process_Executer (Self : Grt.Processes.Instance_Acc)
   is
      function To_Process_State_Acc is new Ada.Unchecked_Conversion
        (Grt.Processes.Instance_Acc, Process_State_Acc);

      Process : Process_State_Acc renames
        To_Process_State_Acc (Self);
   begin
      --  For debugger
      Current_Process := Process;

      Instance_Pool := Process.Pool'Access;

      if Trace_Simulation then
         Put (" run process: ");
         Disp_Instance_Name (Process.Top_Instance);
         Put_Line (" (" & Disp_Location (Process.Proc) & ")");
      end if;

      Execute_Sequential_Statements (Process);

      --  Sanity checks.
      if not Is_Empty (Expr_Pool) then
         raise Internal_Error;
      end if;

      case Get_Kind (Process.Proc) is
         when Iir_Kind_Sensitized_Process_Statement =>
            if Process.Instance.In_Wait_Flag then
               raise Internal_Error;
            end if;
            if Process.Instance.Stmt = Null_Iir then
               Process.Instance.Stmt :=
                 Get_Sequential_Statement_Chain (Process.Proc);
            end if;
         when Iir_Kind_Process_Statement =>
            if not Process.Instance.In_Wait_Flag then
               raise Internal_Error;
            end if;
         when others =>
            raise Internal_Error;
      end case;

      Instance_Pool := null;
      Current_Process := null;
   end Process_Executer;

   type Convert_Mode is (Convert_In, Convert_Out);

   type Convert_Instance_Type is record
      Mode : Convert_Mode;
      Instance : Block_Instance_Acc;
      Func : Iir;
      Src : Iir_Value_Literal_Acc;
      Dst : Iir_Value_Literal_Acc;
   end record;

   type Convert_Instance_Acc is access Convert_Instance_Type;

   procedure Conversion_Proc (Data : System.Address) is
      Conv : Convert_Instance_Type;
      pragma Import (Ada, Conv);
      for Conv'Address use Data;

      Src : Iir_Value_Literal_Acc;
      Dst : Iir_Value_Literal_Acc;

      Expr_Mark : Mark_Type;
   begin
      pragma Assert (Instance_Pool = null);
      Instance_Pool := Global_Pool'Access;
      Mark (Expr_Mark, Expr_Pool);
      Current_Process := No_Process;

      case Conv.Mode is
         when Convert_In =>
            Src := Execute_Read_Signal_Value
              (Conv.Src, Read_Signal_Effective_Value);
         when Convert_Out =>
            Src := Execute_Read_Signal_Value
              (Conv.Src, Read_Signal_Driving_Value);
      end case;

      Dst := Execute_Assoc_Conversion (Conv.Instance, Conv.Func, Src);

      Check_Bounds (Conv.Dst, Dst, Conv.Func);

      case Conv.Mode is
         when Convert_In =>
            Execute_Write_Signal (Conv.Dst, Dst, Write_Signal_Effective_Value);
         when Convert_Out =>
            Execute_Write_Signal (Conv.Dst, Dst, Write_Signal_Driving_Value);
      end case;

      Release (Expr_Mark, Expr_Pool);
      Instance_Pool := null;
   end Conversion_Proc;

   -- Add a driver for signal designed by VAL (via index field) for instance
   -- INSTANCE of process PROC.
   -- FIXME: default value.
   procedure Add_Source (Instance : Block_Instance_Acc;
                         Sig : Iir_Value_Literal_Acc;
                         Val : Iir_Value_Literal_Acc) is
   begin
      case Val.Kind is
         when Iir_Value_B1 =>
            Grt.Signals.Ghdl_Signal_Add_Port_Driver_B1 (Sig.Sig, Val.B1);
         when Iir_Value_E8 =>
            Grt.Signals.Ghdl_Signal_Add_Port_Driver_E8 (Sig.Sig, Val.E8);
         when Iir_Value_E32 =>
            Grt.Signals.Ghdl_Signal_Add_Port_Driver_E32 (Sig.Sig, Val.E32);
         when Iir_Value_I64 =>
            Grt.Signals.Ghdl_Signal_Add_Port_Driver_I64 (Sig.Sig, Val.I64);
         when Iir_Value_F64 =>
            Grt.Signals.Ghdl_Signal_Add_Port_Driver_F64 (Sig.Sig, Val.F64);
         when Iir_Value_Array =>
            for I in Sig.Val_Array.V'Range loop
               Add_Source (Instance, Sig.Val_Array.V (I), Val.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            for I in Sig.Val_Record.V'Range loop
               Add_Source
                 (Instance, Sig.Val_Record.V (I), Val.Val_Record.V (I));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Add_Source;

   --  Add drivers for process PROC.
   --  Note: this is done recursively on the callees of PROC.
   procedure Elaborate_Drivers (Instance: Block_Instance_Acc; Proc: Iir)
   is
      Driver_List: Iir_List;
      It : List_Iterator;
      El: Iir;
      Val : Iir_Value_Literal_Acc;
      Sig : Iir_Value_Literal_Acc;
      Marker : Mark_Type;
   begin
      if Trace_Drivers then
         Put ("Drivers for ");
         Disp_Instance_Name (Instance);
         Put_Line (": " & Disp_Node (Proc));
      end if;

      Driver_List := Trans_Analyzes.Extract_Drivers (Proc);

      -- Some processes have no driver list (assertion).
      It := List_Iterate_Safe (Driver_List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         if Trace_Drivers then
            Put_Line (' ' & Disp_Node (El));
         end if;

         Mark (Marker, Expr_Pool);
         --  The signal name is evaluated twice, but as it is globally static,
         --  it shouldn't have any side-effect.  So not optimized but safe.
         Sig := Execute_Signal_Name (Instance, El, Signal_Sig);
         Val := Execute_Signal_Name (Instance, El, Signal_Init);
         Add_Source (Instance, Sig, Val);
         Release (Marker, Expr_Pool);

         Next (It);
      end loop;
   end Elaborate_Drivers;

   --  Call Ghdl_Process_Add_Sensitivity for each scalar subelement of
   --  SIG.
   procedure Process_Add_Sensitivity (Sig: Iir_Value_Literal_Acc) is
   begin
      case Sig.Kind is
         when Iir_Value_Signal =>
            Grt.Processes.Ghdl_Process_Add_Sensitivity (Sig.Sig);
         when Iir_Value_Array =>
            for I in Sig.Val_Array.V'Range loop
               Process_Add_Sensitivity (Sig.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            for I in Sig.Val_Record.V'Range loop
               Process_Add_Sensitivity (Sig.Val_Record.V (I));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Process_Add_Sensitivity;

   procedure Register_Sensitivity
     (Instance : Block_Instance_Acc; List : Iir_List)
   is
      It : List_Iterator;
      Sig : Iir;
      Marker : Mark_Type;
   begin
      It := List_Iterate (List);
      while Is_Valid (It) loop
         Sig := Get_Element (It);
         Mark (Marker, Expr_Pool);
         Process_Add_Sensitivity (Execute_Name (Instance, Sig, True));
         Release (Marker, Expr_Pool);
         Next (It);
      end loop;
   end Register_Sensitivity;

   procedure Create_Processes
   is
      use Grt.Processes;
      El : Iir;
      Instance : Block_Instance_Acc;
      Instance_Grt : Grt.Processes.Instance_Acc;
   begin
      Processes_State := new Process_State_Array (1 .. Processes_Table.Last);

      for I in Processes_Table.First .. Processes_Table.Last loop
         Instance := Processes_Table.Table (I);
         El := Instance.Label;

         Instance_Pool := Processes_State (I).Pool'Access;
         Instance.Stmt := Get_Sequential_Statement_Chain (El);

         Processes_State (I).Top_Instance := Instance;
         Processes_State (I).Proc := El;
         Processes_State (I).Instance := Instance;

         Current_Process := Processes_State (I)'Access;
         Instance_Grt := To_Instance_Acc (Processes_State (I)'Address);
         case Get_Kind (El) is
            when Iir_Kind_Sensitized_Process_Statement =>
               if Get_Postponed_Flag (El) then
                  Ghdl_Postponed_Sensitized_Process_Register
                    (Instance_Grt,
                     Process_Executer'Access,
                     null, System.Null_Address);
               else
                  Ghdl_Sensitized_Process_Register
                    (Instance_Grt,
                     Process_Executer'Access,
                     null, System.Null_Address);
               end if;

               --  Register sensitivity.
               Register_Sensitivity (Instance, Get_Sensitivity_List (El));

            when Iir_Kind_Process_Statement =>
               if Get_Postponed_Flag (El) then
                  Ghdl_Postponed_Process_Register
                    (Instance_Grt,
                     Process_Executer'Access,
                     null, System.Null_Address);
               else
                  Ghdl_Process_Register
                    (Instance_Grt,
                     Process_Executer'Access,
                     null, System.Null_Address);
               end if;

            when others =>
               raise Internal_Error;
         end case;

         --  LRM93 12.4.4  Other Concurrent Statements
         --  All other concurrent statements are either process
         --  statements or are statements for which there is an
         --  equivalent process statement.
         --  Elaboration of a process statement proceeds as follows:
         --  1.  The process declarative part is elaborated.
         Elaborate_Declarative_Part
           (Instance, Get_Declaration_Chain (El));

         --  2.  The drivers required by the process statement
         --      are created.
         --  3.  The initial transaction defined by the default value
         --      associated with each scalar signal driven by the
         --      process statement is inserted into the corresponding
         --      driver.
         --  FIXME: do it for drivers in called subprograms too.
         Elaborate_Drivers (Instance, El);

         if not Is_Empty (Expr_Pool) then
            raise Internal_Error;
         end if;

         --  Elaboration of all concurrent signal assignment
         --  statements and concurrent assertion statements consists
         --  of the construction of the equivalent process statement
         --  followed by the elaboration of the equivalent process
         --  statement.
         --  [GHDL:  this is done by canonicalize.  ]

         --  FIXME: check passive statements,
         --  check no wait statement in sensitized processes.

         Instance_Pool := null;
      end loop;

      if Trace_Simulation then
         Disp_Signals_Value;
      end if;
   end Create_Processes;

   procedure PSL_Process_Executer (Self : Grt.Processes.Instance_Acc);
   pragma Convention (C, PSL_Process_Executer);

   procedure PSL_Assert_Finalizer (Self : Grt.Processes.Instance_Acc);
   pragma Convention (C, PSL_Assert_Finalizer);

   type PSL_Entry_Acc is access all PSL_Entry;
   function To_PSL_Entry_Acc is new Ada.Unchecked_Conversion
     (Grt.Processes.Instance_Acc, PSL_Entry_Acc);

   function Execute_Psl_Expr (Instance : Block_Instance_Acc;
                              Expr : PSL_Node;
                              Eos : Boolean)
                             return Boolean
   is
      use PSL.Nodes;
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr
           | N_HDL_Bool =>
            declare
               E : constant Iir := Get_HDL_Node (Expr);
               Rtype : constant Iir := Get_Base_Type (Get_Type (E));
               Res   : Iir_Value_Literal_Acc;
            begin
               Res := Execute_Expression (Instance, E);
               if Rtype = Vhdl.Std_Package.Boolean_Type_Definition then
                  return Res.B1 = True;
               elsif Rtype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type then
                  return Res.E8 = 3 or Res.E8 = 7; --  1 or H
               else
                  Error_Kind ("execute_psl_expr", Expr);
               end if;
            end;
         when N_True =>
            return True;
         when N_EOS =>
            return Eos;
         when N_Not_Bool =>
            return not Execute_Psl_Expr (Instance, Get_Boolean (Expr), Eos);
         when N_And_Bool =>
            return Execute_Psl_Expr (Instance, Get_Left (Expr), Eos)
              and Execute_Psl_Expr (Instance, Get_Right (Expr), Eos);
         when N_Or_Bool =>
            return Execute_Psl_Expr (Instance, Get_Left (Expr), Eos)
              or Execute_Psl_Expr (Instance, Get_Right (Expr), Eos);
         when others =>
            Error_Kind ("execute_psl_expr", Expr);
      end case;
   end Execute_Psl_Expr;

   procedure PSL_Process_Executer (Self : Grt.Processes.Instance_Acc)
   is
      use PSL.NFAs;

      E : constant PSL_Entry_Acc := To_PSL_Entry_Acc (Self);
      Nvec : Boolean_Vector (E.States.all'Range);
      Marker : Mark_Type;
      V : Boolean;

      NFA : PSL_NFA;
      S : NFA_State;
      S_Num : Nat32;
      Ed : NFA_Edge;
      Sd : NFA_State;
      Sd_Num : Nat32;
   begin
      --  Exit now if already covered (never set for assertion).
      if E.Done then
         return;
      end if;

      Instance_Pool := Global_Pool'Access;
      Current_Process := No_Process;

      Mark (Marker, Expr_Pool);
      V := Execute_Psl_Expr (E.Instance, Get_PSL_Clock (E.Stmt), False);
      Release (Marker, Expr_Pool);
      if V then
         Nvec := (others => False);
         case Get_Kind (E.Stmt) is
            when Iir_Kind_Psl_Cover_Directive
              | Iir_Kind_Psl_Endpoint_Declaration =>
               Nvec (0) := True;
            when others =>
               null;
         end case;

         --  For each state: if set, evaluate all outgoing edges.
         NFA := Get_PSL_NFA (E.Stmt);
         S := Get_First_State (NFA);
         while S /= No_State loop
            S_Num := Get_State_Label (S);

            if E.States (S_Num) then
               Ed := Get_First_Src_Edge (S);
               while Ed /= No_Edge loop
                  Sd := Get_Edge_Dest (Ed);
                  Sd_Num := Get_State_Label (Sd);

                  if not Nvec (Sd_Num) then
                     Mark (Marker, Expr_Pool);
                     V := Execute_Psl_Expr
                       (E.Instance, Get_Edge_Expr (Ed), False);
                     Release (Marker, Expr_Pool);
                     if V then
                        Nvec (Sd_Num) := True;
                     end if;
                  end if;

                  Ed := Get_Next_Src_Edge (Ed);
               end loop;
            end if;

            S := Get_Next_State (S);
         end loop;

         --  Check fail state.
         S := Get_Final_State (NFA);
         S_Num := Get_State_Label (S);
         pragma Assert (S_Num = Get_PSL_Nbr_States (E.Stmt) - 1);
         case Get_Kind (E.Stmt) is
            when Iir_Kind_Psl_Assert_Directive =>
               if Nvec (S_Num) then
                  Execute_Failed_Assertion
                    (E.Instance, "psl assertion", E.Stmt,
                     "assertion violation", 2);
               end if;
            when Iir_Kind_Psl_Assume_Directive =>
               if Nvec (S_Num) then
                  Execute_Failed_Assertion
                    (E.Instance, "psl assumption", E.Stmt,
                     "assumption violation", 2);
               end if;
            when Iir_Kind_Psl_Cover_Directive =>
               if Nvec (S_Num) then
                  if Get_Report_Expression (E.Stmt) /= Null_Iir then
                     Execute_Failed_Assertion
                       (E.Instance, "psl cover", E.Stmt,
                        "sequence covered", 0);
                  end if;
                  E.Done := True;
               end if;
            when Iir_Kind_Psl_Endpoint_Declaration =>
               declare
                  Info : constant Sim_Info_Acc := Get_Info (E.Stmt);
               begin
                  E.Instance.Objects (Info.Slot).B1 := Ghdl_B1 (Nvec (S_Num));
               end;
            when others =>
               Error_Kind ("PSL_Process_Executer", E.Stmt);
         end case;

         E.States.all := Nvec;
      end if;

      Instance_Pool := null;
      Current_Process := null;
   end PSL_Process_Executer;

   procedure PSL_Assert_Finalizer (Self : Grt.Processes.Instance_Acc)
   is
      use PSL.NFAs;
      Ent : constant PSL_Entry_Acc := To_PSL_Entry_Acc (Self);

      NFA : constant PSL_NFA := Get_PSL_NFA (Ent.Stmt);
      S : NFA_State;
      E : NFA_Edge;
      Sd : NFA_State;
      S_Num : Int32;
   begin
      S := Get_Final_State (NFA);
      E := Get_First_Dest_Edge (S);
      while E /= No_Edge loop
         Sd := Get_Edge_Src (E);

         if PSL.NFAs.Utils.Has_EOS (Get_Edge_Expr (E)) then

            S_Num := Get_State_Label (Sd);

            if Ent.States (S_Num)
              and then
              Execute_Psl_Expr (Ent.Instance, Get_Edge_Expr (E), True)
            then
               Execute_Failed_Assertion
                 (Ent.Instance, "psl assertion", Ent.Stmt,
                  "assertion violation", 2);
               exit;
            end if;
         end if;

         E := Get_Next_Dest_Edge (E);
      end loop;
   end PSL_Assert_Finalizer;

   procedure Create_PSL is
   begin
      for I in PSL_Table.First .. PSL_Table.Last loop
         declare
            E : PSL_Entry renames PSL_Table.Table (I);
         begin
            --  Create the vector.
            E.States := new Boolean_Vector'
              (0 .. Get_PSL_Nbr_States (E.Stmt) - 1 => False);
            E.States (0) := True;

            Grt.Processes.Ghdl_Process_Register
              (To_Instance_Acc (E'Address), PSL_Process_Executer'Access,
               null, System.Null_Address);

            Register_Sensitivity
              (E.Instance, Get_PSL_Clock_Sensitivity (E.Stmt));

            case Get_Kind (E.Stmt) is
               when Iir_Kind_Psl_Assert_Directive
                  | Iir_Kind_Psl_Assume_Directive =>
                  if Get_PSL_EOS_Flag (E.Stmt) then
                     Grt.Processes.Ghdl_Finalize_Register
                       (To_Instance_Acc (E'Address),
                        PSL_Assert_Finalizer'Access);
                  end if;
               when Iir_Kind_Psl_Cover_Directive =>
                  --  TODO
                  null;
               when others =>
                  null;
            end case;
         end;
      end loop;

      --  Finalizer ?
   end Create_PSL;

   function Create_Shadow_Signal (Sig : Iir_Value_Literal_Acc)
                                 return Iir_Value_Literal_Acc
   is
      Val : Ghdl_Value_Ptr;
   begin
      case Sig.Kind is
         when Iir_Value_Signal =>
            Val := new Value_Union;
            case Sig.Sig.Mode is
               when Mode_I64 =>
                  Val.I64 := 0;
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_I64
                       (Val, null, System.Null_Address));
               when Mode_B1 =>
                  Val.B1 := False;
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_B1
                       (Val, null, System.Null_Address));
               when Mode_E8 =>
                  Val.E8 := 0;
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_E8
                       (Val, null, System.Null_Address));
               when Mode_E32 =>
                  Val.E32 := 0;
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_E32
                       (Val, null, System.Null_Address));
               when Mode_F64 =>
                  Val.F64 := 0.0;
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_F64
                       (Val, null, System.Null_Address));
               when Mode_I32 =>
                  raise Internal_Error;
            end case;
         when Iir_Value_Array =>
            declare
               Res : Iir_Value_Literal_Acc;
            begin
               Res := Unshare_Bounds (Sig, Instance_Pool);
               for I in Res.Val_Array.V'Range loop
                  Res.Val_Array.V (I) :=
                    Create_Shadow_Signal (Sig.Val_Array.V (I));
               end loop;
               return Res;
            end;
         when Iir_Value_Record =>
            declare
               Res : Iir_Value_Literal_Acc;
            begin
               Res := Create_Record_Value
                 (Sig.Val_Record.Len, Instance_Pool);
               for I in Res.Val_Record.V'Range loop
                  Res.Val_Record.V (I) :=
                    Create_Shadow_Signal (Sig.Val_Record.V (I));
               end loop;
               return Res;
            end;
         when Iir_Value_Scalars
           | Iir_Value_Access
           | Iir_Value_Range
           | Iir_Value_Protected
           | Iir_Value_Terminal
           | Iir_Value_Quantity
           | Iir_Value_File
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Create_Shadow_Signal;

   function Get_Leftest_Signal (Val : Iir_Value_Literal_Acc)
                               return Iir_Value_Literal_Acc is
   begin
      case Val.Kind is
         when Iir_Value_Signal =>
            return Val;
         when Iir_Value_Array =>
            return Get_Leftest_Signal (Val.Val_Array.V (1));
         when Iir_Value_Record =>
            return Get_Leftest_Signal (Val.Val_Record.V (1));
         when others =>
            raise Internal_Error;
      end case;
   end Get_Leftest_Signal;

   procedure Add_Conversion (Conv : Convert_Instance_Acc)
   is
      Src_Left : Grt.Signals.Ghdl_Signal_Ptr;
      Src_Len : Ghdl_Index_Type;
      Dst_Left : Grt.Signals.Ghdl_Signal_Ptr;
      Dst_Len : Ghdl_Index_Type;
   begin
      Conv.Src := Unshare_Bounds (Conv.Src, Instance_Pool);
      Conv.Dst := Unshare_Bounds (Conv.Dst, Instance_Pool);

      Src_Left := Get_Leftest_Signal (Conv.Src).Sig;
      Src_Len := Ghdl_Index_Type (Get_Nbr_Of_Scalars (Conv.Src));

      Dst_Left := Get_Leftest_Signal (Conv.Dst).Sig;
      Dst_Len := Ghdl_Index_Type (Get_Nbr_Of_Scalars (Conv.Dst));

      case Conv.Mode is
         when Convert_In =>
            Grt.Signals.Ghdl_Signal_In_Conversion (Conversion_Proc'Address,
                                                   Conv.all'Address,
                                                   Src_Left, Src_Len,
                                                   Dst_Left, Dst_Len);
         when Convert_Out =>
            Grt.Signals.Ghdl_Signal_Out_Conversion (Conversion_Proc'Address,
                                                    Conv.all'Address,
                                                    Src_Left, Src_Len,
                                                    Dst_Left, Dst_Len);
      end case;
   end Add_Conversion;

   type Connect_Mode is (Connect_Source, Connect_Effective);

   -- Add a driving value PORT to signal SIG, ie: PORT is a source for SIG.
   -- As a side effect, this connect the signal SIG with the port PORT.
   -- PORT is the formal, while SIG is the actual.
   procedure Connect (Sig: Iir_Value_Literal_Acc;
                      Port: Iir_Value_Literal_Acc;
                      Mode : Connect_Mode)
   is
   begin
      case Sig.Kind is
         when Iir_Value_Array =>
            if Port.Kind /= Sig.Kind then
               raise Internal_Error;
            end if;

            if Sig.Val_Array.Len /= Port.Val_Array.Len then
               raise Internal_Error;
            end if;
            for I in Sig.Val_Array.V'Range loop
               Connect (Sig.Val_Array.V (I), Port.Val_Array.V (I), Mode);
            end loop;
            return;
         when Iir_Value_Record =>
            if Port.Kind /= Sig.Kind then
               raise Internal_Error;
            end if;
            if Sig.Val_Record.Len /= Port.Val_Record.Len then
               raise Internal_Error;
            end if;
            for I in Sig.Val_Record.V'Range loop
               Connect (Sig.Val_Record.V (I), Port.Val_Record.V (I), Mode);
            end loop;
            return;
         when Iir_Value_Signal =>
            pragma Assert (Port.Kind = Iir_Value_Signal);
            -- Here, SIG and PORT are simple signals (not composite).
            -- PORT is a source for SIG.
            case Mode is
               when Connect_Source =>
                  Grt.Signals.Ghdl_Signal_Add_Source
                    (Sig.Sig, Port.Sig);
               when Connect_Effective =>
                  Grt.Signals.Ghdl_Signal_Effective_Value
                    (Port.Sig, Sig.Sig);
            end case;
         when Iir_Value_E32 =>
            if Mode = Connect_Source then
               raise Internal_Error;
            end if;
            Grt.Signals.Ghdl_Signal_Associate_E32 (Port.Sig, Sig.E32);
         when Iir_Value_I64 =>
            if Mode = Connect_Source then
               raise Internal_Error;
            end if;
            Grt.Signals.Ghdl_Signal_Associate_I64 (Port.Sig, Sig.I64);
         when Iir_Value_B1 =>
            if Mode = Connect_Source then
               raise Internal_Error;
            end if;
            Grt.Signals.Ghdl_Signal_Associate_B1 (Port.Sig, Sig.B1);
         when Iir_Value_E8 =>
            if Mode = Connect_Source then
               raise Internal_Error;
            end if;
            Grt.Signals.Ghdl_Signal_Associate_E8 (Port.Sig, Sig.E8);
         when others =>
            raise Internal_Error;
      end case;
   end Connect;

   procedure Set_Connect
     (Formal_Instance : Block_Instance_Acc;
      Formal_Expr : Iir_Value_Literal_Acc;
      Local_Instance : Block_Instance_Acc;
      Local_Expr : Iir_Value_Literal_Acc;
      Inter : Iir;
      Assoc : Iir_Association_Element_By_Expression)
   is
      pragma Unreferenced (Formal_Instance);
      Formal : constant Iir := Get_Formal (Assoc);
   begin
      if False and Trace_Elaboration then
         Put ("connect formal ");
         Put (Iir_Mode'Image (Get_Mode (Inter)));
         Put (" ");
         Disp_Iir_Value (Formal_Expr, Get_Type (Formal));
         Put (" with actual ");
         Disp_Iir_Value (Local_Expr, Get_Type (Get_Actual (Assoc)));
         New_Line;
      end if;

      case Get_Mode (Inter) is
         when Iir_Out_Mode
           | Iir_Inout_Mode
           | Iir_Buffer_Mode
           | Iir_Linkage_Mode =>
            --  FORMAL_EXPR is a source for LOCAL_EXPR.
            declare
               Out_Conv : constant Iir := Get_Formal_Conversion (Assoc);
               Src : Iir_Value_Literal_Acc;
            begin
               if Out_Conv /= Null_Iir then
                  Src := Create_Shadow_Signal (Local_Expr);
                  Add_Conversion
                    (new Convert_Instance_Type'
                       (Mode => Convert_Out,
                        Instance => Local_Instance,
                        Func => Out_Conv,
                        Src => Formal_Expr,
                        Dst => Src));
               else
                  Src := Formal_Expr;
               end if;
               --  LRM93 12.6.2
               --  A signal is said to be active [...] if one of its source
               --  is active.
               Connect (Local_Expr, Src, Connect_Source);
            end;

         when Iir_In_Mode =>
            null;
         when Iir_Unknown_Mode =>
            raise Internal_Error;
      end case;

      case Get_Mode (Inter) is
         when Iir_In_Mode
           | Iir_Inout_Mode
           | Iir_Buffer_Mode
           | Iir_Linkage_Mode =>
            declare
               In_Conv : constant Iir := Get_Actual_Conversion (Assoc);
               Src : Iir_Value_Literal_Acc;
            begin
               if In_Conv /= Null_Iir then
                  Src := Create_Shadow_Signal (Formal_Expr);
                  Add_Conversion
                    (new Convert_Instance_Type'
                       (Mode => Convert_In,
                        Instance => Local_Instance,
                        Func => In_Conv,
                        Src => Local_Expr,
                        Dst => Src));
               else
                  Src := Local_Expr;
               end if;
               Connect (Src, Formal_Expr, Connect_Effective);
            end;
         when Iir_Out_Mode =>
            null;
         when Iir_Unknown_Mode =>
            raise Internal_Error;
      end case;
   end Set_Connect;

   procedure Create_Connects is
   begin
      --  New signals may be created (because of conversions).
      Instance_Pool := Global_Pool'Access;

      for I in Connect_Table.First .. Connect_Table.Last loop
         declare
            E : Connect_Entry renames Connect_Table.Table (I);
         begin
            Set_Connect (E.Formal_Instance, E.Formal,
                         E.Actual_Instance, E.Actual,
                         E.Inter, E.Assoc);
         end;
      end loop;

      Instance_Pool := null;
   end Create_Connects;

   procedure Set_Disconnection (Val : Iir_Value_Literal_Acc;
                                Time : Iir_Value_Time) is
   begin
      case Val.Kind is
         when Iir_Value_Signal =>
            Grt.Signals.Ghdl_Signal_Set_Disconnect (Val.Sig, Std_Time (Time));
         when Iir_Value_Record =>
            for I in Val.Val_Record.V'Range loop
               Set_Disconnection (Val.Val_Record.V (I), Time);
            end loop;
         when Iir_Value_Array =>
            for I in Val.Val_Array.V'Range loop
               Set_Disconnection (Val.Val_Array.V (I), Time);
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Set_Disconnection;

   procedure Create_Disconnections is
   begin
      for I in Disconnection_Table.First .. Disconnection_Table.Last loop
         declare
            E : Disconnection_Entry renames Disconnection_Table.Table (I);
         begin
            Set_Disconnection (E.Sig, E.Time);
         end;
      end loop;
   end Create_Disconnections;

   procedure Create_Guard_Signal (Instance : Block_Instance_Acc;
                                  Sig_Guard : Iir_Value_Literal_Acc;
                                  Val_Guard : Iir_Value_Literal_Acc;
                                  Guard : Iir)
   is
      procedure Add_Guard_Sensitivity (Sig : Iir_Value_Literal_Acc) is
      begin
         case Sig.Kind is
            when Iir_Value_Signal =>
               Grt.Signals.Ghdl_Signal_Guard_Dependence (Sig.Sig);
            when Iir_Value_Array =>
               for I in Sig.Val_Array.V'Range loop
                  Add_Guard_Sensitivity (Sig.Val_Array.V (I));
               end loop;
            when Iir_Value_Record =>
               for I in Sig.Val_Record.V'Range loop
                  Add_Guard_Sensitivity (Sig.Val_Record.V (I));
               end loop;
            when others =>
               raise Internal_Error;
         end case;
      end Add_Guard_Sensitivity;

      Dep_List : Iir_List;
      Dep_It : List_Iterator;
      Dep : Iir;
      Data : Guard_Instance_Acc;
   begin
      Data := new Guard_Instance_Type'(Instance => Instance,
                                       Guard => Guard);
      Sig_Guard.Sig := Grt.Signals.Ghdl_Signal_Create_Guard
        (To_Ghdl_Value_Ptr (Val_Guard.B1'Address),
         Data.all'Address, Guard_Func'Access);
      Dep_List := Get_Guard_Sensitivity_List (Guard);
      Dep_It := List_Iterate (Dep_List);
      while Is_Valid (Dep_It) loop
         Dep := Get_Element (Dep_It);
         Add_Guard_Sensitivity (Execute_Name (Instance, Dep, True));
         Next (Dep_It);
      end loop;

      --  FIXME: free mem
   end Create_Guard_Signal;

   procedure Create_Implicit_Signal (Sig : Iir_Value_Literal_Acc;
                                     Val : Iir_Value_Literal_Acc;
                                     Time : Std_Time;
                                     Prefix : Iir_Value_Literal_Acc;
                                     Kind : Mode_Signal_Type)
   is
      procedure Register_Prefix (Pfx : Iir_Value_Literal_Acc) is
      begin
         case Pfx.Kind is
            when Iir_Value_Signal =>
               Grt.Signals.Ghdl_Signal_Attribute_Register_Prefix (Pfx.Sig);
            when Iir_Value_Array =>
               for I in Pfx.Val_Array.V'Range loop
                  Register_Prefix (Pfx.Val_Array.V (I));
               end loop;
            when Iir_Value_Record =>
               for I in Pfx.Val_Record.V'Range loop
                  Register_Prefix (Pfx.Val_Record.V (I));
               end loop;
            when others =>
               raise Internal_Error;
         end case;
      end Register_Prefix;
   begin
      case Kind is
         when Mode_Stable =>
            Sig.Sig := Grt.Signals.Ghdl_Create_Stable_Signal
              (To_Ghdl_Value_Ptr (Val.B1'Address), Time);
         when Mode_Quiet =>
            Sig.Sig := Grt.Signals.Ghdl_Create_Quiet_Signal
              (To_Ghdl_Value_Ptr (Val.B1'Address), Time);
         when Mode_Transaction =>
            Sig.Sig := Grt.Signals.Ghdl_Create_Transaction_Signal
              (To_Ghdl_Value_Ptr (Val.B1'Address));
         when others =>
            raise Internal_Error;
      end case;
      Register_Prefix (Prefix);
   end Create_Implicit_Signal;

   procedure Create_Delayed_Signal (Sig : Iir_Value_Literal_Acc;
                                    Val : Iir_Value_Literal_Acc;
                                    Pfx : Iir_Value_Literal_Acc;
                                    Time : Std_Time)
   is
      Val_Ptr : Ghdl_Value_Ptr;
   begin
      case Pfx.Kind is
         when Iir_Value_Array =>
            for I in Sig.Val_Array.V'Range loop
               Create_Delayed_Signal
                 (Sig.Val_Array.V (I), Val.Val_Array.V (I),
                  Pfx.Val_Array.V (I), Time);
               end loop;
         when Iir_Value_Record =>
            for I in Pfx.Val_Record.V'Range loop
               Create_Delayed_Signal
                 (Sig.Val_Record.V (I), Val.Val_Record.V (I),
                  Pfx.Val_Array.V (I), Time);
            end loop;
         when Iir_Value_Signal =>
            case Iir_Value_Scalars (Val.Kind) is
               when Iir_Value_I64 =>
                  Val_Ptr := To_Ghdl_Value_Ptr (Val.I64'Address);
               when Iir_Value_E32 =>
                  Val_Ptr := To_Ghdl_Value_Ptr (Val.E32'Address);
               when Iir_Value_F64 =>
                  Val_Ptr := To_Ghdl_Value_Ptr (Val.F64'Address);
               when Iir_Value_B1 =>
                  Val_Ptr := To_Ghdl_Value_Ptr (Val.B1'Address);
               when Iir_Value_E8 =>
                  Val_Ptr := To_Ghdl_Value_Ptr (Val.E8'Address);
            end case;
            Sig.Sig := Grt.Signals.Ghdl_Create_Delayed_Signal
              (Pfx.Sig, Val_Ptr, Time);
         when others =>
            raise Internal_Error;
      end case;
   end Create_Delayed_Signal;

   -- Create a new signal, using DEFAULT as initial value.
   -- Set its number.
   procedure Create_User_Signal (Block: Block_Instance_Acc;
                                 Mode : Mode_Signal_Type;
                                 Signal: Iir;
                                 Sig : Iir_Value_Literal_Acc;
                                 Val : Iir_Value_Literal_Acc)
   is
      use Grt.Signals;

      procedure Create_Signal (Val : Iir_Value_Literal_Acc;
                               Sig : Iir_Value_Literal_Acc;
                               Sig_Type: Iir;
                               Already_Resolved : Boolean)
      is
         Sub_Resolved : Boolean := Already_Resolved;
         Resolv_Func : Iir;
         Resolv_Instance : Resolv_Instance_Acc;
      begin
         if not Already_Resolved
           and then Get_Kind (Sig_Type) in Iir_Kinds_Subtype_Definition
         then
            Resolv_Func := Get_Resolution_Indication (Sig_Type);
         else
            Resolv_Func := Null_Iir;
         end if;
         if Resolv_Func /= Null_Iir then
            Sub_Resolved := True;
            Resolv_Instance := new Resolv_Instance_Type'
              (Func => Get_Named_Entity (Resolv_Func),
               Block => Block,
               Sig => Sig);
            Grt.Signals.Ghdl_Signal_Create_Resolution
              (Resolution_Proc'Access,
               Resolv_Instance.all'Address,
               System.Null_Address,
               Ghdl_Index_Type (Get_Nbr_Of_Scalars (Val)));
         end if;
         case Val.Kind is
            when Iir_Value_Array =>
               declare
                  Sig_El_Type : constant Iir :=
                    Get_Element_Subtype (Get_Base_Type (Sig_Type));
               begin
                  for I in Val.Val_Array.V'Range loop
                     Create_Signal (Val.Val_Array.V (I), Sig.Val_Array.V (I),
                                    Sig_El_Type, Sub_Resolved);
                  end loop;
               end;
            when Iir_Value_Record =>
               declare
                  List : constant Iir_Flist := Get_Elements_Declaration_List
                    (Get_Base_Type (Sig_Type));
                  El : Iir_Element_Declaration;
               begin
                  for I in Val.Val_Record.V'Range loop
                     El := Get_Nth_Element (List, Natural (I - 1));
                     Create_Signal (Val.Val_Record.V (I), Sig.Val_Record.V (I),
                                    Get_Type (El), Sub_Resolved);
                  end loop;
               end;

            when Iir_Value_I64 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_I64
                 (To_Ghdl_Value_Ptr (Val.I64'Address),
                  null, System.Null_Address);
            when Iir_Value_B1 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_B1
                 (To_Ghdl_Value_Ptr (Val.B1'Address),
                  null, System.Null_Address);
            when Iir_Value_E8 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_E8
                 (To_Ghdl_Value_Ptr (Val.E8'Address),
                  null, System.Null_Address);
            when Iir_Value_E32 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_E32
                 (To_Ghdl_Value_Ptr (Val.E32'Address),
                  null, System.Null_Address);
            when Iir_Value_F64 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_F64
                 (To_Ghdl_Value_Ptr (Val.F64'Address),
                  null, System.Null_Address);

            when Iir_Value_Signal
              | Iir_Value_Range
              | Iir_Value_File
              | Iir_Value_Access
              | Iir_Value_Protected
              | Iir_Value_Quantity
              | Iir_Value_Terminal
              | Iir_Value_Instance =>
               raise Internal_Error;
         end case;
      end Create_Signal;

      Sig_Type: constant Iir := Get_Type (Signal);
      Kind : Kind_Signal_Type;

      type Iir_Kind_To_Kind_Signal_Type is
        array (Iir_Signal_Kind) of Kind_Signal_Type;
      Iir_Kind_To_Kind_Signal : constant Iir_Kind_To_Kind_Signal_Type :=
        (Iir_Register_Kind  => Kind_Signal_Register,
         Iir_Bus_Kind       => Kind_Signal_Bus);
   begin
      if Get_Guarded_Signal_Flag (Signal) then
         Kind := Iir_Kind_To_Kind_Signal (Get_Signal_Kind (Signal));
      else
         Kind := Kind_Signal_No;
      end if;

      Grt.Signals.Ghdl_Signal_Set_Mode (Mode, Kind, True);

      Create_Signal (Val, Sig, Sig_Type, False);
   end Create_User_Signal;

   procedure Create_Signals is
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            E : Signal_Entry renames Signals_Table.Table (I);
         begin
            case E.Kind is
               when Mode_Guard =>
                  Create_Guard_Signal (E.Instance, E.Sig, E.Val, E.Decl);
               when Mode_Stable | Mode_Quiet | Mode_Transaction =>
                  Create_Implicit_Signal
                    (E.Sig, E.Val, E.Time, E.Prefix, E.Kind);
               when Mode_Delayed =>
                  Create_Delayed_Signal (E.Sig, E.Val, E.Prefix, E.Time);
               when Mode_Signal_User =>
                  Create_User_Signal
                    (E.Instance, E.Kind, E.Decl, E.Sig, E.Val);
               when Mode_Conv_In | Mode_Conv_Out | Mode_End =>
                  raise Internal_Error;
            end case;
         end;
      end loop;
   end Create_Signals;

   procedure Ghdl_Elaborate is
   begin
      Elaboration.Elaborate_Design (Top_Config);

      if Disp_Stats then
         Disp_Design_Stats;
      end if;

      if Disp_Ams then
         Debugger.AMS.Disp_Characteristic_Expressions;
      end if;

      -- There is no inputs.
      -- All the simulation is done via time, so it must be displayed.
      Disp_Time_Before_Values := True;

      -- Initialisation.
      if Trace_Simulation then
         Put_Line ("Initialisation:");
      end if;

      Create_Signals;
      Create_Connects;
      Create_Disconnections;
      Create_Processes;
      Create_PSL;

      if Disp_Tree then
         Debugger.Disp_Instances_Tree;
      end if;

      if Flag_Interractive then
         Debug (Reason_Elab);
      end if;
   end Ghdl_Elaborate;

   procedure Simulation_Entity (Top_Conf : Iir_Design_Unit)
   is
      use Grt.Errors;
      Ok : C_Boolean;
      Status : Integer;
   begin
      Break_Time := Std_Time'Last;

      Top_Config := Top_Conf;

      Grt.Errors.Error_Hook := Debug_Error'Access;

      if Flag_Interractive then
         Debug (Reason_Start);
      end if;

      Ok := Grt.Main.Run_Elab;
      if not Ok then
         return;
      end if;

      Status := Grt.Main.Run_Through_Longjump
        (Grt.Processes.Simulation_Init'Access);

      if Status = 0 then
         loop
            Status := Grt.Main.Run_Through_Longjump
              (Grt.Processes.Simulation_Cycle'Access);
            exit when Status < 0 or Status = Run_Stop or Status = Run_Finished;

            if Grt.Processes.Next_Time >= Break_Time
              and then Break_Time /= Std_Time'Last
            then
               Debug (Reason_Time);
            end if;

            exit when Grt.Processes.Has_Simulation_Timeout;
         end loop;
      end if;

      Grt.Processes.Simulation_Finish;

      Grt.Main.Run_Finish (Status);
   exception
      when Debugger_Quit =>
         null;
      when Simulation_Finished =>
         null;
   end Simulation_Entity;
end Simul.Simulation.Main;
