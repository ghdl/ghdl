--  Simulation of VHDL
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

with System;
with Ada.Unchecked_Conversion;

with Simple_IO;
with Utils_IO;

with Vhdl.Errors;
with Vhdl.Sem_Inst;
with Vhdl.Canon;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Decls;
with Elab.Debugger;

with Trans_Analyzes;

with Synth.Errors;
with Synth.Vhdl_Stmts; use Synth.Vhdl_Stmts;
with Synth.Vhdl_Expr;
with Synth.Vhdl_Oper;
with Synth.Vhdl_Static_Proc;
with Synth.Flags;
with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;

with Grt.Types; use Grt.Types;
with Grt.Signals; use Grt.Signals;
with Grt.Options;
with Grt.Stdio;
with Grt.Processes;
with Grt.Main;
with Grt.Errors;
with Grt.Lib;
with Grt.Analog_Solver;

package body Simul.Vhdl_Simul is
   function To_Instance_Acc is new Ada.Unchecked_Conversion
     (System.Address, Grt.Processes.Instance_Acc);

   procedure Process_Executer (Self : Grt.Processes.Instance_Acc);
   pragma Convention (C, Process_Executer);

   type Ghdl_Signal_Ptr_Ptr is access all Ghdl_Signal_Ptr;
   function To_Ghdl_Signal_Ptr_Ptr is
      new Ada.Unchecked_Conversion (Memory_Ptr, Ghdl_Signal_Ptr_Ptr);

   Sig_Size : constant Size_Type := Ghdl_Signal_Ptr'Size / 8;

   subtype F64_C_Arr_Ptr is Grt.Analog_Solver.F64_C_Arr_Ptr;

   procedure Residues (T : Ghdl_F64;
                       Y : F64_C_Arr_Ptr;
                       Yp : F64_C_Arr_Ptr;
                       Res : F64_C_Arr_Ptr);
   pragma Export (C, Residues, "grt__analog_solver__residues");

   procedure Set_Quantities_Values (Y : F64_C_Arr_Ptr; Yp: F64_C_Arr_Ptr);
   pragma Export (C, Set_Quantities_Values, "grt__analog_solver__set_values");

   function Sig_Index (Base : Memory_Ptr; Idx : Uns32) return Memory_Ptr is
   begin
      return Base + Size_Type (Idx) * Sig_Size;
   end Sig_Index;

   procedure Write_Sig (Mem : Memory_Ptr; Val : Ghdl_Signal_Ptr) is
   begin
      To_Ghdl_Signal_Ptr_Ptr (Mem).all := Val;
   end Write_Sig;

   function Read_Sig (Mem : Memory_Ptr) return Ghdl_Signal_Ptr is
   begin
      return To_Ghdl_Signal_Ptr_Ptr (Mem).all;
   end Read_Sig;

   function Exec_Sig_Sig (Val : Value_Acc) return Memory_Ptr
   is
      E : Signal_Entry renames Signals_Table.Table (Val.S);
   begin
      return E.Sig;
   end Exec_Sig_Sig;

   function Hook_Signal_Expr (Val : Valtyp) return Valtyp is
   begin
      if Val.Val.Kind = Value_Alias then
         declare
            E : Signal_Entry renames Signals_Table.Table (Val.Val.A_Obj.S);
         begin
            return Create_Value_Memtyp
              ((Val.Typ, E.Val + Val.Val.A_Off.Mem_Off));
         end;
      else
         declare
            E : Signal_Entry renames Signals_Table.Table (Val.Val.S);
         begin
            return Create_Value_Memtyp ((E.Typ, E.Val));
         end;
      end if;
   end Hook_Signal_Expr;

   function Hook_Quantity_Expr (Val : Valtyp) return Valtyp is
   begin
      if Val.Val.Kind = Value_Alias then
         declare
            E : Quantity_Entry renames Quantity_Table.Table (Val.Val.A_Obj.Q);
         begin
            return Create_Value_Memtyp
              ((Val.Typ, E.Val + Val.Val.A_Off.Mem_Off));
         end;
      else
         declare
            E : Quantity_Entry renames Quantity_Table.Table (Val.Val.Q);
         begin
            return Create_Value_Memtyp ((E.Typ, E.Val));
         end;
      end if;
   end Hook_Quantity_Expr;

   procedure Disp_Iir_Location (N : Iir)
   is
      use Simple_IO;
   begin
      if N = Null_Iir then
         Put_Err ("??:??:??");
      else
         Put_Err (Vhdl.Errors.Disp_Location (N));
      end if;
      Put_Err (": ");
   end Disp_Iir_Location;


   procedure Error_Msg_Exec (Loc : Iir; Msg : String)
   is
      use Simple_IO;
   begin
      Disp_Iir_Location (Loc);
      Put_Line_Err (Msg);
      Grt.Errors.Fatal_Error;
   end Error_Msg_Exec;

   procedure Start_Assign_Value_To_Signal (Target: Memtyp;
                                           Rej : Std_Time;
                                           After : Std_Time;
                                           Val : Memtyp) is
   begin
      case Target.Typ.Kind is
         when Type_Logic
           | Type_Bit =>
            Ghdl_Signal_Start_Assign_E8
              (Read_Sig (Target.Mem), Rej, Read_U8 (Val), After);
         when Type_Discrete =>
            if Target.Typ.Sz = 1 then
               Ghdl_Signal_Start_Assign_E8
                 (Read_Sig (Target.Mem), Rej, Read_U8 (Val), After);
            elsif Target.Typ.Sz = 4 then
               Ghdl_Signal_Start_Assign_I32
                 (Read_Sig (Target.Mem), Rej, Read_I32 (Val.Mem), After);
            elsif Target.Typ.Sz = 8 then
               Ghdl_Signal_Start_Assign_I64
                 (Read_Sig (Target.Mem), Rej, Read_I64 (Val.Mem), After);
            else
               raise Internal_Error;
            end if;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Target.Typ.Abound.Len;
               El : constant Type_Acc := Target.Typ.Arr_El;
            begin
               pragma Assert (Val.Typ.Abound.Len = Len);
               for I in 1 .. Len loop
                  Start_Assign_Value_To_Signal
                    ((El, Sig_Index (Target.Mem, (Len - I) * El.W)),
                     Rej, After,
                     (Val.Typ.Arr_El, Val.Mem + Size_Type (I - 1) * El.Sz));
               end loop;
            end;
         when Type_Record =>
            for I in Val.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Val.Typ.Rec.E (I);
               begin
                  Start_Assign_Value_To_Signal
                    ((E.Typ, Sig_Index (Target.Mem, E.Offs.Net_Off)),
                     Rej, After,
                     (E.Typ, Val.Mem + E.Offs.Mem_Off));
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Start_Assign_Value_To_Signal;

   procedure Add_Source (Typ : Type_Acc; Sig : Memory_Ptr; Val : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Logic
           | Type_Bit =>
            Grt.Signals.Ghdl_Signal_Add_Port_Driver_E8
              (Read_Sig (Sig), Read_U8 (Val));
         when Type_Discrete =>
            if Typ.Sz = 1 then
               Grt.Signals.Ghdl_Signal_Add_Port_Driver_E8
                 (Read_Sig (Sig), Read_U8 (Val));
            elsif Typ.Sz = 4 then
               Grt.Signals.Ghdl_Signal_Add_Port_Driver_I32
                 (Read_Sig (Sig), Read_I32 (Val));
            elsif Typ.Sz = 8 then
               Grt.Signals.Ghdl_Signal_Add_Port_Driver_I64
                 (Read_Sig (Sig), Read_I64 (Val));
            else
               raise Internal_Error;
            end if;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Add_Source (Typ.Arr_El,
                              Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                              Val + Size_Type (I - 1) * Typ.Arr_El.Sz);
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Add_Source (Typ.Rec.E (I).Typ,
                           Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off),
                           Val + Typ.Rec.E (I).Offs.Mem_Off);
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Add_Source;

   procedure Create_Process_Drivers (Inst : Synth_Instance_Acc;
                                     Proc : Node;
                                     Driver_List : Iir_List)
   is
      pragma Unreferenced (Proc);
      It : List_Iterator;
      El: Iir;
      Info : Target_Info;
   begin
      -- Some processes have no driver list (assertion).
      It := List_Iterate_Safe (Driver_List);
      while Is_Valid (It) loop
         El := Get_Element (It);

         -- Mark (Marker, Expr_Pool);
         Info := Synth_Target (Inst, El);
         declare
            E : Signal_Entry renames Signals_Table.Table (Info.Obj.Val.S);
         begin
            Add_Source (Info.Targ_Type,
                        Sig_Index (E.Sig, Info.Off.Net_Off),
                        E.Val + Info.Off.Mem_Off);
         end;

         --  Release (Marker, Expr_Pool);

         Next (It);
      end loop;
   end Create_Process_Drivers;

   procedure Create_Process_Drivers (Proc : Process_Index_Type)
   is
      Drv : Driver_Index_Type;
   begin
      Drv := Processes_Table.Table (Proc).Drivers;
      while Drv /= No_Driver_Index loop
         declare
            D : Driver_Entry renames Drivers_Table.Table (Drv);
            S : Signal_Entry renames Signals_Table.Table (D.Sig);
         begin
            pragma Assert (D.Off = No_Value_Offsets);
            Add_Source (S.Typ, S.Sig, S.Val);

            Drv := D.Prev_Proc;
         end;
      end loop;
   end Create_Process_Drivers;

   function Exec_Event_Attribute (Sig : Memtyp) return Boolean is
   begin
      case Sig.Typ.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete =>
            return Read_Sig (Sig.Mem).Event;
         when others =>
            raise Internal_Error;
            return False;
      end case;
   end Exec_Event_Attribute;

   function Exec_Event_Attribute (Inst : Synth_Instance_Acc;
                                  Expr : Node) return Valtyp
   is
      Res : Valtyp;
      Pfx : Target_Info;
      E : Boolean;
   begin
      Pfx := Synth_Target (Inst, Get_Prefix (Expr));
      pragma Assert (Pfx.Kind = Target_Simple);
      --  TODO: alias.
      pragma Assert (Pfx.Obj.Val /= null
                       and then Pfx.Obj.Val.Kind = Value_Signal);
      E := Exec_Event_Attribute
        ((Pfx.Targ_Type,
          Sig_Index (Signals_Table.Table (Pfx.Obj.Val.S).Sig,
                     Pfx.Off.Net_Off)));
      Res := Create_Value_Memory (Boolean_Type);
      Write_U8 (Res.Val.Mem, Boolean'Pos (E));
      return Res;
   end Exec_Event_Attribute;

   function Exec_Dot_Attribute (Inst : Synth_Instance_Acc;
                                Expr : Node) return Valtyp
   is
      Pfx : Target_Info;
   begin
      Pfx := Synth_Target (Inst, Expr);
      pragma Assert (Pfx.Kind = Target_Simple);
      --  TODO: alias.
      pragma Assert (Pfx.Obj.Val /= null
                       and then Pfx.Obj.Val.Kind = Value_Quantity);
      return Hook_Quantity_Expr (Pfx.Obj);
   end Exec_Dot_Attribute;

   procedure Execute_Sequential_Statements (Process : Process_State_Acc);

   function Execute_Condition (Inst : Synth_Instance_Acc;
                               Cond : Node) return Boolean
   is
      Cond_Val : Valtyp;
   begin
      if Cond = Null_Node then
         return True;
      end if;
      Cond_Val := Synth.Vhdl_Expr.Synth_Expression (Inst, Cond);
      return Read_Discrete (Cond_Val) = 1;
   end Execute_Condition;

   function Get_Suspend_State_Var (Inst : Synth_Instance_Acc) return Memory_Ptr
   is
      Src : Node;
      Var : Node;
      State_Mem : Memory_Ptr;
   begin
      Src := Get_Source_Scope (Inst);
      Var := Get_Declaration_Chain (Src);
      pragma Assert (Var /= Null_Node);
      pragma Assert (Get_Kind (Var) = Iir_Kind_Suspend_State_Declaration);
      State_Mem := Get_Value (Inst, Var).Val.Mem;
      return State_Mem;
   end Get_Suspend_State_Var;

   --  Return the statement STMT corresponding to the current state from INST.
   procedure Get_Suspend_State_Statement
     (Inst : Synth_Instance_Acc; Stmt : out Node; Resume : out Boolean)
   is
      Src : Node;
      Var : Node;
      State_Mem : Memory_Ptr;
      State : Int32;
   begin
      State_Mem := Get_Suspend_State_Var (Inst);
      State := Int32 (Read_I32 (State_Mem));
      Src := Get_Source_Scope (Inst);
      if State = 0 then
         Stmt := Get_Sequential_Statement_Chain (Src);
         Resume := False;
      else
         Var := Get_Declaration_Chain (Src);
         Stmt := Get_Suspend_State_Chain (Var);
         loop
            pragma Assert (Stmt /= Null_Node);
            exit when Get_Suspend_State_Index (Stmt) = State;
            Stmt := Get_Suspend_State_Chain (Stmt);
         end loop;
         Resume := True;
      end if;
   end Get_Suspend_State_Statement;

   procedure Finish_Procedure_Call (Process : Process_State_Acc;
                                    Bod : Node;
                                    Stmt : out Node)
   is
      Imp : constant Node := Get_Subprogram_Specification (Bod);
      Caller_Inst : constant Synth_Instance_Acc :=
        Get_Caller_Instance (Process.Instance);
      Resume : Boolean;
   begin
      if not Get_Suspend_Flag (Bod) then
         Process.Instance := Caller_Inst;
         --  TODO: free old inst.
         Stmt := Null_Node;
         return;
      end if;
      Get_Suspend_State_Statement (Caller_Inst, Stmt, Resume);
      pragma Assert (Resume);
      --  Skip the resume statement.
      Stmt := Get_Chain (Stmt);
      pragma Assert (Get_Kind (Stmt) = Iir_Kind_Procedure_Call_Statement);
      Synth_Subprogram_Back_Association
        (Process.Instance, Caller_Inst,
         Get_Interface_Declaration_Chain (Imp),
         Get_Parameter_Association_Chain
           (Get_Procedure_Call (Stmt)));
      Process.Instance := Caller_Inst;
      --  TODO: free old inst.
   end Finish_Procedure_Call;

   procedure Next_Parent_Statement (Process : Process_State_Acc;
                                    First_Parent : Node;
                                    Stmt : out Node)
   is
      N_Stmt : Node;
      Parent : Node;
   begin
      Parent := First_Parent;
      loop
         case Get_Kind (Parent) is
            when Iir_Kind_Sensitized_Process_Statement =>
               Stmt := Null_Node;
               return;
            when Iir_Kind_Process_Statement =>
               Stmt := Get_Sequential_Statement_Chain (Parent);
               return;
            when Iir_Kind_If_Statement
              | Iir_Kind_Case_Statement =>
               Stmt := Parent;
            when Iir_Kind_For_Loop_Statement =>
               declare
                  Param : constant Node :=
                    Get_Parameter_Specification (Parent);
                  Val : Valtyp;
               begin
                  --  Update index
                  Val := Get_Value (Process.Instance, Param);
                  Update_Index (Val.Typ.Drange, Val);

                  --  Test.
                  if Elab.Vhdl_Objtypes.In_Range (Val.Typ.Drange,
                                                  Read_Discrete (Val))
                  then
                     Stmt := Get_Sequential_Statement_Chain (Parent);
                     return;
                  end if;

                  --  End of loop.
                  Synth.Vhdl_Stmts.Finish_For_Loop_Statement
                    (Process.Instance, Parent);
                  Stmt := Parent;
               end;
            when Iir_Kind_While_Loop_Statement =>
               if Execute_Condition (Process.Instance, Get_Condition (Parent))
               then
                  Stmt := Get_Sequential_Statement_Chain (Parent);
                  return;
               else
                  Stmt := Parent;
               end if;
            when Iir_Kind_Procedure_Body =>
               Finish_Procedure_Call (Process, Parent, Stmt);
               exit when Stmt = Null_Node;
            when others =>
               Vhdl.Errors.Error_Kind ("next_statement", Parent);
         end case;

         N_Stmt := Get_Chain (Stmt);
         if N_Stmt /= Null_Node then
            Stmt := N_Stmt;
            return;
         end if;

         Parent := Get_Parent (Stmt);
      end loop;
   end Next_Parent_Statement;

   procedure Next_Statement (Process : Process_State_Acc;
                             Stmt : in out Node)
   is
      N_Stmt : Node;
   begin
      N_Stmt := Get_Chain (Stmt);
      if N_Stmt /= Null_Node then
         Stmt := N_Stmt;
         return;
      end if;

      Next_Parent_Statement (Process, Get_Parent (Stmt), Stmt);
   end Next_Statement;

   procedure Add_Wait_Sensitivity (Typ : Type_Acc; Sig : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete =>
            Grt.Processes.Ghdl_Process_Wait_Add_Sensitivity (Read_Sig (Sig));
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Add_Wait_Sensitivity
                    (Typ.Arr_El, Sig_Index (Sig, (Len - I) * Typ.Arr_El.W));
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Add_Wait_Sensitivity
                 (Typ.Rec.E (I).Typ,
                  Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Add_Wait_Sensitivity;

   procedure Execute_Wait_Statement (Inst : Synth_Instance_Acc;
                                     Stmt : Node)
   is
      Expr : Node;
      List : Node_List;
      Val : Valtyp;
      Timeout : Int64;
   begin
      --  LRM93 8.1
      --  The execution of a wait statement causes the time expression to
      --  be evaluated to determine the timeout interval.
      Expr := Get_Timeout_Clause (Stmt);
      if Expr /= Null_Node then
         Val := Synth.Vhdl_Expr.Synth_Expression (Inst, Expr);
         Timeout := Read_Discrete (Val);
         if Timeout < 0 then
            Error_Msg_Exec (Stmt, "negative timeout value");
         end if;
         Grt.Processes.Ghdl_Process_Wait_Set_Timeout
           (Std_Time (Timeout), null, 0);
      end if;

      List := Get_Sensitivity_List (Stmt);

      Expr := Get_Condition_Clause (Stmt);
      if Expr /= Null_Node and then List = Null_Iir_List then
         List := Create_Iir_List;
         Vhdl.Canon.Canon_Extract_Sensitivity_Expression (Expr, List);
         Set_Sensitivity_List (Stmt, List);
         Set_Is_Ref (Stmt, True);
      end if;

      if List /= Null_Iir_List then
         declare
            It : List_Iterator;
            El : Node;
            Info : Target_Info;
            Sig : Memory_Ptr;
         begin
            It := List_Iterate (List);
            while Is_Valid (It) loop
               El := Get_Element (It);
               Info := Synth_Target (Inst, El);
               Sig := Signals_Table.Table (Info.Obj.Val.S).Sig;
               Add_Wait_Sensitivity
                 (Info.Targ_Type, Sig_Index (Sig, Info.Off.Net_Off));
               Next (It);
            end loop;
         end;
      end if;

      --  LRM93 8.1
      --  It also causes the execution of the corresponding process
      --  statement to be suspended.
      Grt.Processes.Ghdl_Process_Wait_Suspend;
   end Execute_Wait_Statement;

   function Resume_Wait_Statement (Inst : Synth_Instance_Acc;
                                   Stmt : Node) return Boolean is
   begin
      --  LRM93 8.1
      --  The suspended process will resume, at the latest, immediately
      --  after the timeout interval has expired.
      if not Grt.Processes.Ghdl_Process_Wait_Timed_Out then
         --  Compute the condition clause only if the timeout has not
         --  expired.

         --  LRM93 8.1
         --  If such an event occurs, the condition in the condition clause
         --  is evaluated.
         --
         --  if no condition clause appears, the condition clause until true
         --  is assumed.
         if not Execute_Condition (Inst, Get_Condition_Clause (Stmt)) then
            --  LRM93 8.1
            --  If the value of the condition is FALSE, the process will
            --  re-suspend.
            --  Such re-suspension does not involve the recalculation of
            --  the timeout interval.
            Grt.Processes.Ghdl_Process_Wait_Suspend;
            return True;
         end if;
      end if;

      -- LRM93 8.1
      --   If the value of the condition is TRUE, the process will resume.
      -- next statement.
      Grt.Processes.Ghdl_Process_Wait_Close;

      return False;
   end Resume_Wait_Statement;

   procedure Execute_Procedure_Call_Statement (Process : Process_State_Acc;
                                               Stmt : Node;
                                               Next_Stmt : out Node)
   is
      use Vhdl.Errors;
      Inst : constant Synth_Instance_Acc := Process.Instance;
      Call : constant Node := Get_Procedure_Call (Stmt);
      Imp  : constant Node := Get_Implementation (Call);

      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);

      Area_Mark : Areapools.Mark_Type;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Areapools.Mark (Area_Mark, Instance_Pool.all);

      if Get_Implicit_Definition (Imp) /= Iir_Predefined_None then
         declare
            Inter_Chain : constant Node :=
              Get_Interface_Declaration_Chain (Imp);
         begin
            Sub_Inst := Synth_Subprogram_Call_Instance (Inst, Imp, Imp);
            Synth_Subprogram_Association
              (Sub_Inst, Inst, Inter_Chain, Assoc_Chain);

            Synth.Vhdl_Static_Proc.Synth_Static_Procedure
              (Sub_Inst, Imp, Call);
            Synth_Subprogram_Back_Association
              (Sub_Inst, Inst, Inter_Chain, Assoc_Chain);

            Next_Stmt := Null_Node;
         end;
      else
         declare
            Bod : constant Node :=
              Vhdl.Sem_Inst.Get_Subprogram_Body_Origin (Imp);
            Inter_Chain : constant Node :=
              Get_Interface_Declaration_Chain (Imp);
         begin
            if Get_Foreign_Flag (Imp) then
               Synth.Errors.Error_Msg_Synth
                 (+Stmt, "call to foreign %n is not supported", +Imp);
               Next_Stmt := Null_Node;
               return;
            end if;

            Sub_Inst := Synth_Subprogram_Call_Instance (Inst, Imp, Bod);
            --  Note: in fact the uninstantiated scope is the instantiated
            --  one!
            Set_Uninstantiated_Scope (Sub_Inst, Imp);
            Synth_Subprogram_Association
              (Sub_Inst, Inst, Inter_Chain, Assoc_Chain);

            Process.Instance := Sub_Inst;
            Elab.Vhdl_Decls.Elab_Declarations
              (Sub_Inst, Get_Declaration_Chain (Bod), True);

            if Get_Suspend_Flag (Bod) then
               Next_Stmt := Get_Sequential_Statement_Chain (Bod);
               return;
               --  TODO: end of call.
            else
               Execute_Sequential_Statements (Process);
               Synth_Subprogram_Back_Association
                 (Sub_Inst, Inst, Inter_Chain, Assoc_Chain);
               Next_Stmt := Null_Node;
            end if;
         end;
      end if;

      if Elab.Debugger.Flag_Need_Debug then
         Elab.Debugger.Debug_Leave (Sub_Inst);
      end if;

      Free_Elab_Instance (Sub_Inst);
      Areapools.Release (Area_Mark, Instance_Pool.all);
   end Execute_Procedure_Call_Statement;

   procedure Execute_Signal_Assignment (Inst : Synth_Instance_Acc;
                                       Target : Target_Info;
                                       Val : Valtyp;
                                       Loc : Node);

   procedure Execute_Aggregate_Signal_Assignment is
      new Assign_Aggregate (Execute_Signal_Assignment);

   procedure Execute_Signal_Assignment (Inst : Synth_Instance_Acc;
                                        Target : Target_Info;
                                        Val : Valtyp;
                                        Loc : Node)
   is
      use Synth.Vhdl_Expr;
      V : Valtyp;
      Sig : Memtyp;
   begin
      V := Synth_Subtype_Conversion (Inst, Val, Target.Targ_Type, False, Loc);
      pragma Unreferenced (Val);

      case Target.Kind is
         when Target_Aggregate =>
            Execute_Aggregate_Signal_Assignment
              (Inst, Target.Aggr, Target.Targ_Type, V, Loc);

         when Target_Simple =>
            declare
               E : Signal_Entry renames Signals_Table.Table (Target.Obj.Val.S);
            begin
               Sig := (Target.Targ_Type,
                       Sig_Index (E.Sig, Target.Off.Net_Off));
            end;

            Start_Assign_Value_To_Signal (Sig, 0, 0, Get_Value_Memtyp (V));

         when Target_Memory =>
            raise Internal_Error;
      end case;
   end Execute_Signal_Assignment;

   procedure Execute_Waveform_Assignment (Inst : Synth_Instance_Acc;
                                          Target : Target_Info;
                                          Waveform : Node)
   is
      use Synth.Vhdl_Expr;
      Wf : Node;
      Val : Valtyp;
   begin
      Wf := Waveform;
      Val := Synth_Expression_With_Type
        (Inst, Get_We_Value (Wf), Target.Targ_Type);
      Execute_Signal_Assignment (Inst, Target, Val, Wf);
      Wf := Get_Chain (Wf);

      if Wf /= Null_Node then
         raise Internal_Error;
      end if;
   end Execute_Waveform_Assignment;

   procedure Execute_Simple_Signal_Assignment (Inst : Synth_Instance_Acc;
                                               Stmt : Node)
   is
      use Synth.Vhdl_Expr;
      Target : constant Node := Get_Target (Stmt);
      Info : Target_Info;
   begin
      Info := Synth_Target (Inst, Target);

      Execute_Waveform_Assignment (Inst, Info, Get_Waveform_Chain (Stmt));
   end Execute_Simple_Signal_Assignment;

   procedure Execute_Conditional_Signal_Assignment (Inst : Synth_Instance_Acc;
                                                    Stmt : Node)
   is
      use Synth.Vhdl_Expr;
      Target : constant Node := Get_Target (Stmt);
      Cw : Node;
      Cond : Node;
      Info : Target_Info;
   begin
      Info := Synth_Target (Inst, Target);

      Cw := Get_Conditional_Waveform_Chain (Stmt);
      while Cw /= Null_Node loop
         Cond := Get_Condition (Cw);
         if Cond = Null_Node
           or else Execute_Condition (Inst, Cond)
         then
            Execute_Waveform_Assignment
              (Inst, Info, Get_Waveform_Chain (Cw));
            exit;
         end if;
         Cw := Get_Chain (Cw);
      end loop;
   end Execute_Conditional_Signal_Assignment;

   procedure Execute_Selected_Signal_Assignment (Inst : Synth_Instance_Acc;
                                                 Stmt : Node)
   is
      use Synth.Vhdl_Expr;
      Target : constant Node := Get_Target (Stmt);
      Sel : Memtyp;
      Sw : Node;
      Wf : Node;
      Info : Target_Info;
      Eq : Boolean;
   begin
      Info := Synth_Target (Inst, Target);

      Sel := Get_Memtyp (Synth_Expression (Inst, Get_Expression (Stmt)));

      Sw := Get_Selected_Waveform_Chain (Stmt);
      while Sw /= Null_Node loop
         if not Get_Same_Alternative_Flag (Sw) then
            Wf := Get_Associated_Chain (Sw);
         else
            pragma Assert (Get_Associated_Chain (Sw) = Null_Node);
            null;
         end if;
         case Iir_Kinds_Choice (Get_Kind (Sw)) is
            when Iir_Kind_Choice_By_Expression =>
               declare
                  Ch : Valtyp;
               begin
                  Ch := Synth_Expression (Inst, Get_Choice_Expression (Sw));
                  Eq := Is_Equal (Sel, Get_Memtyp (Ch));
               end;
            when Iir_Kind_Choice_By_Others =>
               Eq := True;
            when others =>
               raise Internal_Error;
         end case;
         if Eq then
            Execute_Waveform_Assignment (Inst, Info, Wf);
            exit;
         end if;
         Sw := Get_Chain (Sw);
      end loop;
   end Execute_Selected_Signal_Assignment;

   procedure Execute_Sequential_Statements (Process : Process_State_Acc)
   is
      Inst : Synth_Instance_Acc;
      Src : Node;
      Stmt : Node;
      Resume : Boolean;
   begin
      Inst := Process.Instance;
      Src := Get_Source_Scope (Inst);
      if Get_Kind (Src) = Iir_Kind_Sensitized_Process_Statement
        or else (Get_Kind (Src) = Iir_Kind_Procedure_Body
                   and then not Get_Suspend_Flag (Src))
      then
         Stmt := Get_Sequential_Statement_Chain (Src);
         Resume := True;
      else
         Get_Suspend_State_Statement (Inst, Stmt, Resume);
      end if;

      loop
         Inst := Process.Instance;
         if Elab.Debugger.Flag_Need_Debug then
            Elab.Debugger.Debug_Break (Inst, Stmt);
         end if;

         case Get_Kind (Stmt) is
            when Iir_Kind_Null_Statement =>
               Next_Statement (Process, Stmt);

            when Iir_Kind_For_Loop_Statement =>
               declare
                  Val : Valtyp;
               begin
                  Synth.Vhdl_Stmts.Init_For_Loop_Statement (Inst, Stmt, Val);
                  if Elab.Vhdl_Objtypes.In_Range (Val.Typ.Drange,
                                                  Read_Discrete (Val))
                  then
                     Stmt := Get_Sequential_Statement_Chain (Stmt);
                  else
                     Synth.Vhdl_Stmts.Finish_For_Loop_Statement (Inst, Stmt);
                     Next_Statement (Process, Stmt);
                  end if;
               end;
            when Iir_Kind_While_Loop_Statement =>
               if Execute_Condition (Inst, Get_Condition (Stmt)) then
                  Stmt := Get_Sequential_Statement_Chain (Stmt);
               else
                  Next_Statement (Process, Stmt);
               end if;
            when Iir_Kind_Exit_Statement =>
               if Execute_Condition (Inst, Get_Condition (Stmt)) then
                  declare
                     Label : constant Node := Get_Loop_Label (Stmt);
                  begin
                     loop
                        Stmt := Get_Parent (Stmt);
                        case Get_Kind (Stmt) is
                           when Iir_Kind_For_Loop_Statement =>
                              --  Need to finalize for-loop statements.
                              Synth.Vhdl_Stmts.Finish_For_Loop_Statement
                                (Inst, Stmt);
                              exit when Label = Null_Node
                                or else Label = Stmt;
                           when Iir_Kind_While_Loop_Statement =>
                              exit when Label = Null_Node
                                or else Label = Stmt;
                           when others =>
                              null;
                        end case;
                     end loop;
                  end;
               end if;
               Next_Statement (Process, Stmt);
            when Iir_Kind_Next_Statement =>
               if Execute_Condition (Inst, Get_Condition (Stmt)) then
                  declare
                     Label : constant Node := Get_Loop_Label (Stmt);
                  begin
                     loop
                        Stmt := Get_Parent (Stmt);
                        case Get_Kind (Stmt) is
                           when Iir_Kind_For_Loop_Statement =>
                              --  Need to finalize for-loop statements.
                              if Label = Null_Node or else Label = Stmt
                              then
                                 Next_Parent_Statement (Process, Stmt, Stmt);
                                 exit;
                              else
                                 Synth.Vhdl_Stmts.Finish_For_Loop_Statement
                                   (Inst, Stmt);
                              end if;
                           when Iir_Kind_While_Loop_Statement =>
                              if Label = Null_Node or else Label = Stmt
                              then
                                 Next_Parent_Statement (Process, Stmt, Stmt);
                                 exit;
                              end if;
                           when others =>
                              null;
                        end case;
                     end loop;
                  end;
               else
                  Next_Statement (Process, Stmt);
               end if;
            when Iir_Kind_Return_Statement =>
               pragma Assert (Get_Expression (Stmt) = Null_Node);
               loop
                  Stmt := Get_Parent (Stmt);
                  case Get_Kind (Stmt) is
                     when Iir_Kind_For_Loop_Statement =>
                        --  Need to finalize for-loop statements.
                        Synth.Vhdl_Stmts.Finish_For_Loop_Statement
                          (Inst, Stmt);
                     when Iir_Kind_Procedure_Body =>
                        exit;
                     when others =>
                        null;
                  end case;
               end loop;
               Finish_Procedure_Call (Process, Stmt, Stmt);
               --  For a non-suspend procedure, return now to the caller.
               exit when Stmt = Null_Node;
               Next_Statement (Process, Stmt);

            when Iir_Kind_If_Statement =>
               declare
                  Els : Node;
               begin
                  Els := Stmt;
                  loop
                     if Execute_Condition (Inst, Get_Condition (Els)) then
                        Stmt := Get_Sequential_Statement_Chain (Els);
                        exit;
                     end if;

                     Els := Get_Else_Clause (Els);
                     if Els = Null_Node then
                        Next_Statement (Process, Stmt);
                        exit;
                     end if;
                  end loop;
               end;
            when Iir_Kind_Case_Statement =>
               declare
                  use Synth.Vhdl_Expr;
                  Expr : constant Node := Get_Expression (Stmt);
                  Sel : Valtyp;
               begin
                  Sel := Synth_Expression_With_Basetype (Inst, Expr);
                  Stmt := Synth.Vhdl_Stmts.Execute_Static_Case_Statement
                    (Inst, Stmt, Sel);
               end;

            when Iir_Kind_Assertion_Statement =>
               Synth.Vhdl_Stmts.Execute_Assertion_Statement (Inst, Stmt);
               Next_Statement (Process, Stmt);
            when Iir_Kind_Report_Statement =>
               Synth.Vhdl_Stmts.Execute_Report_Statement (Inst, Stmt);
               Next_Statement (Process, Stmt);

            when Iir_Kind_Variable_Assignment_Statement =>
               Synth.Vhdl_Stmts.Synth_Variable_Assignment (Inst, Stmt);
               Next_Statement (Process, Stmt);
            when Iir_Kind_Conditional_Variable_Assignment_Statement =>
               Synth.Vhdl_Stmts.Synth_Conditional_Variable_Assignment
                 (Inst, Stmt);
               Next_Statement (Process, Stmt);

            when Iir_Kind_Simple_Signal_Assignment_Statement =>
               Execute_Simple_Signal_Assignment (Inst, Stmt);
               Next_Statement (Process, Stmt);
            when Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Execute_Conditional_Signal_Assignment (Inst, Stmt);
               Next_Statement (Process, Stmt);

            when Iir_Kind_Wait_Statement =>
               --  The suspend state is executed instead.
               raise Internal_Error;

            when Iir_Kind_Procedure_Call_Statement =>
               --  Call of a procedure without suspend state.
               declare
                  Next_Stmt : Node;
               begin
                  Execute_Procedure_Call_Statement (Process, Stmt, Next_Stmt);
                  pragma Assert (Next_Stmt = Null_Node);
                  Next_Statement (Process, Stmt);
               end;

            when Iir_Kind_Suspend_State_Statement =>
               declare
                  Stmt2 : constant Node := Get_Chain (Stmt);
                  Next_Stmt : Node;
                  State : Int32;
                  State_Mem : Memory_Ptr;
               begin
                  case Get_Kind (Stmt2) is
                     when Iir_Kind_Wait_Statement =>
                        if Resume then
                           Resume := Resume_Wait_Statement
                             (Process.Instance, Stmt2);
                        else
                           Execute_Wait_Statement (Process.Instance, Stmt2);
                           Resume := True;
                        end if;
                        if Resume then
                           --  Will resume, so first stop!
                           State_Mem := Get_Suspend_State_Var (Inst);
                           State := Get_Suspend_State_Index (Stmt);
                           Write_I32 (State_Mem, Ghdl_I32 (State));
                           exit;
                        else
                           --  Continue execution
                           Stmt := Stmt2;
                           Next_Statement (Process, Stmt);
                        end if;
                     when Iir_Kind_Procedure_Call_Statement =>
                        if Resume then
                           raise Internal_Error;
                        end if;
                        Execute_Procedure_Call_Statement
                          (Process, Stmt2, Next_Stmt);
                        if Next_Stmt /= Null_Node then
                           --  User procedure.
                           --  Save current state.
                           State_Mem := Get_Suspend_State_Var (Inst);
                           State := Get_Suspend_State_Index (Stmt);
                           Write_I32 (State_Mem, Ghdl_I32 (State));

                           --  Start to execute the user procedure.
                           Inst := Process.Instance;
                           Stmt := Next_Stmt;
                        else
                           --  Implicit procedure, was already executed.
                           --  Continue execution
                           Stmt := Stmt2;
                           Next_Statement (Process, Stmt);
                        end if;
                     when others =>
                        raise Internal_Error;
                  end case;
               end;

            when others =>
               Vhdl.Errors.Error_Kind ("execute_sequential_statements", Stmt);
         end case;

         exit when Stmt = Null_Node;
      end loop;
   end Execute_Sequential_Statements;

   procedure Execute_Expression_Association (Proc_Idx : Process_Index_Type)
   is
      use Synth.Vhdl_Expr;
      Proc : Proc_Record_Type renames Processes_Table.Table (Proc_Idx);
      Drv : Driver_Entry renames Drivers_Table.Table (Proc.Drivers);
      Sig : Signal_Entry renames Signals_Table.Table (Drv.Sig);
      Val : Valtyp;
   begin
      Val := Synth_Expression_With_Type
        (Proc.Inst, Get_Actual (Proc.Proc), Drv.Typ);
      Start_Assign_Value_To_Signal
        ((Drv.Typ, Sig.Sig), 0, 0, Get_Value_Memtyp (Val));
   end Execute_Expression_Association;

   procedure Process_Executer (Self : Grt.Processes.Instance_Acc)
   is
      use Simple_IO;

      function To_Process_State_Acc is new Ada.Unchecked_Conversion
        (Grt.Processes.Instance_Acc, Process_State_Acc);

      Process : Process_State_Acc renames
        To_Process_State_Acc (Self);
   begin
      --  For debugger
      Current_Process := Process;

--      Instance_Pool := Process.Pool'Access;

      if Synth.Flags.Flag_Trace_Statements then
         Put (" run process: ");
--         Disp_Instance_Name (Process.Top_Instance);
         Put_Line (" (" & Vhdl.Errors.Disp_Location (Process.Proc) & ")");
      end if;

--      Execute_Sequential_Statements (Process);

      --  Sanity checks.
--      if not Is_Empty (Expr_Pool) then
--         raise Internal_Error;
--      end if;

      case Get_Kind (Process.Proc) is
         when Iir_Kind_Sensitized_Process_Statement =>
--            if Process.Instance.In_Wait_Flag then
--               raise Internal_Error;
--            end if;
            Execute_Sequential_Statements (Process);
         when Iir_Kind_Process_Statement =>
            Execute_Sequential_Statements (Process);
         when Iir_Kind_Concurrent_Assertion_Statement =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Synth.Vhdl_Stmts.Execute_Assertion_Statement
              (Process.Instance, Process.Proc);
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Simple_Signal_Assignment (Process.Instance, Process.Proc);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Conditional_Signal_Assignment
              (Process.Instance, Process.Proc);
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Selected_Signal_Assignment
              (Process.Instance, Process.Proc);
         when Iir_Kind_Association_Element_By_Expression =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Expression_Association (Process.Idx);
         when others =>
            raise Internal_Error;
      end case;

--      Instance_Pool := null;
      Current_Process := null;
   end Process_Executer;

   procedure Add_Sensitivity (Typ : Type_Acc; Sig : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete =>
            Grt.Processes.Ghdl_Process_Add_Sensitivity (Read_Sig (Sig));
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Add_Sensitivity
                    (Typ.Arr_El, Sig_Index (Sig, (Len - I) * Typ.Arr_El.W));
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Add_Sensitivity (Typ.Rec.E (I).Typ,
                                Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Add_Sensitivity;

   procedure Register_Sensitivity (Proc_Idx : Process_Index_Type)
   is
      Sens : Sensitivity_Index_Type;
   begin
      Sens := Processes_Table.Table (Proc_Idx).Sensitivity;
      while Sens /= No_Sensitivity_Index loop
         declare
            S : Sensitivity_Entry renames Sensitivity_Table.Table (Sens);
            Base : constant Memory_Ptr := Signals_Table.Table (S.Sig).Sig;
         begin
            Add_Sensitivity (S.Typ, Sig_Index (Base, S.Off.Net_Off));
            Sens := S.Prev_Proc;
         end;
      end loop;
   end Register_Sensitivity;

   function To_Address is new Ada.Unchecked_Conversion
     (Process_State_Acc, System.Address);

   procedure Create_Process_Sensitized (Proc : Process_State_Acc)
   is
      use Grt.Processes;
      Instance_Grt : Grt.Processes.Instance_Acc;
   begin
      Instance_Grt := To_Instance_Acc (Proc.all'Address);
      if Get_Postponed_Flag (Proc.Proc) then
         Ghdl_Postponed_Sensitized_Process_Register
           (Instance_Grt,
            Process_Executer'Access,
            null, To_Address (Proc));
      else
         Ghdl_Sensitized_Process_Register
           (Instance_Grt,
            Process_Executer'Access,
            null, To_Address (Proc));
      end if;
   end Create_Process_Sensitized;

   procedure Create_Processes
   is
      use Grt.Processes;
      Proc : Node;
      Instance : Synth_Instance_Acc;
      Instance_Grt : Grt.Processes.Instance_Acc;
      Instance_Addr : System.Address;
   begin
      Processes_State := new Process_State_Array (1 .. Processes_Table.Last);

      for I in Processes_Table.First .. Processes_Table.Last loop
         Instance := Processes_Table.Table (I).Inst;
         Proc := Processes_Table.Table (I).Proc;

--         Instance_Pool := Processes_State (I).Pool'Access;
--         Instance.Stmt := Get_Sequential_Statement_Chain (Proc);

         Processes_State (I).Top_Instance := Instance;
         Processes_State (I).Proc := Proc;
         Processes_State (I).Idx := I;
         Processes_State (I).Instance := Instance;

         Current_Process := Processes_State (I)'Access;
         Instance_Addr := Processes_State (I)'Address;
         Instance_Grt := To_Instance_Acc (Instance_Addr);
         case Get_Kind (Proc) is
            when Iir_Kind_Concurrent_Assertion_Statement =>
               Create_Process_Sensitized (Current_Process);
               Register_Sensitivity (I);

            when Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Concurrent_Simple_Signal_Assignment
              | Iir_Kind_Concurrent_Conditional_Signal_Assignment
              | Iir_Kind_Concurrent_Selected_Signal_Assignment =>
               declare
                  Driver_List: Iir_List;
               begin
                  Driver_List := Trans_Analyzes.Extract_Drivers (Proc);
                  Create_Process_Sensitized (Current_Process);
                  Register_Sensitivity (I);
                  Create_Process_Drivers (Instance, Proc, Driver_List);
                  Trans_Analyzes.Free_Drivers_List (Driver_List);
               end;

            when Iir_Kind_Association_Element_By_Expression =>
               Ghdl_Sensitized_Process_Register
                 (Instance_Grt,
                  Process_Executer'Access,
                  null, Instance_Addr);
               Register_Sensitivity (I);
               Create_Process_Drivers (I);

            when Iir_Kind_Process_Statement =>
               declare
                  Driver_List: Iir_List;
               begin
                  Driver_List := Trans_Analyzes.Extract_Drivers (Proc);

                  if Get_Postponed_Flag (Proc) then
                     Ghdl_Postponed_Process_Register
                       (Instance_Grt,
                        Process_Executer'Access,
                        null, Instance_Addr);
                  else
                     Ghdl_Process_Register
                       (Instance_Grt,
                        Process_Executer'Access,
                        null, Instance_Addr);
                  end if;
                  Create_Process_Drivers (Instance, Proc, Driver_List);
                  Trans_Analyzes.Free_Drivers_List (Driver_List);
               end;

            when others =>
               Vhdl.Errors.Error_Kind ("create_processes", Proc);
         end case;

         --  LRM93 12.4.4  Other Concurrent Statements
         --  All other concurrent statements are either process
         --  statements or are statements for which there is an
         --  equivalent process statement.
         --  Elaboration of a process statement proceeds as follows:
         --  1.  The process declarative part is elaborated.
--         Elaborate_Declarative_Part
--           (Instance, Get_Declaration_Chain (Proc));

         --  2.  The drivers required by the process statement
         --      are created.
         --  3.  The initial transaction defined by the default value
         --      associated with each scalar signal driven by the
         --      process statement is inserted into the corresponding
         --      driver.
         --  FIXME: do it for drivers in called subprograms too.
--         Elaborate_Drivers (Instance, Proc);

--         if not Is_Empty (Expr_Pool) then
--            raise Internal_Error;
--         end if;

         --  Elaboration of all concurrent signal assignment
         --  statements and concurrent assertion statements consists
         --  of the construction of the equivalent process statement
         --  followed by the elaboration of the equivalent process
         --  statement.
         --  [GHDL:  this is done by canonicalize.  ]

         --  FIXME: check passive statements,
         --  check no wait statement in sensitized processes.

--         Instance_Pool := null;
      end loop;

--      if Trace_Simulation then
--         Disp_Signals_Value;
--      end if;
   end Create_Processes;

   type Resolv_Instance_Type is record
      Func : Iir;
      Inst : Synth_Instance_Acc;
      Sig : Memory_Ptr;
   end record;
   type Resolv_Instance_Acc is access Resolv_Instance_Type;

   --  The resolution procedure for GRT.
   procedure Resolution_Proc (Instance_Addr : System.Address;
                              Val : System.Address;
                              Bool_Vec : System.Address;
                              Vec_Len : Ghdl_Index_Type;
                              Nbr_Drv : Ghdl_Index_Type;
                              Nbr_Ports : Ghdl_Index_Type);
   pragma Convention (C, Resolution_Proc);

   procedure Resolution_Proc (Instance_Addr : System.Address;
                              Val : System.Address;
                              Bool_Vec : System.Address;
                              Vec_Len : Ghdl_Index_Type;
                              Nbr_Drv : Ghdl_Index_Type;
                              Nbr_Ports : Ghdl_Index_Type) is
   begin
      raise Internal_Error;
   end Resolution_Proc;

   -- Create a new signal, using DEFAULT as initial value.
   -- Set its number.
   procedure Create_User_Signal (Inst : Synth_Instance_Acc;
                                 Mode : Mode_Signal_Type;
                                 Signal: Node;
                                 Typ : Type_Acc;
                                 Sig : Memory_Ptr;
                                 Val : Memory_Ptr)
   is
--      use Grt.Signals;

      procedure Create_Signal (Val : Memory_Ptr;
                               Sig : Memory_Ptr;
                               Sig_Type: Iir;
                               Typ : Type_Acc;
                               Already_Resolved : Boolean)
      is
         Sub_Resolved : Boolean := Already_Resolved;
         Resolv_Func : Iir;
         Resolv_Instance : Resolv_Instance_Acc;
         S : Ghdl_Signal_Ptr;
      begin
         if not Already_Resolved
           and then Get_Kind (Sig_Type) in Iir_Kinds_Subtype_Definition
         then
            Resolv_Func := Get_Resolution_Indication (Sig_Type);
         else
            Resolv_Func := Null_Iir;
         end if;
         if False and Resolv_Func /= Null_Iir then
            Sub_Resolved := True;
            Resolv_Instance := new Resolv_Instance_Type'
              (Func => Get_Named_Entity (Resolv_Func),
               Inst => Inst,
               Sig => Sig);
            Grt.Signals.Ghdl_Signal_Create_Resolution
              (Resolution_Proc'Access,
               Resolv_Instance.all'Address,
               System.Null_Address,
               Ghdl_Index_Type (Typ.W));
         end if;
         case Typ.Kind is
            when Type_Bit =>
               S := Grt.Signals.Ghdl_Create_Signal_B1
                 (To_Ghdl_Value_Ptr (To_Address (Val)),
                  null, System.Null_Address);
               Write_Sig (Sig, S);
            when Type_Logic =>
               S := Grt.Signals.Ghdl_Create_Signal_E8
                 (To_Ghdl_Value_Ptr (To_Address (Val)),
                  null, System.Null_Address);
               Write_Sig (Sig, S);
            when Type_Float =>
               S := Grt.Signals.Ghdl_Create_Signal_F64
                 (To_Ghdl_Value_Ptr (To_Address (Val)),
                  null, System.Null_Address);
               Write_Sig (Sig, S);
            when Type_Discrete =>
               if Typ.Sz = 1 then
                  S := Grt.Signals.Ghdl_Create_Signal_E8
                    (To_Ghdl_Value_Ptr (To_Address (Val)),
                     null, System.Null_Address);
               elsif Typ.Sz = 4 then
                  S := Grt.Signals.Ghdl_Create_Signal_I32
                    (To_Ghdl_Value_Ptr (To_Address (Val)),
                     null, System.Null_Address);
               elsif Typ.Sz = 8 then
                  S := Grt.Signals.Ghdl_Create_Signal_I64
                    (To_Ghdl_Value_Ptr (To_Address (Val)),
                     null, System.Null_Address);
               else
                  raise Internal_Error;
               end if;
               Write_Sig (Sig, S);
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
                     Create_Signal (Val + Size_Type (I - 1) * Typ.Arr_El.Sz,
                                    Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                                    El_Type, Typ.Arr_El, Already_Resolved);
                  end loop;
               end;
            when Type_Record =>
               declare
                  List : constant Iir_Flist := Get_Elements_Declaration_List
                    (Sig_Type);
                  El : Iir_Element_Declaration;
               begin
                  for I in Typ.Rec.E'Range loop
                     El := Get_Nth_Element (List, Natural (I - 1));
                     Create_Signal
                       (Val + Typ.Rec.E (I).Offs.Mem_Off,
                        Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off),
                        Get_Type (El), Typ.Rec.E (I).Typ,
                        Sub_Resolved);
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

      Create_Signal (Val, Sig, Sig_Type, Typ, False);
   end Create_User_Signal;

   function Alloc_Signal_Memory (Vtype : Type_Acc) return Memory_Ptr
   is
      function To_Memory_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Memory_Ptr);
      M : System.Address;
   begin
      Areapools.Allocate (Current_Pool.all,
                          M, Sig_Size * Size_Type (Vtype.W), Sig_Size);
      return To_Memory_Ptr (M);
   end Alloc_Signal_Memory;

   procedure Create_Signal (E : in out Signal_Entry) is
   begin
      E.Sig := Alloc_Signal_Memory (E.Typ);
      case E.Kind is
         when Mode_Guard =>
            --  Create_Guard_Signal (E.Instance, E.Sig, E.Val, E.Decl);
            raise Internal_Error;
         when Mode_Stable | Mode_Quiet | Mode_Transaction =>
            -- Create_Implicit_Signal
            --  (E.Sig, E.Val, E.Time, E.Prefix, E.Kind);
            raise Internal_Error;
         when Mode_Delayed =>
            -- Create_Delayed_Signal (E.Sig, E.Val, E.Prefix, E.Time);
            raise Internal_Error;
         when Mode_Above =>
            raise Internal_Error;
         when Mode_Signal_User =>
            Create_User_Signal (E.Inst, E.Kind, E.Decl, E.Typ, E.Sig, E.Val);
         when Mode_Conv_In | Mode_Conv_Out | Mode_End =>
            raise Internal_Error;
      end case;
   end Create_Signal;

   procedure Create_Signals is
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            E : Signal_Entry renames Signals_Table.Table (I);
         begin
            pragma Assert (E.Sig = null);
            if E.Collapsed_By /= No_Signal_Index then
               E.Sig := Signals_Table.Table (E.Collapsed_By).Sig;
               --  TODO: keep val ?
               E.Val := Signals_Table.Table (E.Collapsed_By).Val;
            else
               Create_Signal (E);
            end if;
         end;
      end loop;
   end Create_Signals;

   procedure Create_Terminals
   is
   begin
      for I in Terminal_Table.First .. Terminal_Table.Last loop
         declare
            T : Terminal_Entry renames Terminal_Table.Table (I);
         begin
            --  Allocate Ref_Val and set it to 0.
            pragma Assert (T.Across_Typ.Kind = Type_Float);
            T.Ref_Val := Alloc_Memory (T.Across_Typ);
            Write_Fp64 (T.Ref_Val, 0.0);

            if not Get_Reference_Terminal_Flag (T.Decl) then
               --  A non-ground reference.
               --  Allocate the reference quantity.
               T.Ref_Idx := Scalar_Quantities_Table.Last + 1;
               Scalar_Quantities_Table.Append
                 ((Idx => Nbr_Solver_Variables,
                   Deriv => No_Scalar_Quantity,
                   Integ => No_Scalar_Quantity));

               Nbr_Solver_Variables :=
                 Nbr_Solver_Variables + Natural (T.Across_Typ.W);
            end if;
         end;
      end loop;
   end Create_Terminals;

   --  Compute solver variables, allocate memory for quantities.
   procedure Create_Quantities
   is
      use Grt.Analog_Solver;
      Num : Natural;
      Idx : Integer;
      Vec : F64_C_Arr_Ptr;
   begin
      --  Compute number of scalar quantities.
      Num := Nbr_Solver_Variables;
      for I in Quantity_Table.First .. Quantity_Table.Last loop
         declare
            Q : Quantity_Entry renames Quantity_Table.Table (I);
            Def : Node;
            Pfx_Info : Target_Info;
         begin
            case Get_Kind (Q.Decl) is
               when Iir_Kind_Free_Quantity_Declaration
                 | Iir_Kind_Through_Quantity_Declaration =>
                  --  For a free or branch quantity:
                  --  * if it is the actual of a OUT formal, then use the
                  --    variable from the formal.
                  --  TODO: handle OUT associations.
                  pragma Assert (Q.Typ.Kind = Type_Float); -- TODO

                  Idx := Num;
                  Num := Num + Natural (Q.Typ.W);

                  Q.Idx := Scalar_Quantities_Table.Last + 1;
                  Scalar_Quantities_Table.Append
                    ((Idx => Idx,
                      Deriv => No_Scalar_Quantity,
                      Integ => No_Scalar_Quantity));

                  Def := Get_Default_Value (Q.Decl);
                  if Def /= Null_Node then
                     --  TODO
                     raise Internal_Error;
                  end if;
                  Q.Val := Alloc_Memory (Q.Typ);
                  Write_Fp64 (Q.Val, 0.0);

                  --  TODO:
                  --  For through quantities, add contribution to terminals.

               when Iir_Kind_Across_Quantity_Declaration =>
                  null;

               when Iir_Kind_Dot_Attribute =>
                  Pfx_Info := Synth_Target (Q.Inst, Get_Prefix (Q.Decl));
                  pragma Assert (Pfx_Info.Kind = Target_Simple);
                  pragma Assert (Pfx_Info.Off = (0, 0));
                  pragma Assert (Pfx_Info.Targ_Type.Kind = Type_Float);
                  declare
                     Pfx : constant Scalar_Quantity_Index :=
                       Quantity_Table.Table (Pfx_Info.Obj.Val.Q).Idx;
                     Pfx_Ent : Scalar_Quantity_Record renames
                       Scalar_Quantities_Table.Table (Pfx);
                  begin
                     if Pfx_Ent.Deriv /= No_Scalar_Quantity then
                        --  There is already a 'Dot, reuse it and done.
                        Q.Idx := Pfx_Ent.Deriv;
                     else
                        --  Create a 'Dot.
                        Pfx_Ent.Deriv := Scalar_Quantities_Table.Last + 1;
                        Q.Idx := Pfx_Ent.Deriv;
                        Scalar_Quantities_Table.Append
                          ((Idx => Num,
                            Deriv => No_Scalar_Quantity,
                            Integ => Pfx));
                        Num := Num + 1;

                        Augmentations_Set.Append
                          ((Kind => Aug_Dot, Q => Q.Idx));
                     end if;

                     Q.Val := Alloc_Memory (Q.Typ);
                     Write_Fp64 (Q.Val, 0.0);
                  end;

               when others =>
                  Vhdl.Errors.Error_Kind ("create_quantities", Q.Decl);
            end case;
         end;
      end loop;

      --  TODO: also for the reference quantity of terminals.

      Nbr_Solver_Variables := Num;

      if Num = 0 then
         --  No AMS
         return;
      end if;

      --  AMS simulation.
      Grt.Processes.Flag_AMS := True;

      --
      --  For 'Dot:
      --  * if the prefix is a quantity, use its corresponding prime.
      --  * if the prefix is 'Dot, create an intermediate variable.

      --  Initialize solver.
      Grt.Analog_Solver.Init (Ghdl_I32 (Num));

      --  LRM 1076.1-2007 12.6.4 Simulation cycle
      --  The value of each implicit quantity of the form ... Q'Dot ... is
      --  set to 0.0
      Vec := Grt.Analog_Solver.Get_Init_Der_Ptr;
      for I in 0 .. Num - 1 loop
         Vec (I) := 0.0;
      end loop;

      --  Set initial values.
      Vec := Grt.Analog_Solver.Get_Init_Val_Ptr;
      for I in Quantity_Table.First .. Quantity_Table.Last loop
         declare
            Q : Quantity_Entry renames Quantity_Table.Table (I);
            Idx : Integer;
         begin
            pragma Assert (Q.Typ.Kind = Type_Float); --  TODO
            Idx := Scalar_Quantities_Table.Table (Q.Idx).Idx;
            if Idx >= 0 then
               Vec (Idx) := Ghdl_F64 (Read_Fp64 (Q.Val));
            end if;
         end;
      end loop;
   end Create_Quantities;

   function Exec_Bit_Edge (Param : Valtyp; Res_Typ : Type_Acc; Val : Ghdl_U8)
                          return Memtyp
   is
      Sig : Ghdl_Signal_Ptr;
      Res : Boolean;
   begin
      Sig := Read_Sig (Sig_Index (Exec_Sig_Sig (Param.Val.A_Obj),
                                  Param.Val.A_Off.Net_Off));
      Res := Sig.Event and then Sig.Value_Ptr.E8 = Val;
      return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
   end Exec_Bit_Edge;

   function Exec_Bit_Rising_Edge (Param : Valtyp; Res_Typ : Type_Acc)
                                 return Memtyp is
   begin
      return Exec_Bit_Edge (Param, Res_Typ, 1);
   end Exec_Bit_Rising_Edge;

   function Exec_Bit_Falling_Edge (Param : Valtyp; Res_Typ : Type_Acc)
                                 return Memtyp is
   begin
      return Exec_Bit_Edge (Param, Res_Typ, 0);
   end Exec_Bit_Falling_Edge;

   function Exec_Std_Edge (Param : Valtyp;
                           Res_Typ : Type_Acc;
                           Prev : Std_Ulogic;
                           Curr : Std_Ulogic) return Memtyp
   is
      Sig : Ghdl_Signal_Ptr;
      Res : Boolean;
   begin
      Sig := Read_Sig (Sig_Index (Exec_Sig_Sig (Param.Val.A_Obj),
                                  Param.Val.A_Off.Net_Off));
      Res := Sig.Event
        and then To_X01 (Std_Ulogic'Val (Sig.Value_Ptr.E8)) = Curr
        and then To_X01 (Std_Ulogic'Val (Sig.Last_Value.E8)) = Prev;
      return Create_Memory_U8 (Boolean'Pos (Res), Res_Typ);
   end Exec_Std_Edge;

   function Exec_Std_Rising_Edge (Param : Valtyp; Res_Typ : Type_Acc)
                                 return Memtyp is
   begin
      return Exec_Std_Edge (Param, Res_Typ, '0', '1');
   end Exec_Std_Rising_Edge;

   function Exec_Std_Falling_Edge (Param : Valtyp; Res_Typ : Type_Acc)
                                  return Memtyp is
   begin
      return Exec_Std_Edge (Param, Res_Typ, '1', '0');
   end Exec_Std_Falling_Edge;

   procedure Exec_Finish (Inst : Synth_Instance_Acc; Imp : Node)
   is
      use Grt.Lib;
      Inter : constant Node := Get_Interface_Declaration_Chain (Imp);
      Param : Valtyp;
      Status : Int64;
   begin
      if Inter /= Null_Node then
         Param := Get_Value (Inst, Inter);
         Status := Read_Discrete (Param);
         Ghdl_Control_Simulation (False, True, Std_Integer (Status));
      else
         Ghdl_Control_Simulation (False, False, 0);
      end if;
   end Exec_Finish;

   procedure Set_Quantities_Values (Y : F64_C_Arr_Ptr; Yp: F64_C_Arr_Ptr)
   is
      pragma Unreferenced (Yp);
   begin
      for I in Quantity_Table.First .. Quantity_Table.Last loop
         declare
            Q : Quantity_Entry renames Quantity_Table.Table (I);
            Idx : Natural;
         begin
            pragma Assert (Q.Typ.Kind = Type_Float);
            Idx := Scalar_Quantities_Table.Table (Q.Idx).Idx;
            Write_Fp64 (Q.Val, Fp64 (Y (Idx)));
         end;
      end loop;
   end Set_Quantities_Values;

   procedure Residues (T : Ghdl_F64;
                       Y : F64_C_Arr_Ptr;
                       Yp : F64_C_Arr_Ptr;
                       Res : F64_C_Arr_Ptr)
   is
      Num : Natural;
      L, R : Valtyp;
      Prev_Time : Ghdl_F64;
   begin
      Set_Quantities_Values (Y, Yp);

      --  Apply time.
      --  TODO: physical time too.
      Prev_Time := Current_Time_AMS;
      Current_Time_AMS := T;

      Num := 0;
      for I in Simultaneous_Table.First .. Simultaneous_Table.Last loop
         declare
            S : Simultaneous_Record renames Simultaneous_Table.Table (I);
         begin
            case Get_Kind (S.Stmt) is
               when Iir_Kind_Simple_Simultaneous_Statement =>
                  L := Synth.Vhdl_Expr.Synth_Expression
                    (S.Inst, Get_Simultaneous_Left (S.Stmt));
                  R := Synth.Vhdl_Expr.Synth_Expression
                    (S.Inst, Get_Simultaneous_Right (S.Stmt));
                  pragma Assert (R.Typ.Kind = Type_Float);
                  pragma Assert (L.Typ.Kind = Type_Float);
                  Res (Num) := Ghdl_F64
                    (Read_Fp64 (L.Val.Mem) - Read_Fp64 (R.Val.Mem));
                  Num := Num + 1;
               when others =>
                  Vhdl.Errors.Error_Kind ("residues", S.Stmt);
            end case;
         end;
      end loop;

      for I in Augmentations_Set.First .. Augmentations_Set.Last loop
         declare
            A : Augmentation_Entry renames Augmentations_Set.Table (I);
         begin
            case A.Kind is
               when Aug_Dot =>
                  declare
                     Q : Scalar_Quantity_Record renames
                       Scalar_Quantities_Table.Table (A.Q);
                     pragma Assert (Q.Integ /= No_Scalar_Quantity);
                     Qi : Scalar_Quantity_Record renames
                       Scalar_Quantities_Table.Table (Q.Integ);
                  begin
                     Res (Num) := Y (Q.Idx) - Yp (Qi.Idx);
                     Num := Num + 1;
                  end;
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end loop;

      pragma Assert (Nbr_Solver_Variables = Num);

      if Trace_Residues then
         declare
            use Simple_IO;
            use Utils_IO;
         begin
            Put ("Residues at ");
            Put_Fp64 (Fp64 (Current_Time_AMS));
            New_Line;
            for I in 0 .. Num -1 loop
               Put ("Y");
               Put_Uns32 (Uns32 (I));
               Put ("=");
               Put_Fp64 (Fp64 (Y (I)));
               Put (", Yp(");
               Put_Uns32 (Uns32 (I));
               Put (")=");
               Put_Fp64 (Fp64 (Yp (I)));
               Put (", R(");
               Put_Uns32 (Uns32 (I));
               Put (")=");
               Put_Fp64 (Fp64 (Res (I)));
               New_Line;
            end loop;
         end;
      end if;

      Current_Time_AMS := Prev_Time;
   end Residues;

   procedure Runtime_Elaborate is
   begin
--      if Disp_Stats then
--         Disp_Design_Stats;
--      end if;

      -- There is no inputs.
      -- All the simulation is done via time, so it must be displayed.
      Disp_Time_Before_Values := True;

      Create_Signals;
      -- Create_Connects;
      -- Create_Disconnections;
      Create_Processes;
      -- Create_PSL;
      Create_Terminals;
      Create_Quantities;

      --  Allow Synth_Expression to handle signals.
      Synth.Vhdl_Expr.Hook_Signal_Expr := Hook_Signal_Expr'Access;
      Synth.Vhdl_Expr.Hook_Event_Attribute := Exec_Event_Attribute'Access;

      Synth.Vhdl_Oper.Hook_Bit_Rising_Edge := Exec_Bit_Rising_Edge'Access;
      Synth.Vhdl_Oper.Hook_Bit_Falling_Edge := Exec_Bit_Falling_Edge'Access;

      Synth.Vhdl_Oper.Hook_Std_Rising_Edge := Exec_Std_Rising_Edge'Access;
      Synth.Vhdl_Oper.Hook_Std_Falling_Edge := Exec_Std_Falling_Edge'Access;

      Synth.Vhdl_Expr.Hook_Quantity_Expr := Hook_Quantity_Expr'Access;
      Synth.Vhdl_Expr.Hook_Dot_Attribute := Exec_Dot_Attribute'Access;

      Synth.Vhdl_Static_Proc.Hook_Finish := Exec_Finish'Access;

      -- if Flag_Interractive then
      --    Debug (Reason_Elab);
      -- end if;
   end Runtime_Elaborate;

   procedure Ghdl_Elaborate;
   pragma Export (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   procedure Ghdl_Elaborate is
   begin
      Runtime_Elaborate;
   end Ghdl_Elaborate;

   Ghdl_Progname : constant String := "ghdl" & ASCII.Nul;

   procedure Simulation
   is
      Ok : C_Boolean;
      Status : Integer;
   begin
      Break_Time := Std_Time'Last;

      Grt.Options.Progname := To_Ghdl_C_String (Ghdl_Progname'Address);
      Grt.Errors.Set_Error_Stream (Grt.Stdio.stdout);

--      Grt.Errors.Error_Hook := Debug_Error'Access;

--      if Flag_Interractive then
--         Debug (Reason_Start);
--      end if;

      Ok := Grt.Main.Run_Elab;
      if not Ok then
         return;
      end if;

      Synth.Flags.Severity_Level := Grt.Options.Severity_Level;

      if Flag_Interractive then
         Elab.Debugger.Debug_Elab (Vhdl_Elab.Top_Instance);
      end if;

      Status := Grt.Main.Run_Through_Longjump
        (Grt.Processes.Simulation_Init'Access);

      if Status = 0 then
         if Grt.Processes.Flag_AMS then
            Grt.Analog_Solver.Start;
         end if;

         loop
            if Break_Time < Grt.Processes.Next_Time then
               Grt.Processes.Next_Time := Break_Time;
            end if;

            Status := Grt.Main.Run_Through_Longjump
              (Grt.Processes.Simulation_Cycle'Access);
            exit when Status < 0
              or Status = Grt.Errors.Run_Stop
              or Status = Grt.Errors.Run_Finished;

            if Current_Time >= Break_Time
              and then Break_Time /= Std_Time'Last
            then
               --  No not break anymore on time,
               Break_Time := Std_Time'Last;
               Elab.Debugger.Debug_Time;
            end if;

            exit when Grt.Processes.Has_Simulation_Timeout;
         end loop;
      end if;

      Grt.Processes.Simulation_Finish;

      Grt.Main.Run_Finish (Status);
   exception
--      when Debugger_Quit =>
--         null;
      when Simulation_Finished =>
         null;
   end Simulation;
end Simul.Vhdl_Simul;
