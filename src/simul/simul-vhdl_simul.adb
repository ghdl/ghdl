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

with Vhdl.Types;
with Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Ieee.Std_Logic_1164;
with Vhdl.Sem_Inst;
with Vhdl.Canon;

with PSL.Types; use PSL.Types;
with PSL.Nodes;
with PSL.NFAs;
with PSL.NFAs.Utils;
with PSL.Errors;

with Elab.Debugger;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Types;
with Elab.Vhdl_Debug;

with Synth.Errors;
with Synth.Vhdl_Stmts; use Synth.Vhdl_Stmts;
with Synth.Vhdl_Expr;
with Synth.Vhdl_Oper;
with Synth.Vhdl_Decls;
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
with Grt.Severity;
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

   function To_Ghdl_Value (Mt : Memtyp) return Value_Union
   is
      Val : Value_Union;
   begin
      case Mt.Typ.Kind is
         when Type_Bit =>
            Val.B1 := Ghdl_B1'Val (Read_U8 (Mt.Mem));
         when Type_Logic =>
            Val.E8 := Read_U8 (Mt.Mem);
         when Type_Discrete =>
            if Mt.Typ.Sz = 1 then
               Val.E8 := Read_U8 (Mt.Mem);
            elsif Mt.Typ.Sz = 4 then
               Val.I32 := Read_I32 (Mt.Mem);
            elsif Mt.Typ.Sz = 8 then
               Val.I64 := Read_I64 (Mt.Mem);
            else
               raise Internal_Error;
            end if;
         when Type_Float =>
            Val.F64 := Ghdl_F64 (Read_Fp64 (Mt.Mem));
         when others =>
            raise Internal_Error;
      end case;
      return Val;
   end To_Ghdl_Value;

   procedure Write_Ghdl_Value (Mt : Memtyp; Val : Value_Union) is
   begin
      case Mt.Typ.Kind is
         when Type_Bit =>
            Write_U8 (Mt.Mem, Ghdl_B1'Pos (Val.B1));
         when Type_Logic =>
            Write_U8 (Mt.Mem, Val.E8);
         when Type_Discrete =>
            if Mt.Typ.Sz = 1 then
               Write_U8 (Mt.Mem, Val.E8);
            elsif Mt.Typ.Sz = 4 then
               Write_I32 (Mt.Mem, Val.I32);
            elsif Mt.Typ.Sz = 8 then
               Write_I64 (Mt.Mem, Val.I64);
            else
               raise Internal_Error;
            end if;
         when Type_Float =>
            Write_Fp64 (Mt.Mem, Fp64 (Val.F64));
         when others =>
            raise Internal_Error;
      end case;
   end Write_Ghdl_Value;

   procedure Assign_Value_To_Signal (Target: Memtyp;
                                     Is_Start : Boolean;
                                     Rej : Std_Time;
                                     After : Std_Time;
                                     Val : Memtyp)
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      case Target.Typ.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete
           | Type_Float =>
            Sig := Read_Sig (Target.Mem);
            if Is_Start then
               if Val = Null_Memtyp then
                  Ghdl_Signal_Start_Assign_Null (Sig, Rej, After);
               else
                  Ghdl_Signal_Start_Assign_Any
                    (Sig, Rej, To_Ghdl_Value (Val), After);
               end if;
            else
               Ghdl_Signal_Next_Assign
                 (Sig, To_Ghdl_Value (Val), After);
            end if;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Target.Typ.Abound.Len;
               El : constant Type_Acc := Target.Typ.Arr_El;
               Smem : Memory_Ptr;
            begin
               pragma Assert (Val.Typ.Abound.Len = Len);
               for I in 1 .. Len loop
                  if Val.Mem = null then
                     Smem := null;
                  else
                     Smem := Val.Mem + Size_Type (I - 1) * El.Sz;
                  end if;
                  Assign_Value_To_Signal
                    ((El, Sig_Index (Target.Mem, (Len - I) * El.W)),
                     Is_Start, Rej, After, (Val.Typ.Arr_El, Smem));
               end loop;
            end;
         when Type_Record =>
            for I in Target.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Target.Typ.Rec.E (I);
                  Smem : Memory_Ptr;
               begin
                  if Val.Mem = null then
                     Smem := null;
                  else
                     Smem := Val.Mem + E.Offs.Mem_Off;
                  end if;
                  Assign_Value_To_Signal
                    ((E.Typ, Sig_Index (Target.Mem, E.Offs.Net_Off)),
                     Is_Start, Rej, After, (E.Typ, Smem));
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Assign_Value_To_Signal;

   procedure Add_Source (Typ : Type_Acc; Sig : Memory_Ptr; Val : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Logic
            | Type_Bit
            | Type_Discrete
            | Type_Float =>
            Grt.Signals.Ghdl_Process_Add_Port_Driver
              (Read_Sig (Sig), To_Ghdl_Value ((Typ, Val)));
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

   procedure Create_Process_Drivers (Proc : Process_Index_Type)
   is
      Drv : Driver_Index_Type;
   begin
      Drv := Processes_Table.Table (Proc).Drivers;
      while Drv /= No_Driver_Index loop
         declare
            D : Driver_Entry renames Drivers_Table.Table (Drv);
            S : Signal_Entry renames Signals_Table.Table (D.Sig.Base);
         begin
            Add_Source (D.Sig.Typ, Sig_Index (S.Sig, D.Sig.Offs.Net_Off),
                        S.Val + D.Sig.Offs.Mem_Off);

            Drv := D.Prev_Proc;
         end;
      end loop;
   end Create_Process_Drivers;

   type Read_Signal_Flag_Enum is
     (Read_Signal_Event,
      Read_Signal_Active,
      --  In order to reuse the same code (that returns immediately if the
      --  attribute is true), we use not driving.
      Read_Signal_Not_Driving);

   function Read_Signal_Flag (Sig : Memtyp; Kind : Read_Signal_Flag_Enum)
                             return Boolean is
   begin
      case Sig.Typ.Kind is
         when Type_Scalars =>
            declare
               S : Ghdl_Signal_Ptr;
            begin
               S := Read_Sig (Sig.Mem);
               case Kind is
                  when Read_Signal_Event =>
                     return S.Event;
                  when Read_Signal_Active =>
                     return S.Active;
                  when Read_Signal_Not_Driving =>
                     --  Ghdl_B1 to boolean.
                     if Grt.Signals.Ghdl_Signal_Driving (S) = True then
                        return False;
                     else
                        return True;
                     end if;
               end case;
            end;
         when Type_Vector
            | Type_Array =>
            declare
               Len : constant Uns32 := Sig.Typ.Abound.Len;
               Sub : Memory_Ptr;
            begin
               for I in 1 .. Len loop
                  Sub := Sig_Index (Sig.Mem, (Len - I) * Sig.Typ.Arr_El.W);
                  if Read_Signal_Flag ((Sig.Typ.Arr_El, Sub), Kind) then
                     return True;
                  end if;
               end loop;
               return False;
            end;
         when Type_Record =>
            declare
               Sub : Memory_Ptr;
            begin
               for I in Sig.Typ.Rec.E'Range loop
                  Sub := Sig_Index (Sig.Mem, Sig.Typ.Rec.E (I).Offs.Net_Off);
                  if Read_Signal_Flag ((Sig.Typ.Rec.E (I).Typ, Sub), Kind) then
                     return True;
                  end if;
               end loop;
               return False;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Read_Signal_Flag;

   function Exec_Signal_Flag_Attribute (Inst : Synth_Instance_Acc;
                                        Expr : Node;
                                        Kind : Read_Signal_Flag_Enum)
                                       return Valtyp
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
      E := Read_Signal_Flag
        ((Pfx.Targ_Type,
          Sig_Index (Signals_Table.Table (Pfx.Obj.Val.S).Sig,
                     Pfx.Off.Net_Off)),
         Kind);
      Res := Create_Value_Memory (Boolean_Type, Expr_Pool'Access);
      Write_U8 (Res.Val.Mem, Boolean'Pos (E));
      return Res;
   end Exec_Signal_Flag_Attribute;

   function Exec_Event_Attribute (Inst : Synth_Instance_Acc;
                                  Expr : Node) return Valtyp is
   begin
      return Exec_Signal_Flag_Attribute (Inst, Expr, Read_Signal_Event);
   end Exec_Event_Attribute;

   function Exec_Active_Attribute (Inst : Synth_Instance_Acc;
                                   Expr : Node) return Valtyp is
   begin
      return Exec_Signal_Flag_Attribute (Inst, Expr, Read_Signal_Active);
   end Exec_Active_Attribute;

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
      Mark : Mark_Type;
      Cond_Val : Valtyp;
      Res : Boolean;
   begin
      if Cond = Null_Node then
         return True;
      end if;

      Mark_Expr_Pool (Mark);
      Cond_Val := Synth.Vhdl_Expr.Synth_Expression (Inst, Cond);
      Res := Read_Discrete (Cond_Val) = 1;
      Release_Expr_Pool (Mark);

      return Res;
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
      Call : Node;
      Resume : Boolean;
   begin
      if not Get_Suspend_Flag (Bod) or else not Process.Has_State then
         Process.Instance := Caller_Inst;
         --  TODO: free old inst.
         Stmt := Null_Node;
         return;
      end if;
      if Caller_Inst = Process.Top_Instance
        and then
        Get_Kind (Process.Proc) = Iir_Kind_Concurrent_Procedure_Call_Statement
      then
         Call := Get_Procedure_Call (Process.Proc);
         Stmt := Null_Node;
      else
         Get_Suspend_State_Statement (Caller_Inst, Stmt, Resume);
         pragma Assert (Resume);
         --  Skip the resume statement.
         Stmt := Get_Chain (Stmt);
         pragma Assert (Get_Kind (Stmt) = Iir_Kind_Procedure_Call_Statement);
         Call := Get_Procedure_Call (Stmt);
      end if;

      Synth.Vhdl_Decls.Finalize_Declarations
        (Process.Instance, Get_Declaration_Chain (Bod), True);
      Synth_Subprogram_Back_Association
        (Process.Instance, Caller_Inst,
         Get_Interface_Declaration_Chain (Imp),
         Get_Parameter_Association_Chain (Call));
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
                  Valid : Boolean;
               begin
                  --  Update index
                  Val := Get_Value (Process.Instance, Param);
                  Update_Index (Val.Typ.Drange, Valid, Val);

                  --  Test.
                  if Valid then
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
               Vhdl.Errors.Error_Kind ("next_parent_statement", Parent);
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
         when Type_Scalars =>
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
      Marker : Mark_Type;
      Expr : Node;
      List : Node_List;
      Val : Valtyp;
      Timeout : Int64;
   begin
      Mark_Expr_Pool (Marker);

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

      Release_Expr_Pool (Marker);

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
      Obj  : constant Node := Get_Method_Object (Call);

      Assoc_Chain : constant Node := Get_Parameter_Association_Chain (Call);

      Area_Mark : Mark_Type;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Areapools.Mark (Area_Mark, Instance_Pool.all);

      if Get_Implicit_Definition (Imp) /= Iir_Predefined_None then
         declare
            Inter_Chain : constant Node :=
              Get_Interface_Declaration_Chain (Imp);
         begin
            pragma Assert (Obj = Null_Node);
            Sub_Inst := Synth_Subprogram_Call_Instance (Inst, Imp, Imp);

            Synth_Subprogram_Associations
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
                 (Inst, Stmt, "call to foreign %n is not supported", +Imp);
               Next_Stmt := Null_Node;
               return;
            end if;

            if Obj /= Null_Node then
               Sub_Inst := Synth_Protected_Call_Instance (Inst, Obj, Imp, Bod);
            else
               Sub_Inst := Synth_Subprogram_Call_Instance (Inst, Imp, Bod);
            end if;

            --  Note: in fact the uninstantiated scope is the instantiated
            --  one!
            Set_Uninstantiated_Scope (Sub_Inst, Imp);
            Synth_Subprogram_Associations
              (Sub_Inst, Inst, Inter_Chain, Assoc_Chain);

            Process.Instance := Sub_Inst;
            Synth.Vhdl_Decls.Synth_Declarations
              (Sub_Inst, Get_Declaration_Chain (Bod), True);

            if Process.Has_State and then Get_Suspend_Flag (Bod) then
               --  The procedure may suspend, in a suspendable process.
               Next_Stmt := Get_Sequential_Statement_Chain (Bod);
               if Next_Stmt /= Null_Node then
                  return;
               end if;
            end if;

            --  No suspension (or no statements).
            Execute_Sequential_Statements (Process);
            Synth.Vhdl_Decls.Finalize_Declarations
              (Sub_Inst, Get_Declaration_Chain (Bod), True);
            Synth_Subprogram_Back_Association
              (Sub_Inst, Inst, Inter_Chain, Assoc_Chain);
            Next_Stmt := Null_Node;
         end;
      end if;

      if Elab.Debugger.Flag_Need_Debug then
         Elab.Debugger.Debug_Leave (Sub_Inst);
      end if;

      Free_Elab_Instance (Sub_Inst);
      Areapools.Release (Area_Mark, Instance_Pool.all);
   end Execute_Procedure_Call_Statement;

   procedure Execute_Waveform_Assignment (Inst : Synth_Instance_Acc;
                                          Target : Target_Info;
                                          Stmt : Node;
                                          Waveform : Node)
   is
      use Synth.Vhdl_Expr;
      V_Aft : Std_Time;
      Start : Boolean;

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
         Sig : Memtyp;
         Mem : Memtyp;
      begin
         case Target.Kind is
            when Target_Aggregate =>
               Execute_Aggregate_Signal_Assignment
                 (Inst, Target.Aggr, Target.Targ_Type, Val, Loc);

            when Target_Simple =>
               declare
                  E : Signal_Entry renames
                    Signals_Table.Table (Target.Obj.Val.S);
               begin
                  Sig := (Target.Targ_Type,
                          Sig_Index (E.Sig, Target.Off.Net_Off));
               end;

               if Val /= No_Valtyp then
                  Mem := Get_Value_Memtyp (Val);
               else
                  Mem := Null_Memtyp;
               end if;
               Assign_Value_To_Signal (Sig, Start, V_Aft, V_Aft, Mem);

            when Target_Memory =>
               raise Internal_Error;
         end case;
      end Execute_Signal_Assignment;

      Wf : Node;
      We : Node;
      Val : Valtyp;
      Aft : Node;
      Rej : Node;
   begin
      Rej := Get_Reject_Time_Expression (Stmt);
      if Rej /= Null_Node then
         raise Internal_Error;
      end if;

      Wf := Waveform;
      Start := True;
      loop
         Aft := Get_Time (Wf);
         if Aft /= Null_Node then
            Val := Synth_Expression (Inst, Aft);
            V_Aft := Std_Time (Read_I64 (Val.Val.Mem));
         else
            V_Aft := 0;
         end if;

         We := Get_We_Value (Wf);
         if Get_Kind (We) = Iir_Kind_Null_Literal then
            Val := No_Valtyp;
         else
            Val := Synth_Expression_With_Type (Inst, We, Target.Targ_Type);
            Val := Synth_Subtype_Conversion
              (Inst, Val, Target.Targ_Type, False, Wf);
         end if;
         Execute_Signal_Assignment (Inst, Target, Val, Wf);

         Wf := Get_Chain (Wf);
         exit when Wf = Null_Node;
         Start := False;
      end loop;
   end Execute_Waveform_Assignment;

   procedure Disconnect_Signal (Sig : Memtyp)
   is
      S : Ghdl_Signal_Ptr;
   begin
      case Sig.Typ.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete
           | Type_Float =>
            S := Read_Sig (Sig.Mem);
            if S.Flags.Sig_Kind /= Kind_Signal_No then
               Ghdl_Signal_Disconnect (S);
            end if;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Sig.Typ.Abound.Len;
               El : constant Type_Acc := Sig.Typ.Arr_El;
            begin
               for I in 1 .. Len loop
                  Disconnect_Signal
                    ((El, Sig_Index (Sig.Mem, (Len - I) * El.W)));
               end loop;
            end;
         when Type_Record =>
            for I in Sig.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Sig.Typ.Rec.E (I);
               begin
                  Disconnect_Signal
                    ((E.Typ, Sig_Index (Sig.Mem, E.Offs.Net_Off)));
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Disconnect_Signal;

   procedure Disconnect_Signal_Target (Target : Target_Info)
   is
      E : Signal_Entry renames Signals_Table.Table (Target.Obj.Val.S);
      Sig : Memtyp;
   begin
      Sig := (Target.Targ_Type, Sig_Index (E.Sig, Target.Off.Net_Off));
      Disconnect_Signal (Sig);
   end Disconnect_Signal_Target;

   function Execute_Maybe_Guarded_Assignment (Inst : Synth_Instance_Acc;
                                              Stmt : Node;
                                              Targ : Target_Info)
                                             return Boolean
   is
      Guard : constant Node := Get_Guard (Stmt);
   begin
      if Guard /= Null_Node
        and then not Execute_Condition (Inst, Guard)
      then
         Disconnect_Signal_Target (Targ);
         return True;
      else
         return False;
      end if;
   end Execute_Maybe_Guarded_Assignment;

   procedure Execute_Simple_Signal_Assignment (Inst : Synth_Instance_Acc;
                                               Stmt : Node;
                                               Concurrent : Boolean)
   is
      Target : constant Node := Get_Target (Stmt);
      Marker : Mark_Type;
      Info : Target_Info;
   begin
      Mark_Expr_Pool (Marker);
      Info := Synth_Target (Inst, Target);

      if Concurrent
        and then Execute_Maybe_Guarded_Assignment (Inst, Stmt, Info)
      then
         Release_Expr_Pool (Marker);
         return;
      end if;

      Execute_Waveform_Assignment
        (Inst, Info, Stmt, Get_Waveform_Chain (Stmt));
      Release_Expr_Pool (Marker);
   end Execute_Simple_Signal_Assignment;

   procedure Execute_Conditional_Signal_Assignment (Inst : Synth_Instance_Acc;
                                                    Stmt : Node;
                                                    Concurrent : Boolean)
   is
      Target : constant Node := Get_Target (Stmt);
      Marker : Mark_Type;
      Cw : Node;
      Cond : Node;
      Info : Target_Info;
   begin
      Mark_Expr_Pool (Marker);
      Info := Synth_Target (Inst, Target);

      if Concurrent
        and then Execute_Maybe_Guarded_Assignment (Inst, Stmt, Info)
      then
         Release_Expr_Pool (Marker);
         return;
      end if;

      Cw := Get_Conditional_Waveform_Chain (Stmt);
      while Cw /= Null_Node loop
         Cond := Get_Condition (Cw);
         if Cond = Null_Node
           or else Execute_Condition (Inst, Cond)
         then
            Execute_Waveform_Assignment
              (Inst, Info, Stmt, Get_Waveform_Chain (Cw));
            exit;
         end if;
         Cw := Get_Chain (Cw);
      end loop;
      Release_Expr_Pool (Marker);
   end Execute_Conditional_Signal_Assignment;

   procedure Execute_Selected_Signal_Assignment (Inst : Synth_Instance_Acc;
                                                 Stmt : Node;
                                                 Concurrent : Boolean)
   is
      use Synth.Vhdl_Expr;
      Target : constant Node := Get_Target (Stmt);
      Marker : Mark_Type;
      Sel : Memtyp;
      Sw : Node;
      Wf : Node;
      Info : Target_Info;
      Eq : Boolean;
   begin
      Mark_Expr_Pool (Marker);
      Info := Synth_Target (Inst, Target);

      if Concurrent
        and then Execute_Maybe_Guarded_Assignment (Inst, Stmt, Info)
      then
         Release_Expr_Pool (Marker);
         return;
      end if;

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
            when Iir_Kind_Choice_By_Range =>
               declare
                  Bnd : Discrete_Range_Type;
               begin
                  Elab.Vhdl_Types.Synth_Discrete_Range
                    (Inst, Get_Choice_Range (Sw), Bnd);
                  Eq := In_Range (Bnd, Read_Discrete (Sel));
               end;
            when Iir_Kind_Choice_By_Others =>
               Eq := True;
            when others =>
               raise Internal_Error;
         end case;
         if Eq then
            Execute_Waveform_Assignment (Inst, Info, Stmt, Wf);
            exit;
         end if;
         Sw := Get_Chain (Sw);
      end loop;
      Release_Expr_Pool (Marker);
   end Execute_Selected_Signal_Assignment;

   procedure Assertion_Report_Msg (Inst : Synth_Instance_Acc;
                                   Stmt : Node;
                                   Severity : Natural;
                                   Msg : Valtyp)
   is
      pragma Unreferenced (Inst);
      use Grt.Severity;
      use Grt.Errors;
   begin
      Report_S (Vhdl.Errors.Disp_Location (Stmt));
      Diag_C (":@");
      Diag_C_Now;
      Diag_C (":(");
      if Get_Kind (Stmt) = Iir_Kind_Report_Statement then
         Diag_C ("report");
      else
         Diag_C ("assert");
      end if;
      Diag_C (' ');
      case Severity is
         when Note_Severity =>
            Diag_C ("note");
         when Warning_Severity =>
            Diag_C ("warning");
         when Error_Severity =>
            Diag_C ("error");
         when Failure_Severity =>
            Diag_C ("failure");
         when others =>
            Diag_C ("??");
      end case;
      Diag_C ("): ");

      if Msg = No_Valtyp then
         Diag_C ("Assertion violation.");
      else
         Diag_C (Value_To_String (Msg));
      end if;
      Report_E;
   end Assertion_Report_Msg;

   procedure Execute_Assertion_Statement (Inst : Synth_Instance_Acc;
                                          Stmt : Node)
   is
      use Grt.Options;
   begin
      if Execute_Condition (Inst, Get_Assertion_Condition (Stmt)) then
         return;
      end if;

      --  TODO: ieee asserts vs user asserts.
      case Asserts_Policy is
         when Enable_Asserts =>
            null;
         when Disable_Asserts =>
            return;
         when Disable_Asserts_At_Time_0 =>
            if Current_Time = 0 then
               return;
            end if;
      end case;

      Exec_Failed_Assertion (Inst, Stmt);
   end Execute_Assertion_Statement;

   procedure Execute_Sequential_Statements_Inner (Process : Process_State_Acc;
                                                  First_Stmt : Node;
                                                  Is_Resume : Boolean)
   is
      Inst : Synth_Instance_Acc;
      Stmt : Node;
      Resume : Boolean;
   begin
      Stmt := First_Stmt;
      Resume := Is_Resume;

      loop
         Inst := Process.Instance;
         if Synth.Flags.Flag_Trace_Statements then
            Elab.Vhdl_Debug.Put_Stmt_Trace (Stmt);
         end if;
         if Elab.Debugger.Flag_Need_Debug then
            Elab.Debugger.Debug_Break (Inst, Stmt);
         end if;

         pragma Assert (Is_Expr_Pool_Empty);

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
                     Label : Node;
                  begin
                     Label := Get_Loop_Label (Stmt);
                     if Label /= Null_Node then
                        Label := Get_Named_Entity (Label);
                     end if;

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
                     Label : Node;
                  begin
                     Label := Get_Loop_Label (Stmt);
                     if Label /= Null_Node then
                        Label := Get_Named_Entity (Label);
                     end if;

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
               pragma Assert (Is_Expr_Pool_Empty);
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
                  Marker : Mark_Type;
                  Sel : Valtyp;
               begin
                  Mark_Expr_Pool (Marker);
                  Sel := Synth_Expression_With_Basetype (Inst, Expr);
                  Stmt := Synth.Vhdl_Stmts.Execute_Static_Case_Statement
                    (Inst, Stmt, Sel);
                  Release_Expr_Pool (Marker);
               end;

            when Iir_Kind_Assertion_Statement =>
               Execute_Assertion_Statement (Inst, Stmt);
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
               Execute_Simple_Signal_Assignment (Inst, Stmt, False);
               Next_Statement (Process, Stmt);
            when Iir_Kind_Conditional_Signal_Assignment_Statement =>
               Execute_Conditional_Signal_Assignment (Inst, Stmt, False);
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
                  pragma Assert (Is_Expr_Pool_Empty);
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
                        pragma Assert (Is_Expr_Pool_Empty);
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
   end Execute_Sequential_Statements_Inner;

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
         --  No suspend, simply execute.
         Stmt := Get_Sequential_Statement_Chain (Src);
         Resume := True;
      else
         --  Find the resume instruction (or start instruction).
         Get_Suspend_State_Statement (Inst, Stmt, Resume);
      end if;

      if Stmt = Null_Node then
         --  No statement, return.
         if Get_Kind (Src) = Iir_Kind_Procedure_Body then
            Finish_Procedure_Call (Process, Src, Stmt);
         end if;
      else
         --  There are statements
         Execute_Sequential_Statements_Inner (Process, Stmt, Resume);
      end if;
   end Execute_Sequential_Statements;

   procedure Wait_For_Concurrent_Procedure_Call (Proc : Process_State_Acc)
   is
      Sens : Sensitivity_Index_Type;
   begin
      Sens := Processes_Table.Table (Proc.Idx).Sensitivity;
      while Sens /= No_Sensitivity_Index loop
         declare
            S : Sensitivity_Entry renames Sensitivity_Table.Table (Sens);
            Base : constant Memory_Ptr := Signals_Table.Table (S.Sig.Base).Sig;
         begin
            Add_Wait_Sensitivity (S.Sig.Typ,
                                  Sig_Index (Base, S.Sig.Offs.Net_Off));
            Sens := S.Prev_Proc;
         end;
      end loop;
      Grt.Processes.Ghdl_Process_Wait_Suspend;
   end Wait_For_Concurrent_Procedure_Call;

   procedure Execute_Concurrent_Procedure_Call (Proc : Process_State_Acc)
   is
      Next_Stmt : Node;
      Resume : Boolean;
   begin
      if Proc.Instance = null then
         --  Resume after implicit wait statement.
         raise Internal_Error;
      elsif Proc.Instance = Proc.Top_Instance then
         --  Call procedure.
         Execute_Procedure_Call_Statement (Proc, Proc.Proc, Next_Stmt);
         if Next_Stmt = Null_Node then
            --  Fully executed.
            --  Execute implicit wait.
            Wait_For_Concurrent_Procedure_Call (Proc);
         else
            --  Execute.
            Execute_Sequential_Statements_Inner (Proc, Next_Stmt, False);
            if Proc.Instance = Proc.Top_Instance then
               --  Do implicit wait.
               Wait_For_Concurrent_Procedure_Call (Proc);
            end if;
         end if;
      else
         --  Resume within the procedure.
         Get_Suspend_State_Statement (Proc.Instance, Next_Stmt, Resume);
         Execute_Sequential_Statements_Inner (Proc, Next_Stmt, Resume);
         if Proc.Instance = Proc.Top_Instance then
            --  Do implicit wait
            Wait_For_Concurrent_Procedure_Call (Proc);
         end if;
      end if;
   end Execute_Concurrent_Procedure_Call;

   procedure Execute_Expression_Association (Proc_Idx : Process_Index_Type)
   is
      use Synth.Vhdl_Expr;
      Mark : Mark_Type;
      Proc : Proc_Record_Type renames Processes_Table.Table (Proc_Idx);
      Drv : Driver_Entry renames Drivers_Table.Table (Proc.Drivers);
      Sig : Signal_Entry renames Signals_Table.Table (Drv.Sig.Base);
      Val : Valtyp;
   begin
      Mark_Expr_Pool (Mark);
      Val := Synth_Expression_With_Type
        (Proc.Inst, Get_Actual (Proc.Proc), Drv.Sig.Typ);
      Assign_Value_To_Signal
        ((Drv.Sig.Typ, Sig.Sig), True, 0, 0, Get_Value_Memtyp (Val));
      Release_Expr_Pool (Mark);
   end Execute_Expression_Association;

   function To_Process_State_Acc is new Ada.Unchecked_Conversion
     (Grt.Processes.Instance_Acc, Process_State_Acc);

   procedure Process_Executer (Self : Grt.Processes.Instance_Acc)
   is
      use Simple_IO;

      Process : Process_State_Acc renames
        To_Process_State_Acc (Self);
   begin
      --  For debugger
      Current_Process := Process;

      Instance_Pool := Process.Pool;

      --  Sanity checks.
      pragma Assert (Is_Expr_Pool_Empty);

      if Synth.Flags.Flag_Trace_Statements then
         Put ("run process: ");
         Elab.Vhdl_Debug.Disp_Instance_Path (Process.Top_Instance);
         Put_Line (" (" & Vhdl.Errors.Disp_Location (Process.Proc) & ")");
      end if;

      case Get_Kind (Process.Proc) is
         when Iir_Kind_Sensitized_Process_Statement =>
--            if Process.Instance.In_Wait_Flag then
--               raise Internal_Error;
--            end if;
            Execute_Sequential_Statements (Process);
            pragma Assert (Areapools.Is_Empty (Instance_Pool.all));
         when Iir_Kind_Process_Statement =>
            Execute_Sequential_Statements (Process);
         when Iir_Kind_Concurrent_Assertion_Statement =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Assertion_Statement (Process.Instance, Process.Proc);
            pragma Assert (Areapools.Is_Empty (Instance_Pool.all));
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Simple_Signal_Assignment
              (Process.Instance, Process.Proc, True);
            pragma Assert (Areapools.Is_Empty (Instance_Pool.all));
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Conditional_Signal_Assignment
              (Process.Instance, Process.Proc, True);
            pragma Assert (Areapools.Is_Empty (Instance_Pool.all));
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Selected_Signal_Assignment
              (Process.Instance, Process.Proc, True);
            pragma Assert (Areapools.Is_Empty (Instance_Pool.all));
         when Iir_Kind_Association_Element_By_Expression =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Expression_Association (Process.Idx);
            pragma Assert (Areapools.Is_Empty (Instance_Pool.all));
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            if Elab.Debugger.Flag_Need_Debug then
               Elab.Debugger.Debug_Break (Process.Instance, Process.Proc);
            end if;
            Execute_Concurrent_Procedure_Call (Process);
         when others =>
            raise Internal_Error;
      end case;

      Instance_Pool := null;
      Current_Process := null;
   end Process_Executer;

   procedure Add_Sensitivity (Typ : Type_Acc; Sig : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Logic
           | Type_Bit
           | Type_Discrete
           | Type_Float =>
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
            Base : constant Memory_Ptr := Signals_Table.Table (S.Sig.Base).Sig;
         begin
            Add_Sensitivity (S.Sig.Typ, Sig_Index (Base, S.Sig.Offs.Net_Off));
            Sens := S.Prev_Proc;
         end;
      end loop;
   end Register_Sensitivity;

   function To_Address is new Ada.Unchecked_Conversion
     (Process_State_Acc, System.Address);

   procedure Create_Process_Sensitized (Proc : Process_State_Acc)
   is
      use Grt.Processes;
      Instance_Grt : constant Grt.Processes.Instance_Acc :=
        To_Instance_Acc (Proc.all'Address);
   begin
      --  As those processes only suspend at the end, they don't need a
      --  specific stack and can share the same stack.
      Proc.Pool := Process_Pool'Access;

      if Get_Postponed_Flag (Proc.Proc) then
         Ghdl_Postponed_Sensitized_Process_Register (Instance_Grt,
                                                     Process_Executer'Access,
                                                     null, To_Address (Proc));
      else
         Ghdl_Sensitized_Process_Register (Instance_Grt,
                                           Process_Executer'Access,
                                           null, To_Address (Proc));
      end if;
   end Create_Process_Sensitized;

   procedure PSL_Process_Executer (Self : Grt.Processes.Instance_Acc);
   pragma Convention (C, PSL_Process_Executer);

   procedure PSL_Assert_Finalizer (Self : Grt.Processes.Instance_Acc);
   pragma Convention (C, PSL_Assert_Finalizer);

   function Execute_Psl_Expr (Inst : Synth_Instance_Acc;
                              Expr : PSL_Node;
                              Eos : Boolean)
                             return Boolean
   is
      use Vhdl.Types;
      use PSL.Nodes;
   begin
      case Get_Kind (Expr) is
         when N_HDL_Expr
           | N_HDL_Bool =>
            declare
               E : constant Vhdl_Node := Get_HDL_Node (Expr);
               Rtype : constant Vhdl_Node := Get_Base_Type (Get_Type (E));
               Res   : Valtyp;
               V     : Ghdl_U8;
            begin
               Res := Synth.Vhdl_Expr.Synth_Expression (Inst, E);
               if Rtype = Vhdl.Std_Package.Boolean_Type_Definition then
                  return Read_U8 (Res.Val.Mem) = 1;
               elsif Rtype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type then
                  V := Read_U8 (Res.Val.Mem);
                  return V = 3 or V = 7; --  1 or H
               else
                  PSL.Errors.Error_Kind ("execute_psl_expr", Expr);
               end if;
            end;
         when N_True =>
            return True;
         when N_EOS =>
            return Eos;
         when N_Not_Bool =>
            return not Execute_Psl_Expr (Inst, Get_Boolean (Expr), Eos);
         when N_And_Bool =>
            return Execute_Psl_Expr (Inst, Get_Left (Expr), Eos)
              and Execute_Psl_Expr (Inst, Get_Right (Expr), Eos);
         when N_Or_Bool =>
            return Execute_Psl_Expr (Inst, Get_Left (Expr), Eos)
              or Execute_Psl_Expr (Inst, Get_Right (Expr), Eos);
         when others =>
            PSL.Errors.Error_Kind ("execute_psl_expr", Expr);
      end case;
   end Execute_Psl_Expr;

   procedure PSL_Process_Executer (Self : Grt.Processes.Instance_Acc)
   is
      use PSL.NFAs;

      E : constant Process_State_Acc := To_Process_State_Acc (Self);
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

      Instance_Pool := Process_Pool'Access;
--      Current_Process := No_Process;

      Mark_Expr_Pool (Marker);
      V := Execute_Psl_Expr (E.Instance, Get_PSL_Clock (E.Proc), False);
      Release_Expr_Pool (Marker);
      if V then
         Nvec := (others => False);
         case Get_Kind (E.Proc) is
            when Iir_Kind_Psl_Cover_Directive
              | Iir_Kind_Psl_Endpoint_Declaration =>
               Nvec (0) := True;
            when others =>
               null;
         end case;

         --  For each state: if set, evaluate all outgoing edges.
         NFA := Get_PSL_NFA (E.Proc);
         S := Get_First_State (NFA);
         while S /= No_State loop
            S_Num := Get_State_Label (S);

            if E.States (S_Num) then
               Ed := Get_First_Src_Edge (S);
               while Ed /= No_Edge loop
                  Sd := Get_Edge_Dest (Ed);
                  Sd_Num := Get_State_Label (Sd);

                  if not Nvec (Sd_Num) then
                     Mark_Expr_Pool (Marker);
                     V := Execute_Psl_Expr
                       (E.Instance, Get_Edge_Expr (Ed), False);
                     Release_Expr_Pool (Marker);
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
         pragma Assert (S_Num = Get_PSL_Nbr_States (E.Proc) - 1);
         case Get_Kind (E.Proc) is
            when Iir_Kind_Psl_Assert_Directive =>
               if Nvec (S_Num) then
                  Exec_Failed_Assertion (E.Instance, E.Proc);
               end if;
            when Iir_Kind_Psl_Assume_Directive =>
               if Nvec (S_Num) then
                  Exec_Failed_Assertion (E.Instance, E.Proc);
               end if;
            when Iir_Kind_Psl_Cover_Directive =>
               if Nvec (S_Num) then
                  if Get_Report_Expression (E.Proc) /= Null_Iir then
                     Exec_Failed_Assertion (E.Instance, E.Proc);
                  end if;
                  E.Done := True;
               end if;
--            when Iir_Kind_Psl_Endpoint_Declaration =>
--               declare
--                  Info : constant Sim_Info_Acc := Get_Info (E.Stmt);
--               begin
--                E.Instance.Objects (Info.Slot).B1 := Ghdl_B1 (Nvec (S_Num));
--               end;
            when others =>
               Vhdl.Errors.Error_Kind ("PSL_Process_Executer", E.Proc);
         end case;

         E.States.all := Nvec;
      end if;

      Instance_Pool := null;
--      Current_Process := null;
   end PSL_Process_Executer;

   procedure PSL_Assert_Finalizer (Self : Grt.Processes.Instance_Acc)
   is
      use PSL.NFAs;
      Ent : constant Process_State_Acc := To_Process_State_Acc (Self);

      NFA : constant PSL_NFA := Get_PSL_NFA (Ent.Proc);
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
               Exec_Failed_Assertion (Ent.Instance, Ent.Proc);
               exit;
            end if;
         end if;

         E := Get_Next_Dest_Edge (E);
      end loop;
   end PSL_Assert_Finalizer;

   procedure Create_PSL (Proc : in out Process_State_Type;
                         Inst : System.Address)
   is
      Stmt : constant Node := Proc.Proc;
   begin
      --  Create the vector.
      Proc.States := new Boolean_Vector'
        (0 .. Get_PSL_Nbr_States (Stmt) - 1 => False);
      Proc.States (0) := True;

      Grt.Processes.Ghdl_Process_Register
        (To_Instance_Acc (Inst), PSL_Process_Executer'Access,
         null, System.Null_Address);

      case Get_Kind (Proc.Proc) is
         when Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Assume_Directive =>
            if Get_PSL_EOS_Flag (Proc.Proc) then
               Grt.Processes.Ghdl_Finalize_Register
                 (To_Instance_Acc (Inst), PSL_Assert_Finalizer'Access);
            end if;
         when Iir_Kind_Psl_Cover_Directive =>
            --  TODO
            null;
         when others =>
            null;
      end case;
   end Create_PSL;

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

         Processes_State (I) := (Kind => Kind_Process,
                                 Has_State => False,
                                 Top_Instance => Instance,
                                 Proc => Proc,
                                 Idx => I,
                                 Instance => Instance,
                                 Pool => <>);

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
               Create_Process_Sensitized (Current_Process);
               Register_Sensitivity (I);
               Create_Process_Drivers (I);

            when Iir_Kind_Association_Element_By_Expression =>
               Processes_State (I).Pool := Process_Pool'Access;
               Ghdl_Sensitized_Process_Register
                 (Instance_Grt,
                  Process_Executer'Access,
                  null, Instance_Addr);
               Register_Sensitivity (I);
               Create_Process_Drivers (I);

            when Iir_Kind_Process_Statement
              | Iir_Kind_Concurrent_Procedure_Call_Statement =>
               --  As those processes can suspend, they need a dedicated
               --  stack.
               Current_Process.Pool := new Areapools.Areapool;
               Current_Process.Has_State := True;

               if Get_Postponed_Flag (Proc) then
                  Ghdl_Postponed_Process_Register (Instance_Grt,
                                                   Process_Executer'Access,
                                                   null, Instance_Addr);
               else
                  Ghdl_Process_Register (Instance_Grt,
                                         Process_Executer'Access,
                                         null, Instance_Addr);
               end if;
               Create_Process_Drivers (I);

            when Iir_Kind_Psl_Assert_Directive =>
               Processes_State (I) := (Kind => Kind_PSL,
                                       Has_State => False,
                                       Top_Instance => Instance,
                                       Proc => Proc,
                                       Idx => I,
                                       Instance => Instance,
                                       Done => False,
                                       States => null);
               Create_PSL (Processes_State (I), Processes_State (I)'Address);

            when others =>
               Vhdl.Errors.Error_Kind ("create_processes", Proc);
         end case;

         pragma Assert (Areapools.Is_Empty (Expr_Pool));
      end loop;
   end Create_Processes;

   type Resolver_Read_Mode is (Read_Port, Read_Driver);

   procedure Resolver_Read_Value (Dst : Memtyp;
                                  Sig : Memory_Ptr;
                                  Mode : Resolver_Read_Mode;
                                  Index : Ghdl_Index_Type) is
   begin
      case Dst.Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            declare
               S : constant Ghdl_Signal_Ptr := Read_Sig (Sig);
               Val : Ghdl_Value_Ptr;
            begin
               case Mode is
                  when Read_Port =>
                     Val := Ghdl_Signal_Read_Port (S, Index);
                  when Read_Driver =>
                     Val := Ghdl_Signal_Read_Driver (S, Index);
               end case;
               Write_Ghdl_Value (Dst, Val.all);
            end;
         when Type_Vector
           | Type_Array =>
            declare
               Typ : constant Type_Acc := Dst.Typ;
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Resolver_Read_Value
                    ((Typ.Arr_El, Dst.Mem + Size_Type (I - 1) * Typ.Arr_El.Sz),
                     Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                     Mode, Index);
               end loop;
            end;
         when Type_Record =>
            for I in Dst.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Dst.Typ.Rec.E (I);
               begin
                  Resolver_Read_Value ((E.Typ, Dst.Mem + E.Offs.Mem_Off),
                                       Sig_Index (Sig, E.Offs.Net_Off),
                                       Mode, Index);
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Resolver_Read_Value;

   type Read_Signal_Enum is
     (
      Read_Signal_Last_Value,

      --  For conversion functions.
      Read_Signal_Driving_Value,
      Read_Signal_Effective_Value --,

      --  'Driving_Value
--      Read_Signal_Driver_Value
     );

   procedure Exec_Read_Signal (Sig: Memory_Ptr;
                               Val : Memtyp;
                               Attr : Read_Signal_Enum)
   is
      S : Ghdl_Signal_Ptr;
   begin
      case Val.Typ.Kind is
         when Type_Scalars =>
            S := Read_Sig (Sig);
            case Attr is
               when Read_Signal_Driving_Value =>
                  Write_Ghdl_Value (Val, S.Driving_Value);
               when Read_Signal_Effective_Value =>
                  Write_Ghdl_Value (Val, S.Value_Ptr.all);
               when Read_Signal_Last_Value =>
                  Write_Ghdl_Value (Val, S.Last_Value);
            end case;
         when Type_Vector
           | Type_Array =>
            declare
               Typ : constant Type_Acc := Val.Typ;
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Exec_Read_Signal
                    (Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                     (Typ.Arr_El, Val.Mem + Size_Type (I - 1) * Typ.Arr_El.Sz),
                     Attr);
               end loop;
            end;
         when Type_Record =>
            for I in Val.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Val.Typ.Rec.E (I);
               begin
                  Exec_Read_Signal (Sig_Index (Sig, E.Offs.Net_Off),
                                    (E.Typ, Val.Mem + E.Offs.Mem_Off),
                                    Attr);
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Exec_Read_Signal;

   function Exec_Last_Value_Attribute (Inst : Synth_Instance_Acc;
                                       Expr : Node) return Valtyp
   is
      Pfx : Target_Info;
      Res : Valtyp;
      S : Memory_Ptr;
   begin
      Pfx := Synth_Target (Inst, Get_Prefix (Expr));

      Res := Create_Value_Memory (Pfx.Targ_Type, Expr_Pool'Access);

      S := Sig_Index (Signals_Table.Table (Pfx.Obj.Val.S).Sig,
                      Pfx.Off.Net_Off);

      Exec_Read_Signal (S, Get_Memtyp (Res), Read_Signal_Last_Value);
      return Res;
   end Exec_Last_Value_Attribute;

   type Read_Signal_Last_Enum is
     (
      Read_Signal_Last_Event,
      Read_Signal_Last_Active
     );

   function Exec_Read_Signal_Last (Sig: Memory_Ptr;
                                   Val : Memtyp;
                                   Attr : Read_Signal_Last_Enum)
                                  return Std_Time
   is
      Res, T : Std_Time;
      S : Ghdl_Signal_Ptr;
   begin
      case Val.Typ.Kind is
         when Type_Scalars =>
            S := Read_Sig (Sig);
            case Attr is
               when Read_Signal_Last_Event =>
                  return S.Last_Event;
               when Read_Signal_Last_Active =>
                  return S.Last_Active;
            end case;
         when Type_Vector
           | Type_Array =>
            declare
               Typ : constant Type_Acc := Val.Typ;
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               Res := Std_Time'First;
               for I in 1 .. Len loop
                  T := Exec_Read_Signal_Last
                    (Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                     (Typ.Arr_El, Val.Mem + Size_Type (I - 1) * Typ.Arr_El.Sz),
                     Attr);
                  Res := Std_Time'Max (Res, T);
               end loop;
               return Res;
            end;
         when Type_Record =>
            Res := Std_Time'First;
            for I in Val.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Val.Typ.Rec.E (I);
               begin
                  T := Exec_Read_Signal_Last
                    (Sig_Index (Sig, E.Offs.Net_Off),
                     (E.Typ, Val.Mem + E.Offs.Mem_Off),
                     Attr);
                  Res := Std_Time'Max (Res, T);
               end;
            end loop;
            return Res;
         when others =>
            raise Internal_Error;
      end case;
   end Exec_Read_Signal_Last;

   function Exec_Signal_Last_Attribute (Inst : Synth_Instance_Acc;
                                        Expr : Node;
                                        Attr : Read_Signal_Last_Enum)
                                       return Valtyp
   is
      Pfx : Target_Info;
      Res : Valtyp;
      T : Std_Time;
      S : Memory_Ptr;
   begin
      Pfx := Synth_Target (Inst, Get_Prefix (Expr));

      Res := Create_Value_Memory (Get_Subtype_Object (Inst, Get_Type (Expr)),
                                  Expr_Pool'Access);

      S := Sig_Index (Signals_Table.Table (Pfx.Obj.Val.S).Sig,
                      Pfx.Off.Net_Off);

      T := Exec_Read_Signal_Last (S, Get_Memtyp (Res), Attr);
      Write_I64 (Res.Val.Mem, Ghdl_I64 (T));
      return Res;
   end Exec_Signal_Last_Attribute;

   function Exec_Last_Event_Attribute (Inst : Synth_Instance_Acc;
                                       Expr : Node) return Valtyp is
   begin
      return Exec_Signal_Last_Attribute (Inst, Expr, Read_Signal_Last_Event);
   end Exec_Last_Event_Attribute;

   function Exec_Last_Active_Attribute (Inst : Synth_Instance_Acc;
                                        Expr : Node) return Valtyp is
   begin
      return Exec_Signal_Last_Attribute (Inst, Expr, Read_Signal_Last_Active);
   end Exec_Last_Active_Attribute;

   type Write_Signal_Enum is
     (Write_Signal_Driving_Value,
      Write_Signal_Effective_Value);

   procedure Exec_Write_Signal (Sig: Memory_Ptr;
                                Val : Memtyp;
                                Attr : Write_Signal_Enum)
   is
      S : Ghdl_Signal_Ptr;
   begin
      case Val.Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            S := Read_Sig (Sig);
            case Attr is
               when Write_Signal_Driving_Value =>
                  S.Driving_Value := To_Ghdl_Value (Val);
               when Write_Signal_Effective_Value =>
                  S.Value_Ptr.all := To_Ghdl_Value (Val);
            end case;
         when Type_Vector
           | Type_Array =>
            declare
               Typ : constant Type_Acc := Val.Typ;
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Exec_Write_Signal
                    (Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                     (Typ.Arr_El, Val.Mem + Size_Type (I - 1) * Typ.Arr_El.Sz),
                     Attr);
               end loop;
            end;
         when Type_Record =>
            for I in Val.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Val.Typ.Rec.E (I);
               begin
                  Exec_Write_Signal (Sig_Index (Sig, E.Offs.Net_Off),
                                     (E.Typ, Val.Mem + E.Offs.Mem_Off),
                                     Attr);
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Exec_Write_Signal;

   type Resolv_Instance_Type is record
      Func : Iir;
      Inst : Synth_Instance_Acc;
      Sig : Memory_Ptr;
      Idx_Typ : Type_Acc;
      Arr_Typ : Type_Acc;
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
                              Nbr_Ports : Ghdl_Index_Type)
   is
      pragma Unreferenced (Val);

      R : Resolv_Instance_Type;
      pragma Import (Ada, R);
      for R'Address use Instance_Addr;

      type Bool_Array is array (1 .. Nbr_Drv) of Boolean;
      Vec : Bool_Array;
      pragma Import (Ada, Vec);
      for Vec'Address use Bool_Vec;

      Len : constant Iir_Index32 := Iir_Index32 (Vec_Len + Nbr_Ports);
      Bnd : Bound_Type;

      El_Typ : constant Type_Acc := R.Arr_Typ.Uarr_El;
      Stride : constant Size_Type := El_Typ.Sz;
      Arr_Typ : Type_Acc;
      Arr : Memtyp;
      Off : Size_Type;

      Res : Valtyp;

      Expr_Marker, Inst_Marker : Mark_Type;
   begin
      Mark_Expr_Pool (Expr_Marker);
      Instance_Pool := Process_Pool'Access;
      Areapools.Mark (Inst_Marker, Instance_Pool.all);
      pragma Assert (Areapools.Is_Empty (Instance_Pool.all));

      --  Create the type.
      Bnd := Elab.Vhdl_Types.Create_Bounds_From_Length (R.Idx_Typ.Drange, Len);
      Arr_Typ := Create_Array_Type (Bnd, True, El_Typ);

      --  Allocate the array.
      Arr := Create_Memory (Arr_Typ);

      --  Write ports.
      Off := 0;
      for I in 1 .. Nbr_Ports loop
         Resolver_Read_Value ((El_Typ, Arr.Mem + Off),
                              R.Sig, Read_Port, I - 1);
         Off := Off + Stride;
      end loop;

      --  Write drivers.
      for I in 1 .. Nbr_Drv loop
         if Vec (I) then
            Resolver_Read_Value ((El_Typ, Arr.Mem + Off),
                                 R.Sig, Read_Driver, I - 1);
            Off := Off + Stride;
         end if;
      end loop;

      --  Call resolution function
      Res := Exec_Resolution_Call (R.Inst, R.Func, Create_Value_Memtyp (Arr));

      --  Set driving value.
      Exec_Write_Signal (R.Sig, (Res.Typ, Res.Val.Mem),
                         Write_Signal_Driving_Value);

      Release_Expr_Pool (Expr_Marker);
      Areapools.Release (Inst_Marker, Instance_Pool.all);

      pragma Assert (Is_Expr_Pool_Empty);
      pragma Assert (Areapools.Is_Empty (Instance_Pool.all));
   end Resolution_Proc;

   function Create_Scalar_Signal (Typ : Type_Acc; Val : Ghdl_Value_Ptr)
                                 return Ghdl_Signal_Ptr is
   begin
      case Typ.Kind is
         when Type_Bit =>
            return Grt.Signals.Ghdl_Create_Signal_B1
              (Val, null, System.Null_Address);
         when Type_Logic =>
            return Grt.Signals.Ghdl_Create_Signal_E8
              (Val, null, System.Null_Address);
         when Type_Float =>
            return Grt.Signals.Ghdl_Create_Signal_F64
              (Val, null, System.Null_Address);
         when Type_Discrete =>
            if Typ.Sz = 1 then
               return Grt.Signals.Ghdl_Create_Signal_E8
                 (Val, null, System.Null_Address);
            elsif Typ.Sz = 4 then
               return Grt.Signals.Ghdl_Create_Signal_I32
                 (Val, null, System.Null_Address);
            elsif Typ.Sz = 8 then
               return Grt.Signals.Ghdl_Create_Signal_I64
                 (Val, null, System.Null_Address);
            else
               raise Internal_Error;
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Scalar_Signal;

   procedure Create_User_Signal (Idx : Signal_Index_Type)
   is
      E : Signal_Entry renames Signals_Table.Table (Idx);

      procedure Create_Signal (Val : Memory_Ptr;
                               Sig_Off : Uns32;
                               Sig_Type: Iir;
                               Typ : Type_Acc;
                               Vec : Nbr_Sources_Array;
                               Already_Resolved : Boolean)
      is
         Sub_Resolved : Boolean := Already_Resolved;
         Resolv_Func : Node;
         Resolv_Instance : Resolv_Instance_Acc;
         S : Ghdl_Signal_Ptr;
         Arr_Type : Node;
         Idx_Type : Node;
      begin
         if not Already_Resolved
           and then Get_Kind (Sig_Type) in Iir_Kinds_Subtype_Definition
         then
            Resolv_Func := Get_Resolution_Indication (Sig_Type);
            if Resolv_Func /= Null_Node then
               Resolv_Func := Get_Named_Entity (Resolv_Func);
            end if;
            if Resolv_Func /= Null_Node
              and then
              (Vec (Sig_Off).Total > 1
                 or else Resolv_Func /= Vhdl.Ieee.Std_Logic_1164.Resolved)
            then
               Sub_Resolved := True;
               Arr_Type :=
                 Get_Type (Get_Interface_Declaration_Chain (Resolv_Func));
               Idx_Type := Vhdl.Utils.Get_Index_Type (Arr_Type, 0);
               Resolv_Instance := new Resolv_Instance_Type'
                 (Func => Resolv_Func,
                  Inst => E.Inst,
                  Sig => Sig_Index (E.Sig, Sig_Off),
                  Idx_Typ => Get_Subtype_Object (E.Inst, Idx_Type),
                  Arr_Typ => Get_Subtype_Object (E.Inst, Arr_Type));
               Grt.Signals.Ghdl_Signal_Create_Resolution
                 (Resolution_Proc'Access,
                  Resolv_Instance.all'Address,
                  System.Null_Address,
                  Ghdl_Index_Type (Typ.W));
            end if;
         end if;
         case Typ.Kind is
            when Type_Scalars =>
               S := Create_Scalar_Signal
                 (Typ, To_Ghdl_Value_Ptr (To_Address (Val)));
               Write_Sig (Sig_Index (E.Sig, Sig_Off), S);
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
                                    Sig_Off + (Len - I) * Typ.Arr_El.W,
                                    El_Type, Typ.Arr_El,
                                    Vec, Sub_Resolved);
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
                        Sig_Off + Typ.Rec.E (I).Offs.Net_Off,
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
      end Create_Signal;

      Sig_Type: constant Iir := Get_Type (E.Decl);
      Kind : Kind_Signal_Type;

      type Iir_Kind_To_Kind_Signal_Type is
        array (Iir_Signal_Kind) of Kind_Signal_Type;
      Iir_Kind_To_Kind_Signal : constant Iir_Kind_To_Kind_Signal_Type :=
        (Iir_Register_Kind  => Kind_Signal_Register,
         Iir_Bus_Kind       => Kind_Signal_Bus);
   begin
      if Get_Guarded_Signal_Flag (E.Decl) then
         Kind := Iir_Kind_To_Kind_Signal (Get_Signal_Kind (E.Decl));
      else
         Kind := Kind_Signal_No;
      end if;

      Grt.Signals.Ghdl_Signal_Set_Mode (E.Kind, Kind, True);

      Create_Signal (E.Val, 0, Sig_Type, E.Typ, E.Nbr_Sources.all, False);
   end Create_User_Signal;

   type Guard_Instance_Type is record
      Instance : Synth_Instance_Acc;
      Guard : Iir;
   end record;

   type Guard_Instance_Acc is access Guard_Instance_Type;

   function Guard_Func (Data : System.Address) return Ghdl_B1;
   pragma Convention (C, Guard_Func);

   function Guard_Func (Data : System.Address) return Ghdl_B1
   is
      use Areapools;

      Guard : Guard_Instance_Type;
      pragma Import (Ada, Guard);
      for Guard'Address use Data;

      Val : Boolean;

      Prev_Instance_Pool : constant Areapool_Acc := Instance_Pool;
   begin
      Instance_Pool := Process_Pool'Access;

      Val := Execute_Condition
        (Guard.Instance, Get_Guard_Expression (Guard.Guard));

      Instance_Pool := Prev_Instance_Pool;

      return Ghdl_B1'Val (Boolean'Pos (Val));
   end Guard_Func;

   procedure Add_Guard_Sensitivity (Typ : Type_Acc; Sig : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Scalars =>
            Grt.Signals.Ghdl_Signal_Guard_Dependence (Read_Sig (Sig));
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Add_Guard_Sensitivity
                    (Typ.Arr_El, Sig_Index (Sig, (Len - I) * Typ.Arr_El.W));
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Add_Guard_Sensitivity
                 (Typ.Rec.E (I).Typ,
                  Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Add_Guard_Sensitivity;

   procedure Create_Guard_Signal (Idx : Signal_Index_Type)
   is
      E : Signal_Entry renames Signals_Table.Table (Idx);

      Dep_List : Iir_List;
      Dep_It : List_Iterator;
      S : Ghdl_Signal_Ptr;
      Data : Guard_Instance_Acc;
   begin
      Data := new Guard_Instance_Type'(Instance => E.Inst, Guard => E.Decl);

      S := Grt.Signals.Ghdl_Signal_Create_Guard
        (To_Ghdl_Value_Ptr (To_Address (E.Val)),
         Data.all'Address, Guard_Func'Access);
      Write_Sig (E.Sig, S);

      Dep_List := Get_Guard_Sensitivity_List (E.Decl);
      Dep_It := List_Iterate (Dep_List);
      while Is_Valid (Dep_It) loop
         declare
            El : constant Node := Get_Element (Dep_It);
            Sig_Mem : Memory_Ptr;
            Info : Target_Info;
         begin
            Info := Synth_Target (E.Inst, El);
            Sig_Mem := Signals_Table.Table (Info.Obj.Val.S).Sig;
            Add_Guard_Sensitivity
              (Info.Targ_Type, Sig_Index (Sig_Mem, Info.Off.Net_Off));
         end;
         Next (Dep_It);
      end loop;
   end Create_Guard_Signal;

   procedure Create_Delayed_Signal (Sig : Memory_Ptr;
                                    Val : Memory_Ptr;
                                    Pfx : Memory_Ptr;
                                    Typ : Type_Acc;
                                    Time : Std_Time) is
   begin
      case Typ.Kind is
         when Type_Scalars =>
            declare
               S : Ghdl_Signal_Ptr;
            begin
               S := Grt.Signals.Ghdl_Create_Delayed_Signal
                 (Read_Sig (Pfx), To_Ghdl_Value_Ptr (To_Address (Val)), Time);
               Write_Sig (Sig, S);
            end;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Create_Delayed_Signal
                    (Sig_Index (Sig,  (Len - I) * Typ.Arr_El.W),
                     Val + Size_Type (I - 1) * Typ.Arr_El.Sz,
                     Sig_Index (Pfx, (Len - I) * Typ.Arr_El.W),
                     Typ.Arr_El, Time);
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Typ.Rec.E (I);
               begin
                  Create_Delayed_Signal
                    (Sig_Index (Sig, E.Offs.Net_Off),
                     Val + E.Offs.Mem_Off,
                     Sig_Index (Pfx, E.Offs.Net_Off),
                     E.Typ, Time);
               end;
            end loop;

         when Type_Slice
           | Type_Access
           | Type_Unbounded_Vector
           | Type_Unbounded_Array
           | Type_Unbounded_Record
           | Type_File
           | Type_Protected =>
            raise Internal_Error;
      end case;
   end Create_Delayed_Signal;

   procedure Register_Prefix (Typ : Type_Acc; Sig : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Scalars =>
            Grt.Signals.Ghdl_Signal_Attribute_Register_Prefix (Read_Sig (Sig));
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Register_Prefix
                    (Typ.Arr_El, Sig_Index (Sig, (Len - I) * Typ.Arr_El.W));
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Register_Prefix
                 (Typ.Rec.E (I).Typ,
                  Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Register_Prefix;

   function Alloc_Signal_Memory (Vtype : Type_Acc) return Memory_Ptr
   is
      function To_Memory_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Memory_Ptr);
      M : System.Address;
   begin
      Areapools.Allocate (Global_Pool,
                          M, Sig_Size * Size_Type (Vtype.W), Sig_Size);
      return To_Memory_Ptr (M);
   end Alloc_Signal_Memory;

   function To_Memory_Ptr (S : Sub_Signal_Type) return Memory_Ptr is
   begin
      return Sig_Index (Signals_Table.Table (S.Base).Sig, S.Offs.Net_Off);
   end To_Memory_Ptr;

   function To_Memtyp (S : Sub_Signal_Type) return Memtyp is
   begin
      return (S.Typ, To_Memory_Ptr (S));
   end To_Memtyp;

   procedure Create_Signal (Idx : Signal_Index_Type)
   is
      E : Signal_Entry renames Signals_Table.Table (Idx);
      S : Ghdl_Signal_Ptr;
   begin
      E.Sig := Alloc_Signal_Memory (E.Typ);
      case E.Kind is
         when Mode_Guard =>
            Create_Guard_Signal (Idx);
         when Mode_Quiet =>
            S := Grt.Signals.Ghdl_Create_Quiet_Signal
              (To_Ghdl_Value_Ptr (To_Address (E.Val)), E.Time);
            Write_Sig (E.Sig, S);
            Register_Prefix (E.Pfx.Typ, To_Memory_Ptr (E.Pfx));
         when Mode_Stable =>
            S := Grt.Signals.Ghdl_Create_Stable_Signal
              (To_Ghdl_Value_Ptr (To_Address (E.Val)), E.Time);
            Write_Sig (E.Sig, S);
            Register_Prefix (E.Pfx.Typ, To_Memory_Ptr (E.Pfx));
         when Mode_Transaction =>
            -- Create_Implicit_Signal
            --  (E.Sig, E.Val, E.Time, E.Prefix, E.Kind);
            raise Internal_Error;
         when Mode_Delayed =>
            Create_Delayed_Signal (E.Sig, E.Val, To_Memory_Ptr (E.Pfx),
                                   E.Typ, E.Time);
         when Mode_Above =>
            raise Internal_Error;
         when Mode_Signal_User =>
            Create_User_Signal (Idx);
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
               --  E.Val will be assigned in Collapse_Signals.
            else
               Create_Signal (I);
            end if;
         end;
      end loop;
   end Create_Signals;

   procedure Set_Disconnect
     (Typ : Type_Acc; Sig : Memory_Ptr; Val : Std_Time) is
   begin
      case Typ.Kind is
         when Type_Scalars =>
            Grt.Signals.Ghdl_Signal_Set_Disconnect (Read_Sig (Sig), Val);
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Set_Disconnect (Typ.Arr_El,
                                  Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                                  Val);
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               Set_Disconnect (Typ.Rec.E (I).Typ,
                               Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off),
                               Val);
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Set_Disconnect;

   procedure Create_Disconnections is
   begin
      for I in Disconnect_Table.First .. Disconnect_Table.Last loop
         declare
            E : Disconnect_Entry renames Disconnect_Table.Table (I);
            S : Memtyp;
         begin
            S := To_Memtyp (E.Sig);
            Set_Disconnect (S.Typ, S.Mem, E.Val);
         end;
      end loop;
   end Create_Disconnections;

   --  Add an extra driver for undriven collapsed out signals.
   procedure Add_Extra_Driver_To_Signal (Sig : Memory_Ptr;
                                         Typ : Type_Acc;
                                         Init : Memory_Ptr;
                                         Off : Uns32;
                                         Vec : Nbr_Sources_Array) is
   begin
      case Typ.Kind is
         when Type_Scalars =>
            if Vec (Off).Nbr_Drivers = 0
              and then Vec (Off).Nbr_Conns = 0
            then
               Grt.Signals.Ghdl_Signal_Add_Extra_Driver
                 (Read_Sig (Sig), To_Ghdl_Value ((Typ, Init)));
            end if;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
               El : constant Type_Acc := Typ.Arr_El;
            begin
               for I in 1 .. Len loop
                  Add_Extra_Driver_To_Signal
                    (Sig_Index (Sig, (Len - I) * El.W), El,
                     Init + Size_Type (I - 1) * El.Sz,
                     Off + (Len - I) * El.W, Vec);
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Typ.Rec.E (I);
               begin
                  Add_Extra_Driver_To_Signal
                    (Sig_Index (Sig, E.Offs.Net_Off), E.Typ,
                     Init + E.Offs.Mem_Off, Off + E.Offs.Net_Off, Vec);
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Add_Extra_Driver_To_Signal;

   procedure Collapse_Signals is
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            E : Signal_Entry renames Signals_Table.Table (I);
         begin
            if E.Collapsed_By /= No_Signal_Index then
               if Get_Mode (E.Decl) in Iir_Out_Modes then
                  --  As an out connection creates a source, if a signal is
                  --  collapsed and has no source, an extra source needs to be
                  --  created.
                  Add_Extra_Driver_To_Signal
                    (E.Sig, E.Typ, E.Val, 0, E.Nbr_Sources.all);
               end if;
               --  The signal value is the value of the collapsed signal.
               if Get_Mode (E.Decl) in Iir_Out_Modes then
                  --  Keep default value.
                  Copy_Memory (Signals_Table.Table (E.Collapsed_By).Val,
                               E.Val,
                               E.Typ.Sz);
                  Exec_Write_Signal
                    (Signals_Table.Table (E.Collapsed_By).Sig,
                     (E.Typ, E.Val), Write_Signal_Driving_Value);
               end if;

               E.Val := Signals_Table.Table (E.Collapsed_By).Val;
            end if;
         end;
      end loop;
   end Collapse_Signals;

   type Connect_Mode is (Connect_Source, Connect_Effective);

   -- Add a driving value PORT to signal SIG, ie: PORT is a source for SIG.
   -- As a side effect, this connect the signal SIG with the port PORT.
   -- PORT is the formal, while SIG is the actual.
   procedure Connect (Dst : Memtyp;
                      Src : Memtyp;
                      Mode : Connect_Mode) is
   begin
      pragma Assert (Dst.Typ.Kind = Src.Typ.Kind);

      case Dst.Typ.Kind is
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Dst.Typ.Abound.Len;
               Etyp : constant Type_Acc := Dst.Typ.Arr_El;
            begin
               if Len /= Src.Typ.Abound.Len then
                  raise Internal_Error;
               end if;
               for I in 1 .. Len loop
                  Connect ((Etyp, Sig_Index (Dst.Mem, (Len - I) * Etyp.W)),
                           (Src.Typ.Arr_El,
                            Sig_Index (Src.Mem, (Len - I) * Etyp.W)),
                           Mode);
               end loop;
            end;
            return;
         when Type_Record =>
            for I in Dst.Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Dst.Typ.Rec.E (I);
               begin
                  Connect ((E.Typ, Sig_Index (Dst.Mem, E.Offs.Net_Off)),
                           (Src.Typ.Rec.E (I).Typ,
                            Sig_Index (Src.Mem, E.Offs.Net_Off)),
                           Mode);
               end;
            end loop;
         when Type_Logic
           | Type_Bit
           | Type_Discrete
           | Type_Float =>
            declare
               S, D : Ghdl_Signal_Ptr;
            begin
               S := Read_Sig (Src.Mem);
               D := Read_Sig (Dst.Mem);
               case Mode is
                  when Connect_Source =>
                     Grt.Signals.Ghdl_Signal_Add_Source (D, S);
                  when Connect_Effective =>
                     Grt.Signals.Ghdl_Signal_Effective_Value (D, S);
               end case;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Connect;

   procedure Create_Shadow_Signal (Sig : Memory_Ptr;
                                   Val : Memory_Ptr;
                                   Typ : Type_Acc)
   is
      S : Ghdl_Signal_Ptr;
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            S := Create_Scalar_Signal
              (Typ, To_Ghdl_Value_Ptr (To_Address (Val)));
            Write_Sig (Sig, S);
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Create_Shadow_Signal
                    (Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                     Val + Size_Type (I - 1) * Typ.Arr_El.Sz,
                     Typ.Arr_El);
               end loop;
            end;
         when Type_Record =>
            for I in Typ.Rec.E'Range loop
               declare
                  E : Rec_El_Type renames Typ.Rec.E (I);
               begin
                  Create_Shadow_Signal (Sig_Index (Sig, E.Offs.Net_Off),
                                        Val + E.Offs.Mem_Off,
                                        E.Typ);
               end;
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Shadow_Signal;

   type Convert_Mode is (Convert_In, Convert_Out);

   type Convert_Instance_Type is record
      Mode : Convert_Mode;
      Inst : Synth_Instance_Acc;
      Func : Iir;
      Src_Sig : Memory_Ptr;
      Src_Typ : Type_Acc;
      Dst_Sig : Memory_Ptr;
      Dst_Typ : Type_Acc;
   end record;

   type Convert_Instance_Acc is access Convert_Instance_Type;

   procedure Conversion_Proc (Data : System.Address) is
      Conv : Convert_Instance_Type;
      pragma Import (Ada, Conv);
      for Conv'Address use Data;

      Val : Memtyp;
      Dst : Memtyp;
      Dst_Val : Valtyp;

      Expr_Marker, Inst_Marker : Mark_Type;
   begin
      Areapools.Mark (Inst_Marker, Process_Pool);
      Mark_Expr_Pool (Expr_Marker);
      Instance_Pool := Process_Pool'Access;
      Current_Process := null;

      Val := Create_Memory (Conv.Src_Typ);
      case Conv.Mode is
         when Convert_In =>
            Exec_Read_Signal (Conv.Src_Sig, Val, Read_Signal_Effective_Value);
         when Convert_Out =>
            Exec_Read_Signal (Conv.Src_Sig, Val, Read_Signal_Driving_Value);
      end case;

      Dst_Val := Create_Value_Memory (Val, Current_Pool);
      Dst_Val := Synth_Association_Conversion
        (Conv.Inst, Conv.Func, Dst_Val, Conv.Dst_Typ);
      if Dst_Val = No_Valtyp then
         Grt.Errors.Fatal_Error;
      end if;

      Convert_Type_Width (Dst_Val.Typ);
      pragma Assert (Dst_Val.Typ.Wkind = Wkind_Sim);
      Dst := Synth.Vhdl_Expr.Get_Value_Memtyp (Dst_Val);

      case Conv.Mode is
         when Convert_In =>
            Exec_Write_Signal
              (Conv.Dst_Sig, Dst, Write_Signal_Effective_Value);
         when Convert_Out =>
            Exec_Write_Signal
              (Conv.Dst_Sig, Dst, Write_Signal_Driving_Value);
      end case;

      Release_Expr_Pool (Expr_Marker);
      Areapools.Release (Inst_Marker, Process_Pool);
      Instance_Pool := null;
   end Conversion_Proc;

   function Get_Leftest_Signal (Sig : Memory_Ptr; Typ : Type_Acc)
                               return Ghdl_Signal_Ptr is
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete
           | Type_Float =>
            return Read_Sig (Sig);
         when Type_Vector
           | Type_Array =>
            return Get_Leftest_Signal
              (Sig_Index (Sig, (Typ.Abound.Len - 1) * Typ.Arr_El.W),
               Typ.Arr_El);
         when Type_Record =>
            declare
               E : Rec_El_Type renames Typ.Rec.E (1);
            begin
               return Get_Leftest_Signal
                 (Sig_Index (Sig, E.Offs.Net_Off), E.Typ);
            end;
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
      Src_Left := Get_Leftest_Signal (Conv.Src_Sig, Conv.Src_Typ);
      Src_Len := Ghdl_Index_Type (Conv.Src_Typ.W);

      Dst_Left := Get_Leftest_Signal (Conv.Dst_Sig, Conv.Dst_Typ);
      Dst_Len := Ghdl_Index_Type (Conv.Dst_Typ.W);

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

   procedure Create_Connect (C : Connect_Entry) is
   begin
      if C.Drive_Actual then
         declare
            Out_Conv : constant Node := Get_Formal_Conversion (C.Assoc);
            Csig : Memory_Ptr;
            Cval : Memory_Ptr;
            Ctyp : Type_Acc;
            Form, Form2 : Memtyp;
         begin
            Form := To_Memtyp (C.Formal);

            if Out_Conv /= Null_Node then
               --  From formal to actual.
               Ctyp := C.Actual.Typ;
               Csig := Alloc_Signal_Memory (Ctyp);
               Cval := Alloc_Memory (Ctyp, Global_Pool'Access);
               Create_Shadow_Signal (Csig, Cval, Ctyp);
               Form2 := (Ctyp, Csig);
               Add_Conversion
                 (new Convert_Instance_Type'(Mode => Convert_Out,
                                             Inst => C.Assoc_Inst,
                                             Func => Out_Conv,
                                             Src_Sig => Form.Mem,
                                             Src_Typ => Form.Typ,
                                             Dst_Sig => Form2.Mem,
                                             Dst_Typ => Form2.Typ));
            else
               Form2 := Form;
            end if;

            --  LRM93 12.6.2
            --  A signal is said to be active [...] if one of its source
            --  is active.
            Connect (To_Memtyp (C.Actual), Form2, Connect_Source);
         end;
      end if;

      if C.Drive_Formal then
         declare
            In_Conv : constant Node := Get_Actual_Conversion (C.Assoc);
            Csig : Memory_Ptr;
            Cval : Memory_Ptr;
            Ctyp : Type_Acc;
            Act, Act2 : Memtyp;
         begin
            Act := To_Memtyp (C.Actual);

            if In_Conv /= Null_Node then
               Ctyp := C.Formal.Typ;
               Csig := Alloc_Signal_Memory (Ctyp);
               Cval := Alloc_Memory (Ctyp, Global_Pool'Access);
               Create_Shadow_Signal (Csig, Cval, Ctyp);
               Act2 := (Ctyp, Csig);
               Add_Conversion
                 (new Convert_Instance_Type'(Mode => Convert_In,
                                             Inst => C.Assoc_Inst,
                                             Func => In_Conv,
                                             Src_Sig => Act.Mem,
                                             Src_Typ => Act.Typ,
                                             Dst_Sig => Act2.Mem,
                                             Dst_Typ => Act2.Typ));
            else
               Act2 := Act;
            end if;
            Connect (To_Memtyp (C.Formal), Act2, Connect_Effective);
         end;
      end if;
   end Create_Connect;

   procedure Signal_Associate_Cst (Sig : Memory_Ptr;
                                   Typ : Type_Acc;
                                   Val : Memory_Ptr) is
   begin
      case Typ.Kind is
         when Type_Bit
           | Type_Logic
           | Type_Discrete =>
            declare
               S : constant Ghdl_Signal_Ptr := Read_Sig (Sig);
               V : Value_Union;
            begin
               case S.Mode is
                  when Mode_B1 =>
                     V.B1 := Ghdl_B1'Val (Read_U8 (Val));
                     S.Value_Ptr.B1 := V.B1;
                     S.Driving_Value.B1 := V.B1;
                  when Mode_E8 =>
                     V.E8 := Read_U8 (Val);
                     S.Value_Ptr.E8 := V.E8;
                     S.Driving_Value.E8 := V.E8;
                  when Mode_I32 =>
                     V.I32 := Read_I32 (Val);
                     S.Value_Ptr.I32 := V.I32;
                     S.Driving_Value.I32 := V.I32;
                  when Mode_I64 =>
                     V.I64 := Read_I64 (Val);
                     S.Value_Ptr.I64 := V.I64;
                     S.Driving_Value.I64 := V.I64;
                  when others =>
                     raise Internal_Error;
               end case;
            end;
         when Type_Vector
           | Type_Array =>
            declare
               Len : constant Uns32 := Typ.Abound.Len;
            begin
               for I in 1 .. Len loop
                  Signal_Associate_Cst
                    (Sig_Index (Sig, (Len - I) * Typ.Arr_El.W),
                     Typ.Arr_El,
                     Val + Size_Type (I - 1) * Typ.Arr_El.Sz);
               end loop;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Signal_Associate_Cst;

   procedure Create_Connects is
   begin
      for I in Connect_Table.First .. Connect_Table.Last loop
         declare
            C : Connect_Entry renames Connect_Table.Table (I);
            Val : Valtyp;
            Marker : Mark_Type;
         begin
            if not C.Collapsed then
               if C.Actual.Base /= No_Signal_Index then
                  Create_Connect (C);
               elsif Get_Expr_Staticness (Get_Actual (C.Assoc)) >= Globally
               then
                  Mark_Expr_Pool (Marker);
                  Val := Synth.Vhdl_Expr.Synth_Expression_With_Type
                    (C.Assoc_Inst, Get_Actual (C.Assoc), C.Formal.Typ);
                  Val := Strip_Alias_Const (Val);
                  Signal_Associate_Cst
                    (Sig_Index (Signals_Table.Table (C.Formal.Base).Sig,
                                C.Formal.Offs.Net_Off),
                     C.Formal.Typ,
                     Val.Val.Mem);
                  Release_Expr_Pool (Marker);
               end if;
            end if;
         end;
      end loop;
   end Create_Connects;

   procedure Create_Terminals
   is
   begin
      for I in Terminal_Table.First .. Terminal_Table.Last loop
         declare
            T : Terminal_Entry renames Terminal_Table.Table (I);
         begin
            --  Allocate Ref_Val and set it to 0.
            pragma Assert (T.Across_Typ.Kind = Type_Float);
            T.Ref_Val := Alloc_Memory (T.Across_Typ, Global_Pool'Access);
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
                  Q.Val := Alloc_Memory (Q.Typ, Global_Pool'Access);
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

                     Q.Val := Alloc_Memory (Q.Typ, Global_Pool'Access);
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

      pragma Assert (Is_Expr_Pool_Empty);

      Create_Signals;
      pragma Assert (Is_Expr_Pool_Empty);
      Create_Connects;
      Create_Disconnections;
      pragma Assert (Is_Expr_Pool_Empty);
      Create_Processes;
      pragma Assert (Is_Expr_Pool_Empty);
      Create_Terminals;
      Create_Quantities;
      pragma Assert (Is_Expr_Pool_Empty);
      Collapse_Signals;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Allow Synth_Expression to handle signals.
      --  This is done after elaboration as signals are available only after
      --  elaboration.
      Synth.Vhdl_Expr.Hook_Signal_Expr := Hook_Signal_Expr'Access;
      Synth.Vhdl_Expr.Hook_Event_Attribute := Exec_Event_Attribute'Access;
      Synth.Vhdl_Expr.Hook_Active_Attribute := Exec_Active_Attribute'Access;
      Synth.Vhdl_Expr.Hook_Last_Value_Attribute :=
        Exec_Last_Value_Attribute'Access;
      Synth.Vhdl_Expr.Hook_Last_Event_Attribute :=
        Exec_Last_Event_Attribute'Access;
      Synth.Vhdl_Expr.Hook_Last_Active_Attribute :=
        Exec_Last_Active_Attribute'Access;

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

      Elab.Debugger.Error_Hook := Grt.Errors.Fatal_Error'Access;

      pragma Assert (Areapools.Is_Empty (Expr_Pool));

      if Flag_Debug_Elab then
         Elab.Debugger.Debug_Elab (Vhdl_Elab.Top_Instance);
      end if;

      Ok := Grt.Main.Run_Elab;
      if not Ok then
         return;
      end if;

      pragma Assert (Areapools.Is_Empty (Expr_Pool));
      pragma Assert (Areapools.Is_Empty (Process_Pool));

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

         Grt.Errors.Set_Error_Stream (Grt.Stdio.stdout);
         Assertion_Report_Handler := Assertion_Report_Msg'Access;

         pragma Assert (Areapools.Is_Empty (Expr_Pool));
         pragma Assert (Areapools.Is_Empty (Process_Pool));

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

      Grt.Main.Run_Finish (Status);
   exception
--      when Debugger_Quit =>
--         null;
      when Simulation_Finished =>
         null;
   end Simulation;
end Simul.Vhdl_Simul;
