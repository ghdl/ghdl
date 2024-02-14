--  Statements synthesis for verilog
--  Copyright (C) 2021 Tristan Gingold
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

with Ada.Unchecked_Deallocation;
with Types; use Types;

with Netlists; use Netlists;
with Netlists.Utils;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;

with Verilog.Types; use Verilog.Types;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Sem_Utils;
with Verilog.Allocates;
with Verilog.Vpi;

with Elab.Memtype;

with Synth.Errors; use Synth.Errors;
with Synth.Verilog_Errors; use Synth.Verilog_Errors;
with Synth.Verilog_Values; use Synth.Verilog_Values;
with Synth.Verilog_Exprs; use Synth.Verilog_Exprs;
with Synth.Verilog_Environment; use Synth.Verilog_Environment;
use Synth.Verilog_Environment.Env;
with Synth.Verilog_Sources; use Synth.Verilog_Sources;

package body Synth.Verilog_Stmts is
   --  True if synthesizing for an initial statement (at t=0)
   Is_Initial : Boolean := False;

   --  Instance to be used during a VPI call.
   Current_Vpi_Inst : Synth_Instance_Acc;

   procedure Synth_Stmt (Inst : Synth_Instance_Acc; N : Node);

   procedure Synth_If (Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      True_Stmt : constant Node := Get_True_Stmt (N);
      False_Stmt : constant Node := Get_False_Stmt (N);
      Cond : Valtyp;
      Cond_Net : Net;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      Cond := Synth_Condition (Inst, Get_Condition (N));
      if Is_Static (Cond) then
         case Read_Logic (Cond.Mem) is
            when V_0 =>
               Synth_Stmt (Inst, False_Stmt);
            when V_1 =>
               Synth_Stmt (Inst, True_Stmt);
            when V_X
              | V_Z =>
               --  TODO
               raise Internal_Error;
         end case;
      else
         Push_Phi;
         Synth_Stmt (Inst, True_Stmt);
         Pop_Phi (Phi_True);

         Push_Phi;
         Synth_Stmt (Inst, False_Stmt);
         Pop_Phi (Phi_False);

         Cond_Net := Get_Net (Ctxt, Cond);
         Merge_Phis (Ctxt, Cond_Net, Phi_True, Phi_False, Get_Location (N));
      end if;
   end Synth_If;

   --  Synthesize case item N.
   --  CASE_STMT is the enclosing statement.
   --  DEFAULT is the default case item to be executed if none of the items
   --   is selected.  The default case can happen anywhere, so it is just
   --   collected.
   --  SEL is the case expression.
   procedure Synth_Case_Item (Inst : Synth_Instance_Acc;
                              Case_Stmt : Node;
                              N : Node;
                              Default : Node;
                              Sel : Net)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Next : Node;
      Stmt : Node;
      Expr : Valtyp;
      Cond : Net;
      Phi_This : Phi_Type;
      Phi_Next : Phi_Type;
   begin
      Next := Get_Chain (N);

      if Get_Kind (N) = N_Default_Case_Item then
         --  There is only one default case.
         pragma Assert (Default = Null_Node);

         if Next /= Null_Node then
            --  Handle it last.
            Synth_Case_Item (Inst, Case_Stmt, Next, N, Sel);
         else
            --  It is the last one
            Synth_Stmt (Inst, Get_Statement (N));
         end if;
      else
         --  Extract the next case and the statement for this case.
         if Get_Same_Case_Flag (N) then
            --  Next share the same statements.
            while Get_Same_Case_Flag (Next) loop
               Next := Get_Chain (Next);
            end loop;
            Stmt := Get_Statement (Next);
            Next := Get_Chain (Next);
         else
            Stmt := Get_Statement (N);
         end if;

         --  If this is the last possible statement to be executed (because
         --  there is no next one and no default one) *AND* if the case is
         --  full, no need to do comparaisons.  Just execute the statement
         --  as if it was the default one.
         if Next = Null_Node
           and then Default = Null_Node
           and then Get_Attribute_Full (Case_Stmt)
         then
            Synth_Stmt (Inst, Stmt);
            return;
         end if;

         --  The statement.
         Push_Phi;
         Synth_Stmt (Inst, Stmt);
         Pop_Phi (Phi_This);

         --  The statement for the next case (or default case if any).
         Push_Phi;
         if Next /= Null_Node then
            Synth_Case_Item (Inst, Case_Stmt, Next, Default, Sel);
         elsif Default /= Null_Node then
            Synth_Stmt (Inst, Get_Statement (Default));
         else
            --  No default.
            null;
         end if;
         Pop_Phi (Phi_Next);

         --  Do the comparaison of the expression with all the cases,
         --  and select the branch.
         declare
            N1 : Node;
            C1 : Net;
         begin
            N1 := N;
            Cond := No_Net;
            loop
               Expr := Synth_Expression (Inst, Get_Expression (N1));
               C1 := Build_Compare (Ctxt, Id_Eq, Get_Net (Ctxt, Expr), Sel);
               Set_Location (C1, N1);
               if Cond = No_Net then
                  Cond := C1;
               else
                  Cond := Build_Dyadic (Ctxt, Id_Or, Cond, C1);
                  Set_Location (Cond, N1);
               end if;
               exit when not Get_Same_Case_Flag (N1);
               N1 := Get_Chain (N1);
            end loop;
         end;
         Merge_Phis (Ctxt, Cond, Phi_This, Phi_Next, Get_Location (N));
      end if;
   end Synth_Case_Item;

   procedure Synth_Case (Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Item : Node;
      Sel : Valtyp;
      Sel_Net : Net;
   begin
      Sel := Synth_Expression (Inst, Get_Expression (N));
      if Is_Static (Sel) then
         --  TODO
         null;
      end if;

      --  TODO: parallel case

      Sel_Net := Get_Net (Ctxt, Sel);

      Item := Get_Case_Items (N);

      Synth_Case_Item (Inst, N, Item, Null_Node, Sel_Net);
   end Synth_Case;

   procedure Synth_For (Inst : Synth_Instance_Acc; N : Node)
   is
      Cond : constant Node := Get_Condition (N);
      Stmt : constant Node := Get_Statement (N);
      Step : constant Node := Get_Step_Assign (N);
      Val : Valtyp;
   begin
      --  TODO: declaration.
      Synth_Stmt (Inst, Get_For_Initialization (N));

      --  TODO: limit the number of iterations ?
      loop
         Val := Synth_Condition (Inst, Cond);

         if not Is_Static (Val) then
            Error_Msg_Synth
              (Inst, +Cond, "result of 'for' condition is not static");
            exit;
         end if;

         case Read_Logic (Val.Mem) is
            when V_0 =>
               exit;
            when V_1 =>
               Synth_Stmt (Inst, Stmt);
            when V_X
              | V_Z =>
               exit;
         end case;

         Synth_Stmt (Inst, Step);
      end loop;
   end Synth_For;

   procedure Initial_Clear (Mt : Memtyp)
   is
      use Elab.Memtype;
   begin
      case Get_Kind (Mt.Typ) is
         when N_Logic_Type =>
            To_Logic_Ptr (To_Address (Mt.Mem)).all := V_X;
         when N_Array_Cst =>
            declare
               El_Typ : constant Node := Get_Type_Element_Type (Mt.Typ);
               El_Sz : constant Size_Type :=
                 Size_Type (Verilog.Allocates.Get_Storage_Size (El_Typ));
               Arr_Len : constant Int32 :=
                 Verilog.Sem_Utils.Compute_Length (Mt.Typ);
               Off : Size_Type;
            begin
               Off := 0;
               for I in 0 .. Arr_Len - 1 loop
                  Initial_Clear ((Mt.Mem + Off, El_Typ));
                  Off := Off + El_Sz;
               end loop;
            end;
         when N_Log_Packed_Array_Cst =>
            Set_X (To_Logvec_Ptr (To_Address (Mt.Mem)),
                   Get_Type_Width (Mt.Typ));
         when others =>
            Error_Kind ("initial_clear", Mt.Typ);
      end case;
   end Initial_Clear;

   procedure Assign_Initial_Target (Inst : Synth_Instance_Acc;
                                    Targ : Node;
                                    Res : out Memtyp;
                                    Bit_Off : out Bit_Offset)
   is
      use Elab.Memtype;
      Mem : Memory_Ptr;
      Dest : Valtyp;
      Off : Name_Offsets;
      Doff : Net;
   begin
      Synth_Name (Inst, Targ, Dest, Doff, Off);
      --  pragma Assert (Off.Mem_Off = 0);
      --  pragma Assert (Off.Bit_Off = 0);
      pragma Assert (Doff = No_Net);
      pragma Assert (Dest.Kind = Value_Wire);

      case Get_Kind (Dest.W) is
         when Wire_Unset =>
            Set_Kind (Dest.W, Wire_Variable);
            --  FIXME: allocate on initial pool.
            Mem := Allocate_Memory (Inst, Dest.Typ);
            Res := (Mem, Dest.Typ);
            Initial_Clear (Res);
            Phi_Assign_Static (Dest.W, Res);
         when Wire_Variable =>
            pragma Assert (Is_Static_Wire (Dest.W));
            Res := Get_Static_Wire (Dest.W);
         when others =>
            raise Internal_Error;
      end case;

      Res.Mem := Res.Mem + Off.Mem_Off;
      Bit_Off := Off.Bit_Off;
   end Assign_Initial_Target;

   procedure Assign_Initial (Inst : Synth_Instance_Acc;
                             Targ : Node;
                             Val : Valtyp)
   is
      use Elab.Memtype;
      Targ_Typ : constant Node := Get_Expr_Type (Targ);
      Dest : Memtyp;
      Bit_Off : Bit_Offset;
      Change : Boolean;
   begin
      case Get_Kind (Targ) is
         when N_Part_Select_Cst =>
            raise Internal_Error;
         when N_Name
            | N_Indexed_Name =>
            Assign_Initial_Target (Inst, Targ, Dest, Bit_Off);
         when others =>
            Error_Kind ("assign_initial", Targ);
      end case;

      case Get_Kind (Targ_Typ) is
         when N_Logic_Type =>
            Write_U8 (Dest.Mem, Read_U8 (Val.Mem));
         when N_Log_Packed_Array_Cst =>
            Compute_Part_Insert
              (To_Logvec_Ptr (To_Address (Dest.Mem)),
               Bit_Off,
               To_Logvec_Ptr (To_Address (Val.Mem)), 0,
               Get_Type_Width (Targ_Typ),
               Change);
         when others =>
            Error_Kind ("assign_initial(type)", Targ_Typ);
      end case;
   end Assign_Initial;

   --  Assign value N to TARG.
   --  BLOCKING is true for '<=', false for 'assign' and '='.
   --  TODO: handle constant assign.
   procedure Synth_Assign_Single
     (Inst : Synth_Instance_Acc; Blocking : Boolean; Targ : Node; Val : Valtyp)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Dest : Valtyp;
      Doff : Net;
      Off : Name_Offsets;
      R : Net;
      N : Net;
      Wd : Uns32;
   begin
      if Is_Initial then
         --  Assignement within an 'initial' statement.
         --  if Doff /= No_Net then
         --     Error_Msg_Synth
         --       (+Targ, "target of initial statement must be static");
         --     return;
         --  end if;
         if not Is_Static (Val) then
            Error_Msg_Synth
              (Inst, Targ, "value of an initial assignment must be static");
            return;
         end if;

         pragma Assert (Blocking);
         Assign_Initial (Inst, Targ, Val);
      else
         Synth_Name (Inst, Targ, Dest, Doff, Off);

         case Get_Kind (Dest.W) is
            when Wire_Unset =>
               if Blocking then
                  Set_Kind (Dest.W, Wire_Variable);
               else
                  Set_Kind (Dest.W, Wire_Signal);
               end if;
            when Wire_Signal =>
               if Blocking then
                  Error_Msg_Synth
                    (Inst, Targ,
                     "mixing blocking and non-blocking assignments");
               end if;
            when Wire_Variable =>
               if not Blocking then
                  Error_Msg_Synth
                    (Inst, Targ,
                     "mixing blocking and non-blocking assignments");
               end if;
            when Wire_None
              | Wire_Enable
              | Wire_Input
              | Wire_Output
              | Wire_Inout =>
               null;
         end case;
         if Doff /= No_Net then
            Wd := Get_Type_Bitwidth (Get_Expr_Type (Get_Name (Targ)));
            R := Get_Current_Assign_Value (Ctxt, Dest.W, 0, Wd);
            pragma Assert (Off.Net_Off = 0);
            N := Get_Net (Ctxt, Val);
            N := Build_Dyn_Insert (Ctxt, R, N, Doff, 0);
            Set_Location (N, Targ);
            Phi_Assign_Net (Ctxt, Dest.W, N, Off.Net_Off);
         else
            N := Get_Net (Ctxt, Val);
            Phi_Assign_Net (Ctxt, Dest.W, N, Off.Net_Off);
         end if;
      end if;
   end Synth_Assign_Single;

   procedure Synth_Blocking_Assign_Vpi
     (Frame : Frame_Ptr; Target : Node; Value : Data_Ptr; Typ : Node) is
   begin
      pragma Assert (Frame = null);
      Assign_Initial (Current_Vpi_Inst, Target,
                      (Kind => Value_Memory,
                       Typ => Typ,
                       Mem => Elab.Memtype.To_Memory_Ptr (Value)));
   end Synth_Blocking_Assign_Vpi;

   procedure Synth_Assign (Inst : Synth_Instance_Acc;
                           Blocking : Boolean;
                           Targ : Node;
                           Expr : Valtyp) is
   begin
      case Get_Kind (Targ) is
         when N_Name
            | N_Bit_Select
            | N_Indexed_Name
            | N_Part_Select_Cst =>
            Synth_Assign_Single (Inst, Blocking, Targ, Expr);
         when N_Concatenation =>
            declare
               Expr_Off : Uns32;
               Wd : Uns32;
               El, E : Node;
               E_Typ : Node;
            begin
               Expr_Off := Uns32 (Get_Type_Width (Expr.Typ));
               El := Get_Expressions (Targ);
               while El /= Null_Node loop
                  E := Get_Expression (El);
                  E_Typ := Get_Expr_Type (E);
                  Wd := Get_Type_Bitwidth (E_Typ);
                  Expr_Off := Expr_Off - Wd;

                  --  Synth target.
                  Synth_Assign (Inst, Blocking, E,
                                Synth_Extract (Inst, Expr, Expr_Off, E_Typ));

                  El := Get_Chain (El);
               end loop;
               pragma Assert (Expr_Off = 0);
            end;
         when others =>
            Error_Kind ("synth_assign", Targ);
      end case;
   end Synth_Assign;

   procedure Synth_Proc_Assign
     (Inst : Synth_Instance_Acc; N : Node; Blocking : Boolean)
   is
      Expr : Valtyp;
   begin
      Expr := Synth_Expression (Inst, Get_Expression (N));
      Synth_Assign (Inst, Blocking, Get_Lvalue (N), Expr);
   end Synth_Proc_Assign;

   procedure Synth_Seq_Block (Inst : Synth_Instance_Acc; N : Node)
   is
      Stmt : Node;
   begin
      if Get_Identifier (N) /= No_Name_Id then
         raise Internal_Error;
      end if;
      if Get_Block_Item_Declaration_Chain (N) /= Null_Node then
         raise Internal_Error;
      end if;
      Stmt := Get_Statements_Chain (N);
      while Stmt /= Null_Node loop
         Synth_Stmt (Inst, Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Synth_Seq_Block;

   procedure Synth_System_Call (Inst : Synth_Instance_Acc; N : Node) is
   begin
      if not Is_Initial then
         Warning_Msg_Synth (+N, "system call to %i is ignored", (1 => +N));
         return;
      end if;
      Current_Vpi_Inst := Inst;
      Verilog.Vpi.Call_Systask_Calltf (null, Get_Sys_Tf_Id (N), N);
      Current_Vpi_Inst := null;
   end Synth_System_Call;

   procedure Synth_Stmt (Inst : Synth_Instance_Acc; N : Node) is
   begin
      if N = Null_Node then
         return;
      end if;

      case Get_Kind (N) is
         when N_If =>
            Synth_If (Inst, N);
         when N_Noblk_Assign =>
            Synth_Proc_Assign (Inst, N, False);
         when N_Blocking_Assign =>
            Synth_Proc_Assign (Inst, N, True);
         when N_Seq_Block =>
            Synth_Seq_Block (Inst, N);
         when N_Case =>
            Synth_Case (Inst, N);
         when N_For =>
            Synth_For (Inst, N);
         when N_Subroutine_Call_Stmt =>
            declare
               Call : constant Node := Get_Call (N);
            begin
               case Get_Kind (Call) is
                  when N_System_Call =>
                     Synth_System_Call (Inst, Call);
                  when others =>
                     Error_Kind ("synth_stmt(call)", Call);
               end case;
            end;
         when others =>
            Error_Kind ("synth_stmt", N);
      end case;
   end Synth_Stmt;

   procedure Synth_Comb_Process (Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
   begin
      Push_Phi;
      Synth_Stmt (Inst, N);
      Pop_And_Merge_Phi (Ctxt, Get_Location (N));
   end Synth_Comb_Process;

   procedure Synth_FF_Single_Process_Inner (Inst : Synth_Instance_Acc;
                                            Cond : Net;
                                            Is_Pos : Boolean;
                                            Loc : Node;
                                            Stmt : Node;
                                            Proc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Edge : Net;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      if Is_Pos then
         Edge := Build_Posedge (Ctxt, Cond);
      else
         Edge := Build_Negedge (Ctxt, Cond);
      end if;
      Set_Location (Edge, Loc);

      Push_Phi;
      Synth_Stmt (Inst, Stmt);
      Pop_Phi (Phi_True);

      Push_Phi;
      Pop_Phi (Phi_False);

      Merge_Phis (Ctxt, Edge, Phi_True, Phi_False, Get_Location (Proc));
   end Synth_FF_Single_Process_Inner;

   procedure Synth_FF_Single_Process (Inst : Synth_Instance_Acc;
                                      Ev : Node;
                                      Stmt : Node;
                                      Proc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Edge_Expr : constant Node := Get_Expression (Ev);
      Is_Pos : constant Boolean := Nkinds_Edge (Get_Kind (Ev)) = N_Posedge;
      Cond : Net;
   begin
      Push_Phi;

      Cond := Get_Net (Ctxt, Synth_Expression (Inst, Edge_Expr));

      Synth_FF_Single_Process_Inner (Inst, Cond, Is_Pos, Ev, Stmt, Proc);

      Pop_And_Merge_Phi (Ctxt, Get_Location (Proc));
   end Synth_FF_Single_Process;

   function Count_Edge_Events (Ev : Node) return Nat32 is
   begin
      case Get_Kind (Ev) is
         when Nkinds_Edge =>
            return 1;
         when N_Or =>
            declare
               L, R : Node;
               Res : Nat32;
            begin
               L := Ev;
               Res := 0;
               loop
                  R := Get_Right (L);
                  L := Get_Left (L);
                  if Get_Kind (R) in Nkinds_Edge then
                     Res := Res + 1;
                  else
                     return Count_Edge_Events (L)
                       + Count_Edge_Events (R)
                       + Res;
                  end if;
                  if Get_Kind (L) /= N_Or then
                     return Res + Count_Edge_Events (L);
                  end if;
               end loop;
            end;
         when others =>
            Error_Kind ("count_edge_events", Ev);
      end case;
   end Count_Edge_Events;

   --  Describe the event control.
   type Edge_Entry is record
      --  The HDL expression.
      Ev : Node;

      --  True for posedge, False for negedge.
      Is_Pos : Boolean;

      --  The net of the edge operator.
      N : Net;
   end record;

   type Edge_Array is array (Nat32 range <>) of Edge_Entry;
   type Edge_Arr_Acc is access Edge_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Edge_Array, Edge_Arr_Acc);

   procedure Fill_Edge_Events
     (Evs : in out Edge_Array; Pos : in out Nat32; Ev : Node) is
   begin
      case Get_Kind (Ev) is
         when Nkinds_Edge =>
            Pos := Pos + 1;
            Evs (Pos) := (Ev, False, No_Net);
         when N_Or =>
            declare
               L, R : Node;
            begin
               L := Ev;
               loop
                  R := Get_Right (L);
                  L := Get_Left (L);
                  if Get_Kind (R) in Nkinds_Edge then
                     Pos := Pos + 1;
                     Evs (Pos) := (R, False, No_Net);
                  else
                     Fill_Edge_Events (Evs, Pos, L);
                     Fill_Edge_Events (Evs, Pos, R);
                     return;
                  end if;
                  if Get_Kind (L) /= N_Or then
                     Fill_Edge_Events (Evs, Pos, L);
                     return;
                  end if;
               end loop;
            end;
         when others =>
            Error_Kind ("fill_edge_events", Ev);
      end case;
   end Fill_Edge_Events;

   function Is_Same_Cond (Ev : Edge_Entry; Cond_Net : Net) return Boolean
   is
      use Netlists.Utils;
      N : Net;
   begin
      if Ev.Is_Pos then
         N := Cond_Net;
      else
         declare
            Parent : constant Instance := Get_Net_Parent (Cond_Net);
         begin
            if Get_Id (Parent) = Id_Not then
               N := Get_Input_Net (Parent, 0);
            else
               --  Different edge.
               return False;
            end if;
         end;
      end if;
      return Same_Net (Ev.N, N);
   end Is_Same_Cond;

   --  Synthesize a complex always process.
   --  We need to extract the edge corresponding to the clock, so we have to
   --  match each event with the condition of each 'if' statement.  The last
   --  event is for the last 'else', which is the clock.
   procedure Synth_Complex_Edge_Process_If (Inst : Synth_Instance_Acc;
                                            Stmt : Node;
                                            Evs : in out Edge_Array;
                                            Proc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Cond : Node;
      Cond_Net : Net;
      False_Stmt : Node;
      Phi_True : Phi_Type;
      Phi_False : Phi_Type;
   begin
      --  Follow the if statements.
      if Get_Kind (Stmt) /= N_If then
         raise Internal_Error;
      end if;

      Cond := Get_Condition (Stmt);
      Cond_Net := Get_Net (Ctxt, Synth_Expression (Inst, Cond));

      --  Find the condition in EVS.
      declare
         Pos : Nat32;
      begin
         Pos := 0;
         for I in Evs'Range loop
            if Is_Same_Cond (Evs (I), Cond_Net) then
               Pos := I;
               exit;
            end if;
         end loop;
         if Pos = 0 then
            --  Not found.
            Error_Msg_Synth
              (Inst, +Cond, "cannot find corresponding edge expression");
            return;
         end if;

         --  Remove the matching event from the list.
         Evs (Pos) := Evs (Evs'Last);
      end;

      Push_Phi;
      Synth_Stmt (Inst, Get_True_Stmt (Stmt));
      Pop_Phi (Phi_True);

      Push_Phi;
      False_Stmt := Get_False_Stmt (Stmt);
      if Evs'Last = 2 then
         Synth_FF_Single_Process_Inner
           (Inst, Evs (1).N, Evs (1).Is_Pos, Evs (1).Ev, False_Stmt, Proc);
      else
         Synth_Complex_Edge_Process_If
           (Inst, False_Stmt, Evs (1 .. Evs'Last - 1), Proc);
      end if;
      Pop_Phi (Phi_False);

      Merge_Phis (Ctxt, Cond_Net, Phi_True, Phi_False, Get_Location (Stmt));
   end Synth_Complex_Edge_Process_If;

   --  Complex always process controlled by edge events EVS.
   procedure Synth_Complex_Edge_Process_1 (Inst : Synth_Instance_Acc;
                                           Stmt : Node;
                                           Ev : Node;
                                           Evs : in out Edge_Array;
                                           Proc : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Num : Nat32;
   begin
      --  Fill the array.
      Num := Evs'First - 1;
      Fill_Edge_Events (Evs, Num, Ev);
      pragma Assert (Num = Evs'Last);

      --  Compute the nets for the events.
      for I in Evs'Range loop
         declare
            Ev : constant Node := Evs (I).Ev;
            Edge_Expr : constant Node := Get_Expression (Ev);
         begin
            Evs (I).Is_Pos := Get_Kind (Ev) = N_Posedge;
            Evs (I).N := Get_Net (Ctxt, Synth_Expression (Inst, Edge_Expr));
         end;
      end loop;

      Push_Phi;

      Synth_Complex_Edge_Process_If (Inst, Stmt, Evs, Proc);

      Pop_And_Merge_Phi (Ctxt, Get_Location (Proc));
   end Synth_Complex_Edge_Process_1;

   --  Complex always process controlled by (edge) event EV.
   procedure Synth_Complex_Edge_Process
     (Inst : Synth_Instance_Acc; Proc : Node; Stmt : Node; Ev : Node)
   is
      Nbr_Events : constant Nat32 := Count_Edge_Events (Ev);
      Stmt1 : Node;
   begin
      --  Check statement: expect an 'if' statement (maybe within a begin-end
      --  block).
      Stmt1 := Stmt;
      if Get_Kind (Stmt1) = N_Seq_Block then
         pragma Assert (Get_Block_Item_Declaration_Chain (Stmt1) = Null_Node);
         Stmt1 := Get_Statements_Chain (Stmt1);
      end if;
      if Get_Kind (Stmt1) /= N_If
        or else Get_Chain (Stmt1) /= Null_Node
      then
         Error_Msg_Synth
           (Inst, Stmt, "edged always statement must contain an if statement");
         return;
      end if;

      --  1. Count number of event element
      --  2. Allocate and populate the array (TODO: duplicates ?)
      if Nbr_Events < 8 then
         declare
            Evs : Edge_Array (1 .. 8);
         begin
            Synth_Complex_Edge_Process_1
              (Inst, Stmt1, Ev, Evs (1 .. Nbr_Events), Proc);
         end;
      else
         declare
            Evs : Edge_Arr_Acc;
         begin
            Evs := new Edge_Array (1 .. Nbr_Events);
            Synth_Complex_Edge_Process_1 (Inst, Stmt1, Ev, Evs.all, Proc);
            Free (Evs);
         end;
      end if;
   end Synth_Complex_Edge_Process;

   procedure Synth_Continuous_Assign (Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Expr : Valtyp;
   begin
      Push_Phi;
      Expr := Synth_Expression (Inst, Get_Expression (N));
      Synth_Assign (Inst, True, Get_Lvalue (N), Expr);
      Pop_And_Merge_Phi (Ctxt, Get_Location (N));
   end Synth_Continuous_Assign;

   procedure Synth_Net_Init (Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Expr : Valtyp;
   begin
      Push_Phi;
      Expr := Synth_Expression (Inst, Get_Expression (N));
      Synth_Assign_Single (Inst, True, N, Expr);
      Pop_And_Merge_Phi (Ctxt, Get_Location (N));
   end Synth_Net_Init;

   function Has_Edge_In_Event_List (Ev : Node) return Boolean is
   begin
      case Get_Kind (Ev) is
         when N_Or =>
            --  As the 'or' operator is left-associative, try right first.
            return Has_Edge_In_Event_List (Get_Right (Ev))
              or else Has_Edge_In_Event_List (Get_Left (Ev));
         when N_Posedge
           | N_Negedge =>
            return True;
         when N_Name =>
            return False;
         when others =>
            Error_Kind ("has_edge_in_event_list", Ev);
      end case;
   end Has_Edge_In_Event_List;

   procedure Synth_Always (Inst : Synth_Instance_Acc; N : Node)
   is
      Stmt : constant Node := Get_Statement (N);
      Sub_Stmt : Node;
      Ev : Node;
   begin
      --  The first statement must be an event control.
      if Get_Kind (Stmt) /= N_Event_Control then
         raise Internal_Error;
      end if;

      Sub_Stmt := Get_Statement (Stmt);

      --  IEEE 1364.1 5.1 Modeling combinatorial logic
      --  When using an always statement, the event list shall not contain
      --  an edge event.

      Ev := Get_Expression (Stmt);
      case Get_Kind (Ev) is
         when N_Implicit_Event =>
            Synth_Comb_Process (Inst, Sub_Stmt);
         when N_Posedge =>
            Synth_FF_Single_Process (Inst, Ev, Sub_Stmt, N);
         when N_Or =>
            if not Has_Edge_In_Event_List (Ev) then
               Synth_Comb_Process (Inst, Sub_Stmt);
            else
               Synth_Complex_Edge_Process (Inst, N, Sub_Stmt, Ev);
            end if;
         when others =>
            Error_Kind ("synth_always", Ev);
      end case;
   end Synth_Always;

   procedure Synth_Always_Comb (Inst : Synth_Instance_Acc; N : Node) is
   begin
      --  TODO: check no latch
      Synth_Comb_Process (Inst, Get_Statement (N));
   end Synth_Always_Comb;

   procedure Synth_Initial (Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
   begin
      pragma Assert (not Is_Initial);
      Is_Initial := True;
      Push_Phi;
      Synth_Stmt (Inst, Get_Statement (N));
      Pop_And_Merge_Initial_Phi (Ctxt, Get_Location (N));
      Is_Initial := False;
   end Synth_Initial;

   procedure Synth_Gate_Buf (Inst : Synth_Instance_Acc; N : Node)
   is
      Terms : constant Node := Get_Gate_Terminals (N);
      Term_Out, Term_Inp, Term_Next : Node;
      Expr : Valtyp;
   begin
      --  Find the last terminal (it's the input).
      Term_Inp := Terms;
      loop
         Term_Next := Get_Chain (Term_Inp);
         exit when Term_Next = Null_Node;
         Term_Inp := Term_Next;
      end loop;

      Expr := Synth_Expression (Inst, Get_Expression (Term_Inp));

      --  Assign.
      Term_Out := Terms;
      while Term_Out /= Term_Inp loop
         Synth_Assign (Inst, True, Get_Expression (Term_Out), Expr);
         Term_Out := Get_Chain (Term_Out);
      end loop;
   end Synth_Gate_Buf;

   procedure Synth_Input_Gate (Inst : Synth_Instance_Acc;
                               Op : Dyadic_Module_Id;
                               N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Term_Out, Term_Inp, Term_Expr : Node;
      Res, Last_Val : Valtyp;
      Last_Net : Net;
   begin
      Term_Out := Get_Gate_Terminals (N);
      Term_Inp := Get_Chain (Term_Out);
      Res := Synth_Expression (Inst, Get_Expression (Term_Inp));

      loop
         Term_Inp := Get_Chain (Term_Inp);
         exit when Term_Inp = Null_Node;
         Term_Expr := Get_Expression (Term_Inp);
         Last_Val := Synth_Expression (Inst, Term_Expr);
         Last_Net := Build_Dyadic
           (Ctxt, Op, Get_Net (Ctxt, Res), Get_Net (Ctxt, Last_Val));
         Set_Location (Last_Net, N);
         Res := Create_Value_Net (Last_Net, Get_Expr_Type (Term_Expr));
      end loop;

      Synth_Assign (Inst, True, Get_Expression (Term_Out), Res);
   end Synth_Input_Gate;

   procedure Synth_Gate (Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
   begin
      Push_Phi;

      case Get_Kind (N) is
         when N_Gate_Buf =>
            Synth_Gate_Buf (Inst, N);
         when N_Gate_Or =>
            Synth_Input_Gate (Inst, Id_Or, N);
         when others =>
            Error_Kind ("synth_gate", N);
      end case;
      Pop_And_Merge_Phi (Ctxt, Get_Location (N));
      null;
   end Synth_Gate;
end Synth.Verilog_Stmts;
