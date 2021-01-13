--  Interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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
with Simul.Execution; use Simul.Execution;
with Simul.Debugger; use Simul.Debugger;
with Areapools; use Areapools;
with Grt.Signals;
with Grt.Processes;
with Grtlink;
pragma Unreferenced (Grtlink);

package body Simul.Simulation is

   function Value_To_Iir_Value (Mode : Mode_Type; Val : Value_Union)
                               return Iir_Value_Literal_Acc is
   begin
      case Mode is
         when Mode_B1 =>
            return Create_B1_Value (Val.B1);
         when Mode_E8 =>
            return Create_E8_Value (Val.E8);
         when Mode_E32 =>
            return Create_E32_Value (Val.E32);
         when Mode_I64 =>
            return Create_I64_Value (Val.I64);
         when Mode_F64 =>
            return Create_F64_Value (Val.F64);
         when others =>
            raise Internal_Error;  -- FIXME
      end case;
   end Value_To_Iir_Value;

   procedure Iir_Value_To_Value (Src : Iir_Value_Literal_Acc;
                                 Dst : out Value_Union) is
   begin
      case Iir_Value_Scalars (Src.Kind) is
         when Iir_Value_B1 =>
            Dst.B1 := Src.B1;
         when Iir_Value_E8 =>
            Dst.E8 := Src.E8;
         when Iir_Value_E32 =>
            Dst.E32 := Src.E32;
         when Iir_Value_I64 =>
            Dst.I64 := Src.I64;
         when Iir_Value_F64 =>
            Dst.F64 := Src.F64;
      end case;
   end Iir_Value_To_Value;

   type Read_Signal_Flag_Enum is
     (Read_Signal_Event,
      Read_Signal_Active,
      --  In order to reuse the same code (that returns immediately if the
      --  attribute is true), we use not driving.
      Read_Signal_Not_Driving);

   function Read_Signal_Flag (Lit: Iir_Value_Literal_Acc;
                              Kind : Read_Signal_Flag_Enum)
                             return Boolean
   is
   begin
      case Lit.Kind is
         when Iir_Value_Array =>
            for I in Lit.Val_Array.V'Range loop
               if Read_Signal_Flag (Lit.Val_Array.V (I), Kind) then
                  return True;
               end if;
            end loop;
            return False;
         when Iir_Value_Record =>
            for I in Lit.Val_Record.V'Range loop
               if Read_Signal_Flag (Lit.Val_Record.V (I), Kind) then
                  return True;
               end if;
            end loop;
            return False;
         when Iir_Value_Signal =>
            case Kind is
               when Read_Signal_Event =>
                  return Lit.Sig.Event;
               when Read_Signal_Active =>
                  return Lit.Sig.Active;
               when Read_Signal_Not_Driving =>
                  if Grt.Signals.Ghdl_Signal_Driving (Lit.Sig) = True then
                     return False;
                  else
                     return True;
                  end if;
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Read_Signal_Flag;

   function Execute_Event_Attribute (Lit: Iir_Value_Literal_Acc)
                                    return Boolean is
   begin
      return Read_Signal_Flag (Lit, Read_Signal_Event);
   end Execute_Event_Attribute;

   function Execute_Active_Attribute (Lit: Iir_Value_Literal_Acc)
                                     return Boolean is
   begin
      return Read_Signal_Flag (Lit, Read_Signal_Active);
   end Execute_Active_Attribute;

   function Execute_Driving_Attribute (Lit: Iir_Value_Literal_Acc)
                                      return Boolean is
   begin
      return not Read_Signal_Flag (Lit, Read_Signal_Not_Driving);
   end Execute_Driving_Attribute;

   function Execute_Read_Signal_Value
     (Sig: Iir_Value_Literal_Acc; Attr : Read_Signal_Value_Enum)
     return Iir_Value_Literal_Acc
   is
      Res: Iir_Value_Literal_Acc;
   begin
      case Sig.Kind is
         when Iir_Value_Array =>
            Res := Copy_Array_Bound (Sig);
            for I in Sig.Val_Array.V'Range loop
               Res.Val_Array.V (I) :=
                 Execute_Read_Signal_Value (Sig.Val_Array.V (I), Attr);
            end loop;
            return Res;
         when Iir_Value_Record =>
            Res := Create_Record_Value (Sig.Val_Record.Len);
            for I in Sig.Val_Record.V'Range loop
               Res.Val_Record.V (I) :=
                 Execute_Read_Signal_Value (Sig.Val_Record.V (I), Attr);
            end loop;
            return Res;
         when Iir_Value_Signal =>
            case Attr is
               when Read_Signal_Last_Value =>
                  return Value_To_Iir_Value
                    (Sig.Sig.Mode, Sig.Sig.Last_Value);
               when Read_Signal_Driver_Value =>
                  case Sig.Sig.Mode is
                     when Mode_F64 =>
                        return Create_F64_Value
                          (Grt.Signals.Ghdl_Signal_Driving_Value_F64
                             (Sig.Sig));
                     when Mode_I64 =>
                        return Create_I64_Value
                          (Grt.Signals.Ghdl_Signal_Driving_Value_I64
                             (Sig.Sig));
                     when Mode_E32 =>
                        return Create_E32_Value
                          (Grt.Signals.Ghdl_Signal_Driving_Value_E32
                             (Sig.Sig));
                     when Mode_B1 =>
                        return Create_B1_Value
                          (Grt.Signals.Ghdl_Signal_Driving_Value_B1
                             (Sig.Sig));
                     when others =>
                        raise Internal_Error;
                  end case;
               when Read_Signal_Effective_Value =>
                  return Value_To_Iir_Value
                    (Sig.Sig.Mode, Sig.Sig.Value_Ptr.all);
               when Read_Signal_Driving_Value =>
                  return Value_To_Iir_Value
                    (Sig.Sig.Mode, Sig.Sig.Driving_Value);
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Execute_Read_Signal_Value;

   procedure Execute_Write_Signal (Sig: Iir_Value_Literal_Acc;
                                   Val : Iir_Value_Literal_Acc;
                                   Attr : Write_Signal_Enum) is
   begin
      case Sig.Kind is
         when Iir_Value_Array =>
            pragma Assert (Val.Kind = Iir_Value_Array);
            pragma Assert (Sig.Val_Array.Len = Val.Val_Array.Len);
            for I in Sig.Val_Array.V'Range loop
               Execute_Write_Signal
                 (Sig.Val_Array.V (I), Val.Val_Array.V (I), Attr);
            end loop;
         when Iir_Value_Record =>
            pragma Assert (Val.Kind = Iir_Value_Record);
            pragma Assert (Sig.Val_Record.Len = Val.Val_Record.Len);
            for I in Sig.Val_Record.V'Range loop
               Execute_Write_Signal
                 (Sig.Val_Record.V (I), Val.Val_Record.V (I), Attr);
            end loop;
         when Iir_Value_Signal =>
            pragma Assert (Val.Kind in Iir_Value_Scalars);
            case Attr is
               when Write_Signal_Driving_Value =>
                  Iir_Value_To_Value (Val, Sig.Sig.Driving_Value);
               when Write_Signal_Effective_Value =>
                  Iir_Value_To_Value (Val, Sig.Sig.Value_Ptr.all);
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Execute_Write_Signal;

   function Execute_Last_Value_Attribute (Indirect: Iir_Value_Literal_Acc)
     return Iir_Value_Literal_Acc is
   begin
      return Execute_Read_Signal_Value (Indirect, Read_Signal_Last_Value);
   end Execute_Last_Value_Attribute;

   function Execute_Driving_Value_Attribute (Indirect: Iir_Value_Literal_Acc)
                                            return Iir_Value_Literal_Acc is
   begin
      return Execute_Read_Signal_Value (Indirect, Read_Signal_Driver_Value);
   end Execute_Driving_Value_Attribute;

   type Signal_Read_Last_Type is
     (Read_Last_Event,
      Read_Last_Active);

   --  Return the Last_Event absolute time.
   function Execute_Read_Signal_Last (Indirect: Iir_Value_Literal_Acc;
                                      Kind : Signal_Read_Last_Type)
                                     return Ghdl_I64
   is
      Res: Ghdl_I64;
   begin
      case Indirect.Kind is
         when Iir_Value_Array =>
            Res := Ghdl_I64'First;
            for I in Indirect.Val_Array.V'Range loop
               Res := Ghdl_I64'Max
                 (Res, Execute_Read_Signal_Last (Indirect.Val_Array.V (I),
                                                 Kind));
            end loop;
            return Res;
         when Iir_Value_Record =>
            Res := Ghdl_I64'First;
            for I in Indirect.Val_Record.V'Range loop
               Res := Ghdl_I64'Max
                 (Res, Execute_Read_Signal_Last (Indirect.Val_Record.V (I),
                                                 Kind));
            end loop;
            return Res;
         when Iir_Value_Signal =>
            case Kind is
               when Read_Last_Event =>
                  return Ghdl_I64 (Indirect.Sig.Last_Event);
               when Read_Last_Active =>
                  return Ghdl_I64 (Indirect.Sig.Last_Active);
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Execute_Read_Signal_Last;

   function Execute_Last_Event_Attribute (Indirect: Iir_Value_Literal_Acc)
                                         return Ghdl_I64 is
   begin
      return Execute_Read_Signal_Last (Indirect, Read_Last_Event);
   end Execute_Last_Event_Attribute;

   function Execute_Last_Active_Attribute (Indirect: Iir_Value_Literal_Acc)
                                          return Ghdl_I64 is
   begin
      return Execute_Read_Signal_Last (Indirect, Read_Last_Active);
   end Execute_Last_Active_Attribute;

   function Execute_Signal_Value (Indirect: Iir_Value_Literal_Acc)
     return Iir_Value_Literal_Acc
   is
      Res: Iir_Value_Literal_Acc;
   begin
      case Indirect.Kind is
         when Iir_Value_Array =>
            Res := Copy_Array_Bound (Indirect);
            for I in Indirect.Val_Array.V'Range loop
               Res.Val_Array.V (I) :=
                 Execute_Signal_Value (Indirect.Val_Array.V (I));
            end loop;
            return Res;
         when Iir_Value_Record =>
            Res := Create_Record_Value (Indirect.Val_Record.Len);
            for I in Indirect.Val_Record.V'Range loop
               Res.Val_Record.V (I) :=
                 Execute_Signal_Value (Indirect.Val_Record.V (I));
            end loop;
            return Res;
         when Iir_Value_Signal =>
            return Value_To_Iir_Value
              (Indirect.Sig.Mode, Indirect.Sig.Value_Ptr.all);
         when others =>
            raise Internal_Error;
      end case;
   end Execute_Signal_Value;

   procedure Assign_Value_To_Array_Signal
     (Instance: Block_Instance_Acc;
      Target: Iir_Value_Literal_Acc;
      Transactions: Transaction_Type)
   is
      Sub_Trans : Transaction_Type (Transactions.Len);
   begin
      Sub_Trans.Stmt := Transactions.Stmt;
      Sub_Trans.Reject := Transactions.Reject;

      for J in Target.Val_Array.V'Range loop
         for K in Transactions.Els'Range loop
            declare
               T : Transaction_El_Type renames Transactions.Els (K);
               S : Transaction_El_Type renames Sub_Trans.Els (K);
            begin
               S.After := T.After;

               if T.Value = null then
                  S.Value := null;
               else
                  S.Value := T.Value.Val_Array.V (J);
               end if;
            end;
         end loop;

         Assign_Value_To_Signal
           (Instance, Target.Val_Array.V (J), Sub_Trans);
      end loop;
   end Assign_Value_To_Array_Signal;

   procedure Assign_Value_To_Record_Signal
     (Instance: Block_Instance_Acc;
      Target: Iir_Value_Literal_Acc;
      Transactions: Transaction_Type)
   is
      Sub_Trans : Transaction_Type (Transactions.Len);
   begin
      Sub_Trans.Stmt := Transactions.Stmt;
      Sub_Trans.Reject := Transactions.Reject;

      for J in Target.Val_Record.V'Range loop
         for K in Transactions.Els'Range loop
            declare
               T : Transaction_El_Type renames Transactions.Els (K);
               S : Transaction_El_Type renames Sub_Trans.Els (K);
            begin
               S.After := T.After;

               if T.Value = null then
                  S.Value := null;
               else
                  S.Value := T.Value.Val_Record.V (J);
               end if;
            end;
         end loop;

         Assign_Value_To_Signal
           (Instance, Target.Val_Record.V (J), Sub_Trans);
      end loop;
   end Assign_Value_To_Record_Signal;

   procedure Assign_Value_To_Scalar_Signal
     (Instance: Block_Instance_Acc;
      Target: Iir_Value_Literal_Acc;
      Transactions: Transaction_Type)
   is
      pragma Unreferenced (Instance);
      use Grt.Signals;
   begin
      declare
         El : Transaction_El_Type renames Transactions.Els (1);
      begin
         if El.Value = null then
            Ghdl_Signal_Start_Assign_Null
              (Target.Sig, Transactions.Reject, El.After);
            if Transactions.Els'Last /= 1 then
               raise Internal_Error;
            end if;
            return;
         end if;

         --  FIXME: null transaction, check constraints.
         case Iir_Value_Scalars (El.Value.Kind) is
            when Iir_Value_B1 =>
               Ghdl_Signal_Start_Assign_B1
                 (Target.Sig, Transactions.Reject, El.Value.B1, El.After);
            when Iir_Value_E8 =>
               Ghdl_Signal_Start_Assign_E8
                 (Target.Sig, Transactions.Reject, El.Value.E8, El.After);
            when Iir_Value_E32 =>
               Ghdl_Signal_Start_Assign_E32
                 (Target.Sig, Transactions.Reject, El.Value.E32, El.After);
            when Iir_Value_I64 =>
               Ghdl_Signal_Start_Assign_I64
                 (Target.Sig, Transactions.Reject, El.Value.I64, El.After);
            when Iir_Value_F64 =>
               Ghdl_Signal_Start_Assign_F64
                 (Target.Sig, Transactions.Reject, El.Value.F64, El.After);
         end case;
      end;

      for I in 2 .. Transactions.Els'Last loop
         declare
            El : Transaction_El_Type renames Transactions.Els (I);
         begin
            case Iir_Value_Scalars (El.Value.Kind) is
               when Iir_Value_B1 =>
                  Ghdl_Signal_Next_Assign_B1
                    (Target.Sig, El.Value.B1, El.After);
               when Iir_Value_E8 =>
                  Ghdl_Signal_Next_Assign_E8
                    (Target.Sig, El.Value.E8, El.After);
               when Iir_Value_E32 =>
                  Ghdl_Signal_Next_Assign_E32
                    (Target.Sig, El.Value.E32, El.After);
               when Iir_Value_I64 =>
                  Ghdl_Signal_Next_Assign_I64
                    (Target.Sig, El.Value.I64, El.After);
               when Iir_Value_F64 =>
                  Ghdl_Signal_Next_Assign_F64
                    (Target.Sig, El.Value.F64, El.After);
            end case;
         end;
      end loop;
   end Assign_Value_To_Scalar_Signal;

   procedure Assign_Value_To_Signal
     (Instance: Block_Instance_Acc;
      Target: Iir_Value_Literal_Acc;
      Transaction: Transaction_Type)
   is
   begin
      case Target.Kind is
         when Iir_Value_Array =>
            Assign_Value_To_Array_Signal
              (Instance, Target, Transaction);
         when Iir_Value_Record =>
            Assign_Value_To_Record_Signal
              (Instance, Target, Transaction);
         when Iir_Value_Signal =>
            Assign_Value_To_Scalar_Signal
              (Instance, Target, Transaction);
         when Iir_Value_Scalars
           | Iir_Value_Range
           | Iir_Value_File
           | Iir_Value_Access
           | Iir_Value_Protected
           | Iir_Value_Quantity
           | Iir_Value_Terminal
           | Iir_Value_Instance =>
            raise Internal_Error;
      end case;
   end Assign_Value_To_Signal;

   procedure Disconnect_Signal (Sig : Iir_Value_Literal_Acc) is
   begin
      case Sig.Kind is
         when Iir_Value_Array =>
            for I in Sig.Val_Array.V'Range loop
               Disconnect_Signal (Sig.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            for I in Sig.Val_Array.V'Range loop
               Disconnect_Signal (Sig.Val_Record.V (I));
            end loop;
         when Iir_Value_Signal =>
            Grt.Signals.Ghdl_Signal_Disconnect (Sig.Sig);
         when others =>
            raise Internal_Error;
      end case;
   end Disconnect_Signal;

   --  Call Ghdl_Process_Wait_Add_Sensitivity for each scalar subelement of
   --  SIG.
   procedure Wait_Add_Sensitivity (Sig: Iir_Value_Literal_Acc)
   is
   begin
      case Sig.Kind is
         when Iir_Value_Signal =>
            Grt.Processes.Ghdl_Process_Wait_Add_Sensitivity (Sig.Sig);
         when Iir_Value_Array =>
            for I in Sig.Val_Array.V'Range loop
               Wait_Add_Sensitivity (Sig.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            for I in Sig.Val_Record.V'Range loop
               Wait_Add_Sensitivity (Sig.Val_Record.V (I));
            end loop;
         when others =>
            raise Internal_Error;
      end case;
   end Wait_Add_Sensitivity;

   -- Return true if the process should be suspended.
   function Execute_Wait_Statement (Instance : Block_Instance_Acc;
                                    Stmt: Iir_Wait_Statement)
                                   return Boolean
   is
      Expr: Iir;
      El : Iir;
      List: Iir_List;
      It : List_Iterator;
      Res: Iir_Value_Literal_Acc;
      Status : Boolean;
      Marker : Mark_Type;
   begin
      if not Instance.In_Wait_Flag then
         Mark (Marker, Expr_Pool);

         -- LRM93 8.1
         -- The execution of a wait statement causes the time expression to
         -- be evaluated to determine the timeout interval.
         Expr := Get_Timeout_Clause (Stmt);
         if Expr /= Null_Iir then
            Res := Execute_Expression (Instance, Expr);
            if Res.I64 < 0 then
               Error_Msg_Exec ("negative timeout clause", Stmt);
            end if;
            Grt.Processes.Ghdl_Process_Wait_Set_Timeout
              (Std_Time (Res.I64), null, 0);
         end if;

         -- LRM93 8.1
         -- The suspended process may also resume as a result of an event
         -- occuring on any signal in the sensitivity set of the wait
         -- statement.
         List := Get_Sensitivity_List (Stmt);
         It := List_Iterate_Safe (List);
         while Is_Valid (It) loop
            El := Get_Element (It);
            Wait_Add_Sensitivity (Execute_Name (Instance, El, True));
            Next (It);
         end loop;

         --  LRM93 8.1
         --  It also causes the execution of the corresponding process
         --  statement to be suspended.
         Grt.Processes.Ghdl_Process_Wait_Suspend;
         Instance.In_Wait_Flag := True;
         Release (Marker, Expr_Pool);
         return True;
      else
         --  LRM93 8.1
         --  The suspended process will resume, at the latest, immediately
         --  after the timeout interval has expired.
         if not Grt.Processes.Ghdl_Process_Wait_Timed_Out then
            --  Compute the condition clause only if the timeout has not
            --  expired.

            -- LRM93 8.1
            -- If such an event occurs, the condition in the condition clause
            -- is evaluated.
            --
            -- if no condition clause appears, the condition clause until true
            -- is assumed.
            Status :=
              Execute_Condition (Instance, Get_Condition_Clause (Stmt));
            if not Status then
               -- LRM93 8.1
               -- If the value of the condition is FALSE, the process will
               -- re-suspend.
               -- Such re-suspension does not involve the recalculation of
               -- the timeout interval.
               Grt.Processes.Ghdl_Process_Wait_Suspend;
               return True;
            end if;
         end if;

         -- LRM93 8.1
         --   If the value of the condition is TRUE, the process will resume.
         -- next statement.
         Grt.Processes.Ghdl_Process_Wait_Close;

         Instance.In_Wait_Flag := False;
         return False;
      end if;
   end Execute_Wait_Statement;

   type Resolver_Read_Mode is (Read_Port, Read_Driver);

   function Resolver_Read_Value (Sig : Iir_Value_Literal_Acc;
                                 Mode : Resolver_Read_Mode;
                                 Index : Ghdl_Index_Type)
                                return Iir_Value_Literal_Acc
   is
      use Grt.Signals;
      Val : Ghdl_Value_Ptr;
      Res : Iir_Value_Literal_Acc;
   begin
      case Sig.Kind is
         when Iir_Value_Array =>
            Res := Copy_Array_Bound (Sig);
            for I in Sig.Val_Array.V'Range loop
               Res.Val_Array.V (I) :=
                 Resolver_Read_Value (Sig.Val_Array.V (I), Mode, Index);
            end loop;
         when Iir_Value_Record =>
            Res := Create_Record_Value (Sig.Val_Record.Len);
            for I in Sig.Val_Record.V'Range loop
               Res.Val_Record.V (I) :=
                 Resolver_Read_Value (Sig.Val_Record.V (I), Mode, Index);
            end loop;
         when Iir_Value_Signal =>
            case Mode is
               when Read_Port =>
                  Val := Ghdl_Signal_Read_Port (Sig.Sig, Index);
               when Read_Driver =>
                  Val := Ghdl_Signal_Read_Driver (Sig.Sig, Index);
            end case;
            Res := Value_To_Iir_Value (Sig.Sig.Mode, Val.all);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Resolver_Read_Value;

   procedure Resolution_Proc (Instance_Addr : System.Address;
                              Val : System.Address;
                              Bool_Vec : System.Address;
                              Vec_Len : Ghdl_Index_Type;
                              Nbr_Drv : Ghdl_Index_Type;
                              Nbr_Ports : Ghdl_Index_Type)
   is
      pragma Unreferenced (Val);

      Instance : Resolv_Instance_Type;
      pragma Import (Ada, Instance);
      for Instance'Address use Instance_Addr;

      type Bool_Array is array (1 .. Nbr_Drv) of Boolean;
      Vec : Bool_Array;
      pragma Import (Ada, Vec);
      for Vec'Address use Bool_Vec;
      Off : Iir_Index32;

      Arr : Iir_Value_Literal_Acc;
      Arr_Type : constant Iir :=
        Get_Type (Get_Interface_Declaration_Chain (Instance.Func));

      Res : Iir_Value_Literal_Acc;

      Len : constant Iir_Index32 := Iir_Index32 (Vec_Len + Nbr_Ports);
      Instance_Mark, Expr_Mark : Mark_Type;
   begin
      pragma Assert (Instance_Pool = null);
      Instance_Pool := Global_Pool'Access;
      Mark (Instance_Mark, Instance_Pool.all);
      Mark (Expr_Mark, Expr_Pool);
      Current_Process := No_Process;

      Arr := Create_Array_Value (Len, 1);
      Arr.Bounds.D (1) := Create_Bounds_From_Length
        (Instance.Block,
         Get_Nth_Element (Get_Index_Subtype_List (Arr_Type), 0),
         Len);

      --  First ports
      for I in 1 .. Nbr_Ports loop
         Arr.Val_Array.V (Iir_Index32 (I)) := Resolver_Read_Value
           (Instance.Sig, Read_Port, I - 1);
      end loop;

      --  Then drivers.
      Off := Iir_Index32 (Nbr_Ports) + 1;
      for I in 1 .. Nbr_Drv loop
         if Vec (I) then
            Arr.Val_Array.V (Off) := Resolver_Read_Value
              (Instance.Sig, Read_Driver, I - 1);
            Off := Off + 1;
         end if;
      end loop;

      --  Call resolution function.
      Res := Execute_Resolution_Function (Instance.Block, Instance.Func, Arr);

      --  Set driving value.
      Execute_Write_Signal (Instance.Sig, Res, Write_Signal_Driving_Value);

      Release (Instance_Mark, Instance_Pool.all);
      Release (Expr_Mark, Expr_Pool);
      Instance_Pool := null;
   end Resolution_Proc;

   function Guard_Func (Data : System.Address) return Ghdl_B1
   is
      Guard : Guard_Instance_Type;
      pragma Import (Ada, Guard);
      for Guard'Address use Data;

      Val : Boolean;

      Prev_Instance_Pool : Areapool_Acc;
   begin
      pragma Assert (Instance_Pool = null
                       or else Instance_Pool = Global_Pool'Access);
      Prev_Instance_Pool := Instance_Pool;

      Instance_Pool := Global_Pool'Access;
      Current_Process := No_Process;

      Val := Execute_Condition
        (Guard.Instance, Get_Guard_Expression (Guard.Guard));

      Instance_Pool := Prev_Instance_Pool;

      return Ghdl_B1'Val (Boolean'Pos (Val));
   end Guard_Func;
end Simul.Simulation;
