--  Interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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
with Ada.Text_IO; use Ada.Text_IO;
with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;
with Trans_Analyzes;
with Types; use Types;
with Debugger; use Debugger;
with Simulation.AMS.Debugger;
with Areapools; use Areapools;
with Grt.Stacks;
with Grt.Signals;
with Grt.Processes;
with Grt.Main;
with Grt.Errors;
with Grt.Rtis;

package body Simulation is

   function Value_To_Iir_Value (Mode : Mode_Type; Val : Value_Union)
                               return Iir_Value_Literal_Acc is
   begin
      case Mode is
         when Mode_B1 =>
            return Create_B1_Value (Val.B1);
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
      case Src.Kind is
         when Iir_Value_B1 =>
            Dst.B1 := Src.B1;
         when Iir_Value_E32 =>
            Dst.E32 := Src.E32;
         when Iir_Value_I64 =>
            Dst.I64 := Src.I64;
         when Iir_Value_F64 =>
            Dst.F64 := Src.F64;
         when others =>
            raise Internal_Error;  -- FIXME
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

   type Read_Signal_Value_Enum is
     (Read_Signal_Last_Value,

      --  For conversion functions.
      Read_Signal_Driving_Value,
      Read_Signal_Effective_Value,

      --  'Driving_Value
      Read_Signal_Driver_Value);

   function Execute_Read_Signal_Value (Sig: Iir_Value_Literal_Acc;
                                       Attr : Read_Signal_Value_Enum)
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
                    (Sig.Sig.Mode, Sig.Sig.Value);
               when Read_Signal_Driving_Value =>
                  return Value_To_Iir_Value
                    (Sig.Sig.Mode, Sig.Sig.Driving_Value);
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Execute_Read_Signal_Value;

   type Write_Signal_Enum is
     (Write_Signal_Driving_Value,
      Write_Signal_Effective_Value);

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
                  Iir_Value_To_Value (Val, Sig.Sig.Value);
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
            return Value_To_Iir_Value (Indirect.Sig.Mode, Indirect.Sig.Value);
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
           | Iir_Value_Terminal =>
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
            Grt.Processes.Ghdl_Process_Wait_Set_Timeout (Std_Time (Res.I64));
         end if;

         -- LRM93 8.1
         -- The suspended process may also resume as a result of an event
         -- occuring on any signal in the sensitivity set of the wait
         -- statement.
         List := Get_Sensitivity_List (Stmt);
         if List /= Null_Iir_List then
            for J in Natural loop
               El := Get_Nth_Element (List, J);
               exit when El = Null_Iir;
               Wait_Add_Sensitivity (Execute_Name (Instance, El, True));
            end loop;
         end if;

         --  LRM93 8.1
         --  It also causes the execution of the corresponding process
         --  statement to be suspended.
         Grt.Processes.Ghdl_Process_Wait_Wait;
         Instance.In_Wait_Flag := True;
         Release (Marker, Expr_Pool);
         return True;
      else
         --  LRM93 8.1
         --  The suspended process will resume, at the latest, immediately
         --  after the timeout interval has expired.
         if not Grt.Processes.Ghdl_Process_Wait_Has_Timeout then
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
               Grt.Processes.Ghdl_Process_Wait_Wait;
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

   function To_Instance_Acc is new Ada.Unchecked_Conversion
     (System.Address, Grt.Stacks.Instance_Acc);

   procedure Process_Executer (Self : Grt.Stacks.Instance_Acc);
   pragma Convention (C, Process_Executer);

   procedure Process_Executer (Self : Grt.Stacks.Instance_Acc)
   is
      function To_Process_State_Acc is new Ada.Unchecked_Conversion
        (Grt.Stacks.Instance_Acc, Process_State_Acc);

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
         Get_First_Element (Get_Index_Subtype_List (Arr_Type)),
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

   -- Add a driver for signal designed by VAL (via index field) for instance
   -- INSTANCE of process PROC.
   -- FIXME: default value.
   procedure Add_Source
     (Instance: Block_Instance_Acc; Val: Iir_Value_Literal_Acc; Proc: Iir)
   is
   begin
      case Val.Kind is
         when Iir_Value_Signal =>
            if Proc = Null_Iir then
               -- Can this happen ?
               raise Internal_Error;
            end if;
            Grt.Signals.Ghdl_Process_Add_Driver (Val.Sig);
         when Iir_Value_Array =>
            for I in Val.Val_Array.V'Range loop
               Add_Source (Instance, Val.Val_Array.V (I), Proc);
            end loop;
         when Iir_Value_Record =>
            for I in Val.Val_Record.V'Range loop
               Add_Source (Instance, Val.Val_Record.V (I), Proc);
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
      El: Iir;
      Val: Iir_Value_Literal_Acc;
      Marker : Mark_Type;
   begin
      if Trace_Drivers then
         Ada.Text_IO.Put ("Drivers for ");
         Disp_Instance_Name (Instance);
         Ada.Text_IO.Put_Line (": " & Disp_Node (Proc));
      end if;

      Driver_List := Trans_Analyzes.Extract_Drivers (Proc);

      -- Some processes have no driver list (assertion).
      if Driver_List = Null_Iir_List then
         return;
      end if;

      for I in Natural loop
         El := Get_Nth_Element (Driver_List, I);
         exit when El = Null_Iir;
         if Trace_Drivers then
            Put_Line (' ' & Disp_Node (El));
         end if;

         Mark (Marker, Expr_Pool);
         Val := Execute_Name (Instance, El, True);
         Add_Source (Instance, Val, Proc);
         Release (Marker, Expr_Pool);
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

   procedure Create_Processes
   is
      use Grt.Processes;
      El : Iir;
      Instance : Block_Instance_Acc;
      Instance_Grt : Grt.Stacks.Instance_Acc;
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
               declare
                  Sig_List : Iir_List;
                  Sig : Iir;
                  Marker : Mark_Type;
               begin
                  Sig_List := Get_Sensitivity_List (El);
                  for J in Natural loop
                     Sig := Get_Nth_Element (Sig_List, J);
                     exit when Sig = Null_Iir;
                     Mark (Marker, Expr_Pool);
                     Process_Add_Sensitivity
                       (Execute_Name (Instance, Sig, True));
                     Release (Marker, Expr_Pool);
                  end loop;
               end;

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

         --  LRM93 §12.4.4  Other Concurrent Statements
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

   --  Configuration for the whole design
   Top_Config : Iir_Design_Unit;

   --  Elaborate the design
   procedure Ghdl_Elaborate;
   pragma Export (C, Ghdl_Elaborate, "__ghdl_ELABORATE");

   procedure Set_Disconnection (Val : Iir_Value_Literal_Acc;
                                Time : Iir_Value_Time)
   is
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
            case Port.Kind is
               when Iir_Value_Signal =>
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
               when Iir_Value_Access
                 | Iir_Value_File
                 | Iir_Value_Range
                 | Iir_Value_Scalars --  FIXME: by value
                 | Iir_Value_Record
                 | Iir_Value_Array
                 | Iir_Value_Protected
                 | Iir_Value_Quantity
                 | Iir_Value_Terminal =>
                  --  These cannot be driving value for a signal.
                  raise Internal_Error;
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
         when others =>
            raise Internal_Error;
      end case;
   end Connect;

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

   function Create_Shadow_Signal (Sig : Iir_Value_Literal_Acc)
                                 return Iir_Value_Literal_Acc
   is
   begin
      case Sig.Kind is
         when Iir_Value_Signal =>
            case Sig.Sig.Mode is
               when Mode_I64 =>
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_I64
                       (0, null, System.Null_Address));
               when Mode_B1 =>
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_B1
                       (False, null, System.Null_Address));
               when Mode_E32 =>
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_E32
                       (0, null, System.Null_Address));
               when Mode_F64 =>
                  return Create_Signal_Value
                    (Grt.Signals.Ghdl_Create_Signal_F64
                       (0.0, null, System.Null_Address));
               when Mode_E8
                 | Mode_I32 =>
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
           | Iir_Value_File =>
            raise Internal_Error;
      end case;
   end Create_Shadow_Signal;

   procedure Set_Connect
     (Formal_Instance : Block_Instance_Acc;
      Formal_Expr : Iir_Value_Literal_Acc;
      Local_Instance : Block_Instance_Acc;
      Local_Expr : Iir_Value_Literal_Acc;
      Assoc : Iir_Association_Element_By_Expression)
   is
      pragma Unreferenced (Formal_Instance);
      Formal : constant Iir := Get_Formal (Assoc);
      Inter : constant Iir := Get_Association_Interface (Assoc);
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
               Out_Conv : constant Iir := Get_Out_Conversion (Assoc);
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
               --  LRM93 §12.6.2
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
               In_Conv : constant Iir := Get_In_Conversion (Assoc);
               Src : Iir_Value_Literal_Acc;
            begin
               if In_Conv /= Null_Iir then
                  Src := Create_Shadow_Signal (Formal_Expr);
                  Add_Conversion
                    (new Convert_Instance_Type'
                       (Mode => Convert_In,
                        Instance => Local_Instance,
                        Func => Get_Implementation (In_Conv),
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
                         E.Assoc);
         end;
      end loop;

      Instance_Pool := null;
   end Create_Connects;

   procedure Create_Guard_Signal
     (Instance : Block_Instance_Acc;
      Sig_Guard : Iir_Value_Literal_Acc;
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
      Dep : Iir;
      Data : Guard_Instance_Acc;
   begin
      Data := new Guard_Instance_Type'(Instance => Instance,
                                       Guard => Guard);
      Sig_Guard.Sig := Grt.Signals.Ghdl_Signal_Create_Guard
        (Data.all'Address, Guard_Func'Access);
      Dep_List := Get_Guard_Sensitivity_List (Guard);
      for I in Natural loop
         Dep := Get_Nth_Element (Dep_List, I);
         exit when Dep = Null_Iir;
         Add_Guard_Sensitivity (Execute_Name (Instance, Dep, True));
      end loop;

      --  FIXME: free mem
   end Create_Guard_Signal;

   procedure Create_Implicit_Signal (Sig : Iir_Value_Literal_Acc;
                                     Time : Ghdl_I64;
                                     Prefix : Iir_Value_Literal_Acc;
                                     Kind : Signal_Type_Kind)
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
         when Implicit_Stable =>
            Sig.Sig := Grt.Signals.Ghdl_Create_Stable_Signal (Std_Time (Time));
         when Implicit_Quiet =>
            Sig.Sig := Grt.Signals.Ghdl_Create_Quiet_Signal (Std_Time (Time));
         when Implicit_Transaction =>
            Sig.Sig := Grt.Signals.Ghdl_Create_Transaction_Signal;
         when others =>
            raise Internal_Error;
      end case;
      Register_Prefix (Prefix);
   end Create_Implicit_Signal;

   procedure Create_Delayed_Signal
     (Sig : Iir_Value_Literal_Acc; Pfx : Iir_Value_Literal_Acc; Val : Std_Time)
   is
   begin
      case Pfx.Kind is
            when Iir_Value_Array =>
               for I in Sig.Val_Array.V'Range loop
                  Create_Delayed_Signal
                    (Sig.Val_Array.V (I), Pfx.Val_Array.V (I), Val);
               end loop;
            when Iir_Value_Record =>
               for I in Pfx.Val_Record.V'Range loop
                  Create_Delayed_Signal
                    (Sig.Val_Record.V (I), Pfx.Val_Array.V (I), Val);
               end loop;
         when Iir_Value_Signal =>
            Sig.Sig := Grt.Signals.Ghdl_Create_Delayed_Signal (Pfx.Sig, Val);
         when others =>
            raise Internal_Error;
      end case;
   end Create_Delayed_Signal;

   -- Create a new signal, using DEFAULT as initial value.
   -- Set its number.
   procedure Create_User_Signal (Block: Block_Instance_Acc;
                                 Signal: Iir;
                                 Sig : Iir_Value_Literal_Acc;
                                 Default : Iir_Value_Literal_Acc)
   is
      use Grt.Rtis;
      use Grt.Signals;

      procedure Create_Signal (Lit: Iir_Value_Literal_Acc;
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
               Ghdl_Index_Type (Get_Nbr_Of_Scalars (Lit)));
         end if;
         case Lit.Kind is
            when Iir_Value_Array =>
               declare
                  Sig_El_Type : constant Iir :=
                    Get_Element_Subtype (Get_Base_Type (Sig_Type));
               begin
                  for I in Lit.Val_Array.V'Range loop
                     Create_Signal (Lit.Val_Array.V (I), Sig.Val_Array.V (I),
                                    Sig_El_Type, Sub_Resolved);
                  end loop;
               end;
            when Iir_Value_Record =>
               declare
                  El : Iir_Element_Declaration;
                  List : Iir_List;
               begin
                  List := Get_Elements_Declaration_List
                    (Get_Base_Type (Sig_Type));
                  for I in Lit.Val_Record.V'Range loop
                     El := Get_Nth_Element (List, Natural (I - 1));
                     Create_Signal (Lit.Val_Record.V (I), Sig.Val_Record.V (I),
                                    Get_Type (El), Sub_Resolved);
                  end loop;
               end;

            when Iir_Value_I64 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_I64
                 (Lit.I64, null, System.Null_Address);
            when Iir_Value_B1 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_B1
                 (Lit.B1, null, System.Null_Address);
            when Iir_Value_E32 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_E32
                 (Lit.E32, null, System.Null_Address);
            when Iir_Value_F64 =>
               Sig.Sig := Grt.Signals.Ghdl_Create_Signal_F64
                 (Lit.F64, null, System.Null_Address);

            when Iir_Value_Signal
              | Iir_Value_Range
              | Iir_Value_File
              | Iir_Value_Access
              | Iir_Value_Protected
              | Iir_Value_Quantity
              | Iir_Value_Terminal =>
               raise Internal_Error;
         end case;
      end Create_Signal;

      Sig_Type: constant Iir := Get_Type (Signal);
      Mode : Mode_Signal_Type;
      Kind : Kind_Signal_Type;

      type Iir_Mode_To_Mode_Signal_Type is
        array (Iir_Mode) of Mode_Signal_Type;
      Iir_Mode_To_Mode_Signal : constant Iir_Mode_To_Mode_Signal_Type :=
        (Iir_Unknown_Mode => Mode_Signal,
         Iir_Linkage_Mode => Mode_Linkage,
         Iir_Buffer_Mode => Mode_Buffer,
         Iir_Out_Mode => Mode_Out,
         Iir_Inout_Mode => Mode_Inout,
         Iir_In_Mode => Mode_In);

      type Iir_Kind_To_Kind_Signal_Type is
        array (Iir_Signal_Kind) of Kind_Signal_Type;
      Iir_Kind_To_Kind_Signal : constant Iir_Kind_To_Kind_Signal_Type :=
        (Iir_Register_Kind  => Kind_Signal_Register,
         Iir_Bus_Kind       => Kind_Signal_Bus);
   begin
      case Get_Kind (Signal) is
         when Iir_Kind_Interface_Signal_Declaration =>
            Mode := Iir_Mode_To_Mode_Signal (Get_Mode (Signal));
         when Iir_Kind_Signal_Declaration =>
            Mode := Mode_Signal;
         when others =>
            Error_Kind ("elaborate_signal", Signal);
      end case;

      if Get_Guarded_Signal_Flag (Signal) then
         Kind := Iir_Kind_To_Kind_Signal (Get_Signal_Kind (Signal));
      else
         Kind := Kind_Signal_No;
      end if;

      Grt.Signals.Ghdl_Signal_Set_Mode (Mode, Kind, True);

      Create_Signal (Default, Sig, Sig_Type, False);
   end Create_User_Signal;

   procedure Create_Signals is
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            E : Signal_Entry renames Signals_Table.Table (I);
         begin
            case E.Kind is
               when Guard_Signal =>
                  Create_Guard_Signal (E.Instance, E.Sig, E.Decl);
               when Implicit_Stable | Implicit_Quiet | Implicit_Transaction =>
                  Create_Implicit_Signal (E.Sig, E.Time, E.Prefix, E.Kind);
               when Implicit_Delayed =>
                  Create_Delayed_Signal (E.Sig, E.Prefix, Std_Time (E.Time));
               when User_Signal =>
                  Create_User_Signal (E.Instance, E.Decl, E.Sig, E.Init);
            end case;
         end;
      end loop;
   end Create_Signals;

   procedure Ghdl_Elaborate
   is
      Entity: Iir_Entity_Declaration;

      -- Number of input ports of the top entity.
      In_Signals: Natural;
      El : Iir;
   begin
      Instance_Pool := Global_Pool'Access;

      Elaboration.Elaborate_Design (Top_Config);
      Entity := Iirs_Utils.Get_Entity (Get_Library_Unit (Top_Config));

      if not Is_Empty (Expr_Pool) then
         raise Internal_Error;
      end if;

      Instance_Pool := null;

      -- Be sure there is no IN ports in the top entity.
      El := Get_Port_Chain (Entity);
      In_Signals := 0;
      while El /= Null_Iir loop
         if Get_Mode (El) = Iir_In_Mode then
            In_Signals := In_Signals + 1;
         end if;
         El := Get_Chain (El);
      end loop;

      if In_Signals /= 0 then
         Error_Msg_Elab ("top entity should not have inputs signals", Entity);
         -- raise Simulation_Error;
      end if;

      if Disp_Stats then
         Disp_Design_Stats;
      end if;

      if Disp_Ams then
         Simulation.AMS.Debugger.Disp_Characteristic_Expressions;
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

      if Disp_Tree then
         Debugger.Disp_Instances_Tree;
      end if;

      if Flag_Interractive then
         Debug (Reason_Elab);
      end if;
   end Ghdl_Elaborate;

   procedure Simulation_Entity (Top_Conf : Iir_Design_Unit) is
   begin
      Top_Config := Top_Conf;
      Grt.Processes.One_Stack := True;

      Grt.Errors.Error_Hook := Debug_Error'Access;

      if Flag_Interractive then
         Debug (Reason_Start);
      end if;

      Grt.Main.Run;
   exception
      when Debugger_Quit =>
         null;
      when Simulation_Finished =>
         null;
   end Simulation_Entity;

end Simulation;
