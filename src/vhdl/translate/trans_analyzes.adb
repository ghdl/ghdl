--  Analysis for translation.
--  Copyright (C) 2009 Tristan Gingold
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

with Errorout;
with Simple_IO;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Nodes_Walk; use Vhdl.Nodes_Walk;
with Vhdl.Prints;
with Vhdl.Errors; use Vhdl.Errors;

package body Trans_Analyzes is
   Driver_List : Iir_List;

   Has_After : Boolean;

   function Extract_Driver_Target (Target : Iir) return Walk_Status
   is
      Base : Iir;
      Prefix : Iir;
   begin
      Base := Get_Object_Prefix (Target);
      --  Assigment to subprogram interface does not create a driver.
      if Get_Kind (Base) = Iir_Kind_Interface_Signal_Declaration
        and then Is_Parameter (Base)
      then
         return Walk_Continue;
      end if;

      Prefix := Get_Longest_Static_Prefix (Target);
      Add_Element (Driver_List, Prefix);
      if Has_After then
         Set_After_Drivers_Flag (Base, True);
      end if;
      return Walk_Continue;
   end Extract_Driver_Target;

   --  Set Has_After to True iff WF requires a non-direct driver.
   procedure Extract_Has_After (Wf : Iir) is
   begin
      --  Disconnect, or time expression.
      if Wf = Null_Iir
        or else Get_Chain (Wf) /= Null_Iir
        or else Get_Time (Wf) /= Null_Iir
        or else Get_Kind (Get_We_Value (Wf)) = Iir_Kind_Null_Literal
      then
         Has_After := True;
      end if;
   end Extract_Has_After;

   procedure Extract_Driver_Simple_Signal_Assignment (Stmt : Iir)
   is
      Status : Walk_Status;
      pragma Unreferenced (Status);
      Wf : constant Iir := Get_Waveform_Chain (Stmt);
   begin
      if Is_Null (Wf)
        or else Get_Kind (Wf) /= Iir_Kind_Unaffected_Waveform
      then
         --  Not unaffected or implicit disconnect.
         Extract_Has_After (Wf);
         Status := Walk_Assignment_Target
           (Get_Target (Stmt), Extract_Driver_Target'Access);
      end if;
   end Extract_Driver_Simple_Signal_Assignment;

   procedure Extract_Driver_Conditional_Signal_Assignment (Stmt : Iir)
   is
      Status : Walk_Status;
      pragma Unreferenced (Status);
      Cond_Wf : Iir;
      Wf : Iir;
      Has_Drv : Boolean;
   begin
      Cond_Wf := Get_Conditional_Waveform_Chain (Stmt);
      Has_Drv := False;
      while Cond_Wf /= Null_Iir loop
         Wf := Get_Waveform_Chain (Cond_Wf);
         if Get_Kind (Wf) /= Iir_Kind_Unaffected_Waveform then
            --  Not unaffected
            Extract_Has_After (Wf);
            Has_Drv := True;
         end if;
         Cond_Wf := Get_Chain (Cond_Wf);
      end loop;
      if Has_Drv then
         Status := Walk_Assignment_Target
           (Get_Target (Stmt), Extract_Driver_Target'Access);
      end if;
   end Extract_Driver_Conditional_Signal_Assignment;

   procedure Extract_Driver_Selected_Signal_Assignment (Stmt : Iir)
   is
      Status : Walk_Status;
      pragma Unreferenced (Status);
      Swf : Iir;
      Wf : Iir;
      Has_Drv : Boolean;
   begin
      Swf := Get_Selected_Waveform_Chain (Stmt);
      Has_Drv := False;
      while Swf /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Swf) then
            Wf := Get_Associated_Chain (Swf);
            if Get_Kind (Wf) /= Iir_Kind_Unaffected_Waveform then
               --  Not unaffected
               Extract_Has_After (Wf);
               Has_Drv := True;
            end if;
         end if;
         Swf := Get_Chain (Swf);
      end loop;
      if Has_Drv then
         Status := Walk_Assignment_Target
           (Get_Target (Stmt), Extract_Driver_Target'Access);
      end if;
   end Extract_Driver_Selected_Signal_Assignment;

   procedure Extract_Driver_Procedure_Call (Stmt : Iir)
   is
      Status : Walk_Status;
      pragma Unreferenced (Status);
      Call : constant Iir := Get_Procedure_Call (Stmt);
      Assoc : Iir;
      Formal : Iir;
      Inter : Iir;
   begin
      --  Very pessimist.
      Has_After := True;

      Assoc := Get_Parameter_Association_Chain (Call);
      Inter := Get_Interface_Declaration_Chain (Get_Implementation (Call));
      while Assoc /= Null_Iir loop
         Formal := Get_Association_Interface (Assoc, Inter);
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression
           and then Get_Kind (Formal) = Iir_Kind_Interface_Signal_Declaration
           and then Get_Mode (Formal) /= Iir_In_Mode
         then
            Status := Extract_Driver_Target (Get_Actual (Assoc));
         end if;
         Next_Association_Interface (Assoc, Inter);
      end loop;
   end Extract_Driver_Procedure_Call;

   function Extract_Driver_Stmt (Stmt : Iir) return Walk_Status
   is
      Status : Walk_Status;
      pragma Unreferenced (Status);
   begin
      --  Clear Has_After.  It will be set to True if a signal assignment
      --  has an delay expression or a null transaction.
      --  (It is cleared for any statement, just to factorize code).
      Has_After := False;

      case Iir_Kinds_Sequential_Statement_Ext (Get_Kind (Stmt)) is
         when Iir_Kind_Simple_Signal_Assignment_Statement =>
            Extract_Driver_Simple_Signal_Assignment (Stmt);
         when Iir_Kind_Signal_Force_Assignment_Statement
            | Iir_Kind_Signal_Release_Assignment_Statement =>
            null;
         when Iir_Kind_Conditional_Signal_Assignment_Statement =>
            Extract_Driver_Conditional_Signal_Assignment (Stmt);
         when Iir_Kind_Selected_Waveform_Assignment_Statement =>
            Extract_Driver_Selected_Signal_Assignment (Stmt);
         when Iir_Kind_Procedure_Call_Statement =>
            Extract_Driver_Procedure_Call (Stmt);
         when Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement
           | Iir_Kind_Case_Statement
           | Iir_Kind_If_Statement
           | Iir_Kind_Break_Statement =>
            null;
         when Iir_Kind_Suspend_State_Statement =>
            null;
      end case;
      return Walk_Continue;
   end Extract_Driver_Stmt;

   procedure Extract_Drivers_Sequential_Stmt_Chain (Chain : Iir)
   is
      Status : Walk_Status;
      pragma Unreferenced (Status);
   begin
      Status := Walk_Sequential_Stmt_Chain (Chain, Extract_Driver_Stmt'Access);
   end Extract_Drivers_Sequential_Stmt_Chain;

   procedure Extract_Drivers_Declaration_Chain (Chain : Iir)
   is
      Decl : Iir := Chain;
   begin
      while Decl /= Null_Iir loop

         --  Only procedures and impure functions may contain assignment.
         if Get_Kind (Decl) = Iir_Kind_Procedure_Body
           or else (Get_Kind (Decl) = Iir_Kind_Function_Body
                    and then
                      not Get_Pure_Flag (Get_Subprogram_Specification (Decl)))
         then
            Extract_Drivers_Declaration_Chain (Get_Declaration_Chain (Decl));
            Extract_Drivers_Sequential_Stmt_Chain
              (Get_Sequential_Statement_Chain (Decl));
         end if;

         Decl := Get_Chain (Decl);
      end loop;
   end Extract_Drivers_Declaration_Chain;

   function Extract_Drivers (Proc : Iir) return Iir_List is
   begin
      Driver_List := Create_Iir_List;
      case Get_Kind (Proc) is
         when Iir_Kinds_Process_Statement =>
            Extract_Drivers_Declaration_Chain (Get_Declaration_Chain (Proc));
            Extract_Drivers_Sequential_Stmt_Chain
              (Get_Sequential_Statement_Chain (Proc));
         when Iir_Kind_Concurrent_Simple_Signal_Assignment =>
            Extract_Driver_Simple_Signal_Assignment (Proc);
         when Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
            Extract_Driver_Conditional_Signal_Assignment (Proc);
         when Iir_Kind_Concurrent_Selected_Signal_Assignment =>
            Extract_Driver_Selected_Signal_Assignment (Proc);
         when Iir_Kind_Concurrent_Procedure_Call_Statement =>
            Extract_Driver_Procedure_Call (Proc);
         when Iir_Kind_Concurrent_Assertion_Statement =>
            null;
         when others =>
            Error_Kind ("extract_drivers", Proc);
      end case;
      return Driver_List;
   end Extract_Drivers;

   procedure Free_Drivers_List (List : in out Iir_List)
   is
      It : List_Iterator;
   begin
      It := List_Iterate (List);
      while Is_Valid (It) loop
         Set_After_Drivers_Flag (Get_Object_Prefix (Get_Element (It)), False);
         Next (It);
      end loop;
      Destroy_Iir_List (List);
   end Free_Drivers_List;

   procedure Dump_Drivers (Proc : Iir; List : Iir_List)
   is
      use Simple_IO;
      use Errorout;
      El : Iir;
      It : List_Iterator;
   begin
      Report_Msg (Msgid_Note, Semantic, +Proc,
                  "List of drivers for %n:", (1 => +Proc));
      Report_Msg (Msgid_Note, Semantic, +Proc,
                  " (declared at %l)", (1 => +Proc));
      It := List_Iterate (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         if Get_After_Drivers_Flag (Get_Object_Prefix (El)) then
            Put ("*  ");
         else
            Put ("   ");
         end if;
         Vhdl.Prints.Disp_Vhdl (El);
         New_Line;
         Next (It);
      end loop;
   end Dump_Drivers;

end Trans_Analyzes;
