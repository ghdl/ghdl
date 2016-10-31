--  Analysis for translation.
--  Copyright (C) 2009 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Iirs_Utils; use Iirs_Utils;
with Iirs_Walk; use Iirs_Walk;
with Disp_Vhdl;
with Ada.Text_IO;
with Errorout;

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
        and then
        Get_Kind (Get_Parent (Base)) = Iir_Kind_Procedure_Declaration
      then
         return Walk_Continue;
      end if;

      Prefix := Get_Longuest_Static_Prefix (Target);
      Add_Element (Driver_List, Prefix);
      if Has_After then
         Set_After_Drivers_Flag (Base, True);
      end if;
      return Walk_Continue;
   end Extract_Driver_Target;

   procedure Extract_Has_After (Wf : Iir) is
   begin
      if Wf /= Null_Iir
        and then Get_Chain (Wf) = Null_Iir
        and then Get_Time (Wf) = Null_Iir
        and then Get_Kind (Get_We_Value (Wf)) /= Iir_Kind_Null_Literal
      then
         Has_After := False;
      else
         Has_After := True;
      end if;
   end Extract_Has_After;

   function Extract_Driver_Stmt (Stmt : Iir) return Walk_Status
   is
      Status : Walk_Status;
      pragma Unreferenced (Status);
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Simple_Signal_Assignment_Statement =>
            Extract_Has_After (Get_Waveform_Chain (Stmt));
            Status := Walk_Assignment_Target
              (Get_Target (Stmt), Extract_Driver_Target'Access);
         when Iir_Kind_Conditional_Signal_Assignment_Statement =>
            declare
               Cond_Wf : Iir;
            begin
               Cond_Wf := Get_Conditional_Waveform_Chain (Stmt);
               while Cond_Wf /= Null_Iir loop
                  Extract_Has_After (Get_Waveform_Chain (Cond_Wf));
                  Cond_Wf := Get_Chain (Cond_Wf);
               end loop;
               Status := Walk_Assignment_Target
                 (Get_Target (Stmt), Extract_Driver_Target'Access);
            end;
         when Iir_Kind_Procedure_Call_Statement =>
            declare
               Call : constant Iir := Get_Procedure_Call (Stmt);
               Assoc : Iir;
               Formal : Iir;
               Inter : Iir;
            begin
               --  Very pessimist.
               Has_After := True;

               Assoc := Get_Parameter_Association_Chain (Call);
               Inter := Get_Interface_Declaration_Chain
                 (Get_Implementation (Call));
               while Assoc /= Null_Iir loop
                  Formal := Get_Association_Interface (Assoc, Inter);
                  if Get_Kind (Assoc)
                    = Iir_Kind_Association_Element_By_Expression
                    and then
                    Get_Kind (Formal) = Iir_Kind_Interface_Signal_Declaration
                    and then Get_Mode (Formal) /= Iir_In_Mode
                  then
                     Status := Extract_Driver_Target (Get_Actual (Assoc));
                  end if;
                  Next_Association_Interface (Assoc, Inter);
               end loop;
            end;
         when others =>
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

   function Extract_Drivers (Proc : Iir) return Iir_List
   is
   begin
      Driver_List := Create_Iir_List;
      Extract_Drivers_Declaration_Chain (Get_Declaration_Chain (Proc));
      Extract_Drivers_Sequential_Stmt_Chain
              (Get_Sequential_Statement_Chain (Proc));

      return Driver_List;
   end Extract_Drivers;

   procedure Free_Drivers_List (List : in out Iir_List)
   is
      El : Iir;
   begin
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         Set_After_Drivers_Flag (Get_Object_Prefix (El), False);
      end loop;
      Destroy_Iir_List (List);
   end Free_Drivers_List;

   procedure Dump_Drivers (Proc : Iir; List : Iir_List)
   is
      use Ada.Text_IO;
      use Errorout;
      El : Iir;
   begin
      Report_Msg (Msgid_Note, Semantic, +Proc,
                  "List of drivers for %n:", (1 => +Proc));
      Report_Msg (Msgid_Note, Semantic, +Proc,
                  " (declared at %l)", (1 => +Proc));
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         if Get_After_Drivers_Flag (Get_Object_Prefix (El)) then
            Put ("*  ");
         else
            Put ("   ");
         end if;
         Disp_Vhdl.Disp_Vhdl (El);
         New_Line;
      end loop;
   end Dump_Drivers;

end Trans_Analyzes;
