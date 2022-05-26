--  Walk in iirs nodes.
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

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors; use Vhdl.Errors;

package body Vhdl.Nodes_Walk is
   function Walk_Chain (Chain : Iir; Cb : Walk_Cb) return Walk_Status
   is
      El : Iir;
      Status : Walk_Status := Walk_Continue;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Status := Cb.all (El);
         exit when Status /= Walk_Continue;
         El := Get_Chain (El);
      end loop;
      return Status;
   end Walk_Chain;

   function Walk_Sequential_Stmt (Stmt : Iir; Cb : Walk_Cb) return Walk_Status;


   function Walk_Sequential_Stmt_Chain (Chain : Iir; Cb : Walk_Cb)
                                       return Walk_Status
   is
      El : Iir;
      Status : Walk_Status := Walk_Continue;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Status := Cb.all (El);
         exit when Status /= Walk_Continue;
         Status := Walk_Sequential_Stmt (El, Cb);
         exit when Status /= Walk_Continue;
         El := Get_Chain (El);
      end loop;
      return Status;
   end Walk_Sequential_Stmt_Chain;

   function Walk_Sequential_Stmt (Stmt : Iir; Cb : Walk_Cb) return Walk_Status
   is
      Status : Walk_Status := Walk_Continue;
      Chain : Iir;
   begin
      case Iir_Kinds_Sequential_Statement_Ext (Get_Kind (Stmt)) is
         when Iir_Kind_Simple_Signal_Assignment_Statement
           | Iir_Kind_Conditional_Signal_Assignment_Statement
           | Iir_Kind_Selected_Waveform_Assignment_Statement
           | Iir_Kind_Signal_Force_Assignment_Statement
           | Iir_Kind_Signal_Release_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Variable_Assignment_Statement
           | Iir_Kind_Conditional_Variable_Assignment_Statement
           | Iir_Kind_Break_Statement
           | Iir_Kind_Suspend_State_Statement =>
            null;
         when Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement =>
            Status := Walk_Sequential_Stmt_Chain
              (Get_Sequential_Statement_Chain (Stmt), Cb);
         when Iir_Kind_Case_Statement =>
            Chain := Get_Case_Statement_Alternative_Chain (Stmt);
            while Chain /= Null_Iir loop
               Status := Walk_Sequential_Stmt_Chain
                 (Get_Associated_Chain (Chain), Cb);
               exit when Status /= Walk_Continue;
               Chain := Get_Chain (Chain);
            end loop;
         when Iir_Kind_If_Statement =>
            Chain := Stmt;
            while Chain /= Null_Iir loop
               Status := Walk_Sequential_Stmt_Chain
                 (Get_Sequential_Statement_Chain (Chain), Cb);
               exit when Status /= Walk_Continue;
               Chain := Get_Else_Clause (Chain);
            end loop;
      end case;
      return Status;
   end Walk_Sequential_Stmt;

   function Walk_Assignment_Target (Target : Iir; Cb : Walk_Cb)
                                   return Walk_Status
   is
      Targ : constant Iir := Strip_Reference_Name (Target);
      Chain : Iir;
      Status : Walk_Status := Walk_Continue;
   begin
      case Get_Kind (Targ) is
         when Iir_Kind_Aggregate =>
            Chain := Get_Association_Choices_Chain (Targ);
            while Chain /= Null_Iir loop
               Status :=
                 Walk_Assignment_Target (Get_Associated_Expr (Chain), Cb);
               exit when Status /= Walk_Continue;
               Chain := Get_Chain (Chain);
            end loop;
         when others =>
            Status := Cb.all (Targ);
      end case;
      return Status;
   end Walk_Assignment_Target;

   function Walk_Design_Units (Parent : Iir; Cb : Walk_Cb) return Walk_Status
   is
      El : Iir;
      Status : Walk_Status := Walk_Continue;
   begin
      case Get_Kind (Parent) is
         when Iir_Kind_Library_Declaration =>
            El := Get_Design_File_Chain (Parent);
            while Is_Valid (El) loop
               Status := Walk_Design_Units (El, Cb);
               exit when Status /= Walk_Continue;
               El := Get_Chain (El);
            end loop;
            return Status;
         when Iir_Kind_Design_File =>
            El := Get_First_Design_Unit (Parent);
            while Is_Valid (El) loop
               Status := Cb.all (El);
               exit when Status /= Walk_Continue;
               El := Get_Chain (El);
            end loop;
            return Status;
         when others =>
            Error_Kind ("walk_library_units", Parent);
      end case;
   end Walk_Design_Units;

   function Walk_Concurrent_Statement (Stmt : Iir; Cb : Walk_Cb)
                                      return Walk_Status
   is
      Status : Walk_Status;
   begin
      case Get_Kind (Stmt) is
         when Iir_Kinds_Simple_Concurrent_Statement
           | Iir_Kind_Component_Instantiation_Statement
           | Iir_Kind_Psl_Default_Clock =>
            Status := Cb.all (Stmt);
         when Iir_Kind_Block_Statement =>
            Status := Cb.all (Stmt);
            if Status = Walk_Continue then
               Status := Walk_Concurrent_Statements_Chain
                 (Get_Concurrent_Statement_Chain (Stmt), Cb);
            end if;
         when Iir_Kind_For_Generate_Statement =>
            Status := Cb.all (Stmt);
            if Status = Walk_Continue then
               Status := Walk_Concurrent_Statements_Chain
                 (Get_Concurrent_Statement_Chain
                    (Get_Generate_Statement_Body (Stmt)), Cb);
            end if;
         when Iir_Kind_If_Generate_Statement =>
            declare
               Cl : Node;
            begin
               Status := Cb.all (Stmt);
               Cl := Stmt;
               while Status = Walk_Continue and then Cl /= Null_Node loop
                  Status := Walk_Concurrent_Statements_Chain
                    (Get_Concurrent_Statement_Chain
                       (Get_Generate_Statement_Body (Cl)), Cb);
                  Cl := Get_Generate_Else_Clause (Cl);
               end loop;
            end;
         when others =>
            Error_Kind ("walk_concurrent_statement", Stmt);
      end case;
      return Status;
   end Walk_Concurrent_Statement;

   function Walk_Concurrent_Statements_Chain (Chain : Iir; Cb : Walk_Cb)
                                       return Walk_Status
   is
      Status : Walk_Status;
      El : Iir;
   begin
      El := Chain;
      while Is_Valid (El) loop
         Status := Walk_Concurrent_Statement (El, Cb);
         if Status /= Walk_Continue then
            return Status;
         end if;
         El := Get_Chain (El);
      end loop;

      return Walk_Continue;
   end Walk_Concurrent_Statements_Chain;
end Vhdl.Nodes_Walk;
