--  Synthesis checks
--  Copyright (C) 2021 Ondrej Ille
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
with Vhdl.Canon; use Vhdl.Canon;
with Vhdl.Errors; use Vhdl.Errors;
with Errorout; use Errorout;
with Vhdl.Disp_Tree; use Vhdl.Disp_Tree;
--with Simple_IO; use Simple_IO;
--with Vhdl.Nodes_Meta; use Vhdl.Nodes_Meta;


package body Vhdl.Synthesis_Checks is

   --      "Golden" list of signals is queried by Sensitivity list extraction
   --      mechanism used in case of "all" keyword. This is a good option,
   --      because it is LRM standard way of defining "what all should trigger
   --      the process".
   function Get_Golden_Sensitivity_List (Proc : Iir) return Iir_List is
   begin
      -- FIXME: We have to correctly extract golden list for clocked processes!
      --        LRM extraction algorithm takes all RHS signals which is not
      --        desirable in clocked process!
      return Canon_Extract_Sensitivity_Process (Proc);
   end Get_Golden_Sensitivity_List;

   -- Checks if each signal from "Golden sensitivity list" are present in
   -- sensitivity list of a process. Throws a warning for each missing signal.
   procedure Check_Incomplete_Sensitivity_List (Proc : Iir;
                                                Gold_List : Iir_List;
                                                Real_List : Iir_List)
   is
      Real_It : List_Iterator;
      Gold_It : List_Iterator;

      Gold_Iir : Iir;
      Real_Iir : Iir;

      Match : Boolean;
   begin
      Gold_It := List_Iterate (Gold_List);
      while Is_Valid (Gold_It) loop
         Gold_Iir := Get_Element (Gold_It);
         Match := False;
         Real_It := List_Iterate (Real_List);

         while Is_Valid (Real_It) loop
            --Put_Line ("Checking pair:");
            --Put_Line ("Golden:");
            --Disp_Iir (Get_Element (Gold_It), 4, 1);
            --Put_Line ("Real:");
            --Disp_Iir (Get_Element (Real_It), 4, 1);
            --Put_Line ("Named entity:");
            --Disp_Iir (Get_Named_Entity (Get_Element (Real_It)), 4, 1);
            Real_Iir := Get_Element (Real_It);
            if Get_Named_Entity (Real_Iir) = Gold_Iir then
               Match := True;
               exit;
            end if;
            Next (Real_It);
         end loop;

         if Match = False then
            Warning_Msg_Sem (Warnid_Synth_Sensitivity_Lists, +Proc,
                             "Incomplete sensitivity list! Missing signal: " &
                             Image_Name_Id (Get_Identifier (Gold_Iir)));
         end if;
         Next (Gold_It);
      end loop;
   end Check_Incomplete_Sensitivity_List;

   -- Checks if each signal from process sensitivity list are present in
   -- "Golden sensitivity list". Throws a warning for each missing signal.
   procedure Check_Overspecified_Sensitivity_List (Proc : Iir;
                                                   Gold_List : Iir_List;
                                                   Real_List : Iir_List)
   is
      Real_It : List_Iterator;
      Gold_It : List_Iterator;

      Gold_Iir : Iir;
      Real_Iir : Iir;

      Match : Boolean;
   begin
      Real_It := List_Iterate (Real_List);
      while Is_Valid (Real_It) loop
         Real_Iir := Get_Element (Real_It);
         Match := False;
         Gold_It := List_Iterate (Gold_List);

         while Is_Valid (Gold_It) loop
            Gold_Iir := Get_Element (Gold_It);
            if Get_Named_Entity (Real_Iir) = Gold_Iir then
               Match := True;
               exit;
            end if;
            Next (Gold_It);
         end loop;

         if Match = False then
            Warning_Msg_Sem (Warnid_Synth_Sensitivity_Lists, +Proc,
                             "Over-specified sensitivity list! " &
                             "Redundant signal: " &
                             Image_Name_Id (Get_Identifier (Real_Iir)));
         end if;

         Next (Real_It);
      end loop;
   end Check_Overspecified_Sensitivity_List;

   procedure Check_Sensitivity_Lists (Unit: Iir_Design_Unit) is
      El : Iir;
      Stmt : Iir;
      Kind : Iir_Kind;
      Gold_List : Iir_List;
      Real_List : Iir_List;
   begin
      El := Get_Library_Unit (Unit);
      if Iir_Kinds_Library_Unit (Get_Kind (El)) = Iir_Kind_Architecture_Body
      then
         Stmt := Get_Concurrent_Statement_Chain (El);
         while Stmt /= Null_Iir loop
            Kind := Get_Kind(Stmt);

            -- FIXME: We should to ignore processes if we have "pragma
            --        synthesis off" defined! Such processes will not be
            --        synthesized, therefore shall not be checked!
            if Kind = Iir_Kind_Sensitized_Process_Statement then
               Gold_List := Get_Golden_Sensitivity_List (Stmt);
               Real_List := Get_Sensitivity_List (Stmt);

               -- Skip clocked processes when checking !
               if Get_Is_Clocked_Process (Stmt) = False and
                  Real_List /= Iir_List_All
               then
                  Check_Incomplete_Sensitivity_List (Stmt, Gold_List,
                                                     Real_List);
                  Check_Overspecified_Sensitivity_List (Stmt, Gold_List,
                                                        Real_List);
               end if;
               Destroy_Iir_List (Gold_List);
            end if;
            Stmt := Get_Chain (Stmt);
         end loop;
      end if;
   end Check_Sensitivity_Lists;

   procedure DoSynthesisChecks (Unit: Iir_Design_Unit) is
   begin
      Check_Sensitivity_Lists(Unit);
   end DoSynthesisChecks;

end Vhdl.Synthesis_Checks;