--  Routine to dump (for debugging purpose) a netlist.
--  Copyright (C) 2017 Tristan Gingold
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

with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Iterators; use Netlists.Iterators;
with Netlists.Dump; use Netlists.Dump;

package body Netlists.Disp_Dot is
   procedure Disp_Dot_Instance (Inst : Instance)
   is
      M : constant Module := Get_Module (Inst);
      N : Net;
      I : Input;
      D : Instance;
   begin
      Put ("  i");
      Put_Uns32 (Uns32 (Inst));
      Put (" [label=""");
      Dump_Name (Get_Module_Name (M));
      if False then
         Put (""" id=""");
         Put_Uns32 (Uns32 (Inst));
      end if;
      Put_Line ("""];");

      for Idx in 1 .. Get_Nbr_Outputs (Inst) loop
         N := Get_Output (Inst, Idx - 1);
         I := Get_First_Sink (N);
         while I /= No_Input loop
            D := Get_Input_Parent (I);
            Put ("  i");
            Put_Uns32 (Uns32 (Inst));
            Put (" -> i");
            Put_Uns32 (Uns32 (D));
            Put (" [label=""n");
            Put_Uns32 (Uns32 (N));
            Put ("""]");
            Put_Line (";");
            I := Get_Next_Sink (I);
         end loop;
      end loop;
   end Disp_Dot_Instance;

   procedure Disp_Dot_Module (M : Module) is
   begin
      Put ("digraph m");
      Put_Uns32 (Uns32 (M));
      Put_Line (" {");

      --  Handle inputs.
      declare
         Self : constant Instance := Get_Self_Instance (M);
         N : Net;
         I : Input;
         D : Instance;
      begin
         if Self /= No_Instance then
            for Idx in 1 .. Get_Nbr_Inputs (M) loop
               Put ("  p");
               Put_Uns32 (Uns32 (Idx - 1));
               Put (" [label=""");
               Dump_Name (Get_Input_Desc (M, Idx - 1).Name);
               Put ("""];");
               New_Line;

               N := Get_Output (Self, Idx - 1);
               I := Get_First_Sink (N);
               while I /= No_Input loop
                  D := Get_Input_Parent (I);
                  Put ("  p");
                  Put_Uns32 (Uns32 (Idx - 1));
                  Put (" -> i");
                  Put_Uns32 (Uns32 (D));
                  Put (" [label=""n");
                  Put_Uns32 (Uns32 (N));
                  Put ("""]");
                  Put_Line (";");
                  I := Get_Next_Sink (I);
               end loop;
               New_Line;
            end loop;
         end if;
      end;

      for Inst of Instances (M) loop
         Disp_Dot_Instance (Inst);
         New_Line;
      end loop;

      Put_Line ("}");
   end Disp_Dot_Module;

   procedure Disp_Dot_Top_Module (M : Module) is
   begin
      --  Submodules.
      for S of Sub_Modules (M) loop
         if Get_Id (S) >= Id_User_None then
            Disp_Dot_Module (S);
            exit;
         end if;
      end loop;
   end Disp_Dot_Top_Module;
end Netlists.Disp_Dot;
