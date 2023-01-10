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

   -- That can't be right.
   -- This must already be defined somewhere.
   type Port_Dir is (Port_Input, Port_Output);

   Prefix_Port_Input  : constant String := "pi";
   Prefix_Port_Output : constant String := "po";
   Prefix_Instance    : constant String := "i";

   procedure Put_Port (Dir : in Port_Dir; M : in Module; Idx : in Port_Nbr) is
   begin
      Put("  ");
      case Dir is
         when Port_Input  => Put(Prefix_Port_Input);
         when Port_Output => Put(Prefix_Port_Output);
      end case;
      Put_Uns32 (Uns32 (Idx - 1));
      Put(" [label=""" & "\");
      case Dir is
         when Port_Input  => Dump_Name(Get_Input_Desc (M, Idx - 1).Name);
         when Port_Output => Dump_Name(Get_Output_Desc(M, Idx - 1).Name);
      end case;
      Put("""];");
      New_Line;
   end Put_Port;

   procedure Put_Port_Input (M : in Module; Idx : in Port_Nbr) is
   begin
      Put_Port(Port_Input, M, Idx);
   end Put_Port_Input;

   procedure Put_Instance (Inst : in Instance; M : in Module) is
   begin
      Put ("  " & Prefix_Instance);
      Put_Uns32 (Uns32 (Inst));
      Put (" [label=""");
      Dump_Name (Get_Module_Name (M));
      Put_Line ("""];");
   end Put_Instance;

   procedure Put_Port_Output (M : in Module; Idx : in Port_Nbr) is
   begin
      Put_Port(Port_Output, M, Idx);
   end Put_Port_Output;

   procedure Put_Net_Port_To_Instance (Idx : in Port_Nbr;
                                       D : in Instance; N : in Net) is
   begin
      Put ("  " & Prefix_Port_Input);
      Put_Uns32 (Uns32 (Idx - 1));
      Put (" -> " & Prefix_Instance);
      Put_Uns32 (Uns32 (D));
      Put (" [label=""n");
      Put_Uns32 (Uns32 (N));
      Put ("""]");
      Put_Line (";");
   end Put_Net_Port_To_Instance;

   procedure Put_Net_Instance_To_Port (D : in Instance;
                                       Idx : in Port_Nbr; N : in Net) is
   begin
      Put("  " & Prefix_Instance);
      Put_Uns32(Uns32(D));
      Put(" -> " & Prefix_Port_Output);
      Put_Uns32 (Uns32 (Idx - 1));
      Put (" [label=""n");
      Put_Uns32 (Uns32 (N));
      Put ("""]");
      Put_Line (";");
   end Put_Net_Instance_To_Port;

   procedure Put_Net_Instance_To_Instance (Inst, D : in Instance;
                                           N : in Net) is
   begin
      Put ("  " & Prefix_Instance);
      Put_Uns32 (Uns32 (Inst));
      Put (" -> " & Prefix_Instance);
      Put_Uns32 (Uns32 (D));
      Put (" [label=""n");
      Put_Uns32 (Uns32 (N));
      Put ("""]");
      Put_Line (";");
   end Put_Net_Instance_To_Instance;

   procedure Disp_Dot_Instance (Self : in Instance; Inst : Instance)
   is
      M : constant Module := Get_Module (Inst);
      N : Net;
      I : Input;
      D : Instance;
   begin
      Put_Instance(Inst, M);

      for Idx in 1 .. Get_Nbr_Outputs (Inst) loop
         N := Get_Output (Inst, Idx - 1);
         I := Get_First_Sink (N);
         while I /= No_Input loop
            D := Get_Input_Parent (I);
            if D = Self then
               -- Hold on a second, this net goes straight to
               -- an output port of the top module !
               -- Do not write the net, the top module has
               -- already done it.
               null;
            else
               Put_Net_Instance_To_Instance(Inst, D, N);
            end if;
            I := Get_Next_Sink (I);
         end loop;
      end loop;
   end Disp_Dot_Instance;

   procedure Disp_Dot_Module (M : Module) is
      Self : constant Instance := Get_Self_Instance (M);
      N : Net;
      I : Input;
      D : Instance;
   begin
      Put ("digraph m");
      Put_Uns32 (Uns32 (M));
      Put_Line (" {");

      -- uh ?
      if Self = No_Instance then
         return;
      end if;

      --  Handle inputs and outputs.
      for Idx in 1 .. Get_Nbr_Inputs (M) loop
         Put_Port_Input(M, Idx);

         N := Get_Output (Self, Idx - 1);
         I := Get_First_Sink (N);
         while I /= No_Input loop
            D := Get_Input_Parent (I);
            Put_Net_Port_To_Instance(Idx, D, N);
            I := Get_Next_Sink (I);
         end loop;
         New_Line;
      end loop;

      for Idx in 1 .. Get_Nbr_Outputs(M) loop
         Put_Port_Output(M, Idx);
         I := Get_Input(Self, Idx - 1);
         N := Get_Driver(I);
         D := Get_Net_Parent(N);
         Put_Net_Instance_To_Port(D, Idx, N);
         New_Line;
      end loop;

      for Inst of Instances (M) loop
         Disp_Dot_Instance (Self, Inst);
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
