--  Routine to dump (for debugging purpose) a netlist.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Ada.Text_IO; use Ada.Text_IO;
with Name_Table;
--  with Netlists.Utils; use Netlists.Utils;
with Netlists.Iterators; use Netlists.Iterators;
--  with Netlists.Gates; use Netlists.Gates;

package body Netlists.Disp_Vhdl is
   --  Like Put, but without the leading space (if any).
   procedure Put_Trim (S : String) is
   begin
      if S'First <= S'Last and then S (S'First) = ' ' then
         Put (S (S'First + 1 .. S'Last));
      else
         Put (S);
      end if;
   end Put_Trim;

   procedure Put_Type (W : Width) is
   begin
      if W = 1 then
         Put ("std_logic");
      else
         Put ("std_logic_vector (");
         Put_Trim (Width'Image (W - 1));
         Put (" downto 0)");
      end if;
   end Put_Type;

   procedure Put_Name (N : Sname)
   is
      use Name_Table;
      Prefix : Sname;
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Put ("*nil*");
         return;
      end if;

      Prefix := Get_Sname_Prefix (N);

      case Get_Sname_Kind (N) is
         when Sname_User =>
            if Prefix = No_Sname then
               Put ("\");
            else
               Put_Name (Prefix);
               Put (".");
            end if;
            Put (Image (Get_Sname_Suffix (N)));
         when Sname_Artificial =>
            if Prefix = No_Sname then
               Put ("$");
            else
               Put_Name (Prefix);
               Put (".");
            end if;
            Put (Image (Get_Sname_Suffix (N)));
         when Sname_Version =>
            Put_Name (Prefix);
            Put ("%");
            Put_Trim (Uns32'Image (Get_Sname_Version (N)));
      end case;
   end Put_Name;

   procedure Disp_Entity (M : Module)
   is
      First : Boolean;
   begin
      --  Module id and name.
      Put ("entity ");
      Put_Name (Get_Name (M));
      Put_Line (" is");

      --  Ports.
      First := True;
      for P of Ports_Desc (M) loop
         if First then
            Put_Line ("  port (");
            First := False;
         else
            Put_Line (";");
         end if;
         Put ("    ");
         Put_Name (P.Name);
         Put (" : ");
         case P.Dir is
            when Port_In =>
               Put ("in");
            when Port_Out =>
               Put ("out");
            when Port_Inout =>
               Put ("inout");
         end case;
         Put (' ');
         Put_Type (P.W);
      end loop;
      if not First then
         Put_Line (");");
      end if;

      Put ("end entity ");
      Put_Name (Get_Name (M));
      Put_Line (";");
      New_Line;
   end Disp_Entity;

   procedure Disp_Net_Name (N : Net) is
   begin
      if N = No_Net then
         Put ("<unassigned>");
      else
         declare
            Inst : constant Instance := Get_Parent (N);
            Idx : constant Port_Idx := Get_Port_Idx (N);
         begin
            if Is_Self_Instance (Inst) then
               Put_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name);
            else
               Put_Name (Get_Name (Inst));
               Put ('.');
               Put_Name (Get_Output_Desc (Get_Module (Inst), Idx).Name);
            end if;
         end;
      end if;
   end Disp_Net_Name;

   procedure Disp_Architecture (M : Module)
   is
      First : Boolean;
   begin
      Put ("architecture rtl of ");
      Put_Name (Get_Name (M));
      Put_Line (" is");

      --  Dummy display:
      --  * generate one signal per net
      --  * generate instances

      --  Display signal declarations.
      --  There are as many signals as gate outputs.
      for Inst of Instances (M) loop
         if not Is_Self_Instance (Inst) then
            for N of Outputs (Inst) loop
               Put ("  signal ");
               Disp_Net_Name (N);
               Put (" : ");
               Put_Type (Get_Width (N));
               Put_Line (";");
            end loop;
         end if;
      end loop;

      Put_Line ("begin");

      --  Output assignments.
      declare
         Inst : constant Instance := Get_Self_Instance (M);
         Idx : Port_Idx;
      begin
         Idx := 0;
         for I of Inputs (Inst) loop
            Put ("  ");
            Put_Name (Get_Output_Desc (M, Idx).Name);
            Put (" <= ");
            Disp_Net_Name (Get_Driver (I));
            New_Line;
            Idx := Idx + 1;
         end loop;
      end;

      for Inst of Instances (M) loop
         declare
            Imod : constant Module := Get_Module (Inst);
            Idx : Port_Idx;
            P_Idx : Param_Idx;
         begin
            Put ("  ");
            Put_Name (Get_Name (Inst));
            Put (" : work.");
            Put_Name (Get_Name (Imod));

            if Get_Nbr_Params (Imod) /= 0 then
               Put_Line (" generic map (");
               First := True;
               Idx := 0;
               for P in Params (Inst) loop
                  if First then
                     First := False;
                  else
                     Put_Line (",");
                  end if;
                  Put ("    ");
                  P_Idx := Get_Param_Idx (P);
                  Put_Name (Get_Param_Desc (Imod, P_Idx).Name);
                  Put (" => ");
                  Put_Trim (Uns32'Image (Get_Param_Uns32 (Inst, P_Idx)));
               end loop;
               Put_Line (")");
               Put_Line ("    port map (");
            else
               Put_Line (" port map (");
            end if;

            First := True;
            --  Inputs
            Idx := 0;
            for I of Inputs (Inst) loop
               if First then
                  First := False;
               else
                  Put_Line (",");
               end if;
               Put ("    ");
               Put_Name (Get_Input_Desc (Imod, Idx).Name);
               Idx := Idx + 1;
               Put (" => ");
               Disp_Net_Name (Get_Driver (I));
            end loop;
            --  Outputs
            Idx := 0;
            for O of Outputs (Inst) loop
               if First then
                  First := False;
               else
                  Put_Line (", ");
               end if;
               Put ("    ");
               Put_Name (Get_Output_Desc (Imod, Idx).Name);
               Idx := Idx + 1;
               Put (" => ");
               Disp_Net_Name (O);
            end loop;
            Put_Line (");");
         end;
      end loop;

      Put_Line ("end rtl;");
      New_Line;
   end Disp_Architecture;

   procedure Disp_Vhdl (M : Module) is
   begin
      for S of Sub_Modules (M) loop
         if Get_Id (S) >= Id_User_None then
            Disp_Vhdl (S);
         end if;
      end loop;

      Disp_Entity (M);
      Disp_Architecture (M);
   end Disp_Vhdl;
end Netlists.Disp_Vhdl;
