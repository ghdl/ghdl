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
with Name_Table; use Name_Table;
--  with Netlists.Utils; use Netlists.Utils;
with Netlists.Iterators; use Netlists.Iterators;
with Netlists.Gates; use Netlists.Gates;

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

   procedure Put_Uns32 (V : Uns32) is
   begin
      Put_Trim (Uns32'Image (V));
   end Put_Uns32;

   procedure Put_Type (W : Width) is
   begin
      if W = 1 then
         Put ("std_logic");
      else
         Put ("std_logic_vector (");
         Put_Uns32 (W - 1);
         Put (" downto 0)");
      end if;
   end Put_Type;

   procedure Put_Id (N : Name_Id) is
   begin
      Put (Name_Table.Image (N));
   end Put_Id;

   procedure Put_Name_Version (N : Sname) is
   begin
      Put_Uns32 (Get_Sname_Version (N));
   end Put_Name_Version;

   procedure Put_Name_1 (N : Sname)
   is
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
               Put_Name_1 (Prefix);
               Put (".");
            end if;
            Put_Id (Get_Sname_Suffix (N));
         when Sname_Artificial =>
            if Prefix = No_Sname then
               Put ("$");
            else
               Put_Name_1 (Prefix);
               Put (".");
            end if;
            Put (Image (Get_Sname_Suffix (N)));
         when Sname_Version =>
            Put_Name_1 (Prefix);
            Put ("%");
            Put_Name_Version (N);
      end case;
   end Put_Name_1;

   procedure Put_Name (N : Sname) is
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Put ("*nil*");
         return;
      end if;

      if Get_Sname_Kind (N) = Sname_User
        and then Get_Sname_Prefix (N) = No_Sname
      then
         Put (Name_Table.Image (Get_Sname_Suffix (N)));
      else
         Put_Name_1 (N);
      end if;
   end Put_Name;

   procedure Put_Interface_Name (N : Sname) is
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Put ("*nil*");
         return;
      end if;

      if Get_Sname_Kind (N) = Sname_Artificial
        and then Get_Sname_Prefix (N) = No_Sname
      then
         Put (Name_Table.Image (Get_Sname_Suffix (N)));
      else
         Put ("*err*");
      end if;
   end Put_Interface_Name;

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
            Inst_Name : Sname;
            Port_Name : Sname;
         begin
            if Is_Self_Instance (Inst) then
               Put_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name);
            else
               Inst_Name := Get_Name (Inst);
               Port_Name := Get_Output_Desc (Get_Module (Inst), Idx).Name;
               if Get_Sname_Kind (Inst_Name) = Sname_Version then
                  Put ("net_");
                  Put_Name_Version (Inst_Name);
                  Put ("_");
                  Put_Interface_Name (Port_Name);
               else
                  Put_Name (Inst_Name);
                  Put ('.');
                  Put_Name (Port_Name);
               end if;
            end if;
         end;
      end if;
   end Disp_Net_Name;

   procedure Disp_Instance_Gate (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
      Idx : Port_Idx;
      P_Idx : Param_Idx;
      Name : Sname;
      First : Boolean;
   begin
      Put ("  ");
      Name := Get_Name (Inst);
      if Get_Sname_Kind (Name) = Sname_Version then
         Put ("inst_");
         Put_Name_Version (Name);
      else
         Put_Name (Name);
      end if;
      Put (" : gsynth.gate_");
      --  Gate name
      Name := Get_Name (Imod);
      pragma Assert (Get_Sname_Kind (Name) = Sname_Artificial
                       and then Get_Sname_Prefix (Name) = No_Sname);
      Put_Id (Get_Sname_Suffix (Name));

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
            Put_Interface_Name (Get_Param_Desc (Imod, P_Idx).Name);
            Put (" => ");
            Put_Uns32 (Get_Param_Uns32 (Inst, P_Idx));
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
         Put_Interface_Name (Get_Input_Desc (Imod, Idx).Name);
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
         Put_Interface_Name (Get_Output_Desc (Imod, Idx).Name);
         Idx := Idx + 1;
         Put (" => ");
         Disp_Net_Name (O);
      end loop;
      Put_Line (");");
   end Disp_Instance_Gate;

   Bchar : constant array (Uns32 range 0 .. 1) of Character := "01";

   type Net_Array is array (Positive range <>) of Net;
   NL : constant Character := ASCII.LF;

   procedure Disp_Template (S : String; N : Net_Array)
   is
      I : Positive;
      C : Character;
   begin
      I := S'First;
      while I <= S'Last loop
         C := S (I);
         if C = '\' then
            I := I + 1;
            C := S (I);
            Disp_Net_Name (N (Character'Pos (C) - Character'Pos ('0')));
         else
            Put (C);
         end if;
         I := I + 1;
      end loop;
   end Disp_Template;

   procedure Disp_Instance_Inline (Inst : Instance)
   is
      Imod : constant Module := Get_Module (Inst);
   begin
      case Get_Id (Imod) is
         when Id_Output =>
            Put ("  ");
            Disp_Net_Name (Get_Output (Inst, 0));
            Put (" <= ");
            Disp_Net_Name (Get_Input_Net (Inst, 0));
            Put_Line (";  -- (output)");
         when Id_Not =>
            Put ("  ");
            Disp_Net_Name (Get_Output (Inst, 0));
            Put (" <= not ");
            Disp_Net_Name (Get_Input_Net (Inst, 0));
            Put_Line (";");
         when Id_Extract =>
            declare
               O : constant Net := Get_Output (Inst, 0);
               Wd : constant Width := Get_Width (O);
               Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
            begin
               Disp_Template ("  \1 <= \2 (", (O, Get_Input_Net (Inst, 0)));
               if Wd > 1 then
                  Put_Uns32 (Off + Wd - 1);
                  Put (" downto ");
               end if;
               Put_Uns32 (Off);
               Put_Line (");");
            end;
         when Id_Const_UB32 =>
            declare
               O : constant Net := Get_Output (Inst, 0);
               Wd : constant Width := Get_Width (O);
               V : constant Uns32 := Get_Param_Uns32 (Inst, 0);
            begin
               Put ("  ");
               Disp_Net_Name (Get_Output (Inst, 0));
               Put (" <= ");
               if Wd = 1 then
                  Put (''');
                  Put (Bchar (V));
                  Put (''');
               else
                  Put ('"');
                  for I in 0 .. Wd - 1 loop
                     Put (Bchar ((V / 2**Natural (I)) and 1));
                  end loop;
                  Put ('"');
               end if;
               Put_Line (";");
            end;
         when Id_Adff =>
            declare
               Clk : constant Net := Get_Input_Net (Inst, 0);
               D : constant Net := Get_Input_Net (Inst, 1);
               Rst : constant Net := Get_Input_Net (Inst, 2);
               Rst_Val : constant Net := Get_Input_Net (Inst, 3);
               O : constant Net := Get_Output (Inst, 0);
            begin
               Disp_Template
                 ("  process (\1, \3)" & NL &
                  "  begin" & NL &
                  "    if \3 = '1' then" & NL &
                  "      \5 <= \4;" & NL &
                  "    elsif rising_edge (\1) then" & NL &
                  "      \5 <= \2;" & NL &
                  "    end if;" & NL &
                  "  end process;" & NL,
                  (1 => Clk, 2 => D, 3 => Rst, 4 => Rst_Val, 5 => O));
            end;
         when Id_Dff =>
            declare
               Clk : constant Net := Get_Input_Net (Inst, 0);
               D : constant Net := Get_Input_Net (Inst, 1);
               O : constant Net := Get_Output (Inst, 0);
            begin
               Disp_Template
                 ("  process (\1)" & NL &
                  "  begin" & NL &
                  "    if rising_edge (\1) then" & NL &
                  "      \3 <= \2;" & NL &
                  "    end if;" & NL &
                  "  end process;" & NL, (1 => Clk, 2 => D, 3 => O));
            end;
         when Id_Mux2 =>
            Disp_Template ("  \4 <= \2 when \1 = '0' else \3;" & NL,
                           (1 => Get_Input_Net (Inst, 0),
                            2 => Get_Input_Net (Inst, 1),
                            3 => Get_Input_Net (Inst, 2),
                            4 => Get_Output (Inst, 0)));
         when others =>
            Disp_Instance_Gate (Inst);
      end case;
   end Disp_Instance_Inline;

   procedure Disp_Architecture (M : Module) is
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
         Disp_Instance_Inline (Inst);
      end loop;

      Put_Line ("end rtl;");
      New_Line;
   end Disp_Architecture;

   procedure Disp_Vhdl (M : Module; Is_Top : Boolean) is
   begin
      for S of Sub_Modules (M) loop
         if Get_Id (S) >= Id_User_None then
            Disp_Vhdl (S, False);
         end if;
      end loop;

      if not Is_Top then
         Disp_Entity (M);
         Disp_Architecture (M);
      end if;
   end Disp_Vhdl;

   procedure Disp_Vhdl (M : Module) is
   begin
      Disp_Vhdl (M, True);
   end Disp_Vhdl;
end Netlists.Disp_Vhdl;
