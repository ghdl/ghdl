--  Simulation of VHDL
--  Copyright (C) 2023 Tristan Gingold
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

with Areapools;

with Synth.Flags;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Debugger;

with Simul.Vhdl_Debug;
with Simul.Vhdl_Elab;

with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Stdio; use Grt.Stdio;
with Grt.Options;
with Grt.Processes;
with Grt.Errors;
with Grt.Main;
with Grt.Disp_Signals;

package body Simul.Main is
   Ghdl_Progname : constant String := "ghdl" & ASCII.Nul;

   procedure Csv_File_Open is
   begin
      if Csv_Filename /= null then
         if Csv_Filename.all = "-" then
            Csv_File := stdout;
         else
            declare
               Filename : constant String := Csv_Filename.all & ASCII.NUL;
               W : constant String := 'w' & ASCII.NUL;
            begin
               Csv_File := fopen (Filename'Address, W'Address);
               if Csv_File = NULL_Stream then
                  Grt.Errors.Error_S ("cannot open '");
                  Grt.Errors.Diag_C (Csv_Filename.all);
                  Grt.Errors.Error_E ("' for --wave-csv");
               end if;
            end;
         end if;
      else
         Csv_File := NULL_Stream;
      end if;
   end Csv_File_Open;

   procedure Simulation
   is
      Ok : C_Boolean;
      Status : Integer;
   begin
      Break_Time := Std_Time'Last;
      Break_Step := False;

      Grt.Options.Progname := To_Ghdl_C_String (Ghdl_Progname'Address);
      Grt.Errors.Set_Error_Stream (Grt.Stdio.stdout);

      Elab.Debugger.Error_Hook := Grt.Errors.Fatal_Error'Access;
      Simul.Vhdl_Debug.Init;

      pragma Assert (Areapools.Is_Empty (Expr_Pool));

      if Flag_Debug_Elab then
         Elab.Debugger.Debug_Elab (Vhdl_Elab.Top_Instance);
      end if;

      Ok := Grt.Main.Run_Elab;
      if not Ok then
         return;
      end if;

      pragma Assert (Areapools.Is_Empty (Expr_Pool));
      pragma Assert (Areapools.Is_Empty (Process_Pool));

      --  Copy flag.
      Synth.Flags.Severity_Level := Grt.Options.Severity_Level;

      if Flag_Interractive then
         Elab.Debugger.Debug_Elab (Vhdl_Elab.Top_Instance);
      end if;

      Grt.Errors.Set_Error_Stream (Grt.Stdio.stdout);

      --  Handle --no-run
      if Grt.Options.Flag_No_Run then
         return;
      end if;

      Csv_File_Open;

      Status := Grt.Main.Run_Through_Longjump
        (Grt.Processes.Simulation_Init'Access);

      if Status = 0 then
         pragma Assert (Areapools.Is_Empty (Expr_Pool));
         pragma Assert (Areapools.Is_Empty (Process_Pool));

         loop
            if Break_Time < Grt.Processes.Next_Time then
               Grt.Processes.Next_Time := Break_Time;
            end if;

            Status := Grt.Main.Run_Through_Longjump
              (Grt.Processes.Simulation_Cycle'Access);
            exit when Status < 0
              or Status = Grt.Errors.Run_Stop;

            if Grt.Options.Trace_Signals then
               Grt.Disp_Signals.Disp_All_Signals;
            end if;

            exit when Status = Grt.Errors.Run_Finished;

            if Break_Step
              or else (Current_Time >= Break_Time
                         and then Break_Time /= Std_Time'Last)
            then
               --  No not break anymore on time,
               Break_Time := Std_Time'Last;
               Break_Step := False;
               Elab.Debugger.Debug_Time (Vhdl_Elab.Top_Instance);
            end if;

            exit when Grt.Processes.Has_Simulation_Timeout;
         end loop;
      end if;

      Grt.Main.Run_Finish (Status);
   exception
--      when Debugger_Quit =>
--         null;
      when Simulation_Finished =>
         null;
   end Simulation;
end Simul.Main;
