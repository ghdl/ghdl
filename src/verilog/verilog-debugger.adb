--  Verilog simple CLI
--  Copyright (C) 2023 Tristan Gingold
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

with Types; use Types;
with Files_Map;
with Name_Table;
with Errorout;
with Debuggers; use Debuggers;
with Simple_IO; use Simple_IO;
with Grt.Readline;
with Grt.Types; use Grt.Types;

with Verilog.Scans;
with Verilog.Tokens;
with Verilog.Nodes;
with Verilog.Parse;
with Verilog.Sem_Stmts;
with Verilog.Disp_Verilog;
with Verilog.Allocates;
with Verilog.Simulation;
with Verilog.Vpi;

package body Verilog.Debugger is
   Prompt : String (1 .. 8) := "     > " & ASCII.NUL;
   Cmd_Number : Natural := 0;

   procedure Parse_Command (Line : String)
   is
      use Verilog.Nodes;
      use Verilog.Tokens;
      use Verilog.Allocates;
      File : Source_File_Entry;
      Stmt : Node;
      Proc : Node;
      P : Process_Acc;
   begin
      --  Put the line in a buffer.
      File := Files_Map.Create_Source_File_From_String
        (Name_Table.Get_Identifier ("*cli*" & Prompt (1 .. 5) & '*'),
         Line);

      --  Wrap the statement within N_Debug
      Proc := Create_Node (N_Debug);
      Set_Location (Proc, Files_Map.File_To_Location (File));
      Set_Parent (Proc, Verilog.Vpi.Interractive_Scope);

      --  Parse the line.
      Verilog.Scans.Set_File (File);
      Verilog.Scans.Scan;
      pragma Assert (Verilog.Parse.Current_Scope = Null_Node);
      Verilog.Parse.Current_Scope := Proc;
      Stmt := Verilog.Parse.Parse_Statement (Proc);
      Verilog.Parse.Current_Scope := Null_Node;
      if Verilog.Scans.Current_Token /= Tok_Eof then
         Put_Line ("garbage at end of expression ignored");
      end if;
      Verilog.Scans.Close_File;
      if Errorout.Nbr_Errors /= 0 then
         Put_Line ("error while parsing expression, command aborted");
         Errorout.Nbr_Errors := 0;
         Files_Map.Unload_Last_Source_File (File);
         return;
      end if;

      --  Analyze the line.
      Verilog.Sem_Stmts.Sem_Statement (Stmt);
      if Errorout.Nbr_Errors /= 0 then
         Put_Line ("error while analysing expression, command aborted");
         Errorout.Nbr_Errors := 0;
         Files_Map.Unload_Last_Source_File (File);
         return;
      end if;

      Set_Statement (Proc, Stmt);

      Verilog.Disp_Verilog.Disp_Item (Stmt);

      --  Create the process and schedule it.
      P := Verilog.Allocates.Allocate_Proc (Proc);
      Verilog.Simulation.Execute_Statements (P.Link, P);

      Cmd_Number := Cmd_Number + 1;
   end Parse_Command;

   function Run_Command (Line : String) return Boolean
   is
      P : Positive;
   begin
      P := Skip_Blanks (Line);
      if P > Line'Last then
         return False;
      end if;

      if Line (P) = '.' then
         --  Continue
         return True;
      elsif Line (P) = ',' then
         --  Step...
         Verilog.Vpi.Vpip_Control := Natural (Verilog.Vpi.VpiStop);
         return True;
      else
         Parse_Command (Line (P .. Line'Last));
         return False;
      end if;
   end Run_Command;

   procedure Debug_CLI
   is
      use Grt.Readline;
      Raw_Line : Ghdl_C_String;
   begin
      loop
         --  Setup prompt.
         declare
            S : constant String := Natural'Image (Cmd_Number);
         begin
            for I in reverse S'Range loop
               Prompt (4 - (S'Last - I)) := S (I);
            end loop;
         end;

         --  User input.
         loop
            Raw_Line := Readline (To_Ghdl_C_String (Prompt'Address));

            --  Discard empty lines.
            exit when Raw_Line /= null and then Raw_Line (1) /= ASCII.NUL;
         end loop;

         declare
            Line_Last : constant Natural := strlen (Raw_Line);
            Line : String renames Raw_Line (1 .. Line_Last);
         begin
            if Run_Command (Line) then
               exit;
            end if;
         end;
      end loop;
   end Debug_CLI;
end Verilog.Debugger;
