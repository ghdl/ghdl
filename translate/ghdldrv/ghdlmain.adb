--  GHDL driver - main part.
--  Copyright (C) 2002 - 2010 Tristan Gingold
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
with Ada.Text_IO;
with Ada.Command_Line;
with Version;
with Bug;
with Options;

package body Ghdlmain is
   procedure Init (Cmd : in out Command_Type)
   is
      pragma Unreferenced (Cmd);
   begin
      null;
   end Init;

   procedure Decode_Option (Cmd : in out Command_Type;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
      pragma Unreferenced (Cmd);
      pragma Unreferenced (Option);
      pragma Unreferenced (Arg);
   begin
      Res := Option_Bad;
   end Decode_Option;

   procedure Disp_Long_Help (Cmd : Command_Type)
   is
      pragma Unreferenced (Cmd);
      use Ada.Text_IO;
   begin
      Put_Line ("This command does not accept options.");
   end Disp_Long_Help;

   First_Cmd : Command_Acc := null;
   Last_Cmd : Command_Acc := null;

   procedure Register_Command (Cmd : Command_Acc) is
   begin
      if First_Cmd = null then
         First_Cmd := Cmd;
      else
         Last_Cmd.Next := Cmd;
      end if;
      Last_Cmd := Cmd;
   end Register_Command;

   --  Find the command.
   function Find_Command (Action : String) return Command_Acc
   is
      Cmd : Command_Acc;
   begin
      Cmd := First_Cmd;
      while Cmd /= null loop
         if Decode_Command (Cmd.all, Action) then
            return Cmd;
         end if;
         Cmd := Cmd.Next;
      end loop;
      return null;
   end Find_Command;

   --  Command help.
   type Command_Help is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Help; Name : String) return Boolean;
   procedure Decode_Option (Cmd : in out Command_Help;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   function Get_Short_Help (Cmd : Command_Help) return String;
   procedure Perform_Action (Cmd : in out Command_Help; Args : Argument_List);

   function Decode_Command (Cmd : Command_Help; Name : String) return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-h" or else Name = "--help";
   end Decode_Command;

   procedure Decode_Option (Cmd : in out Command_Help;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
      pragma Unreferenced (Cmd);
      pragma Unreferenced (Option);
      pragma Unreferenced (Arg);
   begin
      Res := Option_End;
   end Decode_Option;

   function Get_Short_Help (Cmd : Command_Help) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-h or --help [CMD] Disp this help or [help on CMD]";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Help; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);

      use Ada.Text_IO;
      use Ada.Command_Line;
      C : Command_Acc;
   begin
      if Args'Length = 0 then
         Put_Line ("usage: " & Command_Name & " COMMAND [OPTIONS] ...");
         Put_Line ("COMMAND is one of:");
         C := First_Cmd;
         while C /= null loop
            Put_Line (Get_Short_Help (C.all));
            C := C.Next;
         end loop;
         New_Line;
         Put_Line ("To display the options of a GHDL program,");
         Put_Line ("  run your programm with the --help option.");
         Put_Line ("Also see --options-help for analyzer options.");
         New_Line;
         Put_Line ("Please, refer to the GHDL manual for more information.");
         Put_Line ("Report bugs on http://gna.org/projects/ghdl");
      elsif Args'Length = 1 then
         C := Find_Command (Args (1).all);
         if C = null then
            Error ("Command '" & Args (1).all & "' is unknown.");
            raise Option_Error;
         end if;
         Put_Line (Get_Short_Help (C.all));
         Disp_Long_Help (C.all);
      else
         Error ("Command '--help' accepts at most one argument.");
         raise Option_Error;
      end if;
   end Perform_Action;

   --  Command options help.
   type Command_Option_Help is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Option_Help; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Option_Help) return String;
   procedure Perform_Action (Cmd : in out Command_Option_Help;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Option_Help; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--options-help";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Option_Help) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--options-help     Disp help for analyzer options";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Option_Help;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      if Args'Length /= 0 then
         Error
           ("warning: command '--option-help' does not accept any argument");
      end if;
      Options.Disp_Options_Help;
   end Perform_Action;

   --  Command Version
   type Command_Version is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Version; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Version) return String;
   procedure Perform_Action (Cmd : in out Command_Version;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Version; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-v" or Name = "--version";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Version) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-v or --version    Disp ghdl version";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Version;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Ada.Text_IO;
   begin
      Put_Line (Version.Ghdl_Release);
      Put_Line (" Compiled with " & Bug.Get_Gnat_Version);
      if Version_String /= null then
         Put (" ");
         Put (Version_String.all);
      end if;
      New_Line;
      Put_Line ("Written by Tristan Gingold.");
      New_Line;
      --  Display copyright.  Assume 80 cols terminal.
      Put_Line ("Copyright (C) 2003 - 2010 Tristan Gingold.");
      Put_Line ("GHDL is free software, covered by the "
                & "GNU General Public License.  There is NO");
      Put_Line ("warranty; not even for MERCHANTABILITY or"
                & " FITNESS FOR A PARTICULAR PURPOSE.");
      if Args'Length /= 0 then
         Error ("warning: command '--version' does not accept any argument");
      end if;
   end Perform_Action;

   --  Disp MSG on the standard output with the command name.
   procedure Error (Msg : String)
   is
      use Ada.Command_Line;
      use Ada.Text_IO;
   begin
      Put (Standard_Error, Command_Name);
      Put (Standard_Error, ": ");
      Put_Line (Standard_Error, Msg);
      --Has_Error := True;
   end Error;

   procedure Main
   is
      use Ada.Command_Line;
      Cmd : Command_Acc;
      Arg_Index : Natural;
      First_Arg : Natural;

   begin
      if Argument_Count = 0 then
         Error ("missing command, try " & Command_Name & " --help");
         raise Option_Error;
      end if;

      Cmd := Find_Command (Argument (1));
      if Cmd = null then
         Error ("unknown command '" & Argument (1) & "', try --help");
         raise Option_Error;
      end if;

      Init (Cmd.all);

      --  decode options.

      First_Arg := 0;
      Arg_Index := 2;
      while Arg_Index <= Argument_Count loop
         declare
            Arg : constant String := Argument (Arg_Index);
            Res : Option_Res;
         begin
            if Arg (1) = '-' then
               --  Argument is an option.

               if First_Arg > 0 then
                  Error ("options after file");
                  raise Option_Error;
               end if;

               Decode_Option (Cmd.all, Arg, "", Res);
               case Res is
                  when Option_Bad =>
                     Error ("unknown option '" & Arg & "' for command '"
                            & Argument (1) & "'");
                     raise Option_Error;
                  when Option_Ok =>
                     Arg_Index := Arg_Index + 1;
                  when Option_Arg_Req =>
                     if Arg_Index + 1 > Argument_Count then
                        Error ("option '" & Arg & "' requires an argument");
                        raise Option_Error;
                     end if;
                     Decode_Option
                       (Cmd.all, Arg, Argument (Arg_Index + 1), Res);
                     if Res /= Option_Arg then
                        raise Program_Error;
                     end if;
                     Arg_Index := Arg_Index + 2;
                  when Option_Arg =>
                     raise Program_Error;
                  when Option_End =>
                     First_Arg := Arg_Index;
                     exit;
               end case;
            else
               First_Arg := Arg_Index;
               exit;
            end if;
         end;
      end loop;

      if First_Arg = 0 then
         First_Arg := Argument_Count + 1;
      end if;

      declare
         Args : Argument_List (1 .. Argument_Count - First_Arg + 1);
      begin
         for I in Args'Range loop
            Args (I) := new String'(Argument (First_Arg + I - 1));
         end loop;
         Perform_Action (Cmd.all, Args);
         for I in Args'Range loop
            Free (Args (I));
         end loop;
      end;
      --if Flags.Dump_Stats then
      --   Name_Table.Disp_Stats;
      --   Iirs.Disp_Stats;
      --end if;
      Set_Exit_Status (Success);
   exception
      when Option_Error
        | Compile_Error
        | Errorout.Compilation_Error =>
         Set_Exit_Status (Failure);
      when Exec_Error =>
         Set_Exit_Status (3);
      when E: others =>
         Bug.Disp_Bug_Box (E);
         Set_Exit_Status (2);
   end Main;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Help);
      Register_Command (new Command_Version);
      Register_Command (new Command_Option_Help);
   end Register_Commands;
end Ghdlmain;

