--  GHDL driver - main part.
--  Copyright (C) 2002 - 2010 Tristan Gingold
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
with Ada.Command_Line;
with Ada.Unchecked_Deallocation;

with Simple_IO;
with Filesystem;
with Version;
with Bug;
with Errorout; use Errorout;
with Errorout.Console;
with Default_Paths;

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
                            Res : out Option_State)
   is
      pragma Unreferenced (Cmd);
      pragma Unreferenced (Option);
      pragma Unreferenced (Arg);
   begin
      Res := Option_Unknown;
   end Decode_Option;

   procedure Disp_Long_Help (Cmd : Command_Type)
   is
      pragma Unreferenced (Cmd);
      use Simple_IO;
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

   function Decode_Command (Cmd : Command_Str_Type; Name : String)
                           return Boolean is
   begin
      return Name = Cmd.Cmd_Str.all;
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Str_Type) return String is
   begin
      return Cmd.Help_Str.all;
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Str_Disp;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Args);
   begin
      Simple_IO.Put_Line (Cmd.Disp.all);
      Success := True;
   end Perform_Action;


   --  Command help.
   type Command_Help is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Help; Name : String) return Boolean;
   procedure Decode_Option (Cmd : in out Command_Help;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);

   function Get_Short_Help (Cmd : Command_Help) return String;
   procedure Perform_Action (Cmd : in out Command_Help;
                             Args : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Help; Name : String) return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return
        Name = "help" or else
        Name = "--help" or else
        Name = "-h";
   end Decode_Command;

   procedure Decode_Option (Cmd : in out Command_Help;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
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
      return "help [CMD]"
        & ASCII.LF & "  Display this help or [help on CMD]"
        & ASCII.LF & "  aliases: -h, --help";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Help;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);

      use Simple_IO;
      use Ada.Command_Line;
      C : Command_Acc;
   begin
      --  Default
      Success := False;

      if Args'Length = 0 then
         Put_Line ("usage: " & Command_Name & " COMMAND [OPTIONS] ...");
         Put_Line ("COMMAND is one of:");
         C := First_Cmd;
         while C /= null loop
            declare
               S : constant String := Get_Short_Help (C.all);
            begin
               if S'Length > 1 and then S (S'First) /= '!' then
                  Put_Line (S);
               end if;
            end;
            C := C.Next;
         end loop;
         New_Line;
         Put_Line ("To display the options of a GHDL program,");
         Put_Line ("  run your program with the 'help' option.");
         Put_Line ("Also see 'opts-help' for analyzer options.");
         New_Line;
         Put_Line ("Please, refer to the GHDL manual for more information.");
         Put_Line ("Report issues on https://github.com/ghdl/ghdl");
      elsif Args'Length = 1 then
         C := Find_Command (Args (1).all);
         if C = null then
            Error ("Command '" & Args (1).all & "' is unknown.");
            return;
         end if;
         Put_Line (Get_Short_Help (C.all));
         Disp_Long_Help (C.all);
      else
         Error ("Command 'help' accepts at most one argument.");
         return;
      end if;
      Success := True;
   end Perform_Action;

   --  Command help options.
   type Command_Help_Option is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Help_Option; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Help_Option) return String;
   procedure Perform_Action (Cmd : in out Command_Help_Option;
                             Args : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Help_Option; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return
        Name = "help-options" or else
        Name = "--help-options" or else
        Name = "opts-help" or else
        Name = "--options-help";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Help_Option) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "help-options"
        & ASCII.LF & "  Display help for analyzer options"
        & ASCII.LF & "  alias: --help-options, opts-help, --options-help";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Help_Option;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);
   begin
      if Args'Length /= 0 then
         Error ("warning: command 'opts-help' does not accept any argument");
      end if;
      Options.Disp_Help_Options;
      Success := True;
   end Perform_Action;

   --  Command help-warnings
   type Command_Help_Warnings is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Help_Warnings; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Help_Warnings) return String;
   procedure Perform_Action (Cmd : in out Command_Help_Warnings;
                             Args : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Help_Warnings; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return
        Name = "help-warnings" or else
        Name = "--help-warnings";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Help_Warnings) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "help-warnings"
        & ASCII.LF & "  Display help about all the warnings"
        & ASCII.LF & "  alias: --help-warnings";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Help_Warnings;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);
   begin
      if Args'Length /= 0 then
         Error
           ("warning: command 'help-warnings' does not accept any argument");
      end if;
      Options.Disp_Help_Warnings;
      Success := True;
   end Perform_Action;

   --  Command Version
   type Command_Version is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Version; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Version) return String;
   procedure Perform_Action (Cmd : in out Command_Version;
                             Args : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Version; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return
        Name = "version" or else
        Name = "--version" or else
        Name = "-v";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Version) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "version"
        & ASCII.LF & "  Display ghdl version"
        & ASCII.LF & "  aliases: -v, --version";
   end Get_Short_Help;

   procedure Disp_Ghdl_Version
   is
      use Simple_IO;
   begin
      Put ("GHDL ");
      Put (Version.Ghdl_Ver);
      Put (' ');
      Put_Line (Version.Ghdl_Release);
   end Disp_Ghdl_Version;

   procedure Perform_Action (Cmd : in out Command_Version;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);
      use Simple_IO;
   begin
      Success := True;

      if Args'Length /= 0 then
         if Args (1).all = "ref" or else Args (1).all = "--ref" then
            Put_Line (Version.Ghdl_Ref);
         elsif Args (1).all = "hash" or else Args (1).all = "--hash" then
            Put_Line (Version.Ghdl_Hash);
         else
            Error ("warning: 'version' subcommand '"
                     & Args(1).all & "' not supported");
            Success := False;
         end if;
         return;
      end if;
      Disp_Ghdl_Version;
      Put_Line (" Compiled with " & Bug.Get_Gnat_Version);
      if Version_String /= null then
         Put (" ");
         Put (Version_String.all);
      end if;
      New_Line;
      Put_Line ("Written by Tristan Gingold.");
      New_Line;
      --  Display copyright.  Assume 80 cols terminal.
      Put_Line ("Copyright (C) 2003 - 2025 Tristan Gingold.");
      Put_Line ("GHDL is free software, covered by the "
                & "GNU General Public License.  There is NO");
      Put_Line ("warranty; not even for MERCHANTABILITY or"
                & " FITNESS FOR A PARTICULAR PURPOSE.");
   end Perform_Action;

   --  Disp MSG on the standard output with the command name.
   procedure Error (Msg : String)is
   begin
      Report_Msg (Msgid_Error, Option, No_Source_Coord, Msg);
   end Error;

   procedure Warning (Msg : String) is
   begin
      Report_Msg (Msgid_Warning, Option, No_Source_Coord, Msg);
   end Warning;

   function Index (Str : String; C : Character) return Natural is
   begin
      for I in Str'Range loop
         if Str (I) = C then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   --  Decode command CMD_NAME and return the command_type.
   --  If the command is not known, emit an error message and
   --  raise Option_Error.
   function Find_Command_With_Error (Cmd_Name : String) return Command_Acc
   is
      Cmd : Command_Acc;
   begin
      --  Decode command.
      Cmd := Find_Command (Cmd_Name);
      if Cmd = null then
         Error ("unknown command '" & Cmd_Name & "', try 'help'");
         raise Option_Error;
      end if;

      return Cmd;
   end Find_Command_With_Error;

   procedure Decode_Command_Options (Cmd : in out Command_Type'Class;
                                     Args : String_Acc_Array;
                                     First_Arg : out Natural)
   is
      Arg_Index : Natural;
   begin
      Init (Cmd);

      --  Decode options.

      First_Arg := 0;
      Arg_Index := Args'First;
      while Arg_Index <= Args'Last loop
         declare
            Arg : constant String_Acc := Args (Arg_Index);
            Res : Option_State;
         begin
            if Arg (1) = '-' then
               --  Argument is an option.

               if First_Arg > 0 then
                  Error ("options after file");
                  raise Option_Error;
               end if;

               Decode_Option (Cmd, Arg.all, "", Res);
               case Res is
                  when Option_Unknown =>
                     Error ("unknown command option '" & Arg.all & "'");
                     raise Option_Error;
                  when Option_Err =>
                     raise Option_Error;
                  when Option_Ok =>
                     Arg_Index := Arg_Index + 1;
                  when Option_Arg_Req =>
                     if Arg_Index + 1 > Args'Last then
                        Error
                          ("option '" & Arg.all & "' requires an argument");
                        raise Option_Error;
                     end if;
                     Decode_Option
                       (Cmd, Arg.all, Args (Arg_Index + 1).all, Res);
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
         First_Arg := Args'Last + 1;
      end if;
   end Decode_Command_Options;

   Is_Windows : constant Boolean :=
     Default_Paths.Shared_Library_Extension = ".dll";

   function Convert_Path_To_Unix (Path : String) return String is
   begin
      if Is_Windows then
         declare
            Res : String := Path;
         begin
            --  Convert path separators.
            for I in Res'Range loop
               if Res (I) = '\' then
                  Res (I) := '/';
               end if;
            end loop;
            --  Convert C: to /C/
            if Res'Length > 2
              and then (Res (Res'First) in 'a' .. 'z'
                          or else Res (Res'First) in 'A' .. 'Z')
              and then Res (Res'First + 1) = ':'
            then
               Res (Res'First + 1) := '/';
               return '/' & Res;
            end if;
            return Res;
         end;
      else
         return Path;
      end if;
   end Convert_Path_To_Unix;

   type String_Acc_Arr_Acc is access String_Acc_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (String_Acc_Array, String_Acc_Arr_Acc);

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Acc);

   --  Resize LIST to LEN elements.
   procedure Resize (List : in out String_Acc_Arr_Acc; Len : Natural)
   is
      pragma Assert (List'First = 1);
      List_Len : constant Natural := List'Last;
      Res : String_Acc_Arr_Acc;
   begin
      Res := new String_Acc_Array (1 .. Len);
      if Len > List_Len then
         --  Increase.
         Res (1 .. List_Len) := List.all;
      else
         --  Shrink.
         Res.all := List (1 .. Len);
      end if;
      Free (List);
      List := Res;
   end Resize;

   --  Read a response file and return the list of arguments.
   --  Return null if the file cannot be read.
   --  Neither escape nor quotes are handled.
   function Read_Response_File (Filename : String) return String_Acc_Arr_Acc
   is
      function Is_Blank (C : Character) return Boolean is
      begin
         return C = ' ' or C = ASCII.HT or C = ASCII.CR or C = ASCII.LF;
      end Is_Blank;

      use Filesystem;
      Buf : String_Acc;
      Fd : Filesystem.File_Descriptor;
      Len : Long_Integer;
      Res : String_Acc_Arr_Acc;
      Narg : Natural;
      F, P : Natural;
   begin
      --  Open and read content.
      Open_Read (Fd, Filename);
      if Is_Error (Fd) then
         Error ("cannot open response file '" & Filename & "'");
         return null;
      end if;
      Len := File_Length (Fd);
      Buf := new String (1 .. Natural (Len));
      Read (Fd, Buf.all'Address, Len);
      Close (Fd);

      Narg := 0;
      Res := new String_Acc_Array (1 .. 32);
      P := Buf'First;
      loop
         --  Skip spaces, newlines...
         while P <= Buf'Last and then Is_Blank (Buf (P)) loop
            P := P + 1;
         end loop;
         exit when P > Buf'Last;

         --  Eat non-blank characters.
         F := P;
         while P <= Buf'Last and then not Is_Blank (Buf (P)) loop
            P := P + 1;
         end loop;

         --  Expand res (if needed).
         if Narg = Res'Last then
            Resize (Res, 2 * Narg);
         end if;

         --  Append
         Narg := Narg + 1;
         declare
            --  Make the string 1 based.
            subtype S is String (1 .. P - F);
         begin
            Res (Narg) := new String'(S (Buf (F .. P - 1)));
         end;

         --  Skip blank.
         P := P + 1;
      end loop;

      --  Shrink result.
      if Res'Last /= Narg then
         Resize (Res, Narg);
      end if;

      return Res;
   end Read_Response_File;

   procedure Main
   is
      use Ada.Command_Line;
      Args : String_Acc_Arr_Acc;
      Arg_Index : Natural;
   begin
      --  Set program name for error message.
      Errorout.Console.Set_Program_Name (Command_Name);
      Errorout.Console.Install_Handler;

      --  Initialize global structures.
      Options.Initialize;

      --  Handle case of no argument
      if Argument_Count = 0 then
         Error ("missing command, try: " & Command_Name & " help");
         raise Option_Error;
      end if;

      Args := new String_Acc_Array (1 .. Argument_Count);
      for I in Args'Range loop
         Args (I) := new String'(Argument (I));
         pragma Assert (Args (I)'First = 1);
         if Args (I)'Last < 1 then
            Error ("empty argument on the command line (#"
                     & Natural'Image (I) & ")");
            raise Option_Error;
         end if;
      end loop;

      --  Expand response files
      Arg_Index := 1;
      while Arg_Index <= Args'Last loop
         if Args (Arg_Index).all (1) = '@' then
            declare
               Rsp_Arg : String_Acc := Args (Arg_Index);
               Rsp_File : constant String := Rsp_Arg (2 .. Rsp_Arg'Last);
               Exp_Args : String_Acc_Arr_Acc;
               Exp_Length : Natural;
               New_Args : String_Acc_Arr_Acc;
            begin
               Exp_Args := Read_Response_File (Rsp_File);
               if Exp_Args = null then
                  raise Option_Error;
               end if;

               Exp_Length := Exp_Args'Length;
               New_Args :=
                 new String_Acc_Array (1 .. Args'Last + Exp_Length - 1);

               --  Copy arguments from the response file.
               New_Args (1 .. Arg_Index - 1) := Args (1 .. Arg_Index - 1);
               New_Args (Arg_Index .. Arg_Index + Exp_Length - 1) :=
                 Exp_Args.all;
               New_Args (Arg_Index + Exp_Length .. New_Args'Last) :=
                 Args (Arg_Index + 1 .. Args'Last);

               --  Free old arguments array.
               Free (Args);
               --  Free response file array.
               Free (Exp_Args);
               --  Free response file name.
               Free (Rsp_Arg);

               Args := New_Args;
               Arg_Index := Arg_Index + Exp_Length;
            end;
         else
            Arg_Index := Arg_Index + 1;
         end if;
      end loop;

      declare
         Cmd : Command_Acc;
         First_Arg : Natural;
         Action_Success : Boolean;
      begin
         Cmd := Find_Command_With_Error (Args (1).all);
         Decode_Command_Options (Cmd.all, Args (2 .. Args'Last), First_Arg);

         --  Set before running the action, so that it can be changed.
         Set_Exit_Status (Success);

         declare
            Cmd_Args : String_Acc_Array (1 .. Args'Last - First_Arg + 1);
         begin
            for I in Cmd_Args'Range loop
               Cmd_Args (I) := Args (First_Arg + I - 1);
            end loop;
            Perform_Action (Cmd.all, Cmd_Args, Action_Success);

            if not Action_Success then
               Set_Exit_Status (Failure);
            end if;
         end;
      end;

      --  Free args.
      for I in Args'Range loop
         Free (Args (I));
      end loop;
      Free (Args);

      --if Flags.Dump_Stats then
      --   Name_Table.Disp_Stats;
      --   Iirs.Disp_Stats;
      --end if;
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
      Register_Command (new Command_Help_Option);
      Register_Command (new Command_Help_Warnings);
   end Register_Commands;
end Ghdlmain;
