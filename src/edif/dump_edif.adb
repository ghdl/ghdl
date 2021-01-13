--  EDIF main program.
--  Copyright (C) 2019 Tristan Gingold
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Types; use Types;
with Name_Table;
with Std_Names;
with Files_Map;
with Str_Table;
with Errorout;
with Errorout.Console;
with Edif.Tokens; use Edif.Tokens;
with Edif.Scans; use Edif.Scans;
with Edif.Nodes; use Edif.Nodes;
with Edif.Parse; use Edif.Parse;
with Edif.Disp_Edif;

procedure Dump_Edif is
   procedure Usage is
   begin
      Put_Line ("usage: " & Command_Name & " [--scan|--raw] FILE");
   end Usage;

   procedure Error_Msg_Option (Msg : String) is
   begin
      Put_Line (Standard_Error, Msg);
      raise Fatal_Error;
   end Error_Msg_Option;

   N : Natural;

   type Cmd_Type is (Cmd_None, Cmd_Scan, Cmd_Raw);
   Cmd : Cmd_Type;

   Id : Name_Id;
   Dir_Id : Name_Id;
   Sfe : Source_File_Entry;
begin
   Set_Exit_Status (Failure);

   Errorout.Console.Install_Handler;
   Errorout.Console.Set_Program_Name (Command_Name);
   Std_Names.Std_Names_Initialize;
   Files_Map.Initialize;

   --  Decode options.
   Cmd := Cmd_None;
   N := 1;
   while N <= Argument_Count loop
      declare
         Opt : constant String := Argument (N);
      begin
         exit when Opt (1) /= '-' and Opt (1) /= '+';

         if Opt = "--scan" then
            Cmd := Cmd_Scan;
         elsif Opt = "--raw" then
            Cmd := Cmd_Raw;
         else
            Usage;
            return;
         end if;
      end;
      N := N + 1;
   end loop;

   --  Stop now if no arguments.
   if N > Argument_Count then
      Usage;
      return;
   end if;

   --  Parse files on the command line.
   while N <= Argument_Count loop
      --  Load the file.
      Id := Name_Table.Get_Identifier (Argument (N));
      Dir_Id := Null_Identifier;
      Files_Map.Normalize_Pathname (Dir_Id, Id);
      Sfe := Files_Map.Read_Source_File (Dir_Id, Id);
      if Sfe = No_Source_File_Entry then
         Error_Msg_Option ("cannot open " & Argument (N));
      end if;

      --  Parse file.
      Set_File (Sfe);

      case Cmd is
         when Cmd_Scan =>
            declare
               Indent : Natural;
               Need_Nl : Boolean;
               Need_Sp : Boolean;

               procedure Maybe_Nl is
               begin
                  if Need_Nl then
                     New_Line;
                     Put ((1 .. 2 * Indent => ' '));
                     Need_Nl := False;
                     Need_Sp := False;
                  elsif Need_Sp then
                     Put (' ');
                     Need_Sp := False;
                  end if;
               end Maybe_Nl;
            begin
               Indent := 0;
               Need_Nl := False;
               Need_Sp := False;
               loop
                  Scan;

                  case Current_Token is
                     when Tok_Keyword =>
                        New_Line;
                        Put ((1 .. 2 * Indent => ' '));
                        Need_Nl := False;
                        Put ('(');
                        Put (Name_Table.Image (Current_Identifier));
                        Need_Sp := True;
                        Indent := Indent + 1;
                     when Tok_Right_Paren =>
                        Put (')');
                        Need_Nl := True;
                        Indent := Indent - 1;
                     when Tok_Symbol =>
                        Maybe_Nl;
                        Put (Name_Table.Image (Current_Identifier));
                        Need_Sp := True;
                     when Tok_String =>
                        Maybe_Nl;
                        Put ('"');
                        Put (Str_Table.String_String8
                               (Current_String, Nat32 (Current_String_Len)));
                        Put ('"');
                        Need_Sp := True;
                     when Tok_Number =>
                        Maybe_Nl;
                        declare
                           S : constant String := Int32'Image (Current_Number);
                           F : Natural;
                        begin
                           if S (1) = ' ' then
                              F := 2;
                           else
                              F := 1;
                           end if;
                           Put (S (F .. S'Last));
                        end;
                        Need_Sp := True;
                     when Tok_Eof =>
                        exit;
                  end case;
               end loop;
            end;
         when Cmd_Raw =>
            declare
               T : Node;
            begin
               T := Parse_File_Simple;
               if Errorout.Nbr_Errors > 0 then
                  raise Fatal_Error;
               end if;
               Edif.Disp_Edif.Disp_Node (T);
            end;
         when Cmd_None =>
            declare
               T : Node;
            begin
               T := Parse_Edif200;
               if Errorout.Nbr_Errors > 0 then
                  raise Fatal_Error;
               end if;
               Edif.Disp_Edif.Disp_Node (T);
            end;
      end case;

      N := N + 1;
   end loop;
   Set_Exit_Status (Success);
exception
   when Fatal_Error =>
      null;
end Dump_Edif;
