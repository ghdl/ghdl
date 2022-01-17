--  Output errors on the console.
--  Copyright (C) 2018 Tristan Gingold
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

with GNAT.OS_Lib;
with Simple_IO;
with Name_Table;
with Files_Map; use Files_Map;
with Flags; use Flags;

package body Errorout.Console is
   --  Name of the program, used to report error message.
   Program_Name : String_Acc := null;

   --  Terminal.

   --  Set Flag_Color_Diagnostics to On or Off if is was Auto.
   procedure Detect_Terminal
   is
      use GNAT.OS_Lib;

      --  Import isatty.
      function isatty (Fd : Integer) return Integer;
      pragma Import (C, isatty);

      --  Awful way to detect if the host is Windows.  Should be replaced by
      --  a host-specific package.
      Is_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

      V : String_Access;
   begin
      if Flag_Color_Diagnostics /= Auto then
         return;
      end if;

      --  Default is off.
      Flag_Color_Diagnostics := Off;

      if Is_Windows then
         --  Off by default on Windows, as the consoles may not support
         --  ANSI control sequences.  Should be replaced by calls to the
         --  Win32 API.
         return;
      else
         --  On Linux/Unix/Mac OS X: use color only when the output is to a
         --  tty.
         if isatty (2) = 0 then
            return;
         end if;

         V := GNAT.OS_Lib.Getenv ("TERM");
         if V = null or else V.all = "dumb" then
            --  No color if TERM=dumb
            --  Should we use a black list, or a white list or terminfo ?
            return;
         end if;

         Flag_Color_Diagnostics := On;
      end if;
   end Detect_Terminal;

   --  Color to be used for various part of messages.
   type Color_Type is (Color_Locus,
                       Color_Note, Color_Warning, Color_Error, Color_Fatal,
                       Color_Message,
                       Color_None);

   --  Switch to COLOR.
   procedure Set_Color (Color : Color_Type)
   is
      use Simple_IO;
      E : constant Character := ASCII.ESC;
   begin
      if Flag_Color_Diagnostics = Off then
         return;
      end if;

      --  Use ANSI sequences.
      --  They are also documented on msdn in 'Console Virtual Terminal
      --  sequences'.

      case Color is
         when Color_Locus   => Put_Err (E & "[1m");    --  Bold
         when Color_Note    => Put_Err (E & "[1;36m"); --  Bold, cyan
         when Color_Warning => Put_Err (E & "[1;35m"); --  Bold, magenta
         when Color_Error   => Put_Err (E & "[1;31m"); --  Bold, red
         when Color_Fatal   => Put_Err (E & "[1;33m"); --  Bold, yellow
         when Color_Message => Put_Err (E & "[0;1m");  --  Normal, bold
         when Color_None    => Put_Err (E & "[0m");    --  Normal
      end case;
   end Set_Color;

   Msg_Len : Natural;
   Current_Error : Error_Record;
   Current_Line : Natural;
   In_Group : Boolean := False;

   procedure Put (Str : String) is
   begin
      Msg_Len := Msg_Len + Str'Length;
      Simple_IO.Put_Err (Str);
   end Put;

   procedure Put (C : Character) is
   begin
      Msg_Len := Msg_Len + 1;
      Simple_IO.Put_Err (C);
   end Put;

   procedure Put_Line (Str : String := "") is
   begin
      Simple_IO.Put_Line_Err (Str);
      Msg_Len := 0;
   end Put_Line;

   procedure Set_Program_Name (Name : String) is
   begin
      Program_Name := new String'(Name);
   end Set_Program_Name;

   procedure Disp_Program_Name is
   begin
      if Program_Name /= null then
         Put (Program_Name.all);
         Put (':');
      end if;
   end Disp_Program_Name;

   procedure Disp_Location (File: Name_Id; Line: Natural; Col: Natural) is
   begin
      if File = Null_Identifier then
         Put ("??");
      else
         Put (Name_Table.Image (File));
      end if;
      Put (':');
      Put (Natural_Image (Line));
      Put (':');
      Put (Natural_Image (Col));
      Put (':');
   end Disp_Location;

   procedure Console_Error_Start (E : Error_Record)
   is
      --- Coord_To_Position (File, Line_Pos, Offset, Name, Col);
      Progname : Boolean;
   begin
      Current_Error := E;

      if In_Group then
         Current_Line := Current_Line + 1;
      else
         pragma Assert (Current_Line <= 1);
         Current_Line := 1;
      end if;

      --  And no program name.
      Progname := False;

      case E.Origin is
         when Option
           | Library =>
            pragma Assert (E.File = No_Source_File_Entry);
            Progname := True;
         when Elaboration =>
            if E.File = No_Source_File_Entry then
               Progname := True;
            end if;
         when others =>
            pragma Assert (E.File /= No_Source_File_Entry);
            null;
      end case;

      Msg_Len := 0;

      if Flag_Color_Diagnostics = On then
         Set_Color (Color_Locus);
      end if;

      if Progname then
         Disp_Program_Name;
      elsif E.File /= No_Source_File_Entry then
         Disp_Location (Get_File_Name (E.File), E.Line, Get_Error_Col (E));
      else
         Disp_Location (Null_Identifier, 0, 0);
      end if;

      --  Display level.
      case E.Id is
         when Msgid_Note =>
            if Flag_Color_Diagnostics = On then
               Set_Color (Color_Note);
            end if;
            Put ("note:");
         when Msgid_Warning | Msgid_Warnings =>
            if Flag_Color_Diagnostics = On then
               Set_Color (Color_Warning);
            end if;
            Put ("warning:");
         when Msgid_Error =>
            if Flag_Color_Diagnostics = On then
               Set_Color (Color_Error);
            end if;
            if Msg_Len = 0
              or else Flag_Color_Diagnostics = On
            then
               --  'error:' is displayed only if not location is present, or
               --  if messages are colored.
               Put ("error:");
            end if;
         when Msgid_Fatal =>
            if Flag_Color_Diagnostics = On then
               Set_Color (Color_Fatal);
            end if;
            Put ("fatal:");
      end case;

      if Flag_Color_Diagnostics = On then
         Set_Color (Color_Message);
      end if;
      Put (' ');
   end Console_Error_Start;

   procedure Console_Message (Str : String) renames Put;

   procedure Console_Message_End is
   begin
      if Current_Line = 1
        and then Flag_Diagnostics_Show_Option
        and then Current_Error.Id in Msgid_Warnings
      then
         Put (" [-W");
         Put (Warning_Image (Current_Error.Id));
         Put ("]");
      end if;

      if Flag_Color_Diagnostics = On then
         Set_Color (Color_None);
      end if;

      Put_Line;

      if Current_Line = 1
        and then Flag_Caret_Diagnostics
        and then (Current_Error.File /= No_Source_File_Entry
                    and Current_Error.Line /= 0)
        and then Get_File_Length (Current_Error.File) > 0
      then
         Put_Line (Extract_Expanded_Line (Current_Error.File,
                                          Current_Error.Line));
         Put_Line ((1 .. Get_Error_Col (Current_Error) - 1 => ' ') & '^');
      end if;
   end Console_Message_End;

   procedure Console_Message_Group (Start : Boolean) is
   begin
      Current_Line := 0;
      pragma Assert (In_Group /= Start);
      In_Group := Start;
   end Console_Message_Group;

   procedure Install_Handler is
   begin
      Detect_Terminal;

      Set_Report_Handler ((Console_Error_Start'Access,
                           Console_Message'Access,
                           Console_Message_End'Access,
                           Console_Message_Group'Access));
   end Install_Handler;
end Errorout.Console;
