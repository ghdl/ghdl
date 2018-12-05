--  Output errors on the console.
--  Copyright (C) 2018 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Ada.Text_IO;
with GNAT.OS_Lib;
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
      --  Import isatty.
      function isatty (Fd : Integer) return Integer;
      pragma Import (C, isatty);

      --  Awful way to detect if the host is Windows.  Should be replaced by
      --  a host-specific package.
      Is_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';
   begin
      if Flag_Color_Diagnostics = Auto then
         if Is_Windows then
            --  Off by default on Windows, as the consoles may not support
            --  ANSI control sequences.  Should be replaced by calls to the
            --  Win32 API.
            Flag_Color_Diagnostics := Off;
         else
            --  On Linux/Unix/Mac OS X: use color only when the output is to a
            --  tty.
            if isatty (2) /= 0 then
               Flag_Color_Diagnostics := On;
            else
               Flag_Color_Diagnostics := Off;
            end if;
         end if;
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
      procedure Put (S : String)
      is
         use Ada.Text_IO;
      begin
         Put (Standard_Error, S);
      end Put;
   begin
      if Flag_Color_Diagnostics = Off then
         return;
      end if;

      --  Use ANSI sequences.
      --  They are also documented on msdn in 'Console Virtual Terminal
      --  sequences'.

      Put (ASCII.ESC & '[');
      case Color is
         when Color_Locus   => Put ("1");    --  Bold
         when Color_Note    => Put ("1;36"); --  Bold, cyan
         when Color_Warning => Put ("1;35"); --  Bold, magenta
         when Color_Error   => Put ("1;31"); --  Bold, red
         when Color_Fatal   => Put ("1;33"); --  Bold, yellow
         when Color_Message => Put ("0;1");  --  Normal, bold
         when Color_None    => Put ("0");    --  Normal
      end case;
      Put ("m");
   end Set_Color;

   Msg_Len : Natural;
   Current_Error : Error_Record;

   procedure Put (Str : String)
   is
      use Ada.Text_IO;
   begin
      Msg_Len := Msg_Len + Str'Length;
      Put (Standard_Error, Str);
   end Put;

   procedure Put (C : Character)
   is
      use Ada.Text_IO;
   begin
      Msg_Len := Msg_Len + 1;
      Put (Standard_Error, C);
   end Put;

   procedure Put_Line (Str : String := "")
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, Str);
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

      Detect_Terminal;

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
      if Flag_Diagnostics_Show_Option
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

      if Flag_Caret_Diagnostics
        and then (Current_Error.File /= No_Source_File_Entry
                    and Current_Error.Line /= 0)
      then
         Put_Line (Extract_Expanded_Line (Current_Error.File,
                                          Current_Error.Line));
         Put_Line ((1 .. Get_Error_Col (Current_Error) - 1 => ' ') & '^');
      end if;
   end Console_Message_End;

   procedure Install_Handler is
   begin
      Set_Report_Handler ((Console_Error_Start'Access,
                           Console_Message'Access,
                           Console_Message_End'Access));
   end Install_Handler;
end Errorout.Console;
