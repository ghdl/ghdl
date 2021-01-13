--  Main procedure of ortho debug back-end.
--  Copyright (C) 2005 Tristan Gingold
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
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ortho_Debug; use Ortho_Debug;
with Ortho_Debug_Front; use Ortho_Debug_Front;
with Ortho_Debug.Disp;
with System; use System;
with Interfaces.C_Streams; use Interfaces.C_Streams;

procedure Ortho_Debug.Main is
   --  Do not output the ortho code.
   Flag_Silent : Boolean := False;

   --  Force output, even in case of crash.
   Flag_Force : Boolean := False;

   I : Natural;
   Argc : Natural;
   Arg : String_Acc;
   Opt : String_Acc;
   Res : Natural;
   File : String_Acc;
   Output : FILEs;
   R : Boolean;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Name => String_Acc, Object => String);
begin
   Ortho_Debug_Front.Init;
   Output := NULL_Stream;

   Set_Exit_Status (Failure);

   --  Decode options.
   Argc := Argument_Count;
   I := 1;
   loop
      exit when I > Argc;
      exit when Argument (I) (1) /= '-';
      if Argument (I) = "--silent" or else Argument (I) = "-quiet" then
         Flag_Silent := True;
         I := I + 1;
      elsif Argument (I) = "--force" then
         Flag_Force := True;
         I := I + 1;
      elsif Argument (I)'Length >= 2 and then Argument (I)(2) = 'g' then
         --  Skip -g[XXX] flags.
         I := I + 1;
      elsif Argument (I)'Length >= 2 and then Argument (I)(2) = 'O' then
         --  Skip -O[XXX] flags.
         I := I + 1;
      elsif Argument (I) = "-o" and then I + 1 <= Argc then
         --  TODO: write the output to the file ?
         if Output /= NULL_Stream then
            Put_Line (Command_Name & ": only one output allowed");
            return;
         end if;
         declare
            Name : String := Argument (I + 1) & ASCII.Nul;
            Mode : String := 'w' & ASCII.Nul;
         begin
            Output := fopen (Name'Address, Mode'Address);
            if Output = NULL_Stream then
               Put_Line (Command_Name & ": cannot open " & Argument (I + 1));
               return;
            end if;
         end;
         I := I + 2;
      else
         Opt := new String'(Argument (I));
         if I < Argc then
            Arg := new String'(Argument (I + 1));
         else
            Arg := null;
         end if;
         Res := Ortho_Debug_Front.Decode_Option (Opt, Arg);
         Unchecked_Deallocation (Opt);
         Unchecked_Deallocation (Arg);
         if Res = 0 then
            Put_Line (Argument (I) & ": unknown option");
            return;
         else
            I := I + Res;
         end if;
      end if;
   end loop;

   --  Initialize tree.
   begin
      Ortho_Debug.Init;

      if I <= Argc then
         R := True;
         for J in I .. Argc loop
            File := new String'(Argument (J));
            R := R and Ortho_Debug_Front.Parse (File);
            Unchecked_Deallocation (File);
         end loop;
      else
         R := Ortho_Debug_Front.Parse (null);
      end if;
      Ortho_Debug.Finish;
   exception
      when others =>
         if not Flag_Force then
            raise;
         else
            R := False;
         end if;
   end;

   --  Write down the result.
   if (R and (Output /= NULL_Stream or not Flag_Silent))
     or Flag_Force
   then
      if Output = NULL_Stream then
         Ortho_Debug.Disp.Init_Context (stdout);
      else
         Ortho_Debug.Disp.Init_Context (Output);
      end if;
      Ortho_Debug.Disp.Disp_Ortho (Ortho_Debug.Top);
      if Output /= NULL_Stream then
         declare
            Status : int;
            pragma Unreferenced (Status);
         begin
            Status := fclose (Output);
         end;
      end if;
   end if;

   if R then
      Set_Exit_Status (Success);
   else
      Set_Exit_Status (Failure);
   end if;
end Ortho_Debug.Main;
