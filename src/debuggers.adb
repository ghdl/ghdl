--  Utilities to create a simple CLI with menus.
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

with Simple_IO; use Simple_IO;
with Grt.Readline;

package body Debuggers is
   function Is_Blank (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT;
   end Is_Blank;

   function Skip_Blanks (S : String) return Positive
   is
      P : Positive := S'First;
   begin
      while P <= S'Last and then Is_Blank (S (P)) loop
         P := P + 1;
      end loop;
      return P;
   end Skip_Blanks;

   --  Return the position of the last character of the word (the last
   --  non-blank character).
   function Get_Word (S : String) return Positive
   is
      P : Positive := S'First;
   begin
      while P <= S'Last and then not Is_Blank (S (P)) loop
         P := P + 1;
      end loop;
      return P - 1;
   end Get_Word;

   function Find_Menu (Menu : Menu_Entry_Acc; Cmd : String)
                      return Menu_Entry_Acc
   is
      function Is_Cmd (Cmd_Name : String; Str : String) return Boolean
      is
         -- Number of characters that were compared.
         P : Natural;
      begin
         P := 0;
         --  Prefix (before the '*').
         loop
            if P = Cmd_Name'Length then
               --  Full match.
               return P = Str'Length;
            end if;
            exit when Cmd_Name (Cmd_Name'First + P) = '*';
            if P = Str'Length then
               --  Command is too short
               return False;
            end if;
            if Cmd_Name (Cmd_Name'First + P) /= Str (Str'First + P) then
               return False;
            end if;
            P := P + 1;
         end loop;
         --  Suffix (after the '*')
         loop
            if P = Str'Length then
               return True;
            end if;
            if P + 1 = Cmd_Name'Length then
               --  String is too long
               return False;
            end if;
            if Cmd_Name (Cmd_Name'First + P + 1) /= Str (Str'First + P) then
               return False;
            end if;
            P := P + 1;
         end loop;
      end Is_Cmd;
      Ent : Menu_Entry_Acc;
   begin
      Ent := Menu.First;
      while Ent /= null loop
         if Is_Cmd (Ent.Name.all, Cmd) then
            return Ent;
         end if;
         Ent := Ent.Next;
      end loop;
      return null;
   end Find_Menu;

   --  Append command to MENU.
   procedure Append_Menu (Menu : Menu_Entry;
                          Name : Cst_String_Acc;
                          Help : Cst_String_Acc;
                          Proc : Menu_Procedure)
   is
      M, L : Menu_Entry_Acc;
   begin
      M := new Menu_Entry'(Kind => Menu_Command,
                           Name => Name,
                           Help => Help,
                           Next => null,
                           Proc => Proc);

      L := Menu.First;
      while L.Next /= null loop
         L := L.Next;
      end loop;
      L.Next := M;
   end Append_Menu;

   procedure Parse_Command (Line : String;
                            P : in out Natural;
                            Menu : in out Menu_Entry_Acc)
   is
      E : Natural;
   begin
      P := Skip_Blanks (Line (P .. Line'Last));
      if P > Line'Last then
         return;
      end if;
      E := Get_Word (Line (P .. Line'Last));
      Menu := Find_Menu (Menu, Line (P .. E));
      if Menu = null then
         Put_Line ("command '" & Line (P .. E) & "' not found");
      end if;
      P := E + 1;
   end Parse_Command;

   procedure Help_Proc (Line : String)
   is
      P : Natural;
      Root : Menu_Entry_Acc := Menu_Top;
   begin
      Put_Line ("This is the help command");
      P := Line'First;
      while P < Line'Last loop
         Parse_Command (Line, P, Root);
         if Root = null then
            return;
         elsif Root.Kind /= Menu_Submenu then
            Put_Line ("Menu entry " & Root.Name.all & " is not a submenu");
            return;
         end if;
      end loop;

      Root := Root.First;
      while Root /= null loop
         Put (Root.Name.all);
         if Root.Kind = Menu_Submenu then
            Put (" (menu)");
         end if;
         New_Line;
         Root := Root.Next;
      end loop;
   end Help_Proc;

   procedure Debug_Loop (Prompt : Ghdl_C_String)
   is
      use Grt.Readline;
      Raw_Line : Ghdl_C_String;
   begin
      Command_Status := Status_Default;
      loop
         loop
            Raw_Line := Readline (Prompt);
            --  Skip empty lines
            if Raw_Line = null or else Raw_Line (1) = ASCII.NUL then
               if Cmd_Repeat /= null then
                  Cmd_Repeat.all ("");
                  case Command_Status is
                     when Status_Default =>
                        null;
                     when Status_Quit =>
                        return;
                  end case;
               end if;
            else
               Cmd_Repeat := null;
               exit;
            end if;
         end loop;
         declare
            Line_Last : constant Natural := strlen (Raw_Line);
            Line : String renames Raw_Line (1 .. Line_Last);
            P, E : Positive;
            Cmd : Menu_Entry_Acc := Menu_Top;
         begin
            --  Find command
            P := 1;
            loop
               E := P;
               Parse_Command (Line, E, Cmd);
               exit when Cmd = null;
               case Cmd.Kind is
                  when Menu_Submenu =>
                     if E > Line_Last then
                        Put_Line ("missing command for submenu "
                                    & Line (P .. E - 1));
                        Cmd := null;
                        exit;
                     end if;
                     P := E;
                  when Menu_Command =>
                     exit;
               end case;
            end loop;

            if Cmd /= null then
               Cmd.Proc.all (Line (E .. Line_Last));

               case Command_Status is
                  when Status_Default =>
                     null;
                  when Status_Quit =>
                     exit;
               end case;
            end if;
         exception
            when Command_Error =>
               null;
         end;
      end loop;
   end Debug_Loop;

end Debuggers;
