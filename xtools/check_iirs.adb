--  Tool to check the coherence of the iirs package.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Check_Iirs_Pkg;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Check_Iirs
is
   type Prg_Mode is (Mode_Generate, Mode_Genfast, Mode_Free);
   Mode : Prg_Mode;
   procedure Usage is
   begin
      Put_Line ("usage: " & Command_Name & " MODE");
      Put_Line ("MODE is one of:");
      Put_Line (" --generate");
      Put_Line (" --genfast");
      Put_Line (" --list-free-fields");
   end Usage;
begin
   if Argument_Count /= 1 then
      Usage;
      Set_Exit_Status (Failure);
      return;
   end if;
   if Argument (1) = "--generate" then
      Mode := Mode_Generate;
   elsif Argument (1) = "--genfast" then
      Mode := Mode_Genfast;
   elsif Argument (1) = "--list-free-fields" then
      Mode := Mode_Free;
   else
      Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   Check_Iirs_Pkg.Read_Fields;
   Check_Iirs_Pkg.Check_Iirs;
   Check_Iirs_Pkg.Read_Desc;
   case Mode is
      when Mode_Generate =>
         Check_Iirs_Pkg.Gen_Func;
      when Mode_Genfast =>
         Check_Iirs_Pkg.Flag_Checks := False;
         Check_Iirs_Pkg.Gen_Func;
      when Mode_Free =>
         Check_Iirs_Pkg.List_Free_Fields;
   end case;
end Check_Iirs;
