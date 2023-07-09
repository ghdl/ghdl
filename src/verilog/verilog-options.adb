--  Verilog command line options
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Verilog.Scans;
with Verilog.Flags; use Verilog.Flags;

package body Verilog.Options is
   function Parse_Option (Option : String) return Boolean
   is
      Opt : constant String (1 .. Option'Length) := Option;
   begin
      if Opt'Length > 8 and then Opt (1 .. 8) = "+incdir+" then
         --  Can be a list of directories (+incdir+AAA+BBB)
         declare
            B : Natural;
            E : Natural;
         begin
            B := 9;
            E := B;
            loop
               if Opt (E) = '+' then
                  if E > B then
                     --  Empty pathes are ignored.
                     Scans.Add_Incdir (Opt (B .. E));
                  end if;
                  E := E + 1;
                  B := E;
                  exit when E > Opt'Last;
               elsif E = Opt'Last then
                  if E > B then
                     Scans.Add_Incdir (Opt (B .. E));
                  end if;
                  exit;
               else
                  E := E + 1;
               end if;
            end loop;
         end;
      elsif Opt (1) = '+' then
         return True;
      elsif Opt'Length > 3 and then Opt (1 .. 2) = "-I" then
         Scans.Add_Incdir (Opt (3 .. Opt'Last));
      elsif Opt'Length > 6 and then Opt (1 .. 6) = "--std=" then
         if Opt (7 .. Opt'Last) = "1995" then
            Flags.Std := Verilog_1995;
         elsif Opt (7 .. Opt'Last) = "2001" then
            Flags.Std := Verilog_2001;
         elsif Opt (7 .. Opt'Last) = "2005" then
            Flags.Std := Verilog_2005;
         elsif Opt (7 .. Opt'Last) = "sv2005" then
            Flags.Std := Verilog_Sv2005;
         elsif Opt (7 .. Opt'Last) = "sv2009" then
            Flags.Std := Verilog_Sv2009;
         else
            return False;
         end if;
      elsif Opt = "--ams" then
         Flags.Flag_AMS := True;
      else
         return False;
      end if;
      return True;
   end Parse_Option;
end Verilog.Options;
