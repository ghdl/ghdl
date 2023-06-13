--  Verilog parse and analysis flags
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

package Verilog.Flags is
   type Standard_Type is (Verilog_Xl, Verilog_Ovi, Verilog_1995,
                          Verilog_2001,
                          Verilog_2005,
                          Verilog_Sv_3_0,
                          Verilog_Sv_3_1,
                          Verilog_Sv_3_1a,
                          Verilog_Sv2005,
                          Verilog_Sv2009,
                          Verilog_Sv2012,
                          Verilog_Sv2017);

   --  The verilog standard.
   Std : Standard_Type := Verilog_Sv2017;

   subtype Verilog_Standard is Standard_Type range Verilog_Xl .. Verilog_2005;
   subtype Systemverilog_Standard is
     Standard_Type range Verilog_Sv_3_0 .. Verilog_Sv2017;

   --  If true support AMS extensions.
   Flag_AMS : Boolean := False;

   --  If true, the parser create a node for parentheses.
   Flag_Keep_Parentheses : Boolean := False;
end Verilog.Flags;
