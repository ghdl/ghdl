--  Routine to disp net names.
--  Copyright (C) 2025 Tristan Gingold
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

package Netlists.Disp_Common is
   --  Languages supported (to create extended identifiers).
   subtype Language_Range is
     Language_Type range Language_Vhdl .. Language_Verilog;

   --  Disp name N without extended character.
   --  Useful when a suffix or a prefix is needed.
   --  Use Is_Extended_Sname if N must be displayed as an extended identifier.
   procedure Put_Name_Inner
     (N : Sname; Lang : Language_Range; Is_Extended : Boolean);

   --  Disp name N (as a normal identifier or as an extended identifier).
   procedure Put_Name (N : Sname; Lang : Language_Range);

   --  Return True IFF N requires an extended identifier for vhdl.
   function Is_Extended_Sname (N : Sname) return Boolean;

   --  Disp the name of a net.  Append the port name when needed.
   procedure Disp_Net_Name (N : Net; Lang : Language_Range);

   --  Return True if INST cannot be displayed inline.
   --  Some gates require a name as an input (and not a bit-string) as they
   --  use indexed names or slice names.
   function Need_Name (Inst : Instance) return Boolean;

   --  Return TRUE if edge INST (posedge or negedge) is used outside clock
   --  inputs.
   function Need_Edge (Inst : Instance) return Boolean;

   --  Disp the name of am instance.
   procedure Put_Instance_Name (Name : Sname; Lang : Language_Range);

   procedure Put_Interface_Name (N : Sname; Lang : Language_Range);
end Netlists.Disp_Common;
