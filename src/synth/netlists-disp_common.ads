--  Routine to disp net names.
--  Copyright (C) 2026 Tristan Gingold
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

   Bchar : constant array (Uns32 range 0 .. 3) of Character := "01ZX";

   procedure Put_Id (N : Name_Id);
   procedure Disp_Binary_Digits (Va : Uns32; Zx : Uns32; W : Natural);

   --  Display the digits of binary value PV.
   procedure Disp_Pval_Binary_Digits (Pv : Pval);

   --  Display PV within double quotes.
   procedure Disp_Pval_Binary (Pv : Pval);

   --  Display parameter value PV as a string.
   procedure Disp_Pval_String (Pv : Pval);

   --  Disp PV as a 64b floating point.
   procedure Disp_Pval_Fp64 (Pv : Pval);

   --  Disp PV (32/64) as an decimal integer.
   procedure Disp_Pval_Integer (Pv : Pval);

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
