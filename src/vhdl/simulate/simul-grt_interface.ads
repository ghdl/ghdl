--  Interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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

with Grt.Types; use Grt.Types;
with Simul.Environments; use Simul.Environments;

package Simul.Grt_Interface is
   procedure Set_Std_String_From_Iir_Value (Str : Std_String;
                                            Val : Iir_Value_Literal_Acc);

   function Build_Bound (Arr : Iir_Value_Literal_Acc) return Std_String_Bound;
end Simul.Grt_Interface;
