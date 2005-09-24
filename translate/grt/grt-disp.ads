--  GHDL Run Time (GRT) - Common display subprograms.
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
with Grt.Signals; use Grt.Signals;
with Grt.Types; use Grt.Types;

package Grt.Disp is
   --  Display SIG number.
   procedure Put_Sig_Index (Sig : Sig_Table_Index);

   --  Disp current time and current delta.
   procedure Disp_Now;

   procedure Disp_Propagation_Kind (Kind : Propagation_Kind_Type);

   --  Disp signals propagation order.
   procedure Disp_Signals_Order;

   --  Disp mode.
   procedure Disp_Mode (Mode : Mode_Type);

   --  Disp value (numeric).
   procedure Disp_Value (Value : Value_Union; Mode : Mode_Type);

end Grt.Disp;
