--  GHDL Run Time (GRT) - Display subprograms for signals.
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
with Grt.Types; use Grt.Types;
with Grt.Signals; use Grt.Signals;
with Grt.Stdio; use Grt.Stdio;

package Grt.Disp_Signals is
   procedure Disp_All_Signals;

   procedure Disp_Signals_Map;

   procedure Disp_Signals_Table;

   procedure Disp_All_Sensitivity;

   procedure Disp_Mode_Signal (Mode : Mode_Signal_Type);

   --  Disp informations on signal SIG.
   --  To be used inside the debugger.
   procedure Disp_A_Signal (Sig : Ghdl_Signal_Ptr);

   --  Put the full name of signal SIG.
   --  This operation is really expensive, since the whole hierarchy is
   --  traversed.
   procedure Put_Signal_Name (Stream : FILEs; Sig : Ghdl_Signal_Ptr);
end Grt.Disp_Signals;
