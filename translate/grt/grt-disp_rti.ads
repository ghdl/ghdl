--  GHDL Run Time (GRT) - RTI dumper.
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
with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Stdio; use Grt.Stdio;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;

package Grt.Disp_Rti is
   --  Disp NAME.  If NAME is null, then disp <anonymous>.
   procedure Disp_Name (Name : Ghdl_C_String);

   --  Disp a value.
   procedure Disp_Value (Stream : FILEs;
                         Rti : Ghdl_Rti_Access;
                         Ctxt : Rti_Context;
                         Obj : in out Address;
                         Is_Sig : Boolean);

   procedure Register;
end Grt.Disp_Rti;
