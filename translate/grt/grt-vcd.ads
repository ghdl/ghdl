--  GHDL Run Time (GRT) - VCD generator.
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
with Grt.Avhpi; use Grt.Avhpi;

package Grt.Vcd is
   --  Abstract type for IO.
   type Vcd_Put_Acc is access procedure (Str : String);
   type Vcd_Putc_Acc is access procedure (C : Character);
   type Vcd_Close_Acc is access procedure;

   Vcd_Put : Vcd_Put_Acc;
   Vcd_Putc : Vcd_Putc_Acc;
   Vcd_Close : Vcd_Close_Acc;

   type Vcd_Var_Kind is (Vcd_Bad,
                         Vcd_Bool,
                         Vcd_Integer32,
                         Vcd_Bit, Vcd_Stdlogic,
                         Vcd_Bitvector, Vcd_Stdlogic_Vector);

   --  Which value to be displayed: effective or driving (for out signals).
   type Vcd_Value_Kind is (Vcd_Effective, Vcd_Driving);

   type Verilog_Wire_Info is record
      Addr : Address;
      Irange : Ghdl_Range_Ptr;
      Kind : Vcd_Var_Kind;
      Val : Vcd_Value_Kind;
   end record;

   procedure Get_Verilog_Wire (Sig : VhpiHandleT;
                               Info : out Verilog_Wire_Info);

   --  Return TRUE if last change time of the wire described by INFO is LAST.
   function Verilog_Wire_Changed (Info : Verilog_Wire_Info;
                                  Last : Std_Time)
                                 return Boolean;

   procedure Register;
end Grt.Vcd;
