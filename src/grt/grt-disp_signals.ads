--  GHDL Run Time (GRT) - Display subprograms for signals.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Types; use Grt.Types;
with Grt.Signals; use Grt.Signals;
with Grt.Stdio; use Grt.Stdio;

package Grt.Disp_Signals is
   procedure Disp_All_Signals;

   procedure Disp_Signals_Map;

   procedure Disp_Signals_Table;

   procedure Disp_All_Sensitivity;

   procedure Disp_Mode_Signal (Mode : Mode_Signal_Type);

   --  Disp attributes of SIG.
   procedure Disp_Single_Signal_Attributes (Sig : Ghdl_Signal_Ptr);

   --  Disp informations on signal SIG.
   --  To be used inside the debugger.
   procedure Disp_A_Signal (Sig : Ghdl_Signal_Ptr);

   --  Put the full name of signal SIG.
   --  This operation is really expensive, since the whole hierarchy is
   --  traversed.
   procedure Put_Signal_Name (Stream : FILEs; Sig : Ghdl_Signal_Ptr);
end Grt.Disp_Signals;
