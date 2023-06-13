--  Environment definition for synthesis.
--  Copyright (C) 2017 Tristan Gingold
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

with Errorout; use Errorout;

with Synth.Errors; use Synth.Errors;
with Verilog.Errors; use Verilog.Errors;

with Synth.Verilog_Exprs;

package body Synth.Verilog_Environment is
   function Get_Bitwidth (Val : Memtyp) return Uns32 is
   begin
      raise Internal_Error;
      return 0;
   end Get_Bitwidth;

   function Is_Equal (L, R : Memtyp) return Boolean is
   begin
      raise Internal_Error;
      return False;
   end Is_Equal;

   function Memtyp_To_Net (Ctxt : Builders.Context_Acc; Val : Memtyp)
                          return Net is
   begin
      return Verilog_Exprs.Memory2net (Ctxt, Val.Mem, Val.Typ);
   end Memtyp_To_Net;

   function Partial_Memtyp_To_Net
     (Ctxt : Builders.Context_Acc; Val : Memtyp; Off : Uns32; Wd : Uns32)
     return Net is
   begin
      raise Internal_Error;
      return No_Net;
   end Partial_Memtyp_To_Net;

   procedure Warning_No_Assignment
     (Decl : Node; First_Off : Uns32; Last_Off : Uns32) is
   begin
      if Last_Off < First_Off then
         Warning_Msg_Synth
           (Warnid_Nowrite, +Decl, "no assignment for %n", +Decl);
      elsif Last_Off = First_Off then
         Warning_Msg_Synth
           (+Decl, "no assignment for offset %v of %n",
            (1 => +First_Off, 2 => +Decl));
      else
         Warning_Msg_Synth
           (+Decl, "no assignment for offsets %v:%v of %n",
                            (+First_Off, +Last_Off, +Decl));
      end if;
   end Warning_No_Assignment;

   procedure Error_Multiple_Assignments
     (Decl : Node; First_Off : Uns32; Last_Off : Uns32) is
   begin
      Error_Msg_Netlist (+Decl, "multiple assignments for %i offsets %v:%v",
                         (+Decl, +First_Off, +Last_Off));
   end Error_Multiple_Assignments;
end Synth.Verilog_Environment;
