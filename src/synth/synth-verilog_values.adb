--  Values definition for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Synth.Verilog_Exprs;

package body Synth.Verilog_Values is
   function Is_Static (V : Valtyp) return Boolean is
   begin
      case V.Kind is
         when Value_None =>
            raise Internal_Error;
         when Value_Net
           | Value_Wire =>
            return False;
         when Value_Memory =>
            return True;
      end case;
   end Is_Static;

   function Create_Value_Net (N : Net; Typ : Node) return Valtyp is
   begin
      return (Kind => Value_Net, Typ => Typ, N => N);
   end Create_Value_Net;

   function Create_Value_Memory (Mem : Memory_Ptr; Typ : Node) return Valtyp is
   begin
      return (Kind => Value_Memory, Typ => Typ, Mem => Mem);
   end Create_Value_Memory;

   function Get_Net (Ctxt : Context_Acc; V : Valtyp) return Net is
   begin
      case V.Kind is
         when Value_None =>
            raise Internal_Error;
         when Value_Net =>
            return V.N;
         when Value_Wire =>
            return Get_Current_Value (Ctxt, V.W);
         when Value_Memory =>
            return Verilog_Exprs.Memory2net (Ctxt, V.Mem, V.Typ);
      end case;
   end Get_Net;
end Synth.Verilog_Values;
