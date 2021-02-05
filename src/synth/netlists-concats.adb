--  Provide a simple way to concat an unknown number of nets.
--  Copyright (C) 2019 Tristan Gingold
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

with Netlists.Folds; use Netlists.Folds;

package body Netlists.Concats is
   procedure Append (C : in out Concat_Type; N : Net) is
   begin
      if C.Len < C.Sarr'Last then
         C.Len := C.Len + 1;
         C.Sarr (C.Len) := N;
      elsif C.Len > C.Sarr'Last then
         C.Len := C.Len + 1;
         Net_Tables.Append (C.Darr, N);
      else
         --  Switch to the dynamic array.
         C.Len := C.Len + 1;
         Net_Tables.Init (C.Darr, 2 * Static_Last);
         Net_Tables.Set_Last (C.Darr, C.Len);
         C.Darr.Table (C.Sarr'Range) := C.Sarr;
         C.Darr.Table (C.Len) := N;
      end if;
   end Append;

   --  Get the concatenation of all nets in C.  Reset C.
   procedure Build (Ctxt : Context_Acc; C : in out Concat_Type; N : out Net) is
   begin
      case C.Len is
         when Int32'First .. 0 =>
            raise Internal_Error;
         when 1 .. Static_Last =>
            N := Build2_Concat (Ctxt, Net_Array (C.Sarr (1 .. C.Len)));
         when Static_Last + 1 .. Int32'Last =>
            --  Compute length.
            pragma Assert (C.Len = Net_Tables.Last (C.Darr));
            N := Build2_Concat (Ctxt, Net_Array (C.Darr.Table (1 .. C.Len)));
            --  Free the vector.
            Net_Tables.Free (C.Darr);
      end case;

      C.Len := 0;
   end Build;
end Netlists.Concats;
