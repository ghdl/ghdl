--  Provide a simple way to concat an unknown number of nets.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

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
         Net_Tables.Init (C.Darr);
         Net_Tables.Set_Last (C.Darr, C.Len);
         C.Darr.Table (C.Sarr'Range) := C.Sarr;
         C.Darr.Table (C.Len) := N;
      end if;
   end Append;

   --  Get the concatenation of all nets in C.  Reset C.
   procedure Build (Ctxt : Context_Acc; C : in out Concat_Type; N : out Net)
   is
      Inst : Instance;
      Wd : Width;
   begin
      case C.Len is
         when Int32'First .. 0 =>
            raise Internal_Error;
         when 1 =>
            N := C.Sarr (1);
         when 2 =>
            N := Build_Concat2 (Ctxt, C.Sarr (2), C.Sarr (1));
         when 3 =>
            N := Build_Concat3 (Ctxt, C.Sarr (3), C.Sarr (2), C.Sarr (1));
         when 4 =>
            N := Build_Concat4
              (Ctxt, C.Sarr (4), C.Sarr (3), C.Sarr (2), C.Sarr (1));
         when 5 .. Static_Last =>
            --  Compute length.
            Wd := 0;
            for I in 1 .. C.Len loop
               Wd := Wd + Get_Width (C.Sarr (I));
            end loop;

            N := Build_Concatn (Ctxt, Wd, Uns32 (C.Len));
            Inst := Get_Net_Parent (N);
            for I in 1 .. C.Len loop
               Connect (Get_Input (Inst, Port_Idx (C.Len - I)), C.Sarr (I));
            end loop;
         when Static_Last + 1 .. Int32'Last =>
            --  Compute length.
            pragma Assert (C.Len = Net_Tables.Last (C.Darr));
            Wd := 0;
            for I in 1 .. C.Len loop
               Wd := Wd + Get_Width (C.Darr.Table (I));
            end loop;

            N := Build_Concatn (Ctxt, Wd, Uns32 (C.Len));
            Inst := Get_Net_Parent (N);
            for I in Net_Tables.First .. C.Len loop
               Connect (Get_Input (Inst, Port_Idx (C.Len - I)),
                        C.Darr.Table (I));
            end loop;
            --  Free the vector.
            Net_Tables.Free (C.Darr);
      end case;

      C.Len := 0;
   end Build;
end Netlists.Concats;
