--  Values in synthesis.
--  Copyright (C) 2017 Tristan Gingold
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

with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;

package body Synth.Values.Debug is
   procedure Debug_Bound (Bnd : Bound_Type) is
   begin
      Put_Int32 (Bnd.Left);
      Put (' ');
      case Bnd.Dir is
         when Iir_To =>
            Put ("to");
         when Iir_Downto =>
            Put ("downto");
      end case;
      Put (' ');
      Put_Int32 (Bnd.Right);
      Put (" [");
      Put_Uns32 (Bnd.Len);
      Put (']');
   end Debug_Bound;

   procedure Debug_Memtyp (M : Memory_Ptr; Typ : Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Vector =>
            Put ("vector (");
            Debug_Bound (Typ.Vbound);
            Put ("): ");
            for I in 1 .. Typ.Vbound.Len loop
               Put_Uns32 (Uns32 (Read_U8 (M + Size_Type (I - 1))));
            end loop;
         when Type_Array =>
            Put ("arr (");
            for I in 1 .. Typ.Abounds.Ndim loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Bound (Typ.Abounds.D (I));
            end loop;
            Put ("): ");
            for I in 1 .. Get_Array_Flat_Length (Typ) loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Memtyp
                 (M + Size_Type (I - 1) * Typ.Arr_El.Sz, Typ.Arr_El);
            end loop;
         when Type_Record =>
            Put ("rec: (");
            for I in Typ.Rec.E'Range loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Memtyp (M + Typ.Rec.E (I).Moff, Typ.Rec.E (I).Typ);
            end loop;
            Put (")");
         when Type_Discrete =>
            Put ("discrete: ");
            Put_Int64 (Read_Discrete (M, Typ));
         when others =>
            Put ("others");
      end case;
      New_Line;
   end Debug_Memtyp;

   procedure Debug_Valtyp (V : Valtyp) is
   begin
      Debug_Memtyp (V.Val.Mem, V.Typ);
   end Debug_Valtyp;

end Synth.Values.Debug;
