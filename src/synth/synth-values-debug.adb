--  Values in synthesis.
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

with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;

with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Environment.Debug; use Synth.Environment.Debug;

package body Synth.Values.Debug is
   procedure Put_Dir (Dir : Direction_Type) is
   begin
      case Dir is
         when Dir_To =>
            Put ("to");
         when Dir_Downto =>
            Put ("downto");
      end case;
   end Put_Dir;

   procedure Debug_Bound (Bnd : Bound_Type) is
   begin
      Put_Int32 (Bnd.Left);
      Put (' ');
      Put_Dir (Bnd.Dir);
      Put (' ');
      Put_Int32 (Bnd.Right);
      Put (" [");
      Put_Uns32 (Bnd.Len);
      Put (']');
   end Debug_Bound;

   procedure Debug_Typ1 (T : Type_Acc) is
   begin
      case T.Kind is
         when Type_Bit
           | Type_Logic =>
            Put ("bit/logic");
         when Type_Vector =>
            Put ("vector (");
            Debug_Bound (T.Vbound);
            Put (") of ");
            Debug_Typ1 (T.Vec_El);
         when Type_Array =>
            Put ("arr (");
            for I in 1 .. T.Abounds.Ndim loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Bound (T.Abounds.D (I));
            end loop;
            Put (") of ");
            Debug_Typ1 (T.Arr_El);
         when Type_Record =>
            Put ("rec: (");
            Put (")");
         when Type_Unbounded_Record =>
            Put ("unbounded record");
         when Type_Discrete =>
            Put ("discrete: ");
            Put_Int64 (T.Drange.Left);
            Put (' ');
            Put_Dir (T.Drange.Dir);
            Put (' ');
            Put_Int64 (T.Drange.Right);
            if T.Drange.Is_Signed then
               Put (" [signed]");
            else
               Put (" [unsigned]");
            end if;
         when Type_Access =>
            Put ("access");
         when Type_File =>
            Put ("file");
         when Type_Float =>
            Put ("float");
         when Type_Slice =>
            Put ("slice");
         when Type_Unbounded_Vector =>
            Put ("unbounded vector");
         when Type_Unbounded_Array =>
            Put ("unbounded array");
         when Type_Protected =>
            Put ("protected");
      end case;
      Put (' ');
      Put (" al=");
      Put_Int32 (Int32 (T.Al));
      Put (" sz=");
      Put_Uns32 (Uns32 (T.Sz));
      Put (" w=");
      Put_Uns32 (Uns32 (T.W));
   end Debug_Typ1;

   procedure Debug_Typ (T : Type_Acc) is
   begin
      Debug_Typ1 (T);
      New_Line;
   end Debug_Typ;

   procedure Debug_Memtyp (M : Memtyp) is
   begin
      case M.Typ.Kind is
         when Type_Bit
           | Type_Logic =>
            Put ("bit/logic");
         when Type_Vector =>
            Put ("vector (");
            Debug_Bound (M.Typ.Vbound);
            Put ("): ");
            for I in 1 .. M.Typ.Vbound.Len loop
               Put_Uns32 (Uns32 (Read_U8 (M.Mem + Size_Type (I - 1))));
            end loop;
         when Type_Array =>
            Put ("arr (");
            for I in 1 .. M.Typ.Abounds.Ndim loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Bound (M.Typ.Abounds.D (I));
            end loop;
            Put ("): ");
            for I in 1 .. Get_Array_Flat_Length (M.Typ) loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Memtyp
                 ((M.Typ.Arr_El, M.Mem + Size_Type (I - 1) * M.Typ.Arr_El.Sz));
            end loop;
         when Type_Record =>
            Put ("rec: (");
            for I in M.Typ.Rec.E'Range loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Memtyp
                 ((M.Typ.Rec.E (I).Typ, M.Mem + M.Typ.Rec.E (I).Moff));
            end loop;
            Put (")");
         when Type_Discrete =>
            Put ("discrete: ");
            Put_Int64 (Read_Discrete (M));
         when Type_Access =>
            Put ("access");
         when Type_File =>
            Put ("file");
         when Type_Float =>
            Put ("float");
         when Type_Slice =>
            Put ("slice");
         when Type_Unbounded_Vector =>
            Put ("unbounded vector");
         when Type_Unbounded_Array =>
            Put ("unbounded array");
         when Type_Unbounded_Record =>
            Put ("unbounded record");
         when Type_Protected =>
            Put ("protected");
      end case;
      New_Line;
   end Debug_Memtyp;

   procedure Debug_Valtyp (V : Valtyp) is
   begin
      case V.Val.Kind is
         when Value_Memory
           | Value_Const =>
            Debug_Memtyp (Get_Memtyp (V));
         when Value_Net =>
            Put_Line (" net");
         when Value_Wire =>
            Put (" wire");
            Put_Wire_Id (V.Val.W);
            New_Line;
         when Value_File =>
            Put_Line ("a file");
         when Value_Alias =>
            Put_Line ("an alias");
      end case;
   end Debug_Valtyp;

end Synth.Values.Debug;
