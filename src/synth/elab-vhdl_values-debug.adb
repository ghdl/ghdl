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

package body Elab.Vhdl_Values.Debug is
   procedure Put_Dir (Dir : Direction_Type) is
   begin
      case Dir is
         when Dir_To =>
            Put ("to");
         when Dir_Downto =>
            Put ("downto");
      end case;
   end Put_Dir;

   procedure Debug_Bound (Bnd : Bound_Type; Verbose : Boolean) is
   begin
      Put_Int32 (Bnd.Left);
      Put (' ');
      Put_Dir (Bnd.Dir);
      Put (' ');
      Put_Int32 (Bnd.Right);
      if Verbose then
         Put (" [l=");
         Put_Uns32 (Bnd.Len);
         Put (']');
      end if;
   end Debug_Bound;

   procedure Debug_Typ_Phys (T : Type_Acc) is
   begin
      Put ("[al=");
      Put_Int32 (Int32 (T.Al));
      Put (" sz=");
      Put_Uns32 (Uns32 (T.Sz));
      Put (" w=");
      Put_Uns32 (T.W);
      Put (']');
   end Debug_Typ_Phys;

   procedure Debug_Typ1 (T : Type_Acc) is
   begin
      case T.Kind is
         when Type_Bit =>
            Put ("bit");
            Debug_Typ_Phys (T);
         when Type_Logic =>
            Put ("bit/logic");
            Debug_Typ_Phys (T);
         when Type_Vector =>
            Put ("vector ");
            Debug_Typ_Phys (T);
            Put (" (");
            Debug_Bound (T.Abound, True);
            Put (") of ");
            Debug_Typ1 (T.Arr_El);
         when Type_Array =>
            Put ("arr ");
            Debug_Typ_Phys (T);
            Put (" (");
            declare
               It : Type_Acc;
            begin
               It := T;
               loop
                  Debug_Bound (It.Abound, True);
                  exit when It.Alast;
                  Put (", ");
                  It := It.Arr_El;
               end loop;
               Put (") of ");
               Debug_Typ1 (It.Arr_El);
            end;
         when Type_Record =>
            Put ("rec ");
            Debug_Typ_Phys (T);
            Put (" (");
            for I in T.Rec.E'Range loop
               if I /= 1 then
                  Put (", ");
               end if;
               Put ("[noff=");
               Put_Uns32 (T.Rec.E (I).Offs.Net_Off);
               Put (", moff=");
               Put_Uns32 (Uns32 (T.Rec.E (I).Offs.Mem_Off));
               Put ("] ");
               Debug_Typ1 (T.Rec.E (I).Typ);
            end loop;
            Put (")");
         when Type_Unbounded_Record =>
            Put ("unbounded record");
         when Type_Discrete =>
            Put ("discrete ");
            Debug_Typ_Phys (T);
            Put (": ");
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
            Put ("unbounded arr (");
            declare
               It : Type_Acc;
            begin
               It := T;
               loop
                  Put ("<>");
                  exit when It.Ulast;
                  Put (", ");
                  It := It.Uarr_El;
               end loop;
               Put (") of ");
               Debug_Typ1 (It.Uarr_El);
            end;
         when Type_Protected =>
            Put ("protected");
      end case;
   end Debug_Typ1;

   procedure Debug_Typ (T : Type_Acc) is
   begin
      Debug_Typ1 (T);
      New_Line;
   end Debug_Typ;

   procedure Debug_Type_Short (T : Type_Acc) is
   begin
      case T.Kind is
         when Type_Bit =>
            Put ("bit");
         when Type_Logic =>
            Put ("logic");
         when Type_Vector =>
            Debug_Type_Short (T.Arr_El);
            Put ("_vec(");
            Debug_Bound (T.Abound, False);
            Put (")");
         when Type_Array =>
            declare
               It : Type_Acc;
            begin
               Put ("arr (");
               It := T;
               loop
                  Debug_Bound (It.Abound, False);
                  exit when It.Alast;
                  It := It.Arr_El;
                  Put (", ");
               end loop;
               Put (")");
            end;
         when Type_Record =>
            Put ("rec: (");
            Put (")");
         when Type_Unbounded_Record =>
            Put ("unbounded record");
         when Type_Discrete =>
            Put ("discrete");
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
   end Debug_Type_Short;

   procedure Debug_Memtyp (M : Memtyp) is
   begin
      case M.Typ.Kind is
         when Type_Bit
           | Type_Logic =>
            Put ("bit/logic: ");
            Put_Uns32 (Uns32 (Read_U8 (M.Mem)));
         when Type_Vector =>
            Put ("vector (");
            Debug_Bound (M.Typ.Abound, True);
            Put ("): ");
            for I in 1 .. M.Typ.Abound.Len loop
               Put_Uns32 (Uns32 (Read_U8 (M.Mem + Size_Type (I - 1))));
            end loop;
         when Type_Array =>
            declare
               T : Type_Acc;
               El : Type_Acc;
               Len : Uns32;
            begin
               Put ("arr (");
               T := M.Typ;
               Len := 1;
               loop
                  Debug_Bound (T.Abound, True);
                  Len := Len * T.Abound.Len;
                  El := T.Arr_El;
                  exit when T.Alast;
                  T := El;
                  Put (", ");
               end loop;
               Put ("): ");
               for I in 1 .. Len loop
                  if I > 1 then
                     Put (", ");
                  end if;
                  Debug_Memtyp ((El, M.Mem + Size_Type (I - 1) * El.Sz));
               end loop;
            end;
         when Type_Record =>
            Put ("rec: (");
            for I in M.Typ.Rec.E'Range loop
               if I > 1 then
                  Put (", ");
               end if;
               Debug_Memtyp
                 ((M.Typ.Rec.E (I).Typ, M.Mem + M.Typ.Rec.E (I).Offs.Mem_Off));
            end loop;
            Put (")");
         when Type_Discrete =>
            Put ("discrete: ");
            Put_Int64 (Read_Discrete (M));
         when Type_Access =>
            Put ("access: ");
            Put_Uns32 (Uns32 (Read_Access (M)));
         when Type_File =>
            Put ("file");
         when Type_Float =>
            Put ("float: ");
            Put_Fp64 (Read_Fp64 (M.Mem));
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
            Put ("net ");
            Put_Uns32 (V.Val.N);
            Put (' ');
            Debug_Typ1 (V.Typ);
            New_Line;
         when Value_Signal =>
            Put ("signal ");
            Put_Uns32 (Uns32 (V.Val.S));
            Put (": ");
            Debug_Typ1 (V.Typ);
            New_Line;
         when Value_Wire =>
            Put ("wire ");
            Put_Uns32 (V.Val.N);
            New_Line;
         when Value_File =>
            Put_Line ("a file");
         when Value_Quantity =>
            Put ("quantity ");
            Put_Uns32 (Uns32 (V.Val.Q));
            New_Line;
         when Value_Terminal =>
            Put ("terminal ");
            Put_Uns32 (Uns32 (V.Val.T));
            New_Line;
         when Value_Alias =>
            Put ("an alias: ");
            Debug_Typ1 (V.Typ);
            Put (" at offs ");
            Put_Uns32 (V.Val.A_Off.Net_Off);
            Put (" of ");
            Debug_Valtyp ((V.Val.A_Typ, V.Val.A_Obj));
         when Value_Dyn_Alias =>
            Put ("dyn alias: ");
            Debug_Typ1 (V.Typ);
      end case;
   end Debug_Valtyp;

end Elab.Vhdl_Values.Debug;
