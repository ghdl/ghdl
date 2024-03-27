--  GHDL driver - Coverage commands
--  Copyright (C) 2024 Tristan Gingold
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

with System;
with Ada.Unchecked_Conversion;

with Tables;
with Files_Map;
with Simple_IO;
with Errorout;
with Name_Table;

with Vhdl.Nodes; use Vhdl.Nodes;

with Grt.Algos;
with Grt.Types;
with Grt.Stdio;
with Grt.Astdio;

with Ortho_Jit;

with Trans.Coverage;
with Trans_Decls;

package body Ghdlcovout is
   type Coverage_Entry is record
      --  Location of the coverage point (without instantiation).
      Loc : Location_Type;

      --  Original statement/decision.
      N : Node;

      --  Coverage result.
      Res_T : Boolean;
   end record;

   package Cov_Tables is new Tables
     (Table_Component_Type => Coverage_Entry,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 8);

   function Ce_Lt (L, R : Positive) return Boolean is
   begin
      return Cov_Tables.Table (L).Loc <= Cov_Tables.Table (R).Loc;
   end Ce_Lt;

   procedure Ce_Swap (P1 : Positive; P2 : Positive)
   is
      T : Coverage_Entry;
   begin
      T := Cov_Tables.Table (P2);
      Cov_Tables.Table (P2) := Cov_Tables.Table (P1);
      Cov_Tables.Table (P1) := T;
   end Ce_Swap;

   procedure Sort is new Grt.Algos.Heap_Sort (Ce_Lt, Ce_Swap);

   type Byte_Array is array (Natural range <>) of Grt.Types.Ghdl_U8;
   type Byte_Array_Thin_Ptr is access all Byte_Array (Natural);
   function To_Byte_Array_Thin_Ptr is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Byte_Array_Thin_Ptr);

   procedure Fill_Entries
   is
      use Trans.Coverage;
      Last : constant Integer := Cover_Tables.Last (Cover_Table);
      E : Coverage_Entry;
      Loc, N_Loc : Location_Type;
      Counters : Byte_Array_Thin_Ptr;
      Idx : Natural;
   begin
      if Last < Cover_Tables.First then
         --  Empty.
         return;
      end if;

      Counters := To_Byte_Array_Thin_Ptr
        (Ortho_Jit.Get_Address (Trans_Decls.Ghdl_Cov_Counters));

      Cov_Tables.Reserve (Last + 1);
      for I in Cover_Tables.First .. Cover_Tables.Last (Cover_Table) loop
         declare
            use Grt.Types;
            Ce : Trans.Coverage.Coverage_Entry renames Cover_Table.Table (I);
         begin
            Loc := Get_Location (Ce.N);
            loop
               N_Loc := Files_Map.Location_Instance_To_Location (Loc);
               exit when N_Loc = No_Location;
               Loc := N_Loc;
            end loop;

            E := (N => Ce.N,
                  Loc => Loc,
                  Res_T => Counters (I) /= 0);

            if Get_Covered_Flag (Ce.N) then
               E.Res_T := True;
            end if;

            Cov_Tables.Append (E);
         end;
      end loop;

      Sort (Cov_Tables.Last);

      --  Compact: merge entries with the same location.
      Idx := Cov_Tables.First;
      Loc := Cov_Tables.Table (Idx).Loc;
      for I in Cov_Tables.First + 1 .. Cov_Tables.Last loop
         N_Loc := Cov_Tables.Table (I).Loc;
         if N_Loc = Loc then
            if Cov_Tables.Table (I).Res_T then
               Cov_Tables.Table (Idx).Res_T := True;
            end if;
         else
            Idx := Idx + 1;
            if Idx /= I then
               Cov_Tables.Table (Idx) := Cov_Tables.Table (I);
            end if;
            Loc := N_Loc;
         end if;
      end loop;
      Cov_Tables.Set_Last (Idx);
   end Fill_Entries;

   procedure Collect is
   begin
      Fill_Entries;
   end Collect;

   function Open_Coverage_File (C_Filename : String) return Grt.Stdio.FILEs
   is
      use Grt.Stdio;
      F : FILEs;
      Mode : constant String := "w" & ASCII.NUL;
   begin
      F := fopen (C_Filename'Address, Mode'Address);
      if F = NULL_Stream then
         Errorout.Error_Msg_Option
           ("cannot open '"
              & C_Filename (C_Filename'First .. C_Filename'Last - 1)
              & "'");
      end if;
      return F;
   end Open_Coverage_File;

   procedure Write_Coverage_File
   is
      use Grt.Astdio;
      use Grt.Stdio;
      use Files_Map;
      Now_Ts : constant String := Get_Time_Stamp_String (Get_Os_Time_Stamp);
      F : FILEs;
      Idx : Positive;
   begin
      if Output_Filename = null then
         F := Open_Coverage_File ("coverage-" & Now_Ts & ".json" & ASCII.NUL);
      else
         F := Open_Coverage_File (Output_Filename.all & ASCII.NUL);
      end if;
      if F = NULL_Stream then
         return;
      end if;

      Put_Line (F, "{");
      Put_Line (F, " 'app': 'GHDL coverage output file',");
      Put_Line (F, " 'version': '1.0.0',");
      Put_Line (F, " 'testcase': 'unknown',");
      Put_Line (F, " 'timestamp': '" & Now_Ts & "',");
      Put_Line (F, " [");

      Idx := Cov_Tables.First;
      while Idx < Cov_Tables.Last loop
         --  Gather per file.
         declare
            use Grt.Types;
            Loc : constant Location_Type := Cov_Tables.Table (Idx).Loc;
            Sfe : constant Source_File_Entry := Location_To_File (Loc);
            Dir : constant Name_Id := Get_Directory_Name (Sfe);
            Last_Loc : constant Location_Type :=
              File_Pos_To_Location (Sfe, Get_Buffer_Length (Sfe));
            Nloc : Location_Type;
            Line, Nline : Ghdl_I32;
            Res : Boolean;
            Nidx : Natural;
         begin
            Put_Line (F, "  {");
            Put (F, "   'file': '");
            Put (F, Name_Table.Image (Get_File_Name (Sfe)));
            Put_Line (F, "',");
            Put (F, "   'dir': '");
            if Dir = Files_Map.Get_Home_Directory then
               Put (F, ".");
            else
               Put (F, Name_Table.Image (Get_Directory_Name (Sfe)));
            end if;
            Put_Line (F, "',");
            Put (F, "   'sha1': '");
            Put (F, Get_File_Checksum_String (Get_File_Checksum (Sfe)));
            Put_Line (F, "',");
            Put_Line (F, "   'mode': 'stmt',");

            --  Find the max line.
            Nidx := Idx + 1;
            loop
               exit when Nidx = Cov_Tables.Last;
               if Cov_Tables.Table (Nidx).Loc >= Last_Loc then
                  Nidx := Nidx - 1;
                  exit;
               end if;
               Nidx := Nidx + 1;
            end loop;
            Line := Ghdl_I32 (Location_File_To_Line
                                (Cov_Tables.Table (Nidx).Loc, Sfe));
            Put (F, "   'max-line': ");
            Put_I32 (F, Line);
            Put (F, ",");
            New_Line (F);

            Put_Line (F, "   'result': [");

            Nloc := Loc;
            Line := Ghdl_I32 (Location_File_To_Line (Nloc, Sfe));
            loop
               Put (F, "    '");
               Put_I32 (F, Line);
               Put (F, "': ");

               --  Merge results for the same line.
               Res := Cov_Tables.Table (Idx).Res_T;
               loop
                  Idx := Idx + 1;
                  exit when Idx > Cov_Tables.Last;
                  Nloc := Cov_Tables.Table (Idx).Loc;
                  Nline := Ghdl_I32 (Location_File_To_Line (Nloc, Sfe));
                  exit when Nline /= Line;
                  Res := Res or Cov_Tables.Table (Idx).Res_T;
               end loop;

               Put_I32 (F, Boolean'Pos (Res));

               if Idx > Cov_Tables.Last or else Nloc > Last_Loc then
                  New_Line (F);
                  exit;
               end if;

               Line := Nline;
               Put_Line (F, ",");
            end loop;
            Put_Line (F, "   ]");
            Put (F, "  }");
            if Idx < Cov_Tables.Last then
               Put (F, ",");
            end if;
            New_Line (F);
         end;
      end loop;
      Put_Line (F, " ]");
      Put_Line (F, "}");
      fclose (F);
   end Write_Coverage_File;

   procedure Dump
   is
      use Simple_IO;
   begin
      for I in Cov_Tables.First .. Cov_Tables.Last loop
         Put (Files_Map.Image (Cov_Tables.Table (I).Loc));
         if Cov_Tables.Table (I).Res_T then
            Put_Line ("  +");
         else
            Put_Line ("  #");
         end if;
      end loop;
   end Dump;
end Ghdlcovout;
