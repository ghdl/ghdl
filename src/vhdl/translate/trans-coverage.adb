--  Iir to ortho translator.
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

with Trans_Decls; use Trans_Decls;

package body Trans.Coverage is
   use Trans.Helpers;

   procedure Cover_Initialize is
   begin
      if Coverage_Level = Coverage_None then
         return;
      end if;

      Coverage_On := True;

      New_Var_Decl (Ghdl_Cov_Counters, Get_Identifier ("__ghdl_coverage"),
                    O_Storage_External, Ghdl_Bool_Array_Type);
      Cover_Tables.Init (Cover_Table, 128);
   end Cover_Initialize;

   procedure Gen_Cover (Idx : Natural) is
   begin
      New_Assign_Stmt
        (New_Indexed_Element (New_Obj (Ghdl_Cov_Counters),
                              New_Lit (New_Index_Lit (Unsigned_64 (Idx)))),
         New_Lit (Ghdl_Bool_True_Node));
   end Gen_Cover;

   procedure Cover_Statement (N : Iir) is
   begin
      if not Coverage_On then
         return;
      end if;

      Cover_Tables.Append (Cover_Table, (N => N));
      if not Get_Covered_Flag (N) then
         Gen_Cover (Cover_Tables.Last (Cover_Table));
      end if;
   end Cover_Statement;

   function Cover_Decision (N : Iir; Val : O_Enode) return O_Enode is
   begin
      if not Coverage_On then
         return Val;
      end if;

      Cover_Tables.Append (Cover_Table, (N => N));
      Gen_Cover (Cover_Tables.Last (Cover_Table));

      return Val;
   end Cover_Decision;

   procedure Cover_Finalize
   is
      Len : Natural;
      Atype : O_Tnode;
   begin
      if not Coverage_On then
         return;
      end if;

      Len := Cover_Tables.Last (Cover_Table);

      Atype := New_Array_Subtype
        (Ghdl_Bool_Array_Type, Ghdl_Bool_Type,
         New_Index_Lit (Unsigned_64 (Len)));
      New_Type_Decl (Get_Identifier ("__ghdl_coverage_type"), Atype);

      New_Var_Body (Ghdl_Cov_Counters, O_Storage_Public, Atype);
   end Cover_Finalize;
end Trans.Coverage;
