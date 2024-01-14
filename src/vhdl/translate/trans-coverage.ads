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

with Dyn_Tables;

package Trans.Coverage is
   type Coverage_Level_Type is
     (
      --  No coverage
      Coverage_None,

      --  Statement coverage
      Coverage_Stmt,

      --  Decision (+ statement) coverage
      Coverage_Decision
     );

   --  Level of coverage
   Coverage_Level : Coverage_Level_Type := Coverage_None;

   --  Set to false if coverage is not enabled for a particular unit.
   Coverage_On : Boolean := False;

   procedure Cover_Initialize;
   procedure Cover_Finalize;

   procedure Cover_Statement (N : Iir);
   function Cover_Decision (N : Iir; Val : O_Enode) return O_Enode;

   type Coverage_Entry is record
      N : Iir;
   end record;

   package Cover_Tables is new Dyn_Tables
     (Table_Component_Type => Coverage_Entry,
      Table_Index_Type => Integer,
      Table_Low_Bound => 0);

   Cover_Table : Cover_Tables.Instance;
end Trans.Coverage;
