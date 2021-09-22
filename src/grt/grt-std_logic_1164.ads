--  GHDL Run Time (GRT) std_logic_1664 subprograms.
--  Copyright (C) 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Grt.Types; use Grt.Types;

package Grt.Std_Logic_1164 is
   type Std_Ulogic is ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-');

   type Stdlogic_Table_2d is array (Std_Ulogic, Std_Ulogic) of Std_Ulogic;
   type Stdlogic_Table_1d is array (Std_Ulogic) of Std_Ulogic;

   --  LRM08 9.2.3 Relational operators
   Match_Eq_Table : constant Stdlogic_Table_2d :=
     --UX01ZWLH-
     ("UUUUUUUU1",
      "UXXXXXXX1",
      "UX10XX101",
      "UX01XX011",
      "UXXXXXXX1",
      "UXXXXXXX1",
      "UX10XX101",
      "UX01XX011",
      "111111111");

   Match_Lt_Table : constant Stdlogic_Table_2d :=
     --UX01ZWLH-
     ("UUUUUUUUX",
      "UXXXXXXXX",
      "UX01XX01X",
      "UX00XX00X",
      "UXXXXXXXX",
      "UXXXXXXXX",
      "UX01XX01X",
      "UX00XX00X",
      "XXXXXXXXX");

   And_Table : constant Stdlogic_Table_2d :=
     --UX01ZWLH-
     ("UU0UUU0UX",  -- U
      "UX0XXX0XX",  -- X
      "000000000",  -- 0
      "UX01XX01X",  -- 1
      "UX0XXX0XX",  -- Z
      "UX0XXX0XX",  -- W
      "000000000",  -- L
      "UX01XX01X",  -- H
      "UX0XXX0XX"); -- -

   Or_Table : constant Stdlogic_Table_2d :=
     --UX01ZWLH-
     ("UUU1UUU1U",  -- U
      "UXX1XXX1X",  -- X
      "UX01XX01X",  -- 0
      "111111111",  -- 1
      "UXX1XXX1X",  -- Z
      "UXX1XXX1X",  -- W
      "UX01XX01X",  -- L
      "111111111",  -- H
      "UXX1XXX1X"); -- -

   Xor_Table : constant Stdlogic_Table_2d :=
     --UX01ZWLH-
     ("UUUUUUUUU",  -- U
      "UXXXXXXXX",  -- X
      "UX01XX01X",  -- 0
      "UX10XX10X",  -- 1
      "UXXXXXXXX",  -- Z
      "UXXXXXXXX",  -- W
      "UX01XX01X",  -- L
      "UX10XX10X",  -- H
      "UXXXXXXXX"); -- -

   Not_Table : constant Stdlogic_Table_1d := "UX10XX10X";

   function Ghdl_Std_Ulogic_Match_Eq (L, R : Ghdl_E8) return Ghdl_E8;
   function Ghdl_Std_Ulogic_Match_Ne (L, R : Ghdl_E8) return Ghdl_E8;
   function Ghdl_Std_Ulogic_Match_Lt (L, R : Ghdl_E8) return Ghdl_E8;
   function Ghdl_Std_Ulogic_Match_Le (L, R : Ghdl_E8) return Ghdl_E8;
   function Ghdl_Std_Ulogic_Match_Ge (L, R : Ghdl_E8) return Ghdl_E8;
   function Ghdl_Std_Ulogic_Match_Gt (L, R : Ghdl_E8) return Ghdl_E8;
   --  For Gt and Ge, use Lt and Le with swapped parameters.

   function Ghdl_Std_Ulogic_Array_Match_Eq (L : Ghdl_Ptr;
                                            L_Len : Ghdl_Index_Type;
                                            R : Ghdl_Ptr;
                                            R_Len : Ghdl_Index_Type)
                                           return Ghdl_I32;
   function Ghdl_Std_Ulogic_Array_Match_Ne (L : Ghdl_Ptr;
                                            L_Len : Ghdl_Index_Type;
                                            R : Ghdl_Ptr;
                                            R_Len : Ghdl_Index_Type)
                                           return Ghdl_I32;

private
   pragma Export (C, Ghdl_Std_Ulogic_Match_Eq, "__ghdl_std_ulogic_match_eq");
   pragma Export (C, Ghdl_Std_Ulogic_Match_Ne, "__ghdl_std_ulogic_match_ne");
   pragma Export (C, Ghdl_Std_Ulogic_Match_Lt, "__ghdl_std_ulogic_match_lt");
   pragma Export (C, Ghdl_Std_Ulogic_Match_Le, "__ghdl_std_ulogic_match_le");
   pragma Export (C, Ghdl_Std_Ulogic_Match_Ge, "__ghdl_std_ulogic_match_ge");
   pragma Export (C, Ghdl_Std_Ulogic_Match_Gt, "__ghdl_std_ulogic_match_gt");

   pragma Export (C, Ghdl_Std_Ulogic_Array_Match_Eq,
                  "__ghdl_std_ulogic_array_match_eq");
   pragma Export (C, Ghdl_Std_Ulogic_Array_Match_Ne,
                  "__ghdl_std_ulogic_array_match_ne");
end Grt.Std_Logic_1164;
