--  std_logic_1164
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

package Synth.Ieee.Std_Logic_1164 is
   --  From openieee.

   --  Unresolved logic state.
   type Std_Ulogic is
     (
      'U',  --  Uninitialized, this is also the default value.
      'X',  --  Unknown / conflict value (forcing level).
      '0',  --  0 (forcing level).
      '1',  --  1 (forcing level).
      'Z',  --  High impedance.
      'W',  --  Unknown / conflict (weak level).
      'L',  --  0 (weak level).
      'H',  --  1 (weak level).
      '-'   --  Don't care.
     );

   --  Vector of logic state.
   type Std_Logic_Vector is array (Natural range <>) of Std_Ulogic;

   -- type Table_1d is array (Std_Ulogic) of Std_Ulogic;
   type Table_2d is array (Std_Ulogic, Std_Ulogic) of Std_Ulogic;

   And_Table : constant Table_2d :=
     --  UX01ZWLH-
     ("UU0UUU0UU",   -- U
      "UX0XXX0XX",   -- X
      "000000000",   -- 0
      "UX01XX01X",   -- 1
      "UX0XXX0XX",   -- Z
      "UX0XXX0XX",   -- W
      "000000000",   -- L
      "UX01XX01X",   -- H
      "UX0XXX0XX"    -- -
     );

end Synth.Ieee.Std_Logic_1164;
