--  std_logic_1164
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

with Types; use Types;
with Synth.Objtypes; use Synth.Objtypes;

package Synth.Ieee.Std_Logic_1164 is

   --  For std.standard.  Should a package be created ?
   type Bit is ('0', '1');

   function Read_Bit (M : Memory_Ptr; Off : Uns32) return Bit;
   procedure Write_Bit (M : Memory_Ptr; Off : Uns32; Val : Bit);

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

   subtype X01 is Std_Ulogic range 'X' .. '1';

   function Read_Std_Logic (M : Memory_Ptr; Off : Uns32) return Std_Ulogic;
   procedure Write_Std_Logic (M : Memory_Ptr; Off : Uns32; Val : Std_Ulogic);

   --  Read as standard.bit and convert to std_logic.
   function Read_Bit_To_Std_Logic (M : Memory_Ptr; Off : Uns32)
                                   return Std_Ulogic;

   function To_Bit (S : Std_Ulogic; Xmap : Bit) return Bit;

   type Table_1d is array (Std_Ulogic) of Std_Ulogic;
   type Table_2d is array (Std_Ulogic, Std_Ulogic) of Std_Ulogic;

   type Table_1d_X01 is array (Std_Ulogic) of X01;

   To_X01 : constant Table_1d_X01 := "XX01XX01X";

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

   Or_Table : constant Table_2d :=
   --  UX01ZWLH-
     ("UUU1UUU1U",   -- U
      "UXX1XXX1X",   -- X
      "UX01XX01X",   -- 0
      "111111111",   -- 1
      "UXX1XXX1X",   -- Z
      "UXX1XXX1X",   -- W
      "UX01XX01X",   -- L
      "111111111",   -- H
      "UXX1XXX1X"    -- -
     );

   Xor_Table : constant Table_2d :=
   --  UX01ZWLH-
     ("UUUUUUUUU",   -- U
      "UXXXXXXXX",   -- X
      "UX01XX01X",   -- 0
      "UX10XX10X",   -- 1
      "UXXXXXXXX",   -- Z
      "UXXXXXXXX",   -- W
      "UX01XX01X",   -- L
      "UX10XX10X",   -- H
      "UXXXXXXXX"    -- -
     );

   Not_Table : constant Table_1d :=
   --  UX01ZWLH-
      "UX10XX10X";

end Synth.Ieee.Std_Logic_1164;
