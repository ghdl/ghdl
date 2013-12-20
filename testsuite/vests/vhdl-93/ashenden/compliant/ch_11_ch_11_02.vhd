
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: ch_11_ch_11_02.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

package ch_11_02 is

  -- code from book

  type std_ulogic is ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-');

  type std_ulogic_vector is array ( natural range <> ) of std_ulogic;

  function resolved ( s : std_ulogic_vector ) return std_ulogic;

  subtype std_logic is resolved std_ulogic;

  type std_logic_vector is array ( natural range <>) of std_logic;

  subtype X01 is resolved std_ulogic range 'X' to '1'; -- ('X','0','1')
  subtype X01Z is resolved std_ulogic range 'X' to 'Z'; -- ('X','0','1','Z')
  subtype UX01 is resolved std_ulogic range 'U' to '1'; -- ('U','X','0','1')
  subtype UX01Z is resolved std_ulogic range 'U' to 'Z'; -- ('U','X','0','1','Z')

  -- end code from book

end package ch_11_02;



package body ch_11_02 is

  function resolved ( s : std_ulogic_vector ) return std_ulogic is
  begin
  end function resolved;

end package body ch_11_02;
