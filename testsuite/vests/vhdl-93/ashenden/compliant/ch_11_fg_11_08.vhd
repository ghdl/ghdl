
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
-- $Id: ch_11_fg_11_08.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package fg_11_08 is

  type std_ulogic is ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-');
  type std_ulogic_vector is array ( natural range <> ) of std_ulogic;
  function resolved ( s : std_ulogic_vector ) return std_ulogic;

end package fg_11_08;


package body fg_11_08 is

  -- code from book

  type stdlogic_table is array (std_ulogic, std_ulogic) of std_ulogic;
  constant resolution_table : stdlogic_table :=
    -- ---------------------------------------------
    --  'U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-'
    -- ---------------------------------------------
    ( ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U' ),  --  'U'
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  --  'X'
      ( 'U', 'X', '0', 'X', '0', '0', '0', '0', 'X' ),  --  '0'
      ( 'U', 'X', 'X', '1', '1', '1', '1', '1', 'X' ),  --  '1'
      ( 'U', 'X', '0', '1', 'Z', 'W', 'L', 'H', 'X' ),  --  'Z'
      ( 'U', 'X', '0', '1', 'W', 'W', 'W', 'W', 'X' ),  --  'W'
      ( 'U', 'X', '0', '1', 'L', 'W', 'L', 'W', 'X' ),  --  'L'
      ( 'U', 'X', '0', '1', 'H', 'W', 'W', 'H', 'X' ),  --  'H'
      ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' )   --  '-'
      );

  function resolved ( s : std_ulogic_vector ) return std_ulogic is
    variable result : std_ulogic := 'Z';  -- weakest state default
  begin
    if s'length = 1 then
      return s(s'low);
    else
      for i in s'range loop
        result := resolution_table(result, s(i));
      end loop;
    end if;
    return result;
  end function resolved;

  -- end code from book

end package body fg_11_08;
