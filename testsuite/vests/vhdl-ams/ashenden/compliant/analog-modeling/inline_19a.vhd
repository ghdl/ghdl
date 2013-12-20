
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;
                        
entity inline_19a is

end entity inline_19a;


architecture test of inline_19a is

  signal reset, trigger_n : std_ulogic;
  terminal rc_ext : electrical;
  quantity v_rc_ext across rc_ext;
  constant half_vdd : voltage := 2.5;
  
begin

  block_1 : block is

    signal q, q_n : std_ulogic;

  begin

    process is
    begin

      -- code from book

      -- ...
      if reset = '1' or reset = 'H' or v_rc_ext > half_vdd then
        q <= '0'; q_n <= '1';
        break;
      elsif trigger_n = '0' or trigger_n = 'L' then
        q <= '1'; q_n <= '0';
        break;
      end if;
      -- ...

      -- end code from book

      wait;
    end process;

  end block block_1;


  block_2 : block is

    signal q, q_n : std_ulogic;

  begin

    process is
    begin

      -- code from book

      q_n <= '1' after 20 ns;
      break;
 
      -- end code from book

      wait;
    end process;

  end block block_2;


  block_3 : block is

    signal q, q_n : std_ulogic;

  begin

    process is
    begin

      -- code from book

      q_n <= '1';
      break;

      -- end code from book

      wait;
    end process;

  end block block_3;


end architecture test;
