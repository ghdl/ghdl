
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
-- $Id: ch_07_fg_07_15.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_07_15 is
end entity fg_07_15;



library ieee;  use ieee.std_logic_1164.all;

architecture test of fg_07_15 is

  -- code from book

  procedure generate_clock ( signal clk : out std_ulogic;
                             constant Tperiod, Tpulse, Tphase : in time ) is
  begin
    wait for Tphase;
    loop
      clk <= '1', '0' after Tpulse;
      wait for Tperiod;
    end loop;
  end procedure generate_clock;

  -- end code from book

  -- code from book (in text)

  signal phi1, phi2 : std_ulogic := '0';
  -- . . .

  -- end code from book

begin

  -- code from book (in text)

  gen_phi1 : generate_clock ( phi1, Tperiod => 50 ns, Tpulse => 20 ns,
                              Tphase => 0 ns );

  gen_phi2 : generate_clock ( phi2, Tperiod => 50 ns, Tpulse => 20 ns,
                              Tphase => 25 ns );

  -- end code from book

end architecture test;
