
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
-- $Id: ch_07_fg_07_10.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

-- not in book

entity signal_generator is
  generic ( period : delay_length := 20 ns;
            pulse_count : natural := 5 );
end entity signal_generator;

-- end not in book


library ieee;  use ieee.std_logic_1164.all;

architecture top_level of signal_generator is

  signal raw_signal : std_ulogic;
  -- . . .

  procedure generate_pulse_train ( width, separation : in delay_length;
                                   number : in natural;
                                   signal s : out std_ulogic ) is
  begin
    for count in 1 to number loop
      s <= '1', '0' after width;
      wait for width + separation;
    end loop;
  end procedure generate_pulse_train;

begin

  raw_signal_generator : process is
  begin
    -- . . .
    generate_pulse_train ( width => period / 2,
                           separation => period - period / 2,
                           number => pulse_count,
                           s => raw_signal );
    -- . . .
    -- not in book
    wait;
    -- end not in book
  end process raw_signal_generator;

  -- . . .

end architecture top_level;
