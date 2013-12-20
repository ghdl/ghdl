
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
-- $Id: ch_03_fg_03_09.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity edge_triggered_register is
  port ( clock : in bit; 
         d_in : in real;  d_out : out real );
end entity edge_triggered_register;

architecture check_timing of edge_triggered_register is
begin

  store_and_check : process (clock) is
                                      variable stored_value : real;
                                    variable pulse_start : time;
  begin
    case clock is
      when '1' =>
        pulse_start := now;
        stored_value := d_in;
        d_out <= stored_value;
      when '0' =>
        assert now = 0 ns or (now - pulse_start) >= 5 ns
          report "clock pulse too short"; 
    end case;
  end process store_and_check;

end architecture check_timing;
