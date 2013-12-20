
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
-- $Id: ch_13_fg_13_15.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

configuration computer_structure of computer_system is

  for structure

    for interface_decoder : decoder_2_to_4
      use entity work.decoder_3_to_8(basic)
        generic map ( Tpd_01 => prop_delay, Tpd_10 => prop_delay )
        port map ( s0 => in0, s1 => in1, s2 => '0',
                   enable => '1',
                   y0 => out0, y1 => out1, y2 => out2, y3 => out3,
                   y4 => open, y5 => open, y6 => open, y7 => open );
    end for;

    -- . . .

  end for;

end configuration computer_structure;
