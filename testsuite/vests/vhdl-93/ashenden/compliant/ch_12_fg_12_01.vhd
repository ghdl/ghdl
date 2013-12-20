
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
-- $Id: ch_12_fg_12_01.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

-- code from book

entity control_unit is

  generic ( Tpd_clk_out, Tpw_clk : delay_length;
            debug : boolean := false );

  port ( clk : in bit;
         ready : in bit;
         control1, control2 : out bit );

end entity control_unit;

-- end code from book



architecture test of control_unit is
begin
end architecture test;




entity fg_12_01 is
end entity fg_12_01;



architecture test of fg_12_01 is

  signal clk, ready : bit;

begin

  dut1 : entity work.control_unit
    -- code from book (in text)
    generic map ( 200 ps, 1500 ps, false )
    -- end code from book
    port map ( clk, ready, open, open );

  dut2 : entity work.control_unit
    -- code from book (in text)
    generic map ( Tpd_clk_out => 200 ps, Tpw_clk => 1500 ps )
    -- end code from book
    port map ( clk, ready, open, open );

  dut3 : entity work.control_unit
    -- code from book (in text)
    generic map ( 200 ps, 1500 ps, debug => open )
    -- end code from book
    port map ( clk, ready, open, open );

end architecture test;
