
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
-- $Id: ch_06_ovfl.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity overflow_logic is
                 port ( real_accumulator_ovf, imag_accumulator_ovf : in std_ulogic;
                        real_sum, imag_sum : std_ulogic_vector(21 downto 17);
                        ovf : out std_ulogic );
               end entity overflow_logic;
