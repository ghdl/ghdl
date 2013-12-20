
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
-- $Id: ch_15_regm-b.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavior of reg_multiple_out is

begin

  reg: process ( d, latch_en, out_en ) is

                                         variable latched_value : dlx_word;

  begin
    if To_bit(latch_en) = '1' then
      latched_value := To_X01(d);
    end if;
    for index in out_en'range loop
      if To_bit(out_en(index)) = '1' then
	q(index) <= latched_value after Tpd;
      else
	q(index) <= disabled_dlx_word after Tpd;
      end if;
    end loop;
  end process reg;

end architecture behavior;
