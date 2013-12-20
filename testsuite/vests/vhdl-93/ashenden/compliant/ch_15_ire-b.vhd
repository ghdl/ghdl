
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
-- $Id: ch_15_ire-b.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

use work.dlx_instr.all;

architecture behavior of ir_extender is

  subtype upper_6_bits is std_logic_vector(0 to 5);
  subtype upper_16_bits is std_logic_vector(0 to 15);

begin

  extender : process ( d, immed_en, immed_size_26, immed_unsigned ) is
  begin
    if To_bit(immed_en) = '1' then
      if To_bit(immed_size_26) = '1' then  -- 26-bit immediate
      	if To_bit(immed_unsigned) = '1' then
	  q <= upper_6_bits'(others => '0') & d(6 to 31) after Tpd;
	else
	  q <= upper_6_bits'(others => d(6)) & d(6 to 31) after Tpd;
	end if;
      else -- 16-bit immediate
      	if To_bit(immed_unsigned) = '1' then
	  q <= upper_16_bits'(others => '0') & d(16 to 31) after Tpd;
	else
	  q <= upper_16_bits'(others => d(16)) & d(16 to 31) after Tpd;
	end if;
      end if;
    else
      q <= disabled_dlx_word after Tpd;
    end if;
  end process extender;

end architecture behavior;
