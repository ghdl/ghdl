
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

entity decoder_3_to_8 is
  generic ( Tpd_01, Tpd_10 : delay_length );
  port ( s0, s1, s2 : in bit;
         enable : in bit;
         y0, y1, y2, y3, y4, y5, y6, y7 : out bit );
end entity decoder_3_to_8;


-- not in book

architecture basic of decoder_3_to_8 is
begin

  process (enable, s2, s1, s0) is
  begin
    if enable = '0' then
      (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("00000000");
    else
      case bit_vector'(s2, s1, s0) is
        when "000" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("00000001");
        when "001" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("00000010");
        when "010" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("00000100");
        when "011" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("00001000");
        when "100" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("00010000");
        when "101" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("00100000");
        when "110" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("01000000");
        when "111" => (y7, y6, y5, y4, y3, y2, y1, y0) <= bit_vector'("10000000");
      end case;
    end if;
  end process;

end architecture basic;

-- end not in book
