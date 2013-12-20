
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
-- $Id: ch_06_tovec-b.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavioral of to_vector is

begin

  behavior : process (r) is

                           variable temp : integer range -2**15 to 2**15 - 1;
                         variable negative : boolean;
                         variable result : std_ulogic_vector(vec'range);

  begin
    -- scale to [-2**15, +2**15) and convert to integer
    if r * real(2**15) < real(-2**15) then
      temp := -2**15;
    elsif r * real(2**15) >= real(2**15 - 1) then
      temp := 2**15 - 1;
    else
      temp := integer(r * real(2**15));
    end if;
    negative := temp < 0;
    if negative then
      temp := -(temp + 1);
    end if;
    result := (others => '0');
    for index in result'reverse_range loop
      result(index) := to_X01(bit'val(temp rem 2));
      temp := temp / 2;
      exit when temp = 0;
    end loop;
    if negative then
      result := not result;
      result(result'left) := '1';
    end if;
    vec <= result;
  end process behavior;

end architecture behavioral;
