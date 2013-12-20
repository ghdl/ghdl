
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
-- $Id: ch_03_fg_03_08.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity max3 is
  port ( a, b, c : in integer;  z : out integer );
end entity max3;

architecture check_error of max3 is
begin

  maximizer : process (a, b, c)
    variable result : integer;
  begin
    if a > b then
      if a > c then
        result := a;
      else
        result := a;  -- Oops!  Should be: result := c;
      end if;
    elsif  b > c then
      result := b;
    else
      result := c;
    end if;
    assert result >= a and result >= b and result >= c
      report "inconsistent result for maximum"
      severity failure;
    z <= result;
  end process maximizer;

end architecture check_error;
