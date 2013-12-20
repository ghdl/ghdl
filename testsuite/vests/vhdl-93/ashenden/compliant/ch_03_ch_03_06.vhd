
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
-- $Id: ch_03_ch_03_06.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity ch_03_06 is
end entity ch_03_06;

architecture test of ch_03_06 is

  -- code from book:
  
  type alu_func is (pass1, pass2, add, subtract);
  
  -- end of code from book
  
  signal func : alu_func := pass1;
  signal operand1 : integer := 10;
  signal operand2 : integer := 3;

begin

  process_03_2_a : process (func, operand1, operand2) is

                                                        variable result : integer := 0;

  begin

    -- code from book:

    case func is
      when pass1 =>
        result := operand1;
      when pass2 =>
        result := operand2;
      when add =>
        result := operand1 + operand2;
      when subtract =>
        result := operand1 - operand2;
    end case;

    -- end of code from book

  end process process_03_2_a;

  stimulus : process is
  begin
    func <= pass2 after 10 ns,
	    add after 20 ns,
	    subtract after 30 ns;
    wait;
  end process stimulus;


end architecture test;
