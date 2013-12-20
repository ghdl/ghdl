
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
-- $Id: ch_13_fg_13_06.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package counter_types is

  -- code from book (in text)

  subtype digit is bit_vector(3 downto 0);

  -- end code from book

end package counter_types;



use work.counter_types.digit;

entity add_1 is
  port ( d : in digit;  y : out digit );
end entity add_1;


architecture boolean_eqn of add_1 is
begin

  y(0) <= not d(0) after 4 ns;

  y(1) <= (not d(1) and d(0))
          or (d(1) and not d(0)) after 4 ns;

  y(2) <= (not d(2) and d(1) and d(0))
	  or (d(2) and not (d(1) and d(0))) after 4 ns;

  y(3) <= (not d(3) and d(2) and d(1) and d(0))
	  or (d(3) and not (d(2) and d(1) and d(0))) after 4 ns;

end architecture boolean_eqn;


use work.counter_types.digit;

entity buf4 is
  port ( a : in digit;  y : out digit );
end entity buf4;


architecture basic of buf4 is
begin

  y(0) <= a(0) after 2 ns;
  y(1) <= a(1) after 2 ns;
  y(2) <= a(2) after 2 ns;
  y(3) <= a(3) after 2 ns;

end architecture basic;




-- code from book

use work.counter_types.digit;

entity counter is
  port ( clk, clr : in bit;
         q0, q1 : out digit );
end entity counter;

--------------------------------------------------

architecture registered of counter is

  component digit_register is
                             port ( clk, clr : in bit;
                                    d : in digit;
                                    q : out digit );
  end component digit_register;

  signal current_val0, current_val1, next_val0, next_val1 : digit;

begin

  val0_reg : component digit_register
    port map ( clk => clk, clr => clr, d => next_val0,
               q => current_val0 );

  val1_reg : component digit_register
    port map ( clk => clk, clr => clr, d => next_val1,
               q => current_val1 );

  -- other component instances
  -- . . .

  -- not in book

  incr0 : entity work.add_1(boolean_eqn)
    port map ( d => current_val0, y => next_val0 );

  incr1 : entity work.add_1(boolean_eqn)
    port map ( d => current_val1, y => next_val1 );

  buf0 : entity work.buf4(basic)
    port map ( a => current_val0, y => q0 );

  buf1 : entity work.buf4(basic)
    port map ( a => current_val1, y => q1 );

  -- end not in book

end architecture registered;

-- end code from book
