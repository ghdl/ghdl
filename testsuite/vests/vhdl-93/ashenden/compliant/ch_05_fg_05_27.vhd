
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
-- $Id: ch_05_fg_05_27.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

use work.counter_types.all;

-- end not in book


entity counter is
  port ( clk, clr : in bit;
         q0, q1 : out digit );
end entity counter;

--------------------------------------------------

architecture registered of counter is

  signal current_val0, current_val1, next_val0, next_val1 : digit;

begin

  val0_reg : entity work.reg4(struct)
    port map ( d0 => next_val0(0), d1 => next_val0(1),
               d2 => next_val0(2), d3 => next_val0(3),
               q0 => current_val0(0), q1 => current_val0(1),
               q2 => current_val0(2), q3 => current_val0(3),
               clk => clk, clr => clr );

  val1_reg : entity work.reg4(struct)
    port map ( d0 => next_val1(0), d1 => next_val1(1),
               d2 => next_val1(2), d3 => next_val1(3),
               q0 => current_val1(0), q1 => current_val1(1),
               q2 => current_val1(2), q3 => current_val1(3),
               clk => clk, clr => clr );

  incr0 : entity work.add_1(boolean_eqn) -- . . .;
    -- not in book
    port map ( d0 => current_val0(0), d1 => current_val0(1),
               d2 => current_val0(2), d3 => current_val0(3),
	       y0 => next_val0(0), y1 => next_val0(1),
               y2 => next_val0(2), y3 => next_val0(3) );
  -- end not in book

  incr1 : entity work.add_1(boolean_eqn) -- . . .;
    -- not in book
    port map ( d0 => current_val1(0), d1 => current_val1(1),
               d2 => current_val1(2), d3 => current_val1(3),
	       y0 => next_val1(0), y1 => next_val1(1),
               y2 => next_val1(2), y3 => next_val1(3) );
  -- end not in book

  buf0 : entity work.buf4(basic) -- . . .;
    -- not in book
    port map ( a0 => current_val0(0), a1 => current_val0(1),
               a2 => current_val0(2), a3 => current_val0(3),
	       y0 => q0(0), y1 => q0(1),
               y2 => q0(2), y3 => q0(3) );
  -- end not in book

  buf1 : entity work.buf4(basic) -- . . .;
    -- not in book
    port map ( a0 => current_val1(0), a1 => current_val1(1),
               a2 => current_val1(2), a3 => current_val1(3),
	       y0 => q1(0), y1 => q1(1),
               y2 => q1(2), y3 => q1(3) );
  -- end not in book

end architecture registered;
