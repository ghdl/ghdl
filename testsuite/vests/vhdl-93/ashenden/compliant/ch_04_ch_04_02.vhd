
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
-- $Id: ch_04_ch_04_02.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_04_02 is

end entity ch_04_02;


----------------------------------------------------------------


architecture test of ch_04_02 is
begin


  process_04_1_b : process is

                             -- code from book:

                             type symbol is ('a', 't', 'd', 'h', digit, cr, error);
                           type state is range 0 to 6;

                           type transition_matrix is array (state, symbol) of state;

                           variable transition_table : transition_matrix;

                           -- end of code from book

                           variable next_state : state;

                           -- code from book:

                           type point is array (1 to 3) of real;
                           type matrix is array (1 to 3, 1 to 3) of real;

                           variable p, q : point;
                           variable transform : matrix;

                           -- end of code from book

  begin

    next_state := 
      -- code from book:

      transition_table(5, 'd');


    -- end of code from book

    for i in 1 to 3 loop
      for j in 1 to 3 loop
        if i = j then
          transform(i, j) := -1.0;
        else
          transform(i, j) := 0.0;
        end if;
      end loop;
    end loop;
    p := (1.0, 2.0, 3.0);

    -- code from book:

    for i in 1 to 3 loop
      q(i) := 0.0;
      for j in 1 to 3 loop
        q(i) := q(i) + transform(i, j) * p(j);
      end loop;
    end loop;
    -- end of code from book

    wait;
  end process process_04_1_b;


end architecture test;
