
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
-- $Id: ch_18_ch_18_09.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_18_09 is

end entity ch_18_09;


----------------------------------------------------------------


architecture test of ch_18_09 is
begin


  process is

            use std.textio.all;
          variable L : line;

  begin

    write(L, 42, justified => left, field => 5);
    writeline(output, L);
    write(L, 42, justified => right, field => 5);
    writeline(output, L);
    write(L, 123, field => 2);
    writeline(output, L);

    -- code from book:

    write ( L, string'( "fred" ) );
    write ( L, ' ' );
    write ( L, bit_vector'( X"3A" ) );

    -- end of code from book

    writeline(output, L);

    write(L, 3.14159, digits => 2);
    writeline(output, L);
    write(L, 123.4567, digits => 0);
    writeline(output, L);

    write(L, 40 ns, unit => ps);
    writeline(output, L);
    write(L, 23 us, unit => ms);
    writeline(output, L);

    wait;
  end process;


end architecture test;
