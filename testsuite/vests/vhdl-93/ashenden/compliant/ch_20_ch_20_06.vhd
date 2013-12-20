
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
-- $Id: ch_20_ch_20_06.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_20_06 is

end entity ch_20_06;


----------------------------------------------------------------

use std.textio.all;

architecture test of ch_20_06 is

  subtype encoding_type is bit_vector(1 downto 0);
  attribute encoding : encoding_type;

begin


  process1 : process is

                       -- code from book:

                       type controller_state is (idle, active, fail_safe);
                     type load_level is (idle, busy, overloaded);

                     attribute encoding of idle [ return controller_state ] : literal is b"00";
                                                                                         attribute encoding of active [ return controller_state ] : literal is b"01";
                                                                                                                                                               attribute encoding of fail_safe [ return controller_state ] : literal is b"10";

                                                                                                                                                                                                                                        -- end of code from book

                                                                                                                                                                                                                                        variable L : line;

                                                                                                                                                               begin
                                                                                                                                                                 write(L, string'("process1"));
                                                                                                                                                                 writeline(output, L);
                                                                                                                                                                 write(L, idle [ return controller_state ] ' encoding);
                                                                                                                                                                 writeline(output, L);
                                                                                                                                                                 write(L, active [ return controller_state ] ' encoding);
                                                                                                                                                                 writeline(output, L);
                                                                                                                                                                 write(L, fail_safe [ return controller_state ] ' encoding);
                                                                                                                                                                 writeline(output, L);
                                                                                                                                                                 wait;
                                                                                                                                                               end process process1;


                                                                                                                                                               process2 : process is

                                                                                                                                                                                    type controller_state is (idle, active, fail_safe);
                                                                                                                                                                                  type load_level is (idle, busy, overloaded);

                                                                                                                                                                                  attribute encoding of idle : literal is b"11";

                                                                                                                                                                                  variable L : line;

                                                                                                                                                               begin
                                                                                                                                                                 write(L, string'("process2"));
                                                                                                                                                                 writeline(output, L);
                                                                                                                                                                 write(L, idle [ return controller_state ] ' encoding);
                                                                                                                                                                 writeline(output, L);
                                                                                                                                                                 write(L, idle [ return load_level ] ' encoding);
                                                                                                                                                                 writeline(output, L);
                                                                                                                                                                 wait;
                                                                                                                                                               end process process2;


                                                                                         end architecture test;
