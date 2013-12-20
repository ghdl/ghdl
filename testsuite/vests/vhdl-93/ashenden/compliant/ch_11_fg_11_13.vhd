
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
-- $Id: ch_11_fg_11_13.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity fg_11_13 is
               end entity fg_11_13;



               architecture test of fg_11_13 is

                 use work.fg_11_12.all;

                 signal barrier : std_logic;

               begin

                 pullup : barrier <= 'H';

                 -- code from book

                 synchronized_module : process is
                                                 -- . . .
                 begin
                   init_synchronize(barrier);
                   -- . . .
                   loop
                     -- . . .
                     begin_synchronize(barrier);
                     -- . . .    -- perform operation, synchronized with other processes
                     end_synchronize(barrier);
                     -- . . .
                   end loop;
                 end process synchronized_module;

                 -- end code from book

                 another_synchronized_module : process is
                 begin
                   init_synchronize(barrier);
                   loop
                     wait for 10 ns;
                     begin_synchronize(barrier);
                     -- . . .    -- perform operation, synchronized with other processes
                     end_synchronize(barrier);
                   end loop;
                 end process another_synchronized_module;

               end architecture test;
