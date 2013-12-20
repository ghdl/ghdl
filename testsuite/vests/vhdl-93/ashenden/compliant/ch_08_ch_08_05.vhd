
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
-- $Id: ch_08_ch_08_05.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- code from book:

library std, work;  use std.standard.all;

-- end of code from book


                    entity ch_08_05 is

                    end entity ch_08_05;


----------------------------------------------------------------


                    architecture test of ch_08_05 is
                    begin


                      process_08_4_a : process is

                                                 constant a : integer := 10;
                                               constant b : integer := 20;
                                               variable result : boolean;

                      begin

                        -- code from book:

                        result := std.standard."<" ( a, b );

                        -- end of code from book

                        wait;
                      end process process_08_4_a;


                    end architecture test;
