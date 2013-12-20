
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
-- $Id: ch_07_ch_07_02.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_07_02 is

end entity ch_07_02;


----------------------------------------------------------------


architecture test of ch_07_02 is

  constant val1 : integer := 1;

  procedure p ( signal s1, s2 : in bit;  val1 : in integer ) is
  begin
    null;
  end procedure p;

begin


  block_07_3_a : block is

                         signal s1, s2 : bit;

  begin

    -- code from book:

    call_proc : p ( s1, s2, val1 );

    -- end of code from book

  end block block_07_3_a;


  ----------------


  block_07_3_b : block is

                         signal s1, s2 : bit;

  begin

    -- code from book:

    call_proc : process is
    begin
      p ( s1, s2, val1 );
      wait on s1, s2;
    end process call_proc;

    -- end of code from book

  end block block_07_3_b;


end architecture test;
