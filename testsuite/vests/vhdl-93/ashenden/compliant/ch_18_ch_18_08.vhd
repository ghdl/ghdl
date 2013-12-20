
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
-- $Id: ch_18_ch_18_08.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_18_08 is

end entity ch_18_08;


----------------------------------------------------------------


architecture test of ch_18_08 is
begin


  process is

            use std.textio.all;
          file f : text open read_mode is "ch_18_08.dat";
          variable L : line;
          variable ch : character;
          variable s : string(1 to 5);
          variable i : integer;
          variable r : real;

  begin

    readline(f, L);
    read(L, ch);
    report character'image(ch);
    read(L, ch);
    report character'image(ch);

    readline(f, L);
    read(L, s);
    report '"' & s & '"';
    read(L, s);
    report '"' & s & '"';

    readline(f, L);

    -- code from book:

    if L'length < s'length then
      read(L, s(1 to L'length));
    else
      read(L, s);
    end if;

    -- end of code from book

    report '"' & s & '"';

    readline(f, L);
    read(L, i);
    report integer'image(i);
    read(L, r);
    report real'image(r);

    wait;
  end process;


end architecture test;
