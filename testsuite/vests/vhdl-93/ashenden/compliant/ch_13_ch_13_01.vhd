
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
-- $Id: ch_13_ch_13_01.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_13_01 is
end entity ch_13_01;


architecture test of ch_13_01 is

  -- code from book

  component nand3 is
                    port ( a, b, c : in bit := '1';  y : out bit );
  end component nand3;

  -- end code from book

  signal s1, s2, s3 : bit;

begin

  -- code from book

  gate1 : component nand3
    port map ( a => s1, b => s2, c => open, y => s3 );

  -- end code from book

end architecture test;



-- code from book

entity nand2 is
  port ( a, b : in bit := '1';  y : out bit );
end entity nand2;

-- end code from book


configuration ch_13_01_test of ch_13_01 is

  for test

    -- code from book

    for gate1 : nand3
      use entity work.nand2(basic);
    end for;

    -- end code from book

  end for;

end configuration ch_13_01_test;
