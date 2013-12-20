
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
-- $Id: ap_a_fg_a_11.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               entity RAM16x1 is
                 port ( \a<0>\, \a<1>\, \a<2>\, \a<3>\ : in std_ulogic;
                        \d\, \we\ : in std_ulogic;
                        \o\ : out std_ulogic );
               end entity RAM16x1;


               architecture a of RAM16x1 is
               begin
               end architecture a;



               entity fg_a_11 is
               end entity fg_a_11;


               library ieee;  use ieee.std_logic_1164.all;

               architecture test of fg_a_11 is

                 -- code from book

                 component RAM16x1 is
                                     port ( \a<0>\, \a<1>\, \a<2>\, \a<3>\ : in std_ulogic;
                                            \d\, \we\ : in std_ulogic;
                                            \o\ : out std_ulogic );
                 end component RAM16x1;
                 -- . . .

                 -- end code from book

                 signal address : std_ulogic_vector(3 downto 0);
                 signal raminp, ramout : std_ulogic_vector(15 downto 0);
                 signal write_enable : std_ulogic;

               begin

                 -- code from book

                 g1 : for i in 0 to 15 generate
                   rama : component RAM16x1
                     port map ( \a<0>\ => address(0),
                                \a<1>\ => address(1),
                                \a<2>\ => address(2),
                                \a<3>\ => address(3),
                                \d\ => raminp ( i ),
                                \we\ => write_enable,
                                \o\ => ramout ( i ) );
                 end generate g1;

                 -- end code from book

               end architecture test;
