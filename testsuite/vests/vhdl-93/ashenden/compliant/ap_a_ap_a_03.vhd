
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
-- $Id: ap_a_ap_a_03.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ap_a_03 is

end entity ap_a_03;


library ieee;  use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of ap_a_03 is
begin

  b1 : block is
               -- code from book

               type unsigned is array ( natural range <> ) of std_logic;
             type signed is array ( natural range <> ) of std_logic;

             -- end code from book
  begin
  end block b1;


  b2 : block is
               -- code from book

               signal a: integer := 0;
             signal b: signed (4 downto 0 );

             -- end code from book
  begin
    a <= 0, 5 after 10 ns, -5 after 20 ns, 8 after 30 ns;
    -- code from book

    b <= To_signed ( a, b'length );

    -- end code from book

    process (b) is
    begin

      -- code from book

      if std_match ( b, "0-000" ) then
        -- . . .

        -- end code from book
        report "b matches";
      else
        report "b does not match";
      end if;    
    end process;

    

  end block b2;

end architecture test;

