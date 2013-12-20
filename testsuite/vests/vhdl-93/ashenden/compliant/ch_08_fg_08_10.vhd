
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
-- $Id: ch_08_fg_08_10.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity fg_08_10 is
end entity fg_08_10;



architecture test of fg_08_10 is

  -- code from book

  function "<" ( a, b : bit_vector ) return boolean is
    variable tmp1 : bit_vector(a'range) := a;
    variable tmp2 : bit_vector(b'range) := b;
  begin
    tmp1(tmp1'left) := not tmp1(tmp1'left);
    tmp2(tmp2'left) := not tmp2(tmp2'left);
    return std.standard."<" ( tmp1, tmp2 );
  end function "<";

  -- end code from book

  signal a, b : bit_vector(7 downto 0);
  signal result : boolean;

begin

  dut : result <= a < b;

  stimulus : process is
  begin
    wait for 10 ns;
    a <= X"02";  b <= X"04";  wait for 10 ns;
    a <= X"02";  b <= X"02";  wait for 10 ns;
    a <= X"02";  b <= X"01";  wait for 10 ns;
    a <= X"02";  b <= X"FE";  wait for 10 ns;
    a <= X"FE";  b <= X"02";  wait for 10 ns;
    a <= X"FE";  b <= X"FE";  wait for 10 ns;
    a <= X"FE";  b <= X"FC";  wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;
