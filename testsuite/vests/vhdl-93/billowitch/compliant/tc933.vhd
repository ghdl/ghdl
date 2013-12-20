
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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
-- $Id: tc933.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s04b00x00p04n01i00933pkg is
  type    color is ( red, green, blue, yellow, orange, purple );
  function    "+"    (l,r : bit_vector) return bit_vector;
end c10s04b00x00p04n01i00933pkg;

package body c10s04b00x00p04n01i00933pkg is
  function "+" (l,r : bit_vector) return bit_vector is
  begin
    return (B"1111");
  end;
end c10s04b00x00p04n01i00933pkg;


entity c10s04b00x00p04n01i00933ent_a is
  generic ( x : bit_vector(3 downto 0));
end c10s04b00x00p04n01i00933ent_a;

architecture c10s04b00x00p04n01i00933arch_a of c10s04b00x00p04n01i00933ent_a is
begin
  TESTING: PROCESS
  BEGIN
    assert NOT( x=B"1111" )
      report "***PASSED TEST: c10s04b00x00p04n01i00933"
      severity NOTE;
    assert ( x=B"1111" )
      report "***FAILED TEST: c10s04b00x00p04n01i00933 - Items declared via a use clause are visible in the declarative region."
      severity ERROR;
    wait;
  END PROCESS TESTING;
end c10s04b00x00p04n01i00933arch_a;


use work.c10s04b00x00p04n01i00933pkg.all;
ENTITY c10s04b00x00p04n01i00933ent IS
END c10s04b00x00p04n01i00933ent;

ARCHITECTURE c10s04b00x00p04n01i00933arch OF c10s04b00x00p04n01i00933ent IS
  constant c : bit_vector(3 downto 0) := B"1011";
  component d
  end component;
BEGIN
  instance : d;

END c10s04b00x00p04n01i00933arch;

configuration c10s04b00x00p04n01i00933cfg of c10s04b00x00p04n01i00933ent is
  for c10s04b00x00p04n01i00933arch
    -- declarations are visible here !
    for instance : d
      use entity work.c10s04b00x00p04n01i00933ent_a(c10s04b00x00p04n01i00933arch_a) generic map ( B"0100" + c );
    end for;
  end for;
end c10s04b00x00p04n01i00933cfg;
