
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
-- $Id: tc926.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s03b00x00p23n01i00926ent IS
END c10s03b00x00p23n01i00926ent;

ARCHITECTURE c10s03b00x00p23n01i00926arch OF c10s03b00x00p23n01i00926ent IS
  type std_logic is ( 'X', '0', '1', 'W', 'L', 'H', 'Z' );
  type std_logic_vector is array ( natural range <> ) of std_logic;

  function "+" ( l,r : bit_vector ) return bit_vector is
    variable lr : bit_vector ( 1 to l'length ) := l;
    variable rr : bit_vector ( 1 to r'length ) := r;
    variable result : bit_vector ( 1 to l'length );
    variable carry : bit := '0';
  begin
    for i in l'length downto 1 loop
      result(i) := lr(i) xor rr(i) xor carry;
      carry := (lr(i) and rr(i)) or
               (rr(i) and carry) or
               (lr(i) and carry);
    end loop;
    return (result);
  end;

  -- homograph
  function "+" ( l,r : std_logic_vector ) return std_logic_vector is
  begin
  end; 

  signal a : bit_vector ( 15 downto 0 ) := B"0010001010100010";
  signal b : bit_vector ( 15 downto 0 ) := B"0101111101011101";
  signal s : bit_vector ( 15 downto 0 );
BEGIN
  TESTING: PROCESS
  BEGIN
    s <= (a + b) after 10 ns;
    wait for 11 ns;
    assert NOT( s = B"1000000111111111" )
      report "***PASSED TEST: c10s03b00x00p23n01i00926"
      severity NOTE;
    assert ( s = B"1000000111111111" )
      report "***FAILED TEST: c10s03b00x00p23n01i00926 - If one of the two declarations is the implicit declaration of a predefined operation, the predefined operation is laways hidden by teh other homograph."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s03b00x00p23n01i00926arch;
