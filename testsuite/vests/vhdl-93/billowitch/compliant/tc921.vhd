
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
-- $Id: tc921.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c10s03b00x00p16n01i00921ent_a is
  generic ( x : integer; y : real );
end c10s03b00x00p16n01i00921ent_a;

architecture c10s03b00x00p16n01i00921arch_a of c10s03b00x00p16n01i00921ent_a is
begin
  TESTING : PROCESS
  BEGIN
    assert NOT( real(x)=y )
      report "***PASSED TEST: c10s03b00x00p16n01i00921"
      severity NOTE;
    assert ( real(x)=y )
      report "***FAILED TEST: c10s03b00x00p16n01i00921 - Named associated formal generic can be made visible by selection."
      severity ERROR;
    wait;
  END PROCESS TESTING;
end c10s03b00x00p16n01i00921arch_a;

ENTITY c10s03b00x00p16n01i00921ent IS
END c10s03b00x00p16n01i00921ent;

ARCHITECTURE c10s03b00x00p16n01i00921arch OF c10s03b00x00p16n01i00921ent IS
  component d
  end component;
BEGIN
  instance : d;

END c10s03b00x00p16n01i00921arch;

configuration c10s03b00x00p16n01i00921cfg of c10s03b00x00p16n01i00921ent is
  for c10s03b00x00p16n01i00921arch
    for instance : d
      use entity work.c10s03b00x00p16n01i00921ent_a(c10s03b00x00p16n01i00921arch_a) generic map ( x => 10, y => 10.0 );
    end for;
  end for;
end c10s03b00x00p16n01i00921cfg;
