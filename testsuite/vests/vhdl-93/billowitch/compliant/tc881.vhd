
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
-- $Id: tc881.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s01b00x00p06n01i00881PKG is
-- VAL1 is here a constant
  constant VAL1 : INTEGER := 65;
  type DBLINTREC is
    record
-- VAL1 is here a record element
      VAL1  : INTEGER;
      VAL2  : INTEGER;
    end record;
end c10s01b00x00p06n01i00881PKG;


use WORK.c10s01b00x00p06n01i00881PKG.DBLINTREC;
entity c10s01b00x00p06n01i00881ent_a is
  port (
    PS1: in  DBLINTREC;
    PS2: out DBLINTREC
    );
end c10s01b00x00p06n01i00881ent_a;

architecture c10s01b00x00p06n01i00881arch_a of c10s01b00x00p06n01i00881ent_a is
begin
  process
  begin
    PS2.VAL1 <= PS1.VAL1 + 1;
    PS2.VAL2 <= PS1.VAL2 + 2;
    wait;
  end process;
end c10s01b00x00p06n01i00881arch_a;

use WORK.c10s01b00x00p06n01i00881PKG.DBLINTREC;
use WORK.c10s01b00x00p06n01i00881ent_a;
ENTITY c10s01b00x00p06n01i00881ent IS
END c10s01b00x00p06n01i00881ent;

ARCHITECTURE c10s01b00x00p06n01i00881arch OF c10s01b00x00p06n01i00881ent IS

  component c10s01b00x00p06n01i00881ent_a
    port ( PS1: in DBLINTREC; PS2: out DBLINTREC );
  end component;
  for A1: c10s01b00x00p06n01i00881ent_a
    use entity work.c10s01b00x00p06n01i00881ent_a ( c10s01b00x00p06n01i00881arch_a );
  signal S1: DBLINTREC := (3, 9);
  signal S2: DBLINTREC := (0, 0);

BEGIN
  
  A1: c10s01b00x00p06n01i00881ent_a port map ( S1, S2 );

  TESTING: PROCESS
  BEGIN

    wait for 1 ns; -- let a time increment go by so init done
    assert ( S2.VAL1 = 4 )
      report "didn't add to record element S2.VAL1 correctly"
      severity FAILURE;
    assert ( S2.VAL2 = 11 )
      report "didn't add to record element S2.VAL2 correctly"
      severity FAILURE;

    assert NOT( S2.VAL1 = 4 and S2.VAL2 =11 )
      report "***PASSED TEST: c10s01b00x00p06n01i00881"
      severity NOTE;
    assert ( S2.VAL1 = 4 and S2.VAL2 =11 )
      report "***FAILED TEST: c10s01b00x00p06n01i00881 - A declaratione region is formed by a record type declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p06n01i00881arch;
