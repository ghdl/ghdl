
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
-- $Id: tc118.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

Package c04s03b02x00p29n10i00118pkg is
  type Apollo_string is array (INTEGER range 1 to 8) of CHARACTER;
  type V_REGISTER    is array (INTEGER range 0 to 7) of BIT;
end c04s03b02x00p29n10i00118pkg;

use work.c04s03b02x00p29n10i00118pkg.all;
ENTITY c04s03b02x00p29n10i00118ent IS
  port (
    p23 : buffer Boolean        := FALSE;
    p24 : buffer Bit            := '0'  ;
    p25 : buffer Character      := NUL  ;
    p26 : buffer SEVERITY_LEVEL := NOTE ;
    p27 : buffer Integer        := -1   ;
    p28 : buffer Real           := -1.0 ;
    p29 : buffer TIME           := 1 ns ;
    p30 : buffer Natural        := 0    ;
    p31 : buffer Positive       := 1    ;
    p32 : buffer Apollo_string  :=  "abcdefgh";
    p33 : buffer V_register     := B"10010110"
    );
END c04s03b02x00p29n10i00118ent;

ARCHITECTURE c04s03b02x00p29n10i00118arch OF c04s03b02x00p29n10i00118ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    p23  <= not p23 AFTER 10 ns;
    p24  <= not p24 AFTER 10 ns;

    if (p25 /= character'high) then
      p25  <= character'succ (p25) AFTER 10 ns;
    else
      p25  <= character'low  AFTER 10 ns;
    end if;
    
    if (p26 /= severity_level'high) then
      p26  <= severity_level'succ(p26) AFTER 10 ns;
    else
      p26  <= severity_level'low  AFTER 10 ns;
    end if;
    
    p27  <= integer'succ(p27) AFTER 10 ns; -- unlikely to overflow....
    p28  <= p28 + 1.0 AFTER 10 ns;
    p29  <= p29 * 2 AFTER 10 ns;
    p30  <= p30 + 1 AFTER 10 ns;
    p31  <= p31 * 2 AFTER 10 ns;
    p32(2) <= character'succ (p32(2)) AFTER 10 ns;
    p33(1) <= not p33(1) AFTER 10 ns;

    WAIT ON p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33;

    assert NOT(    p23 = TRUE   and
                   p24 = '1'     and 
                   p25 = SOH     and 
                   p26 = WARNING    and 
                   p27 = 0      and 
                   p28 = 0.0    and 
                   p29 = 2 ns    and 
                   p30 = 1       and 
                   p31 = 2       and 
                   p32 = "accdefgh"and
                   p33 = B"11010110"   )
      report "***PASSED TEST: c04s03b02x00p29n10i00118"      severity NOTE;
    assert (    p23 = TRUE   and
                p24 = '1'     and 
                p25 = SOH     and 
                p26 = WARNING    and 
                p27 = 0      and 
                p28 = 0.0    and 
                p29 = 2 ns    and 
                p30 = 1       and 
                p31 = 2       and 
                p32 = "accdefgh"and
                p33 = B"11010110"   )
      report "***FAILED TEST: c04s03b02x00p29n10i00118 - The buffer ports on entities should be able to read."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p29n10i00118arch;
