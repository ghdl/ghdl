
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
-- $Id: tc113.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

Package c04s03b02x00p29n10i00113pkg is
  type Apollo_string is array (INTEGER range 1 to 8) of CHARACTER;
  type V_REGISTER    is array (INTEGER range 0 to 7) of BIT;
end c04s03b02x00p29n10i00113pkg;

use work.c04s03b02x00p29n10i00113pkg.all;
ENTITY c04s03b02x00p29n10i00113ent IS
  port (
    p23 : inout Boolean        := FALSE;
    p24 : inout Bit            := '0'  ;
    p25 : inout Character      := NUL  ;
    p26 : inout SEVERITY_LEVEL := NOTE ;
    p27 : inout Integer        := -1   ;
    p28 : inout Real           := -1.0 ;
    p29 : inout TIME           := 1 fs ;
    p30 : inout Natural        := 0    ;
    p31 : inout Positive       := 1    ;
    p32 : inout Apollo_string  := "abcdefgh";
    p33 : inout V_register     := B"10010110"
    );
END c04s03b02x00p29n10i00113ent;

ARCHITECTURE c04s03b02x00p29n10i00113arch OF c04s03b02x00p29n10i00113ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(    p23 = FALSE   and
                   p24 = '0'     and 
                   p25 = NUL     and 
                   p26 = NOTE    and 
                   p27 = -1      and 
                   p28 = -1.0    and 
                   p29 = 1 fs    and 
                   p30 = 0       and 
                   p31 = 1       and 
                   p32 = "abcdefgh"and
                   p33 = B"10010110"   )
      report "***PASSED TEST: c04s03b02x00p29n10i00113"
      severity NOTE;
    assert (    p23 = FALSE   and
                p24 = '0'     and 
                p25 = NUL     and 
                p26 = NOTE    and 
                p27 = -1      and 
                p28 = -1.0    and 
                p29 = 1 fs    and 
                p30 = 0       and 
                p31 = 1       and 
                p32 = "abcdefgh"and
                p33 = B"10010110"   )
      report "***FAILED TEST:c04s03b02x00p29n10i00113 - Values of INOUT port reading failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p29n10i00113arch;
