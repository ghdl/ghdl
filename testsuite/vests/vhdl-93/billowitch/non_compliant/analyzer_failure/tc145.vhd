
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
-- $Id: tc145.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b02x02p12n01i00145pkg is
  procedure P1 (a    : in integer; b: out integer);
  function  F1 (I1, I2   : in integer) return real;
end c04s03b02x02p12n01i00145pkg;

package body c04s03b02x02p12n01i00145pkg is
  procedure P1 (a: in integer; b: out integer) is
  begin
    b := a;
  end;
  
  function  F1 (I1, I2: in integer) return real is
    variable y : real := 1.0;
  begin
    return (y);
  end;
end c04s03b02x02p12n01i00145pkg;

use work.c04s03b02x02p12n01i00145pkg.all;
ENTITY c04s03b02x02p12n01i00145ent IS
END c04s03b02x02p12n01i00145ent;

ARCHITECTURE c04s03b02x02p12n01i00145arch OF c04s03b02x02p12n01i00145ent IS

BEGIN

  TESTING: PROCESS
    variable x1 : real := 1.0;
    variable x2 : real := 1.0;
    variable y  : real ;
  BEGIN
    P1 (10, F1(b, x1) => x2 );   -- Failure_here
                                     -- function has more than one parameter.
    y := x2;
    assert FALSE
      report "***FAILED TEST: c04s03b02x02p12n01i00145 - Formal part in named association has improper format."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p12n01i00145arch;
