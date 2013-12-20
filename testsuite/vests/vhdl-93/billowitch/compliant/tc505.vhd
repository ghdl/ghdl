
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
-- $Id: tc505.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b02x00p06n01i00505ent IS
  subtype    DAY1    is INTEGER range 1 to 31;
  type       MONTH1    is (   January, February, March, April, May, June, July,
                              August, September, October, November, December);
END c03s02b02x00p06n01i00505ent;

ARCHITECTURE c03s02b02x00p06n01i00505arch OF c03s02b02x00p06n01i00505ent IS
  type DATE1 is record
                  day:    DAY1;
                  month:  MONTH1;
                  year:   INTEGER range 0 to 4000;
                end record;
  type EVENT is record
                  text:   STRING (1 to 20);
                  date:   DATE1;
                end record;
  signal event_signal : EVENT;
BEGIN
  TESTING: PROCESS
  BEGIN
    event_signal <= (text => "Go to Wall Street NY",
                     date => (27, January, 1991)) after 10 ns;
    wait for 20 ns;
    assert NOT( event_signal.text = "Go to Wall Street NY" )
      report "***PASSED TEST: c03s02b02x00p06n01i00505"
      severity NOTE;
    assert ( event_signal.text = "Go to Wall Street NY" )
      report "***FAILED TEST: c03s02b02x00p06n01i00505 - Element declaration test in record type failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p06n01i00505arch;
