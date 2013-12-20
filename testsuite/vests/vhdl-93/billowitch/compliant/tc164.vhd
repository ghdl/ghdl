
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
-- $Id: tc164.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p23n01i00164ent IS
END c04s03b02x02p23n01i00164ent;

ARCHITECTURE c04s03b02x02p23n01i00164arch OF c04s03b02x02p23n01i00164ent IS
  signal p1 : bit;   --added to make it compile
BEGIN
  TESTING: PROCESS(p1)
    VARIABLE v1,v2,v3,v4 : integer;

    PROCEDURE default_test
      (
        param_1 : in integer;
        default : in integer := 22;
        param_3 : out integer;
        param_4 : out integer
        )
    is
    begin
      param_3 := param_1;
      param_4 := default;
    end default_test;

  BEGIN
    v1 := 1919;
    default_test (v1,
                  -- missing association
                  param_3 => v3,
                  param_4 => v4
                  );
    assert NOT( v3=1919 and v4=22 )
      report "***PASSED TEST: c04s03b02x02p23n01i00164"
      severity NOTE;
    assert ( v3=1919 and v4=22 )
      report "***FAILED TEST: c04s03b02x02p23n01i00164 - If an association element is omitted from an association list in order to make use of the default expression on the corresponding interface element, all subsequent association elements in that association list must be named associations."
      severity ERROR;
  END PROCESS TESTING;

END c04s03b02x02p23n01i00164arch;
