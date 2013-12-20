
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
-- $Id: tc15.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p06n01i00015ent IS
END c04s02b00x00p06n01i00015ent;

ARCHITECTURE c04s02b00x00p06n01i00015arch OF c04s02b00x00p06n01i00015ent IS

BEGIN
  TESTING: PROCESS

    subtype   tboolean    is boolean       range FALSE to TRUE;
    subtype tbit      is bit         range '0'   to '1';   
    subtype tcharacter   is character      range 'A'   to 'Z';
    subtype tseverity_level   is severity_level   range NOTE  to ERROR;
    subtype tinteger   is integer      range 1111  to 2222;
    subtype treal      is real         range 1.11  to 2.22;
    subtype ttime      is time         range 1 ns  to 1 hr;
    subtype tnatural   is natural      range 100   to 200;
    subtype tpositive   is positive      range 1000  to 2000;

    variable k1  : tboolean;
    variable k2  : tbit;
    variable k3  : tcharacter;
    variable k4  : tseverity_level;
    variable k5  : tinteger;
    variable k6  : treal;
    variable k7  : ttime;
    variable k8  : tnatural;
    variable k9  : tpositive;

  BEGIN
    assert NOT(   k1   = tboolean'left      and
                  k2   = tbit'left       and
                  k3   = tcharacter'left   and
                  k4   = tseverity_level'left   and
                  k5   = tinteger'left      and
                  k6   = treal'left      and
                  k7   = ttime'left      and
                  k8   = tnatural'left      and
                  k9   = tpositive'left   )
      report "***PASSED TEST: c04s02b00x00p06n01i00015"
      severity NOTE;
    assert (   k1   = tboolean'left      and
               k2   = tbit'left       and
               k3   = tcharacter'left   and
               k4   = tseverity_level'left   and
               k5   = tinteger'left      and
               k6   = treal'left      and
               k7   = ttime'left      and
               k8   = tnatural'left      and
               k9   = tpositive'left   )
      report "***FAILED TEST: c04s02b00x00p06n01i00015 - A type mark denotes a type or a subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p06n01i00015arch;
