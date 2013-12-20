
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
-- $Id: tc1735.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s04b00x00p06n01i01735ent IS
END c09s04b00x00p06n01i01735ent;

ARCHITECTURE c09s04b00x00p06n01i01735arch OF c09s04b00x00p06n01i01735ent IS
  signal arch_s1 : bit;
  signal arch_s2 : boolean;
  signal arch_s3 : character;
  signal arch_s4 : severity_level;
  signal arch_s5 : integer;
  signal arch_s6 : real;
  signal arch_s7 : time;
  signal arch_s8 : positive;
  signal arch_s9 : natural;
BEGIN
  ASSERT arch_s1 /= bit'left 
    REPORT "bit concurrent assertion" 
    severity NOTE;
  ASSERT arch_s2 /= boolean'left 
    REPORT "boolean concurrent assertion" 
    severity NOTE;
  ASSERT arch_s3 /= character'left 
    REPORT "character concurrent assertion" 
    severity NOTE;
  ASSERT arch_s4 /= severity_level'left 
    REPORT "severity_level concurrent assertion" 
    severity NOTE;
  ASSERT arch_s5 /= integer'left 
    REPORT "integer concurrent assertion" 
    severity NOTE;
  ASSERT arch_s6 /= real'left 
    REPORT "real concurrent assertion" 
    severity NOTE;
  ASSERT arch_s7 /= time'left 
    REPORT "time concurrent assertion" 
    severity NOTE;
  ASSERT arch_s8 /= positive'left 
    REPORT "positive concurrent assertion" 
    severity NOTE;
  ASSERT arch_s9 /= natural'left 
    REPORT "natural concurrent assertion" 
    severity NOTE;
  TESTING: PROCESS
  BEGIN
    ASSERT arch_s1 /= bit'left 
      REPORT "bit concurrent assertion" 
      severity NOTE;
    ASSERT arch_s2 /= boolean'left 
      REPORT "boolean concurrent assertion" 
      severity NOTE;
    ASSERT arch_s3 /= character'left 
      REPORT "character concurrent assertion" 
      severity NOTE;
    ASSERT arch_s4 /= severity_level'left 
      REPORT "severity_level concurrent assertion" 
      severity NOTE;
    ASSERT arch_s5 /= integer'left 
      REPORT "integer concurrent assertion" 
      severity NOTE;
    ASSERT arch_s6 /= real'left 
      REPORT "real concurrent assertion" 
      severity NOTE;
    ASSERT arch_s7 /= time'left 
      REPORT "time concurrent assertion" 
      severity NOTE;
    ASSERT arch_s8 /= positive'left 
      REPORT "positive concurrent assertion" 
      severity NOTE;
    ASSERT arch_s9 /= natural'left 
      REPORT "natural concurrent assertion" 
      severity NOTE;
    assert FALSE 
      report "***PASSED TEST: c09s04b00x00p06n01i01735 - This need manual check - The concurrent assertion statement and the sequential assertion should print out the same ASSERTION NOTES."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c09s04b00x00p06n01i01735arch;
