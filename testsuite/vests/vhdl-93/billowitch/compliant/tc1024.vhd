
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
-- $Id: tc1024.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p10n02i01024ent IS
END c06s03b00x00p10n02i01024ent;

ARCHITECTURE c06s03b00x00p10n02i01024arch OF c06s03b00x00p10n02i01024ent IS
  signal pop  : bit;
  signal done : bit;
BEGIN
  TESTING: PROCESS
    variable done : bit := '1';
    variable pop  : bit;
  BEGIN
    pop            :=    done;
    c06s03b00x00p10n02i01024arch.pop   <=   TESTING.done;
    c06s03b00x00p10n02i01024arch.done   <=   TESTING.pop;
    wait for 1 ns;
    assert NOT(c06s03b00x00p10n02i01024arch.pop='1' and c06s03b00x00p10n02i01024arch.done='1') 
      report "***PASSED TEST: c06s03b00x00p10n02i01024" 
      severity NOTE;
    assert (c06s03b00x00p10n02i01024arch.pop='1' and c06s03b00x00p10n02i01024arch.done='1') 
      report "***FAILED TEST: c06s03b00x00p10n02i01024 - An expanded name denoting an entity declared within a named construct is allowed only within the construct." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p10n02i01024arch;
