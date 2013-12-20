
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
-- $Id: tc1310.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p07n02i01310ent IS
END c08s04b00x00p07n02i01310ent;

ARCHITECTURE c08s04b00x00p07n02i01310arch OF c08s04b00x00p07n02i01310ent IS
  type sigrec is
    record
      A1 : bit;
      A2 : integer;
      A3 : character;
      A4 : boolean;
    end record;
  signal   S1 : bit;
  signal   S2 : integer;
  signal   S3 : character;
  signal   S4 : boolean;
BEGIN
  TESTING: PROCESS
  BEGIN
    (S1, S2, S3, S4) <= sigrec'('1', 1, '1', true);
    wait for 1 ns;
    assert NOT( (S1='1')and(S2=1)and(S3='1')and(S4=true) )
      report "***PASSED TEST: c08s04b00x00p07n02i01310"
      severity NOTE;
    assert ( (S1='1')and(S2=1)and(S3='1')and(S4=true) )
      report "***FAILED TEST: c08s04b00x00p07n02i01310 - A waveform element on the rigth-hand side must be the same as the base type of the aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p07n02i01310arch;
