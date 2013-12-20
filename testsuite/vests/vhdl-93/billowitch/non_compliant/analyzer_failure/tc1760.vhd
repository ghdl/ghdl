
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
-- $Id: tc1760.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b01x00p21n01i01760ent IS
  port (PT: inout integer; PT2: inout character); 
END c09s05b01x00p21n01i01760ent;

ARCHITECTURE c09s05b01x00p21n01i01760arch OF c09s05b01x00p21n01i01760ent IS
  signal S1, S2 : boolean;
BEGIN
  PT <=    5 when S1 /= S2 else
           6 when S1 > S2 else
           7 when S1 + S2 else  -- Failure_here
           --ERROR conditions must be like those of
           -- an if statement in a process statement
           9;

  PT2 <=    'A' when S1 - S2 else  -- Failure_here
            --ERROR the waveform must be like if statement
            -- in a process statement.
            'B' when S2 = S2 else
            'C' ;
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c09s05b01x00p21n01i01760 - The if statement must be such that it is in a process statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s05b01x00p21n01i01760arch;
