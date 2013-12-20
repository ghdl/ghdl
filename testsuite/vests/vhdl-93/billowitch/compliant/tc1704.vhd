
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
-- $Id: tc1704.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s02b00x00p07n01i01704ent IS
END c09s02b00x00p07n01i01704ent;

ARCHITECTURE c09s02b00x00p07n01i01704arch OF c09s02b00x00p07n01i01704ent IS
  signal   S :    Bit;
BEGIN
  TESTING: PROCESS( S )
    -- local variables.
    variable INITED : BOOLEAN := FALSE;
    variable CNT    : INTEGER := 0;
    variable NEWTIME: TIME;
    variable k      : integer := 1;
  BEGIN
    -- Take care of the first run.
    if  (not( INITED )) then
      INITED  := TRUE;
      CNT     := 0;
      S       <= (not S) after 1 ns;
      NEWTIME := NOW + 1 ns;

      -- Otherwise, take care of all subsequent runs.
      -- NOTE:  Take care of the last time we will get awakened.
    elsif (NOW /= TIME'HIGH) then
      
      -- Verify that we woke up when S was updated.
      if NOT(( S'EVENT ) and ( NEWTIME = NOW )) then
        k := 0;
      end if;
      
      -- See if we should continue.  If so, do it.
      CNT    := CNT + 1;
      if  (CNT <= 50) then
        S       <= (not S) after 1 ns;
        NEWTIME := NOW + 1 ns;
      end if;
    end if;
    if (CNT = 50) then
      assert NOT( k=1 )
        report "***PASSED TEST: c09s02b00x00p07n01i01704"
        severity NOTE;
      assert ( k=1 )
        report "***FAILED TEST: c09s02b00x00p07n01i01704 - The process statement is assumed to contain an implicit wait statement if a sensitivity list appears following the reserved word process."
        severity ERROR;
    end if;
  END PROCESS TESTING;

END c09s02b00x00p07n01i01704arch;
