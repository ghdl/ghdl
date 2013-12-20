
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
-- $Id: tc1708.vhd,v 1.2 2001-10-26 16:30:30 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s02b00x00p07n01i01708ent IS
END c09s02b00x00p07n01i01708ent;

ARCHITECTURE c09s02b00x00p07n01i01708arch OF c09s02b00x00p07n01i01708ent IS
  procedure call_wait (variable dly : inout time) is
    --
    -- This procedure simply waits for the time
    -- specified in its argument.
    --
  begin
    wait for dly;
    dly := dly + 1 ns;
  end call_wait;
  
  procedure call_waiter (variable dly_time : inout time) is
    --
    -- This procedure just provides an extra level of indirection
    --
  begin
    call_wait(dly_time);
  end call_waiter;
  
  signal trigger : bit;                 -- inter-process communication signal

BEGIN
  TESTING: PROCESS( trigger )
    variable delay : time := 2 ns;
  BEGIN

    call_waiter(delay);       -- use wait indirectly

    assert FALSE 
      report "***FAILED TEST: c09s02b00x00p07n01i01708 - Procedure with an indirect wait was illegal to be placed in a process with an explicit sensitivity list." 
      severity ERROR;
  END PROCESS TESTING;

END c09s02b00x00p07n01i01708arch;
