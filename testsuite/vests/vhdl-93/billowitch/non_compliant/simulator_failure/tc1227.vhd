
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
-- $Id: tc1227.vhd,v 1.2 2001-10-26 16:30:30 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s01b00x00p29n01i01227ent IS
END c08s01b00x00p29n01i01227ent;

ARCHITECTURE c08s01b00x00p29n01i01227arch OF c08s01b00x00p29n01i01227ent IS

  procedure call_wait (constant dly : in time) is
    --
    -- This procedure simply waits for the time
    -- specified in its argument.
    --
  begin
    wait for dly;
  end call_wait;

  procedure indirect_wait (constant dly : in time) is
    --
    -- This procedure calls a procedure to wait for the
    -- time specified in its argument.
    --
  begin
    call_wait (dly);
  end indirect_wait;

  function call_waiter (constant dly : in time) return time is    
    --
    -- This function indirectly calls a procedure to wait
    -- for the time specified in its argument, then
    -- returns an incremented delay.
    --
    -- This is an illegal operation for a function and
    -- will probably be caught at runtime.
    --
  begin
    indirect_wait(dly);
    return dly * 2;
  end call_waiter;

BEGIN
  TESTING: PROCESS
    variable   delay : time := 2 ns;
  BEGIN
    delay   := call_waiter(delay);   -- use wait indirectly
    assert FALSE 
      report "***FAILED TEST: c08s01b00x00p29n01i01227 - Wait statement appears in a procedure that has a parent that is a function subprogram." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p29n01i01227arch;
