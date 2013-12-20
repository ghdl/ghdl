
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
-- $Id: tc1744.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b00x00p03n01i01744ent IS
  port   (parallel_in    : bit_vector (7 downto 0);
          clock      : bit;
          serial_out   : out bit);
END c09s05b00x00p03n01i01744ent;

ARCHITECTURE c09s05b00x00p03n01i01744arch OF c09s05b00x00p03n01i01744ent IS
  constant    bit_time    : time    := 1 ns;
  signal       GUARD      : boolean:= TRUE;
BEGIN
  serial_out <= guarded transport    -- No_failure_here
                parallel_in(7) after 1*bit_time,
                parallel_in(6) after 2*bit_time,
                parallel_in(5) after 3*bit_time,
                parallel_in(4) after 4*bit_time,
                parallel_in(3) after 5*bit_time,
                parallel_in(2) after 6*bit_time,
                parallel_in(1) after 7*bit_time,
                parallel_in(0) after 8*bit_time,
                '0' after 9*bit_time;
  PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c09s05b00x00p03n01i01744" 
      severity NOTE;
    wait;
  END PROCESS;

END c09s05b00x00p03n01i01744arch;
