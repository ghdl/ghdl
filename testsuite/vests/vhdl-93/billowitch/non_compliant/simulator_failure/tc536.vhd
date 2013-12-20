
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
-- $Id: tc536.vhd,v 1.2 2001-10-26 16:30:30 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s03b00x00p05n02i00536ent IS
  port (a,b: in integer; c,d: out integer);
END c03s03b00x00p05n02i00536ent;

ARCHITECTURE c03s03b00x00p05n02i00536arch OF c03s03b00x00p05n02i00536ent IS
  type    typer is array (integer range <>) of integer;
  subtype suber is typer (1 to 10);
  type    arst  is access typer;
BEGIN
  TESTING: PROCESS
    variable correct    : boolean;
    variable varst       : arst;
  BEGIN
    varst       := new typer (1 to 10);
    varst(1)    := 1;
    varst(2)    := 2;
    varst(12)    := 3; -- illegal (LRM 3.3)
    wait for 1 ns;
    assert FALSE
      report "***FAILED TEST: c03s03b00x00p05n02i00536 - An access value belongs to a corresponding subtype of an access type if the value of the designated object satisfies the constraint."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p05n02i00536arch;
