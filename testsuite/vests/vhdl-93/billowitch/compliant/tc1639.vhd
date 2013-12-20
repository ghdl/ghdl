
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
-- $Id: tc1639.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s12b00x00p06n01i01639ent IS
END c08s12b00x00p06n01i01639ent;

ARCHITECTURE c08s12b00x00p06n01i01639arch OF c08s12b00x00p06n01i01639ent IS

BEGIN
  TESTING: PROCESS
    variable correct : boolean := true;
    procedure Proc1(constant p_boolean :boolean ) is
    begin
      if p_boolean = p_boolean then
        return;
      else
        return;   
      end if;
      correct := false;
    end Proc1;
  BEGIN
    Proc1(false);
    assert NOT( correct = true )
      report "***PASSED TEST: c08s12b00x00p06n01i01639"
      severity NOTE;
    assert ( correct = true )
      report "***FAILED TEST: c08s12b00x00p06n01i01639 - A return statement stops execution of a procedure."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s12b00x00p06n01i01639arch;
