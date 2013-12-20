
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
-- $Id: tc1642.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s12b00x00p06n01i01642pkg is

  procedure procI;
  function  funcI return INTEGER;

end c08s12b00x00p06n01i01642pkg;

package body c08s12b00x00p06n01i01642pkg is

  procedure procI is
  begin
    -- Return.
    return;

    -- Statement should NEVER be executed.
    assert (FALSE)
      report "Statement in procedure was executed in error.";
  end procI;

  function funcI return INTEGER is
  begin
    -- Return from the function.
    return( 4 );

    -- Statement should NEVER be executed.
    assert (FALSE)
      report "Statement in function was executed in error.";
  end funcI;

end c08s12b00x00p06n01i01642pkg;

use work.c08s12b00x00p06n01i01642pkg.all;
ENTITY c08s12b00x00p06n01i01642ent IS
END c08s12b00x00p06n01i01642ent;

ARCHITECTURE c08s12b00x00p06n01i01642arch OF c08s12b00x00p06n01i01642ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    -- Execute the procedure.
    procI;
    
    -- Execute the function.
    assert NOT(funcI = 4)
      report "***PASSED TEST: c08s12b00x00p06n01i01642" 
      severity NOTE;
    assert (funcI = 4)
      report "***FAILED TEST: c08s12b00x00p06n01i01642 - The execution of the return statement completes if the type of the expression is of teh result subtype." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s12b00x00p06n01i01642arch;
