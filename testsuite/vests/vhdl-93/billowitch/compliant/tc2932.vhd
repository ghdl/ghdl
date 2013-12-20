
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
-- $Id: tc2932.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s02b00x00p07n01i02932ent IS
END c02s02b00x00p07n01i02932ent;

ARCHITECTURE c02s02b00x00p07n01i02932arch OF c02s02b00x00p07n01i02932ent IS

BEGIN
  TESTING: PROCESS
    variable global_int : integer := 0;
    
    procedure Recursive_subr ( x: integer ) is
    begin
      global_int := global_int + 1;
      if x > 1 then
        Recursive_subr (x-1);
      end if;
    end Recursive_subr ;
  BEGIN
    Recursive_subr (10);
    wait for 5 ns;
    assert NOT( global_int = 10 )
      report "***PASSED TEST: c02s02b00x00p07n01i02932"
      severity NOTE;
    assert ( global_int = 10 )
      report "***FAILED TEST: c02s02b00x00p07n01i02932 - Recursive procedure test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s02b00x00p07n01i02932arch;
