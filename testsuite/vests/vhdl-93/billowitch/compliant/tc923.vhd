
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
-- $Id: tc923.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s03b00x00p19n01i00923ent IS
  type primary is ( red, green, blue );
END c10s03b00x00p19n01i00923ent;

ARCHITECTURE c10s03b00x00p19n01i00923arch OF c10s03b00x00p19n01i00923ent IS

  procedure xxx is
    type    primary is ( red, green, blue );
    constant x : c10s03b00x00p19n01i00923ent.primary := red;
  begin
    assert NOT( x=red )
      report "***PASSED TEST: c10s03b00x00p19n01i00923"
      severity NOTE;
    assert ( x=red )
      report "***FAILED TEST: c10s03b00x00p19n01i00923 - The declarations can be made visible by providing a prefix to the declaration to specify where it had been declared."
      severity ERROR;
  end xxx;
BEGIN
  xxx;
END c10s03b00x00p19n01i00923arch;
