
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
-- $Id: tc896.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s02b00x00p02n02i00896ent IS
  generic ( x : integer := 2 );
END c10s02b00x00p02n02i00896ent;

ARCHITECTURE c10s02b00x00p02n02i00896arch OF c10s02b00x00p02n02i00896ent IS

BEGIN
  -- extended use of declared generic.
  assert NOT( x = 2 )
    report "***PASSED TEST: c10s02b00x00p02n02i00896"
    severity NOTE;
  assert ( x = 2 )
    report "***FAILED TEST: c10s02b00x00p02n02i00896 - The scope of the declaration that occurs immediately within a formal generic declaration extends beyond the immediate scope."
    severity ERROR;

END c10s02b00x00p02n02i00896arch;
