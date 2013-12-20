
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
-- $Id: tc1793.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s07b00x00p06n02i01793ent IS
END c09s07b00x00p06n02i01793ent;

ARCHITECTURE c09s07b00x00p06n02i01793arch OF c09s07b00x00p06n02i01793ent IS
  type   Day is (Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday);

  procedure i_proof_1 (x : integer) is
  begin
  end i_proof_1;

  procedure i_proof_2 (x : character) is
  begin
  end i_proof_2;

  procedure i_proof_3 (x : Day) is
  begin
  end i_proof_3;

BEGIN

  glabel1 : FOR i in 0 to 8 generate
    i_proof_1(i);
  end generate glabel1;

  glabel2 : FOR i in 'A' to 'Z' generate
    i_proof_2(i);
  end generate glabel2;

  glabel3 : FOR i in Monday to Sunday generate
    i_proof_3(i);
  end generate glabel3;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c09s07b00x00p06n02i01793"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c09s07b00x00p06n02i01793arch;
