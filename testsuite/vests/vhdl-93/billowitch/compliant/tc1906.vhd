
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
-- $Id: tc1906.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p10n01i01906ent IS
END c07s01b00x00p10n01i01906ent;

ARCHITECTURE c07s01b00x00p10n01i01906arch OF c07s01b00x00p10n01i01906ent IS
  type    MVL    is ('0','1','X','Z') ;
  signal    Q    : MVL;
  signal    PP,P2    : BIT := '1' ;
  signal    R1    : BIT;
BEGIN
  TESTING: PROCESS
    function "and" (L,R : MVL) return MVL is
      variable V1 : MVL;
    begin
      if (L = '1') then
        V1 := '1' ;
      end if;
      return V1;
    end;
  BEGIN
    Q    <= "and"('1','Z');  -- No_failure_here
    R1    <= PP and P2;
    wait for 1 ns;
    assert NOT((Q='1') and (R1='1'))
      report "***PASSED TEST: c07s01b00x00p10n01i01906"
      severity NOTE;
    assert (( Q='1' ) and (R1='1'))
      report "***FAILED TEST: c07s01b00x00p10n01i01906 - The identification of an overloaded operator depends on the context."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p10n01i01906arch;
