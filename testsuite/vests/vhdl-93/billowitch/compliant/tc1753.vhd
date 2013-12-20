
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
-- $Id: tc1753.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b00x00p25n01i01753ent IS
END c09s05b00x00p25n01i01753ent;

ARCHITECTURE c09s05b00x00p25n01i01753arch OF c09s05b00x00p25n01i01753ent IS
  type byte is array (positive range <>) of bit;

  function F (constant S: byte) return bit is
  begin
    return '0';
  end;

  constant    N    : integer := 4;
  signal       UG,I   : bit_vector(1 to 4);
  signal       GS, UGS   : bit;
  signal       UGT   : F bit register;
BEGIN
  A: (I(1), I(2), I(3), I(N)) <= transport UG(1 to N) after 20 ns;  -- No_failure_here
  
  GS   <=   '1' after 10 ns; 
  B:block (GS = '1')
  begin
    C: UGT <= guarded UGS after 10 ns;  -- No_failure_here
  end block;
  TESTING: PROCESS(I,UGT)
  BEGIN
    assert NOT(I="0000" and UGT='0')  
      report "***PASSED TEST: c09s05b00x00p25n01i01753" 
      severity NOTE;
    assert (I="0000" and UGT='0')  
      report "***FAILED TEST: c09s05b00x00p25n01i01753 - An aggregate target in a concurrent signal assignment statement contains only locally static names, and no two signal names identify the same object, or subelement thereof." 
      severity ERROR;
  END PROCESS TESTING;

END c09s05b00x00p25n01i01753arch;
