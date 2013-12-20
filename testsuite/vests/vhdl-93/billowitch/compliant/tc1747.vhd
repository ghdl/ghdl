
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
-- $Id: tc1747.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b00x00p12n02i01747ent IS
  function resolve_bit ( inputs : bit_vector) return bit is
    VARIABLE val : bit := '0';
  begin
    if inputs'length = 0 then
      return val;
    else
      for i in inputs'range LOOP
        if inputs(i) = '1' then return '1'; end if;
      END LOOP;
      return '0';
    end if;
  end resolve_bit;
END c09s05b00x00p12n02i01747ent;

ARCHITECTURE c09s05b00x00p12n02i01747arch OF c09s05b00x00p12n02i01747ent IS
  signal      a      : resolve_bit bit BUS;
  signal    b      : resolve_bit bit BUS;
  signal    grd    : boolean;

BEGIN
  grd <=    TRUE    after 10 ns,
            FALSE   after 20 ns; 

  block_label : BLOCK (grd)
  begin
    b <= guarded '1' after 1 ns;
  end block block_label;

  block_label_1 : BLOCK (grd)
  begin
    TESTING: PROCESS
    BEGIN
      if GUARD then
        a <= '1' after 1 ns;
      else
        a <= NULL;
      end if;
      wait on GUARD, a;
    END PROCESS TESTING;
  end block block_label_1;
  
  process(a,b)
    variable f1, f2 : integer := 0;
  begin
    if    (now = 11 ns) and (a=b) then
      f1 := 1;
    end if;
    if    (now = 20 ns) and (a=b) then
      f2 := 1;
    end if;
    if (now = 20 ns) then   
      assert NOT((f1=1) and (f2=1)) 
        report "***PASSED TEST: c09s05b00x00p12n02i01747"
        severity NOTE;
      assert ((f1=1) and (f2=1)) 
        report "***FAILED TEST: c09s05b00x00p12n02i01747 - The concurrent guarded signal assignment statement has an equivalent process statement."
        severity ERROR;
    end if;
  end process;

END c09s05b00x00p12n02i01747arch;
