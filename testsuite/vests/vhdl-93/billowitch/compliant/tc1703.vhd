
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
-- $Id: tc1703.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s02b00x00p05n01i01703ent IS
END c09s02b00x00p05n01i01703ent;

ARCHITECTURE c09s02b00x00p05n01i01703arch OF c09s02b00x00p05n01i01703ent IS
  signal s : boolean := false;
BEGIN

  TESTING: PROCESS
    type result_type is (fail, pass);
    variable result : result_type := fail;
    variable i, j   : integer;
    variable k   : integer := 0;
  BEGIN
    --
    -- Test all sequential statements in this process
    --
    s <= true;                        -- signal assignment        
    j := 1;                           -- variable assignment
    i := 0;
    
    L1: while  ( i < 10 )  loop       -- conditional loop
      if i > 2 then
        exit;
      end if;
      case i is
        when 0 =>
          L2: for j in 1 to 3 loop
            case j is
              when 3 =>       -- should never execute because of
                i := i + 1; -- alternative 2
                k := 1;
                exit;
                assert false
                  report "exit in loop 2 case failed."
                  severity note;
              when 2 =>
                i := i + 1;
                next L1;
                k := 1;
                assert false     -- should never execute
                  report "next in loop 2 case failed."
                  severity note;
              when 1 =>
                assert false
                  report "first iteration of loop 2."
                  severity note ;
                next;     -- applies to loop L2
              when others =>
                --
                -- This should never be executed but is
                -- required by the 1076-1987 spec. which
                -- says the subtype of 'j' is the same as
                -- the base type (integer) and not constrained
                -- to the range "1 to 3".
                --
                k := 1;
                assert false
                  report "Should never get here."
                  severity note ;
            end case;
            k := 1;
            assert false      -- should never execute
              report "next in loop 2 failed."
              severity note;
          end loop L2;
        when 2 =>
          s <= false after 5 ns;
          wait for 6 ns;
          assert not s
            report "wait statement in loop L1 failed."
            severity note ;
          
          i := i +1;
        when 1 =>
          null;
          assert false
            report "null statement and next statement worked."
            severity note ;
          i := i +1;
        when others =>
          k := 1;
          assert false
            report "exit in if statement in loop L1 failed."
            severity note ;
          exit;
      end case;
    end loop L1;

    wait for 50 ns;

    assert NOT(s=false and k = 0 and j=1) 
      report "***PASSED TEST: c09s02b00x00p05n01i01703" 
      severity NOTE;
    assert (s=false and k = 0 and j=1) 
      report "***FAILED TEST: c09s02b00x00p05n01i01703 - Process statement execution failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s02b00x00p05n01i01703arch;
