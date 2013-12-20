
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
-- $Id: tc1658.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity E  is
  port ( S_in : in bit; S_out : out bit) ;
end E;

entity C1  is
  port ( A : bit; B :out bit) ;
end C1;

use work.c1;

ENTITY c09s01b00x00p02n01i01658ent IS
  port ( B : bit ) ;
END c09s01b00x00p02n01i01658ent;

ARCHITECTURE c09s01b00x00p02n01i01658arch OF c09s01b00x00p02n01i01658ent IS

BEGIN

  lab : block 
    component C1
      port ( A : bit; B : out bit );
    end component ; -- C1
    
    for all : C1 use entity work.E
      port map ( S_in => A, S_out => B ) ;
    
    type    T1 is ('0', '1');
    subtype T2 is integer range 0 to 7;
    
    signal S1    : real;
    alias  S1_too    : real is S1;
    
    attribute ATTR : T1;
    attribute ATTR of ALL : signal is '1';

  begin
    TESTING: PROCESS
    BEGIN
      assert FALSE 
        report "***PASSED TEST: c09s01b00x00p02n01i01658" 
        severity NOTE;
      wait;
    END PROCESS TESTING;
  end block lab;


END c09s01b00x00p02n01i01658arch;
