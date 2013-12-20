
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
-- $Id: tc2450.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p03n02i02450ent IS
END c07s03b02x02p03n02i02450ent;

ARCHITECTURE c07s03b02x02p03n02i02450arch OF c07s03b02x02p03n02i02450ent IS

BEGIN
  TESTING: PROCESS
    type ENUM is ( ONE );
    
    type A_ARRAY is array ( integer range <> ) of integer;       
    type B_ARRAY is array ( boolean range <> ) of real;
    type C_ARRAY is array ( ENUM range <>, ENUM range <>) of bit;
    
    subtype A_CON is A_ARRAY ( 1 to 4 );
    subtype B_CON is B_ARRAY ( FALSE to TRUE );
    subtype C_CON is C_ARRAY ( ONE to ONE, ONE to ONE );

    function F_A ( PAR : A_ARRAY ) return A_CON is
    begin   return (1,2,3,4);       
    end F_A;
    
    function F_B ( PAR : B_ARRAY ) return B_CON is
    begin   return (1.0, 2.0);      
    end F_B;
    
    function F_C ( PAR : C_ARRAY ) return C_CON is
    begin   return (ONE=>(ONE=>'0'));               
    end F_C;

    variable V_A : A_CON ;
    variable V_B : B_CON ;
    variable V_C : C_CON ;

  BEGIN
    V_A := F_A( F_A( (1,2,others=>3) ) ); -- Failure_here        
    -- SEMANTIC ERROR:  "others" used in aggregate which corresponds to
    --                  an unconstrained formal parameter
    assert FALSE 
      report "***FAILED TEST: c07s03b02x02p03n02i02450 - Others is used in an aggregate which corresponds to an unconstrained formal parameter."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p03n02i02450arch;
