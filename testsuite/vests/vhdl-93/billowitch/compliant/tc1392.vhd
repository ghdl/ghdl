
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
-- $Id: tc1392.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b00x00p04n04i01392ent IS
END c08s05b00x00p04n04i01392ent;

ARCHITECTURE c08s05b00x00p04n04i01392arch OF c08s05b00x00p04n04i01392ent IS
--
  TYPE rec_list IS RECORD
                     a,b,c,d : INTEGER;
                   END RECORD;

BEGIN
  TESTING: PROCESS
--
    VARIABLE rec1 : rec_list := (1,2,3,4);
    VARIABLE rec2 : rec_list := (1,2,3,4);
    VARIABLE rec3 : rec_list := (1,2,3,4);
    VARIABLE rec4 : rec_list := (1,2,3,4);
    VARIABLE rec5 : rec_list := (1,2,3,4);
    VARIABLE rec6 : rec_list := (1,2,3,4);

--
  BEGIN
--
    rec1 := ( rec1.d, rec1.c, rec1.b, rec1.a);
--
    ( rec2.d, rec2.c, rec2.b, rec2.a) := rec2;
--
    (rec3.d, rec3.c, rec3.b, rec3.a)
      :=  rec_list' (rec3.c, rec3.d, rec3.a, rec3.b);
--
    (rec4.a, rec4.b, rec4.c, rec4.d)
      :=  rec_list' ( d=>rec4.a, c=>rec4.b, b=>rec4.c, a=>rec4.d);
--
    ( d=>rec5.a, c=>rec5.b, b=>rec5.c, a=>rec5.d) := rec5;
--
    (rec6.d, rec6.c, rec6.b, rec6.a)
      :=  rec_list' ( d=>rec6.a, c=>rec6.b, b=>rec6.c, a=>rec6.d);
    
--
    ASSERT NOT(   rec1 = (4,3,2,1)   and  
                  rec2 = (4,3,2,1)    and 
                  rec3 = (2,1,4,3)    and 
                  rec4 = (4,3,2,1)    and 
                  rec5 = (4,3,2,1)    and 
                  rec6 = (1,2,3,4)) 
      report "***PASSED TEST: c08s05b00x00p04n04i01392"
      severity NOTE;
    ASSERT (   rec1 = (4,3,2,1)   and  
               rec2 = (4,3,2,1)    and 
               rec3 = (2,1,4,3)    and 
               rec4 = (4,3,2,1)    and 
               rec5 = (4,3,2,1)    and 
               rec6 = (1,2,3,4)) 
      report "***FAILED TEST: c08s05b00x00p04n04i01392 - Record aggregates type variable assignment fail."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b00x00p04n04i01392arch;
