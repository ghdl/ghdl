
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
-- $Id: tc90.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p01n01i00090ent IS
END c04s03b02x00p01n01i00090ent;

ARCHITECTURE c04s03b02x00p01n01i00090arch OF c04s03b02x00p01n01i00090ent IS

  Procedure Variable_params_of_subp (
    VARIABLE  cp1  : in      Boolean        := FALSE;
    VARIABLE  cp2  : in      Bit            := '0';
    VARIABLE  cp3  : in      Character      := '$';
    VARIABLE  cp4  : in      SEVERITY_LEVEL := FAILURE;
    VARIABLE  cp5  : in      Integer        := 5 + 6 ;
    VARIABLE  cp6  : in      Real           := 2.45 ;
    VARIABLE  cp7  : in      TIME           := 0 fs;
    VARIABLE  cp8  : in      Natural        := 10;
    VARIABLE  cp9  : in      Positive       := 99;
    --
    VARIABLE  cp12 : out     Boolean        ;
    VARIABLE  cp13 : out     Bit            ;
    VARIABLE  cp14 : out     Character      ;
    VARIABLE  cp15 : out     SEVERITY_LEVEL ;
    VARIABLE  cp16 : out     Integer        ;
    VARIABLE  cp17 : out     Real           ;
    VARIABLE  cp18 : out     TIME           ;
    VARIABLE  cp19 : out     Natural        ;
    VARIABLE  cp20 : out     Positive       ;
    --
    VARIABLE  cp23 : inout   Boolean        ;
    VARIABLE  cp24 : inout   Bit            ;
    VARIABLE  cp25 : inout   Character      ;
    VARIABLE  cp26 : inout   SEVERITY_LEVEL ;
    VARIABLE  cp27 : inout   Integer        ;
    VARIABLE  cp28 : inout   Real           ;
    VARIABLE  cp29 : inout   TIME           ;
    VARIABLE  cp30 : inout   Natural        ;
    VARIABLE  cp31 : inout   Positive
    ) is
  begin
    -- assign ins to outs
    cp12 := cp1   ;
    cp13 := cp2   ;
    cp14 := cp3   ;
    cp15 := cp4   ;
    cp16 := cp5   ;
    cp17 := cp6   ;
    cp18 := cp7   ;
    cp19 := cp8   ;
    cp20 := cp9   ;
    
    -- assign ins to inouts
    cp23 := cp1   ;
    cp24 := cp2   ;
    cp25 := cp3   ;
    cp26 := cp4   ;
    cp27 := cp5   ;
    cp28 := cp6   ;
    cp29 := cp7   ;
    cp30 := cp8   ;
    cp31 := cp9   ;
    
  end Variable_params_of_subp;
  
BEGIN
  TESTING: PROCESS
    VARIABLE    v1  :    Boolean        := false;
    VARIABLE    v2  :    Bit            := '1';
    VARIABLE    v3  :    Character      := '%';
    VARIABLE    v4  :    SEVERITY_LEVEL := NOTE;
    VARIABLE    v5  :    Integer        := 22121;
    VARIABLE    v6  :    Real           := 2.545;
    VARIABLE    v7  :    TIME           := 12 ns;
    VARIABLE    v8  :    Natural        := 90;
    VARIABLE    v9  :    Positive       := 101;
    
    VARIABLE    v12 :  Boolean        ;
    VARIABLE    v13 :  Bit            ;
    VARIABLE    v14 :  Character      ;
    VARIABLE    v15 :  SEVERITY_LEVEL ;
    VARIABLE    v16 :  Integer        ;
    VARIABLE    v17 :  Real           ;
    VARIABLE    v18 :  TIME           ;
    VARIABLE    v19 :  Natural        ;
    VARIABLE    v20 :  Positive       ;
    
    VARIABLE    v23 :  Boolean        ;
    VARIABLE    v24 :  Bit            ;
    VARIABLE    v25 :  Character      ;
    VARIABLE    v26 :  SEVERITY_LEVEL ;
    VARIABLE    v27 :  Integer        ;
    VARIABLE    v28 :  Real           ;
    VARIABLE    v29 :  TIME           ;
    VARIABLE    v30 :  Natural        ;
    VARIABLE    v31 :  Positive       ;

  BEGIN

    Variable_params_of_subp ( v1,v2,v3,v4,v5,v6,v7,v8,v9,
                              v12,v13,v14,v15,v16,v17,v18,v19,v20,
                              v23,v24,v25,v26,v27,v28,v29,v30,v31
                              );

    assert   v12 = v1 report "  v12 /= v1" severity failure;
    assert   v13 = v2 report "  v13 /= v2" severity failure;
    assert   v14 = v3 report "  v14 /= v3" severity failure;
    assert   v15 = v4 report "  v15 /= v4" severity failure;
    assert   v16 = v5 report "  v16 /= v5" severity failure;
    assert   v17 = v6 report "  v17 /= v6" severity failure;
    assert   v18 = v7 report "  v18 /= v7" severity failure;
    assert   v19 = v8 report "  v19 /= v8" severity failure;
    assert   v20 = v9 report "  v20 /= v9" severity failure;
    
    assert   v23 = v1 report "  v23 /= v1" severity failure;
    assert   v24 = v2 report "  v24 /= v2" severity failure;
    assert   v25 = v3 report "  v25 /= v3" severity failure;
    assert   v26 = v4 report "  v26 /= v4" severity failure;
    assert   v27 = v5 report "  v27 /= v5" severity failure;
    assert   v28 = v6 report "  v28 /= v6" severity failure;
    assert   v29 = v7 report "  v29 /= v7" severity failure;
    assert   v30 = v8 report "  v30 /= v8" severity failure;
    assert   v31 = v9 report "  v31 /= v9" severity failure;
    WAIT for 1 ns;

    assert NOT(       v12 = v1    and
                      v13 = v2    and
                      v14 = v3    and
                      v15 = v4    and
                      v16 = v5    and
                      v17 = v6    and
                      v18 = v7    and
                      v19 = v8    and
                      v20 = v9    and
                      v23 = v1    and
                      v24 = v2    and
                      v25 = v3    and
                      v26 = v4    and
                      v27 = v5    and
                      v28 = v6    and
                      v29 = v7    and
                      v30 = v8    and
                      v31 = v9    )   
      report "***PASSED TEST:c04s03b02x00p01n01i00090"
      severity NOTE;
    assert (            v12 = v1    and
                        v13 = v2    and
                        v14 = v3    and
                        v15 = v4    and
                        v16 = v5    and
                        v17 = v6    and
                        v18 = v7    and
                        v19 = v8    and
                        v20 = v9    and
                        v23 = v1    and
                        v24 = v2    and
                        v25 = v3    and
                        v26 = v4    and
                        v27 = v5    and
                        v28 = v6    and
                        v29 = v7    and
                        v30 = v8    and
                        v31 = v9    )   
      report "***FAILED TEST: c04s03b02x00p01n01i00090 - Variables as the interface objects that appear as variable parameters of subprogram."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p01n01i00090arch;
