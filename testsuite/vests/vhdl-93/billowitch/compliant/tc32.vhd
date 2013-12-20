
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
-- $Id: tc32.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p01n01i00032ent IS
END c04s03b01x01p01n01i00032ent;

ARCHITECTURE c04s03b01x01p01n01i00032arch OF c04s03b01x01p01n01i00032ent IS

--
--          Declaration of composite types
--
  TYPE    U1 IS ARRAY (CHARACTER RANGE <>) OF INTEGER;      -- unconstrained array type
  TYPE    C1 IS ARRAY (5 TO 9)             OF BIT;          -- constrained array type
--
--          Declaration of composite types
--           - records types and subtypes
--
  TYPE month_name IS (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec );

  TYPE R1 IS
    RECORD
      month : month_name;
      day   : INTEGER RANGE 0 TO 31;
      year  : INTEGER RANGE 0 TO 4000;
    END RECORD;
--
--          Declaration of composite - composite types
--
  TYPE US1 IS ARRAY (INTEGER RANGE   <>  ) OF STRING     ( 1      TO  8 );
  TYPE UV1 IS ARRAY (INTEGER RANGE   <>  ) OF BIT_VECTOR ( 3  DOWNTO  0 );
  TYPE UU1 IS ARRAY (INTEGER RANGE   <>  ) OF U1         ('a'     TO 'd');
  TYPE UC1 IS ARRAY (INTEGER RANGE   <>  ) OF C1;
  TYPE UR1 IS ARRAY (INTEGER RANGE   <>  ) OF R1;
  
  TYPE CS1 IS ARRAY (INTEGER RANGE 0 TO 3) OF STRING     ( 1      TO  8 );
  TYPE CV1 IS ARRAY (INTEGER RANGE 0 TO 3) OF BIT_VECTOR ( 3  DOWNTO  0 );
  TYPE CU1 IS ARRAY (INTEGER RANGE 0 TO 3) OF U1         ('a'     TO 'd');
  TYPE CC1 IS ARRAY (INTEGER RANGE 0 TO 3) OF C1;
  TYPE CR1 IS ARRAY (INTEGER RANGE 0 TO 3) OF R1;
  
  TYPE RAR IS RECORD
                eS1 : STRING     ( 1      TO  8 );
                eV1 : BIT_VECTOR ( 3  DOWNTO  0 );
                eU1 : U1         ('a'     TO 'd');
                eC1 : C1                         ;
                eR1 : R1                         ;
              END RECORD;
----------------------------------------------------------------------------------------
--
--          CONSTANT declarations - initial aggregate value
--           NOTE: index constraints for the unconstrained types are
--                  established by the intial value.
--
  CONSTANT US1_con_1 : US1          := (
    (NUL, SOH, STX, ETX, EOT, ENQ, ACK, BEL),
    (BS,  HT,  LF,  VT,  FF,  CR,  SO,  SI ),
    (DLE, DC1, DC2, DC3, DC4, NAK, SYN, ETB),
    (CAN, EM,  SUB, ESC, FSP, GSP, RSP, USP)
    );
  
  CONSTANT UV1_con_1 : UV1          := (
    ('0', '1', '0', '0'),
    ('1', '0', '1', '1'),
    ('1', '0', '0', '0'),
    ('0', '1', '0', '1')
    );
  
  CONSTANT UU1_con_1 : UU1          := (
    (  1,   2,   3,   4),
    (  5,   6,   7,   8),
    (  9,  10,  11,  12),
    ( 13,  14,  15,  16)
    );
  
  CONSTANT UC1_con_1 : UC1          := (
    ('0', '1', '0', '0', '1'),
    ('0', '1', '1', '1', '0'),
    ('0', '0', '0', '1', '0'),
    ('1', '0', '0', '0', '1')
    );
  
  CONSTANT UR1_con_1 : UR1          := (   (Feb,05,1701),
                                           (Apr,10,1802),
                                           (Jun,15,1903),
                                           (Aug,20,2004)  );
  
  CONSTANT CS1_con_1 : CS1          := (
    (NUL, SOH, STX, ETX, EOT, ENQ, ACK, BEL),
    (BS,  HT,  LF,  VT,  FF,  CR,  SO,  SI ),
    (DLE, DC1, DC2, DC3, DC4, NAK, SYN, ETB),
    (CAN, EM,  SUB, ESC, FSP, GSP, RSP, USP)

    );
  
  CONSTANT CV1_con_1 : CV1          := (
    ('0', '1', '0', '0'),
    ('1', '0', '1', '1'),
    ('1', '0', '0', '0'),
    ('0', '1', '0', '1')
    );

  CONSTANT CU1_con_1 : CU1          := (
    (  1,   2,   3,   4),
    (  5,   6,   7,   8),
    (  9,  10,  11,  12),
    ( 13,  14,  15,  16)
    );
  
  CONSTANT CC1_con_1 : CC1          := (
    ('0', '1', '0', '0', '1'),
    ('0', '1', '1', '1', '0'),
    ('0', '0', '0', '1', '0'),
    ('1', '0', '0', '0', '1')
    );
  
  CONSTANT CR1_con_1 : CR1          := ( (Feb,05,1701),
                                         (Apr,10,1802),
                                         (Jun,15,1903),
                                         (Aug,20,2004)  );
  
  CONSTANT RAR_con_1 : RAR          := (
    (SOH, STX, ETX, EOT, ENQ, ACK, BEL, BS ),
    ('1', '1', '0', '0'),
    (  1,   2,   3,   4),
    ('0', '1', '0', '0', '1'),
    (Feb,29,0108) );
  
--
--          CONSTANT declarations - aggregate of strings initial value
--
  CONSTANT US1_con_2 : US1          := ( "@ABCDEFG", "HIJKLMNO", "PQRSTUVW", "XYZ[\]^_" );
  CONSTANT UV1_con_2 : UV1          := (    B"0100",    B"1011",    B"1000",    B"0101" );
  CONSTANT UC1_con_2 : UC1          := (   B"01001",   B"01110",   B"00010",   B"10001" );
  
  CONSTANT CS1_con_2 : CS1          := ( "@ABCDEFG", "HIJKLMNO", "PQRSTUVW", "XYZ[\]^_" );
  CONSTANT CV1_con_2 : CV1          := (    B"0100",    B"1011",    B"1000",    B"0101" );
  CONSTANT CC1_con_2 : CC1          := (   B"01001",   B"01110",   B"00010",   B"10001" );
  
  CONSTANT RAR_con_2 : RAR          := ( "@ABCDEFG",    B"1100",  (1,2,3,4),  B"01001", (Feb,29,0108) );
  
  
-----------------------------------------------------------------------------------------
BEGIN
  TESTING: PROCESS
--
--          Declarationi for generation of BIT test pattern
--
    VARIABLE bval  : BIT;
    VARIABLE index : INTEGER;
    VARIABLE ii    : INTEGER;

    variable k    : integer := 0;
    
    PROCEDURE pattern ( index : INOUT INTEGER; bval : OUT BIT ) IS
--
--                if starting index value is 59, the
--                test pattern is 01001011100001010001111 (repeats)
--
    BEGIN
      IF index > 100
      THEN  bval  := '1';
            index := index - 100;
      ELSE  bval  := '0';
      END IF;
      index := index * 2;
    END;

  BEGIN

----------------------------------------------------------------------------------------
--
--              Verify initial values
--
    FOR I IN 0 TO 3 LOOP   ii := INTEGER'LEFT + I;
                           FOR J IN 1 TO 8 LOOP
                             if (US1_con_1(ii)(J) /= CHARACTER'VAL((I*8)+(J-1))) then
                               k := 1;
                             end if;
                             ASSERT US1_con_1(ii)(J) = CHARACTER'VAL((I*8)+(J-1))
                               REPORT "ERROR: Bad initial value of US1_con_1" SEVERITY FAILURE;
                           END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP  ii := INTEGER'LEFT + I;
                          FOR J IN 3 DOWNTO  0 LOOP
                            pattern ( index, bval );
                            if (UV1_con_1(ii)(J) /= bval) then
                              k := 1;
                            end if;
                            ASSERT UV1_con_1(ii)(J) = bval
                              REPORT "ERROR: Bad initial value of UV1_con_1" SEVERITY FAILURE;
                          END LOOP;
    END LOOP;
    
    index := 0;
    FOR I IN 0 TO 3 LOOP  ii := INTEGER'LEFT + I;
                          FOR J IN 'a' TO 'd' LOOP
                            index := index + 1;
                            if (UU1_con_1(ii)(J) /= index) then
                              k := 1;
                            end if;
                            ASSERT UU1_con_1(ii)(J) = index
                              REPORT "ERROR: Bad initial value of UU1_con_1" SEVERITY FAILURE;
                          END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP  ii := INTEGER'LEFT + I;
                          FOR J IN 5 TO 9 LOOP
                            pattern ( index, bval );
                            if (UC1_con_1(ii)(J) /= bval) then
                              k := 1;
                            end if;
                            ASSERT UC1_con_1(ii)(J) = bval
                              REPORT "ERROR: Bad initial value of UC1_con_1" SEVERITY FAILURE;
                          END LOOP;
    END LOOP;
    
    FOR I IN 0 TO 3 LOOP  ii := INTEGER'LEFT + I;
                          if (UR1_con_1(ii).month /= month_name'VAL((I*2)+1)) then
                            k := 1;
                          end if;
                          ASSERT UR1_con_1(ii).month = month_name'VAL((I*2)+1)
                            REPORT "ERROR: Bad initial value of UR1_con_1(ii).month" SEVERITY FAILURE;
                          if (UR1_con_1(ii).day /= I*5 +5) then
                            k := 1;
                          end if;
                          ASSERT UR1_con_1(ii).day = I*5 + 5
                            REPORT "ERROR: Bad initial value of UR1_con_1(ii).day" SEVERITY FAILURE;
                          if (UR1_con_1(ii).year /= 1701 +(I*101)) then
                            k := 1;
                          end if;
                          ASSERT UR1_con_1(ii).year = 1701 + (I*101)
                            REPORT "ERROR: Bad initial value of UR1_con_1(ii).year" SEVERITY FAILURE;
    END LOOP;
    
--
    FOR I IN 0 TO 3 LOOP
      FOR J IN 1 TO 8 LOOP
        if (CS1_con_1(I)(J) /= CHARACTER'VAL((I*8)+(J-1))) then
          k := 1;
        end if;
        ASSERT CS1_con_1(I)(J) = CHARACTER'VAL((I*8)+(J-1))
          REPORT "ERROR: Bad initial value of CS1_con_1" SEVERITY FAILURE;
      END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP
      FOR J IN 3 DOWNTO  0 LOOP
        pattern ( index, bval );
        if (CV1_con_1(I)(J) /= bval) then
          k := 1;
        end if;
        ASSERT CV1_con_1(I)(J) = bval
          REPORT "ERROR: Bad initial value of CV1_con_1" SEVERITY FAILURE;
      END LOOP;
    END LOOP;
    
    index := 0;
    FOR I IN 0 TO 3 LOOP
      FOR J IN 'a' TO 'd' LOOP
        index := index + 1;
        if (CU1_con_1(I)(J) /= index) then
          k := 1;
        end if;
        ASSERT CU1_con_1(I)(J) = index
          REPORT "ERROR: Bad initial value of CU1_con_1" SEVERITY FAILURE;
      END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP
      FOR J IN 5 TO 9 LOOP
        pattern ( index, bval );
        if (CC1_con_1(I)(J) /= bval) then
          k := 1;
        end if;
        ASSERT CC1_con_1(I)(J) = bval
          REPORT "ERROR: Bad initial value of CC1_con_1" SEVERITY FAILURE;
      END LOOP;
    END LOOP;
    
    FOR I IN 0 TO 3 LOOP
      if (CR1_con_1(I).month /= month_name'VAL((I*2)+1)) then
        k := 1;
      end if;
      ASSERT CR1_con_1(I).month = month_name'VAL((I*2)+1)
        REPORT "ERROR: Bad initial value of CR1_con_1(I).month" SEVERITY FAILURE;
      if (CR1_con_1(I).day /= (I+1)*5) then
        k := 1;
      end if;
      ASSERT CR1_con_1(I).day = (I+1)*5
        REPORT "ERROR: Bad initial value of CR1_con_1(I).day" SEVERITY FAILURE;
      if (CR1_con_1(I).year /= 1701 + (I*101)) then
        k := 1;
      end if;
      ASSERT CR1_con_1(I).year = 1701 + (I*101)
        REPORT "ERROR: Bad initial value of CR1_con_1(I).year" SEVERITY FAILURE;
    END LOOP;
    
--
    FOR J IN 1 TO 8 LOOP
      if (RAR_con_1.eS1(J) /= CHARACTER'VAL(J)) then
        k := 1;
      end if;
      ASSERT RAR_con_1.eS1(J) = CHARACTER'VAL(J)
        REPORT "ERROR: Bad initial value of RAR_con_1.eS1" SEVERITY FAILURE;
    END LOOP;
    
    FOR J IN 3 DOWNTO  0 LOOP
      if (RAR_con_1.eV1(J) /= BIT'VAL(J/2)) then
        k := 1;
      end if;
      ASSERT RAR_con_1.eV1(J) = BIT'VAL(J/2)
        REPORT "ERROR: Bad initial value of RAR_con_1.eV1" SEVERITY FAILURE;
    END LOOP;
    
    index := 0;
    FOR J IN 'a' TO 'd' LOOP
      index := index + 1;
      if (RAR_con_1.eU1(J) /= index) then
        k := 1;
      end if;
      ASSERT RAR_con_1.eU1(J) = index
        REPORT "ERROR: Bad initial value of RAR_con_1.eU1" SEVERITY FAILURE;
    END LOOP;
    
    index := 59;
    FOR J IN 5 TO 9 LOOP
      pattern ( index, bval );
      if (RAR_con_1.eC1(J) /= bval) then
        k := 1;
      end if;
      ASSERT RAR_con_1.eC1(J) = bval
        REPORT "ERROR: Bad initial value of RAR_con_1.eC1" SEVERITY FAILURE;
    END LOOP;
    
    if (RAR_con_1.eR1.month /= FEB) then
      k := 1;
    end if;
    ASSERT RAR_con_1.eR1.month = FEB
      REPORT "ERROR: Bad initial value of RAR_con_1.eR1.month" SEVERITY FAILURE;
    if (RAR_con_1.eR1.day /= 29) then
      k := 1;
    end if;
    ASSERT RAR_con_1.eR1.day = 29
      REPORT "ERROR: Bad initial value of RAR_con_1.eR1.day" SEVERITY FAILURE;
    if (RAR_con_1.eR1.year /= 0108) then
      k := 1;
    end if;
    ASSERT RAR_con_1.eR1.year = 0108
      REPORT "ERROR: Bad initial value of RAR_con_1.eR1.year" SEVERITY FAILURE;
    
-- ----------------------------------------------------------------------------------
    FOR I IN 0 TO 3 LOOP  ii := INTEGER'LEFT + I;
                          FOR J IN 1 TO 8 LOOP
                            if (US1_con_2(ii)(J) /= CHARACTER'VAL((I*8)+(J-1)+64)) then
                              k := 1;
                            end if;
                            ASSERT US1_con_2(ii)(J) = CHARACTER'VAL((I*8)+(J-1)+64)
                              REPORT "ERROR: Bad initial value of US1_con_2" SEVERITY FAILURE;
                          END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP  ii := INTEGER'LEFT + I;
                          FOR J IN 3 DOWNTO  0 LOOP
                            pattern ( index, bval );
                            if (UV1_con_2(ii)(J) /= bval) then
                              k := 1;
                            end if;
                            ASSERT UV1_con_2(ii)(J) = bval
                              REPORT "ERROR: Bad initial value of UV1_con_2" SEVERITY FAILURE;
                          END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP  ii := INTEGER'LEFT + I;
                          FOR J IN 5 TO 9 LOOP
                            pattern ( index, bval );
                            if (UC1_con_2(ii)(J) /= bval) then
                              k := 1;
                            end if;
                            ASSERT UC1_con_2(ii)(J) = bval
                              REPORT "ERROR: Bad initial value of UC1_con_2" SEVERITY FAILURE;
                          END LOOP;
    END LOOP;
    
--
    FOR I IN 0 TO 3 LOOP
      FOR J IN 1 TO 8 LOOP
        if (CS1_con_2(I)(J) /= CHARACTER'VAL((I*8)+(J-1)+64)) then
          k := 1;
        end if;
        ASSERT CS1_con_2(I)(J) = CHARACTER'VAL((I*8)+(J-1)+64)
          REPORT "ERROR: Bad initial value of CS1_con_2" SEVERITY FAILURE;
      END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP
      FOR J IN 3 DOWNTO  0 LOOP
        pattern ( index, bval );
        if (CV1_con_2(I)(J) /= bval) then
          k := 1;
        end if;
        ASSERT CV1_con_2(I)(J) = bval
          REPORT "ERROR: Bad initial value of CV1_con_2" SEVERITY FAILURE;
      END LOOP;
    END LOOP;
    
    index := 59;
    FOR I IN 0 TO 3 LOOP
      FOR J IN 5 TO 9 LOOP
        pattern ( index, bval );
        if (CC1_con_2(I)(J) /= bval) then
          k := 1;
        end if;
        ASSERT CC1_con_2(I)(J) = bval
          REPORT "ERROR: Bad initial value of CC1_con_2" SEVERITY FAILURE;
      END LOOP;
    END LOOP;
    
--
    FOR J IN 1 TO 8 LOOP
      if (RAR_con_2.eS1(J) /= CHARACTER'VAL((J-1)+64)) then
        k := 1;
      end if;
      ASSERT RAR_con_2.eS1(J) = CHARACTER'VAL((J-1)+64)
        REPORT "ERROR: Bad initial value of RAR_con_2.eS1" SEVERITY FAILURE;
    END LOOP;
    
    FOR J IN 3 DOWNTO  0 LOOP
      if (RAR_con_2.eV1(J) /= BIT'VAL(J/2)) then
        k := 1;
      end if;
      ASSERT RAR_con_2.eV1(J) = BIT'VAL(J/2)
        REPORT "ERROR: Bad initial value of RAR_con_2.eV1" SEVERITY FAILURE;
    END LOOP;
    
    index := 0;
    FOR J IN 'a' TO 'd' LOOP
      index := index + 1;
      if (RAR_con_2.eU1(J) /= index) then
        k := 1;
      end if;
      ASSERT RAR_con_2.eU1(J) = index
        REPORT "ERROR: Bad initial value of RAR_con_2.eU1" SEVERITY FAILURE;
    END LOOP;
    
    index := 59;
    FOR J IN 5 TO 9 LOOP
      pattern ( index, bval );
      if (RAR_con_2.eC1(J) /= bval) then
        k := 1;
      end if;
      ASSERT RAR_con_2.eC1(J) = bval
        REPORT "ERROR: Bad initial value of RAR_con_2.eC1" SEVERITY FAILURE;
    END LOOP;
    
    if (RAR_con_2.eR1.month /= FEB) then
      k := 1;
    end if;
    ASSERT RAR_con_2.eR1.month = FEB
      REPORT "ERROR: Bad initial value of RAR_con_2.eR1.month" SEVERITY FAILURE;
    if (RAR_con_2.eR1.day /=29) then
      k := 1;
    end if;
    ASSERT RAR_con_2.eR1.day = 29
      REPORT "ERROR: Bad initial value of RAR_con_2.eR1.day" SEVERITY FAILURE;
    if (RAR_con_2.eR1.year /= 0108) then
      k := 1;
    end if;
    ASSERT RAR_con_2.eR1.year = 0108
      REPORT "ERROR: Bad initial value of RAR_con_1.eR1.year" SEVERITY
      FAILURE;
    
---------------------------------------------------------------------------------------------

    assert NOT(    k = 0  )
      report "***PASSED TEST: c04s03b01x01p01n01i00032"
      severity NOTE;
    assert (    k = 0  )
      report "***FAILED TEST:c04s03b01x01p01n01i00032 - A constant declares a constant of the specified type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x01p01n01i00032arch;
