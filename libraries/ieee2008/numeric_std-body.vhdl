-- -----------------------------------------------------------------
-- 
-- Copyright 2019 IEEE P1076 WG Authors
-- 
-- See the LICENSE file distributed with this work for copyright and
-- licensing information and the AUTHORS file.
-- 
-- This file to you under the Apache License, Version 2.0 (the "License").
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
--
--   Title     :  Standard VHDL Synthesis Packages
--             :  (NUMERIC_STD package body)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  IEEE DASC Synthesis Working Group,
--             :  Accellera VHDL-TC, and IEEE P1076 Working Group
--             :
--   Purpose   :  This package defines numeric types and arithmetic functions
--             :  for use with synthesis tools. Two numeric types are defined:
--             :  -- > UNRESOLVED_UNSIGNED: represents an UNSIGNED number
--             :       in vector form
--             :  -- > UNRESOLVED_SIGNED: represents a SIGNED number
--             :       in vector form
--             :  The base element type is type STD_ULOGIC.
--             :  Aliases U_UNSIGNED and U_SIGNED are defined for the types
--             :  UNRESOLVED_UNSIGNED and UNRESOLVED_SIGNED, respectively.
--             :  Two numeric subtypes are defined:
--             :  -- > UNSIGNED: represents UNSIGNED number in vector form
--             :  -- > SIGNED: represents a SIGNED number in vector form
--             :  The element subtypes are the same subtype as STD_LOGIC.
--             :  The leftmost bit is treated as the most significant bit.
--             :  Signed vectors are represented in two's complement form.
--             :  This package contains overloaded arithmetic operators on
--             :  the SIGNED and UNSIGNED types. The package also contains
--             :  useful type conversions functions, clock detection
--             :  functions, and other utility functions.
--             :
--             :  If any argument to a function is a null array, a null array
--             :  is returned (exceptions, if any, are noted individually).
--
--   Note      :  This package may be modified to include additional data
--             :  required by tools, but it must in no way change the
--             :  external interfaces or simulation behavior of the
--             :  description. It is permissible to add comments and/or
--             :  attributes to the package declarations, but not to change
--             :  or delete any original lines of the package declaration.
--             :  The package body may be changed only in accordance with
--             :  the terms of Clause 16 of this standard.
--             :
-- --------------------------------------------------------------------
-- $Revision: 1220 $
-- $Date: 2008-04-10 17:16:09 +0930 (Thu, 10 Apr 2008) $
-- --------------------------------------------------------------------

package body NUMERIC_STD is

  -- null range array constants

  constant NAU : UNRESOLVED_UNSIGNED (0 downto 1) := (others => '0');
  constant NAS : UNRESOLVED_SIGNED (0 downto 1)   := (others => '0');

  -- implementation controls

  constant NO_WARNING : BOOLEAN := false;  -- default to emit warnings

  -- =========================Local Subprograms =================================

  function SIGNED_NUM_BITS (ARG : INTEGER) return NATURAL is
    variable NBITS : NATURAL;
    variable N     : NATURAL;
  begin
    if ARG >= 0 then
      N := ARG;
    else
      N := -(ARG+1);
    end if;
    NBITS := 1;
    while N > 0 loop
      NBITS := NBITS+1;
      N := N / 2;
    end loop;
    return NBITS;
  end function SIGNED_NUM_BITS;

  function UNSIGNED_NUM_BITS (ARG : NATURAL) return NATURAL is
    variable NBITS : NATURAL;
    variable N     : NATURAL;
  begin
    N := ARG;
    NBITS := 1;
    while N > 1 loop
      NBITS := NBITS+1;
      N := N / 2;
    end loop;
    return NBITS;
  end function UNSIGNED_NUM_BITS;

  ------------------------------------------------------------------------

  -- this internal function computes the addition of two UNRESOLVED_UNSIGNED
  -- with input CARRY
  -- * the two arguments are of the same length

  function ADD_UNSIGNED (L, R : UNRESOLVED_UNSIGNED; C : STD_LOGIC)
    return UNRESOLVED_UNSIGNED
  is
    constant L_LEFT : INTEGER   := L'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is R;
    variable RESULT : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable CBIT   : STD_LOGIC := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT      := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end function ADD_UNSIGNED;

  -- this internal function computes the addition of two UNRESOLVED_SIGNED
  -- with input CARRY
  -- * the two arguments are of the same length

  function ADD_SIGNED (L, R : UNRESOLVED_SIGNED; C : STD_LOGIC)
    return UNRESOLVED_SIGNED
  is
    constant L_LEFT : INTEGER   := L'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_SIGNED(L_LEFT downto 0) is R;
    variable RESULT : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable CBIT   : STD_LOGIC := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT      := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end function ADD_SIGNED;

  -----------------------------------------------------------------------------

  -- this internal procedure computes UNSIGNED division
  -- giving the quotient and remainder.
  procedure DIVMOD (NUM, XDENOM : UNRESOLVED_UNSIGNED;
                    XQUOT, XREMAIN : out UNRESOLVED_UNSIGNED) is
    variable TEMP   : UNRESOLVED_UNSIGNED(NUM'length downto 0);
    variable QUOT   : UNRESOLVED_UNSIGNED(MAXIMUM(NUM'length, XDENOM'length)-1
                                          downto 0);
    alias DENOM     : UNRESOLVED_UNSIGNED(XDENOM'length-1 downto 0) is XDENOM;
    variable TOPBIT : INTEGER;
  begin
    TEMP   := "0"&NUM;
    QUOT   := (others => '0');
    TOPBIT := -1;
    for J in DENOM'range loop
      if DENOM(J) = '1' then
        TOPBIT := J;
        exit;
      end if;
    end loop;
    assert TOPBIT >= 0 report "NUMERIC_STD.DIVMOD: DIV, MOD, or REM by zero"
      severity error;

    for J in NUM'length-(TOPBIT+1) downto 0 loop
      if TEMP(TOPBIT+J+1 downto J) >= "0"&DENOM(TOPBIT downto 0) then
        TEMP(TOPBIT+J+1 downto J) := (TEMP(TOPBIT+J+1 downto J))
                                     -("0"&DENOM(TOPBIT downto 0));
        QUOT(J) := '1';
      end if;
      assert TEMP(TOPBIT+J+1) = '0'
        report "NUMERIC_STD.DIVMOD: internal error in the division algorithm"
        severity error;
    end loop;
    XQUOT   := RESIZE(QUOT, XQUOT'length);
    XREMAIN := RESIZE(TEMP, XREMAIN'length);
  end procedure DIVMOD;

  -----------------Local Subprograms - shift/rotate ops-------------------------

  function XSLL (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR
  is
    constant ARG_L  : INTEGER                           := ARG'length-1;
    alias XARG      : STD_ULOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : STD_ULOGIC_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L downto COUNT) := XARG(ARG_L-COUNT downto 0);
    end if;
    return RESULT;
  end function XSLL;

  function XSRL (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR
  is
    constant ARG_L  : INTEGER                           := ARG'length-1;
    alias XARG      : STD_ULOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : STD_ULOGIC_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L-COUNT downto 0) := XARG(ARG_L downto COUNT);
    end if;
    return RESULT;
  end function XSRL;

  function XSRA (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR
  is
    constant ARG_L  : INTEGER := ARG'length-1;
    alias XARG      : STD_ULOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : STD_ULOGIC_VECTOR(ARG_L downto 0);
    variable XCOUNT : NATURAL := COUNT;
  begin
    if ((ARG'length <= 1) or (XCOUNT = 0)) then return ARG;
    else
      if (XCOUNT > ARG_L) then XCOUNT           := ARG_L;
      end if;
      RESULT(ARG_L-XCOUNT downto 0)             := XARG(ARG_L downto XCOUNT);
      RESULT(ARG_L downto (ARG_L - XCOUNT + 1)) := (others => XARG(ARG_L));
    end if;
    return RESULT;
  end function XSRA;

  function XROL (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR
  is
    constant ARG_L  : INTEGER                           := ARG'length-1;
    alias XARG      : STD_ULOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : STD_ULOGIC_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM : INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L downto COUNTM) := XARG(ARG_L-COUNTM downto 0);
      RESULT(COUNTM-1 downto 0)   := XARG(ARG_L downto ARG_L-COUNTM+1);
    end if;
    return RESULT;
  end function XROL;

  function XROR (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR
  is
    constant ARG_L  : INTEGER                           := ARG'length-1;
    alias XARG      : STD_ULOGIC_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : STD_ULOGIC_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM : INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L-COUNTM downto 0)       := XARG(ARG_L downto COUNTM);
      RESULT(ARG_L downto ARG_L-COUNTM+1) := XARG(COUNTM-1 downto 0);
    end if;
    return RESULT;
  end function XROR;

  -----------------Local Subprograms - Relational ops---------------------------

  --
  -- General "=" for UNRESOLVED_UNSIGNED vectors, same length
  --
  function UNSIGNED_EQUAL (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
  begin
    return STD_ULOGIC_VECTOR(L) = STD_ULOGIC_VECTOR(R);
  end function UNSIGNED_EQUAL;

  --
  -- General "=" for UNRESOLVED_SIGNED vectors, same length
  --
  function SIGNED_EQUAL (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
  begin
    return STD_ULOGIC_VECTOR(L) = STD_ULOGIC_VECTOR(R);
  end function SIGNED_EQUAL;

  --
  -- General "<" for UNRESOLVED_UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
  begin
    return STD_ULOGIC_VECTOR(L) < STD_ULOGIC_VECTOR(R);
  end function UNSIGNED_LESS;

  --
  -- General "<" function for UNRESOLVED_SIGNED vectors, same length
  --
  function SIGNED_LESS (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    variable INTERN_L : UNRESOLVED_SIGNED(0 to L'length-1);
    variable INTERN_R : UNRESOLVED_SIGNED(0 to R'length-1);
  begin
    INTERN_L    := L;
    INTERN_R    := R;
    INTERN_L(0) := not INTERN_L(0);
    INTERN_R(0) := not INTERN_R(0);
    return STD_ULOGIC_VECTOR(INTERN_L) < STD_ULOGIC_VECTOR(INTERN_R);
  end function SIGNED_LESS;

  --
  -- General "<=" function for UNRESOLVED_UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS_OR_EQUAL (L, R : UNRESOLVED_UNSIGNED)
    return BOOLEAN is
  begin
    return STD_ULOGIC_VECTOR(L) <= STD_ULOGIC_VECTOR(R);
  end function UNSIGNED_LESS_OR_EQUAL;

  --
  -- General "<=" function for UNRESOLVED_SIGNED vectors, same length
  --
  function SIGNED_LESS_OR_EQUAL (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    -- Need aliases to assure index direction
    variable INTERN_L : UNRESOLVED_SIGNED(0 to L'length-1);
    variable INTERN_R : UNRESOLVED_SIGNED(0 to R'length-1);
  begin
    INTERN_L                           := L;
    INTERN_R                           := R;
    INTERN_L(0)                        := not INTERN_L(0);
    INTERN_R(0)                        := not INTERN_R(0);
    return STD_ULOGIC_VECTOR(INTERN_L) <= STD_ULOGIC_VECTOR(INTERN_R);
  end function SIGNED_LESS_OR_EQUAL;

  -- =========================Exported Functions ==========================

  -- Id: A.1
  function "abs" (ARG : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant ARG_LEFT : INTEGER := ARG'length-1;
    alias XARG        : UNRESOLVED_SIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT   : UNRESOLVED_SIGNED(ARG_LEFT downto 0);
  begin
    if ARG'length < 1 then return NAS;
    end if;
    RESULT := TO_01(XARG, 'X');
    if (RESULT(RESULT'left) = 'X') then return RESULT;
    end if;
    if RESULT(RESULT'left) = '1' then
      RESULT := -RESULT;
    end if;
    return RESULT;
  end function "abs";

  -- Id: A.2
  function "-" (ARG : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant ARG_LEFT       : INTEGER   := ARG'length-1;
    variable RESULT, XARG01 : UNRESOLVED_SIGNED(ARG_LEFT downto 0);
    variable CBIT           : STD_LOGIC := '1';
  begin
    if ARG'length < 1 then return NAS;
    end if;
    XARG01 := TO_01(ARG, 'X');
    if (XARG01(XARG01'left) = 'X') then return XARG01;
    end if;
    for I in 0 to RESULT'left loop
      RESULT(I) := not(XARG01(I)) xor CBIT;
      CBIT      := CBIT and not(XARG01(I));
    end loop;
    return RESULT;
  end function "-";

  -- ============================================================================

  -- Id: A.3
  function "+" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    return ADD_UNSIGNED(L01, R01, '0');
  end function "+";

  -- Id: A.3R
  function "+" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED
  is
    variable XR : UNRESOLVED_UNSIGNED(L'length-1 downto 0) := (others => '0');
  begin
    XR(0) := R;
    return (L + XR);
  end function "+";

  -- Id: A.3L
  function "+" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED
  is
    variable XL : UNRESOLVED_UNSIGNED(R'length-1 downto 0) := (others => '0');
  begin
    XL(0) := L;
    return (XL + R);
  end function "+";

  -- Id: A.4
  function "+" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    return ADD_SIGNED(L01, R01, '0');
  end function "+";

  -- Id: A.4R
  function "+" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED
  is
    variable XR : UNRESOLVED_SIGNED(L'length-1 downto 0) := (others => '0');
  begin
    XR(0) := R;
    return (L + XR);
  end function "+";

  -- Id: A.4L
  function "+" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED
  is
    variable XL : UNRESOLVED_SIGNED(R'length-1 downto 0) := (others => '0');
  begin
    XL(0) := L;
    return (XL + R);
  end function "+";

  -- Id: A.5
  function "+" (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    return L + TO_UNSIGNED(R, L'length);
  end function "+";

  -- Id: A.6
  function "+" (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'length) + R;
  end function "+";

  -- Id: A.7
  function "+" (L : UNRESOLVED_SIGNED; R : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    return L + TO_SIGNED(R, L'length);
  end function "+";

  -- Id: A.8
  function "+" (L : INTEGER; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return TO_SIGNED(L, R'length) + R;
  end function "+";

  -- ============================================================================

  -- Id: A.9
  function "-" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    return ADD_UNSIGNED(L01, not(R01), '1');
  end function "-";

  -- Id: A.9R
  function "-" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED
  is
    variable XR : UNRESOLVED_UNSIGNED(L'length-1 downto 0) := (others => '0');
  begin
    XR(0) := R;
    return (L - XR);
  end function "-";

  -- Id: A.9L
  function "-" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED
  is
    variable XL : UNRESOLVED_UNSIGNED(R'length-1 downto 0) := (others => '0');
  begin
    XL(0) := L;
    return (XL - R);
  end function "-";

  -- Id: A.10
  function "-" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    return ADD_SIGNED(L01, not(R01), '1');
  end function "-";

  -- Id: A.10R
  function "-" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED
  is
    variable XR : UNRESOLVED_SIGNED(L'length-1 downto 0) := (others => '0');
  begin
    XR(0) := R;
    return (L - XR);
  end function "-";

  -- Id: A.10L
  function "-" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED
  is
    variable XL : UNRESOLVED_SIGNED(R'length-1 downto 0) := (others => '0');
  begin
    XL(0) := L;
    return (XL - R);
  end function "-";

  -- Id: A.11
  function "-" (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    return L - TO_UNSIGNED(R, L'length);
  end function "-";

  -- Id: A.12
  function "-" (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'length) - R;
  end function "-";

  -- Id: A.13
  function "-" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED is
  begin
    return L - TO_SIGNED(R, L'length);
  end function "-";

  -- Id: A.14
  function "-" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
  begin
    return TO_SIGNED(L, R'length) - R;
  end function "-";

  -- ============================================================================

  -- Id: A.15
  function "*" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XXL       : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XXR       : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable XL     : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable XR     : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
    variable RESULT : UNRESOLVED_UNSIGNED((L'length+R'length-1) downto 0) :=
      (others => '0');
    variable ADVAL : UNRESOLVED_UNSIGNED((L'length+R'length-1) downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'left) = 'X') or (XR(XR'left) = 'X')) then
      RESULT := (others => 'X');
      return RESULT;
    end if;
    ADVAL := RESIZE(XR, RESULT'length);
    for I in 0 to L_LEFT loop
      if XL(I) = '1' then RESULT := RESULT + ADVAL;
      end if;
      ADVAL                      := SHIFT_LEFT(ADVAL, 1);
    end loop;
    return RESULT;
  end function "*";

  -- Id: A.16
  function "*" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    variable XL     : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable XR     : UNRESOLVED_SIGNED(R_LEFT downto 0);
    variable RESULT : UNRESOLVED_SIGNED((L_LEFT+R_LEFT+1) downto 0) :=
      (others => '0');
    variable ADVAL  : UNRESOLVED_SIGNED((L_LEFT+R_LEFT+1) downto 0);
  begin
    if ((L_LEFT < 0) or (R_LEFT < 0)) then return NAS;
    end if;
    XL := TO_01(L, 'X');
    XR := TO_01(R, 'X');
    if ((XL(L_LEFT) = 'X') or (XR(R_LEFT) = 'X')) then
      RESULT := (others => 'X');
      return RESULT;
    end if;
    ADVAL := RESIZE(XR, RESULT'length);
    for I in 0 to L_LEFT-1 loop
      if XL(I) = '1' then RESULT := RESULT + ADVAL;
      end if;
      ADVAL                      := SHIFT_LEFT(ADVAL, 1);
    end loop;
    if XL(L_LEFT) = '1' then
      RESULT := RESULT - ADVAL;
    end if;
    return RESULT;
  end function "*";

  -- Id: A.17
  function "*" (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    return L * TO_UNSIGNED(R, L'length);
  end function "*";

  -- Id: A.18
  function "*" (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'length) * R;
  end function "*";

  -- Id: A.19
  function "*" (L : UNRESOLVED_SIGNED; R : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    return L * TO_SIGNED(R, L'length);
  end function "*";

  -- Id: A.20
  function "*" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
  begin
    return TO_SIGNED(L, R'length) * R;
  end function "*";

  -- ============================================================================

  -- Id: A.21
  function "/" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant L_LEFT  : INTEGER := L'length-1;
    constant R_LEFT  : INTEGER := R'length-1;
    alias XXL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XXR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable XL      : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable XR      : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
    variable FQUOT   : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'left) = 'X') or (XR(XR'left) = 'X')) then
      FQUOT := (others => 'X');
      return FQUOT;
    end if;
    DIVMOD(XL, XR, FQUOT, FREMAIN);
    return FQUOT;
  end function "/";

  -- Id: A.22
  function "/" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant L_LEFT  : INTEGER := L'length-1;
    constant R_LEFT  : INTEGER := R'length-1;
    alias XXL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XXR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable XL      : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable XR      : UNRESOLVED_SIGNED(R_LEFT downto 0);
    variable FQUOT   : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
    variable XNUM    : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable XDENOM  : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
    variable QNEG    : BOOLEAN := false;
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'left) = 'X') or (XR(XR'left) = 'X')) then
      FQUOT := (others => 'X');
      return UNRESOLVED_SIGNED(FQUOT);
    end if;
    if XL(XL'left) = '1' then
      XNUM := UNRESOLVED_UNSIGNED(-XL);
      QNEG := true;
    else
      XNUM := UNRESOLVED_UNSIGNED(XL);
    end if;
    if XR(XR'left) = '1' then
      XDENOM := UNRESOLVED_UNSIGNED(-XR);
      QNEG   := not QNEG;
    else
      XDENOM := UNRESOLVED_UNSIGNED(XR);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if QNEG then FQUOT := "0"-FQUOT;
    end if;
    return UNRESOLVED_SIGNED(FQUOT);
  end function "/";

  -- Id: A.23
  function "/" (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED
  is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, UNSIGNED_NUM_BITS(R));
    variable XR, QUOT : UNRESOLVED_UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAU;
    end if;
    if (R_LENGTH > L'length) then
      QUOT := (others => '0');
      return RESIZE(QUOT, L'length);
    end if;
    XR   := TO_UNSIGNED(R, R_LENGTH);
    QUOT := RESIZE((L / XR), QUOT'length);
    return RESIZE(QUOT, L'length);
  end function "/";

  -- Id: A.24
  function "/" (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED
  is
    constant L_LENGTH : NATURAL := MAXIMUM(UNSIGNED_NUM_BITS(L), R'length);
    variable XL, QUOT : UNRESOLVED_UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAU;
    end if;
    XL   := TO_UNSIGNED(L, L_LENGTH);
    QUOT := RESIZE((XL / R), QUOT'length);
    if L_LENGTH > R'length and QUOT(0) /= 'X'
      and QUOT(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => '0')
    then
      assert NO_WARNING report "NUMERIC_STD.""/"": Quotient Truncated"
        severity warning;
    end if;
    return RESIZE(QUOT, R'length);
  end function "/";

  -- Id: A.25
  function "/" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, SIGNED_NUM_BITS(R));
    variable XR, QUOT : UNRESOLVED_SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAS;
    end if;
    if (R_LENGTH > L'length) then
      QUOT := (others => '0');
      return RESIZE(QUOT, L'length);
    end if;
    XR   := TO_SIGNED(R, R_LENGTH);
    QUOT := RESIZE((L / XR), QUOT'length);
    return RESIZE(QUOT, L'length);
  end function "/";

  -- Id: A.26
  function "/" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant L_LENGTH : NATURAL := MAXIMUM(SIGNED_NUM_BITS(L), R'length);
    variable XL, QUOT : UNRESOLVED_SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAS;
    end if;
    XL   := TO_SIGNED(L, L_LENGTH);
    QUOT := RESIZE((XL / R), QUOT'length);
    if L_LENGTH > R'length and QUOT(0) /= 'X'
      and QUOT(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => QUOT(R'length-1))
    then
      assert NO_WARNING report "NUMERIC_STD.""/"": Quotient Truncated"
        severity warning;
    end if;
    return RESIZE(QUOT, R'length);
  end function "/";

  -- ============================================================================

  -- Id: A.27
  function "rem" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant L_LEFT  : INTEGER := L'length-1;
    constant R_LEFT  : INTEGER := R'length-1;
    alias XXL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XXR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable XL      : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable XR      : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
    variable FQUOT   : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'left) = 'X') or (XR(XR'left) = 'X')) then
      FREMAIN := (others => 'X');
      return FREMAIN;
    end if;
    DIVMOD(XL, XR, FQUOT, FREMAIN);
    return FREMAIN;
  end function "rem";

  -- Id: A.28
  function "rem" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant L_LEFT  : INTEGER := L'length-1;
    constant R_LEFT  : INTEGER := R'length-1;
    alias XXL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XXR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable FQUOT   : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
    variable XNUM    : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable XDENOM  : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
    variable RNEG    : BOOLEAN := false;
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    XNUM   := UNRESOLVED_UNSIGNED(TO_01(XXL, 'X'));
    XDENOM := UNRESOLVED_UNSIGNED(TO_01(XXR, 'X'));
    if ((XNUM(XNUM'left) = 'X') or (XDENOM(XDENOM'left) = 'X')) then
      FREMAIN := (others => 'X');
      return UNRESOLVED_SIGNED(FREMAIN);
    end if;
    if XNUM(XNUM'left) = '1' then
      XNUM := UNRESOLVED_UNSIGNED(-UNRESOLVED_SIGNED(XNUM));
      RNEG := true;
    else
      XNUM := UNRESOLVED_UNSIGNED(XNUM);
    end if;
    if XDENOM(XDENOM'left) = '1' then
      XDENOM := UNRESOLVED_UNSIGNED(-UNRESOLVED_SIGNED(XDENOM));
    else
      XDENOM := UNRESOLVED_UNSIGNED(XDENOM);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if RNEG then
      FREMAIN := "0"-FREMAIN;
    end if;
    return UNRESOLVED_SIGNED(FREMAIN);
  end function "rem";

  -- Id: A.29
  function "rem" (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED
  is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, UNSIGNED_NUM_BITS(R));
    variable XR, XREM : UNRESOLVED_UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAU;
    end if;
    XR   := TO_UNSIGNED(R, R_LENGTH);
    XREM := L rem XR;
    if R_LENGTH > L'length and XREM(0) /= 'X'
      and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => '0')
    then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "rem";

  -- Id: A.30
  function "rem" (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED
  is
    constant L_LENGTH : NATURAL := MAXIMUM(UNSIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : UNRESOLVED_UNSIGNED(L_LENGTH-1 downto 0);
  begin
    XL   := TO_UNSIGNED(L, L_LENGTH);
    XREM := XL rem R;
    if L_LENGTH > R'length and XREM(0) /= 'X'
      and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => '0')
    then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "rem";

  -- Id: A.31
  function "rem" (L : UNRESOLVED_SIGNED; R : INTEGER)
    return UNRESOLVED_SIGNED
  is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, SIGNED_NUM_BITS(R));
    variable XR, XREM : UNRESOLVED_SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAS;
    end if;
    XR   := TO_SIGNED(R, R_LENGTH);
    XREM := RESIZE((L rem XR), XREM'length);
    if R_LENGTH > L'length and XREM(0) /= 'X'
      and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => XREM(L'length-1))
    then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "rem";

  -- Id: A.32
  function "rem" (L : INTEGER; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED
  is
    constant L_LENGTH : NATURAL := MAXIMUM(SIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : UNRESOLVED_SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAS;
    end if;
    XL   := TO_SIGNED(L, L_LENGTH);
    XREM := RESIZE((XL rem R), XREM'length);
    if L_LENGTH > R'length and XREM(0) /= 'X'
      and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => XREM(R'length-1))
    then
      assert NO_WARNING report "NUMERIC_STD.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "rem";

  -- ============================================================================

  -- Id: A.33
  function "mod" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant L_LEFT  : INTEGER := L'length-1;
    constant R_LEFT  : INTEGER := R'length-1;
    alias XXL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XXR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable XL      : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable XR      : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
    variable FQUOT   : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'left) = 'X') or (XR(XR'left) = 'X')) then
      FREMAIN := (others => 'X');
      return FREMAIN;
    end if;
    DIVMOD(XL, XR, FQUOT, FREMAIN);
    return FREMAIN;
  end function "mod";

  -- Id: A.34
  function "mod" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant L_LEFT  : INTEGER := L'length-1;
    constant R_LEFT  : INTEGER := R'length-1;
    alias XXL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XXR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable XL      : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable XR      : UNRESOLVED_SIGNED(R_LEFT downto 0);
    variable FQUOT   : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
    variable XNUM    : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
    variable XDENOM  : UNRESOLVED_UNSIGNED(R'length-1 downto 0);
    variable RNEG    : BOOLEAN := false;
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    XL := TO_01(XXL, 'X');
    XR := TO_01(XXR, 'X');
    if ((XL(XL'left) = 'X') or (XR(XR'left) = 'X')) then
      FREMAIN := (others => 'X');
      return UNRESOLVED_SIGNED(FREMAIN);
    end if;
    if XL(XL'left) = '1' then
      XNUM := UNRESOLVED_UNSIGNED(-XL);
    else
      XNUM := UNRESOLVED_UNSIGNED(XL);
    end if;
    if XR(XR'left) = '1' then
      XDENOM := UNRESOLVED_UNSIGNED(-XR);
      RNEG   := true;
    else
      XDENOM := UNRESOLVED_UNSIGNED(XR);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if RNEG and L(L'left) = '1' then
      FREMAIN := "0"-FREMAIN;
    elsif RNEG and FREMAIN /= "0" then
      FREMAIN := FREMAIN-XDENOM;
    elsif L(L'left) = '1' and FREMAIN /= "0" then
      FREMAIN := XDENOM-FREMAIN;
    end if;
    return UNRESOLVED_SIGNED(FREMAIN);
  end function "mod";

  -- Id: A.35
  function "mod" (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED
  is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, UNSIGNED_NUM_BITS(R));
    variable XR, XREM : UNRESOLVED_UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAU;
    end if;
    XR   := TO_UNSIGNED(R, R_LENGTH);
    XREM := RESIZE((L mod XR), XREM'length);
    if R_LENGTH > L'length and XREM(0) /= 'X'
      and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => '0')
    then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "mod";

  -- Id: A.36
  function "mod" (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED
  is
    constant L_LENGTH : NATURAL := MAXIMUM(UNSIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : UNRESOLVED_UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAU;
    end if;
    XL   := TO_UNSIGNED(L, L_LENGTH);
    XREM := RESIZE((XL mod R), XREM'length);
    if L_LENGTH > R'length and XREM(0) /= 'X'
      and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => '0')
    then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "mod";

  -- Id: A.37
  function "mod" (L : UNRESOLVED_SIGNED; R : INTEGER)
    return UNRESOLVED_SIGNED
  is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, SIGNED_NUM_BITS(R));
    variable XR, XREM : UNRESOLVED_SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAS;
    end if;
    XR   := TO_SIGNED(R, R_LENGTH);
    XREM := RESIZE((L mod XR), XREM'length);
    if R_LENGTH > L'length and XREM(0) /= 'X'
      and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => XREM(L'length-1))
    then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "mod";

  -- Id: A.38
  function "mod" (L : INTEGER; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED
  is
    constant L_LENGTH : NATURAL := MAXIMUM(SIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : UNRESOLVED_SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAS;
    end if;
    XL   := TO_SIGNED(L, L_LENGTH);
    XREM := RESIZE((XL mod R), XREM'length);
    if L_LENGTH > R'length and XREM(0) /= 'X'
      and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => XREM(R'length-1))
    then
      assert NO_WARNING report "NUMERIC_STD.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "mod";

  -- ============================================================================
  -- Id: A.39
  function find_leftmost (ARG : UNRESOLVED_UNSIGNED; Y : STD_ULOGIC)
    return INTEGER is
  begin
    for INDEX in ARG'range loop
      if ARG(INDEX) ?= Y then
        return INDEX;
      end if;
    end loop;
    return -1;
  end function find_leftmost;

  -- Id: A.40
  function find_leftmost (ARG : UNRESOLVED_SIGNED; Y : STD_ULOGIC)
    return INTEGER is
  begin
    for INDEX in ARG'range loop
      if ARG(INDEX) ?= Y then
        return INDEX;
      end if;
    end loop;
    return -1;
  end function find_leftmost;

  -- Id: A.41
  function find_rightmost (ARG : UNRESOLVED_UNSIGNED; Y : STD_ULOGIC)
    return INTEGER is
  begin
    for INDEX in ARG'reverse_range loop
      if ARG(INDEX) ?= Y then
        return INDEX;
      end if;
    end loop;
    return -1;
  end function find_rightmost;

  -- Id: A.42
  function find_rightmost (ARG : UNRESOLVED_SIGNED; Y : STD_ULOGIC)
    return INTEGER is
  begin
    for INDEX in ARG'reverse_range loop
      if ARG(INDEX) ?= Y then
        return INDEX;
      end if;
    end loop;
    return -1;
  end function find_rightmost;

  -- ============================================================================

  -- Id: C.1
  function ">" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function ">";

  -- Id: C.2
  function ">" (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not SIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function ">";

  -- Id: C.3
  function ">" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return true;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R01'length), R01);
  end function ">";

  -- Id: C.4
  function ">" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L > 0;
    end if;
    return not SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R01'length), R01);
  end function ">";

  -- Id: C.5
  function ">" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return false;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(L01, TO_UNSIGNED(R, L01'length));
  end function ">";

  -- Id: C.6
  function ">" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 > R;
    end if;
    return not SIGNED_LESS_OR_EQUAL(L01, TO_SIGNED(R, L01'length));
  end function ">";

  -- ============================================================================

  -- Id: C.7
  function "<" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return UNSIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function "<";

  -- Id: C.8
  function "<" (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return SIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function "<";

  -- Id: C.9
  function "<" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return UNSIGNED_LESS(TO_UNSIGNED(L, R01'length), R01);
  end function "<";

  -- Id: C.10
  function "<" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return SIGNED_LESS(TO_SIGNED(L, R01'length), R01);
  end function "<";

  -- Id: C.11
  function "<" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return UNSIGNED_LESS(L01, TO_UNSIGNED(R, L01'length));
  end function "<";

  -- Id: C.12
  function "<" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return SIGNED_LESS(L01, TO_SIGNED(R, L01'length));
  end function "<";

  -- ============================================================================

  -- Id: C.13
  function "<=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function "<=";

  -- Id: C.14
  function "<=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return SIGNED_LESS_OR_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function "<=";

  -- Id: C.15
  function "<=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R01'length), R01);
  end function "<=";

  -- Id: C.16
  function "<=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R01'length), R01);
  end function "<=";

  -- Id: C.17
  function "<=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
  begin
    if (L_LEFT < 0) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(L01, TO_UNSIGNED(R, L01'length));
  end function "<=";

  -- Id: C.18
  function "<=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
  begin
    if (L_LEFT < 0) then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return SIGNED_LESS_OR_EQUAL(L01, TO_SIGNED(R, L01'length));
  end function "<=";

  -- ============================================================================

  -- Id: C.19
  function ">=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not UNSIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function ">=";

  -- Id: C.20
  function ">=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not SIGNED_LESS(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function ">=";

  -- Id: C.21
  function ">=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return L > 0;
    end if;
    return not UNSIGNED_LESS(TO_UNSIGNED(L, R01'length), R01);
  end function ">=";

  -- Id: C.22
  function ">=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L > 0;
    end if;
    return not SIGNED_LESS(TO_SIGNED(L, R01'length), R01);
  end function ">=";

  -- Id: C.23
  function ">=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return 0 > R;
    end if;
    return not UNSIGNED_LESS(L01, TO_UNSIGNED(R, L01'length));
  end function ">=";

  -- Id: C.24
  function ">=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD."">="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 > R;
    end if;
    return not SIGNED_LESS(L01, TO_SIGNED(R, L01'length));
  end function ">=";

  -- ============================================================================

  -- Id: C.25
  function "=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return UNSIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function "=";

  -- Id: C.26
  function "=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return SIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE));
  end function "=";

  -- Id: C.27
  function "=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return false;
    end if;
    return UNSIGNED_EQUAL(TO_UNSIGNED(L, R01'length), R01);
  end function "=";

  -- Id: C.28
  function "=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return false;
    end if;
    return SIGNED_EQUAL(TO_SIGNED(L, R01'length), R01);
  end function "=";

  -- Id: C.29
  function "=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return false;
    end if;
    return UNSIGNED_EQUAL(L01, TO_UNSIGNED(R, L01'length));
  end function "=";

  -- Id: C.30
  function "=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return false;
    end if;
    return SIGNED_EQUAL(L01, TO_SIGNED(R, L01'length));
  end function "=";

  -- ============================================================================

  -- Id: C.31
  function "/=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    return not(UNSIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE)));
  end function "/=";

  -- Id: C.32
  function "/=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    constant R_LEFT : INTEGER := R'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    L01 := TO_01(XL, 'X');
    R01 := TO_01(XR, 'X');
    if ((L01(L01'left) = 'X') or (R01(R01'left) = 'X')) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    return not(SIGNED_EQUAL(RESIZE(L01, SIZE), RESIZE(R01, SIZE)));
  end function "/=";

  -- Id: C.33
  function "/=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_UNSIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return true;
    end if;
    return not(UNSIGNED_EQUAL(TO_UNSIGNED(L, R01'length), R01));
  end function "/=";

  -- Id: C.34
  function "/=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN is
    constant R_LEFT : INTEGER := R'length-1;
    alias XR        : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    variable R01    : UNRESOLVED_SIGNED(R_LEFT downto 0);
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    R01 := TO_01(XR, 'X');
    if (R01(R01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return true;
    end if;
    return not(SIGNED_EQUAL(TO_SIGNED(L, R01'length), R01));
  end function "/=";

  -- Id: C.35
  function "/=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_UNSIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return true;
    end if;
    return not(UNSIGNED_EQUAL(L01, TO_UNSIGNED(R, L01'length)));
  end function "/=";

  -- Id: C.36
  function "/=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    variable L01    : UNRESOLVED_SIGNED(L_LEFT downto 0);
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    L01 := TO_01(XL, 'X');
    if (L01(L01'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.""/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return true;
    end if;
    return not(SIGNED_EQUAL(L01, TO_SIGNED(R, L01'length)));
  end function "/=";

  -- ============================================================================

  -- Id: C.37
  function MINIMUM (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    if UNSIGNED_LESS(L01, R01) then
      return L01;
    else
      return R01;
    end if;
  end function MINIMUM;

  -- Id: C.38
  function MINIMUM (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    if SIGNED_LESS(L01, R01) then
      return L01;
    else
      return R01;
    end if;
  end function MINIMUM;

  -- Id: C.39
  function MINIMUM (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return MINIMUM(TO_UNSIGNED(L, R'length), R);
  end function MINIMUM;

  -- Id: C.40
  function MINIMUM (L : INTEGER; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return MINIMUM(TO_SIGNED(L, R'length), R);
  end function MINIMUM;

  -- Id: C.41
  function MINIMUM (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    return MINIMUM(L, TO_UNSIGNED(R, L'length));
  end function MINIMUM;

  -- Id: C.42
  function MINIMUM (L : UNRESOLVED_SIGNED; R : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    return MINIMUM(L, TO_SIGNED(R, L'length));
  end function MINIMUM;

  -- ============================================================================

  -- Id: C.43
  function MAXIMUM (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    if UNSIGNED_LESS(L01, R01) then
      return R01;
    else
      return L01;
    end if;
  end function MAXIMUM;

  -- Id: C.44
  function MAXIMUM (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
    variable L01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable R01  : UNRESOLVED_SIGNED(SIZE-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    L01 := TO_01(RESIZE(L, SIZE), 'X');
    if (L01(L01'left) = 'X') then return L01;
    end if;
    R01 := TO_01(RESIZE(R, SIZE), 'X');
    if (R01(R01'left) = 'X') then return R01;
    end if;
    if SIGNED_LESS(L01, R01) then
      return R01;
    else
      return L01;
    end if;
  end function MAXIMUM;

  -- Id: C.45
  function MAXIMUM (L : NATURAL; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return MAXIMUM(TO_UNSIGNED(L, R'length), R);
  end function MAXIMUM;

  -- Id: C.46
  function MAXIMUM (L : INTEGER; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return MAXIMUM(TO_SIGNED(L, R'length), R);
  end function MAXIMUM;

  -- Id: C.47
  function MAXIMUM (L : UNRESOLVED_UNSIGNED; R : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    return MAXIMUM(L, TO_UNSIGNED(R, L'length));
  end function MAXIMUM;

  -- Id: C.48
  function MAXIMUM (L : UNRESOLVED_SIGNED; R : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    return MAXIMUM(L, TO_SIGNED(R, L'length));
  end function MAXIMUM;

  -- ============================================================================

  -- Id: C.49
  function "?>" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?>"": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?>"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?>"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L > R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?>";

  -- Id: C.50
  function "?>" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?>"": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?>"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?>"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L > R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?>";

  -- Id: C.51
  function "?>" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return TO_UNSIGNED(L, R'length) ?> R;
  end function "?>";

  -- Id: C.52
  function "?>" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return TO_SIGNED(L, R'length) ?> R;
  end function "?>";

  -- Id: C.53
  function "?>" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC is
  begin
    return L ?> TO_UNSIGNED(R, L'length);
  end function "?>";

  -- Id: C.54
  function "?>" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC is
  begin
    return L ?> TO_SIGNED(R, L'length);
  end function "?>";

  -- ============================================================================

  -- Id: C.55
  function "?<" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?<"": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?<"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?<"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L < R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?<";

  -- Id: C.56
  function "?<" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?<"": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?<"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?<"": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L < R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?<";

  -- Id: C.57
  function "?<" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return TO_UNSIGNED(L, R'length) ?< R;
  end function "?<";

  -- Id: C.58
  function "?<" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return TO_SIGNED(L, R'length) ?< R;
  end function "?<";

  -- Id: C.59
  function "?<" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC is
  begin
    return L ?< TO_UNSIGNED(R, L'length);
  end function "?<";

  -- Id: C.60
  function "?<" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC is
  begin
    return L ?< TO_SIGNED(R, L'length);
  end function "?<";

  -- ============================================================================

  -- Id: C.61
  function "?<=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?<="": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?<="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?<="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L <= R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?<=";

  -- Id: C.62
  function "?<=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?<="": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?<="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?<="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L <= R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?<=";

  -- Id: C.63
  function "?<=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return TO_UNSIGNED(L, R'length) ?<= R;
  end function "?<=";

  -- Id: C.64
  function "?<=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return TO_SIGNED(L, R'length) ?<= R;
  end function "?<=";

  -- Id: C.65
  function "?<=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC is
  begin
    return L ?<= TO_UNSIGNED(R, L'length);
  end function "?<=";

  -- Id: C.66
  function "?<=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC is
  begin
    return L ?<= TO_SIGNED(R, L'length);
  end function "?<=";

  -- ============================================================================

  -- Id: C.67
  function "?>=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?>="": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?>="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?>="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L >= R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?>=";

  -- Id: C.68
  function "?>=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?>="": null detected, returning X"
        severity warning;
      return 'X';
    else
      for i in L'range loop
        if L(i) = '-' then
          report "NUMERIC_STD.""?>="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      for i in R'range loop
        if R(i) = '-' then
          report "NUMERIC_STD.""?>="": '-' found in compare string"
            severity error;
          return 'X';
        end if;
      end loop;
      if IS_X(L) or IS_X(R) then
        return 'X';
      elsif L >= R then
        return '1';
      else
        return '0';
      end if;
    end if;
  end function "?>=";

  -- Id: C.69
  function "?>=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return TO_UNSIGNED(L, R'length) ?>= R;
  end function "?>=";

  -- Id: C.70
  function "?>=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return TO_SIGNED(L, R'length) ?>= R;
  end function "?>=";

  -- Id: C.71
  function "?>=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC is
  begin
    return L ?>= TO_UNSIGNED(R, L'length);
  end function "?>=";

  -- Id: C.72
  function "?>=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC is
  begin
    return L ?>= TO_SIGNED(R, L'length);
  end function "?>=";

  -- ============================================================================

  -- Id: C.73
  function "?=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
    constant L_LEFT          : INTEGER := L'length-1;
    constant R_LEFT          : INTEGER := R'length-1;
    alias XL                 : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR                 : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE            : NATURAL := MAXIMUM(L'length, R'length);
    variable LX              : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable RX              : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable result, result1 : STD_ULOGIC;  -- result
  begin
    -- Logically identical to an "=" operator.
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?="": null detected, returning X"
        severity warning;
      return 'X';
    else
      LX     := RESIZE(XL, SIZE);
      RX     := RESIZE(XR, SIZE);
      result := '1';
      for i in LX'low to LX'high loop
        result1 := LX(i) ?= RX(i);
        if result1 = 'U' then
          return 'U';
        elsif result1 = 'X' or result = 'X' then
          result := 'X';
        else
          result := result and result1;
        end if;
      end loop;
      return result;
    end if;
  end function "?=";

  -- Id: C.74
  function "?=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC is
    constant L_LEFT          : INTEGER := L'length-1;
    constant R_LEFT          : INTEGER := R'length-1;
    alias XL                 : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR                 : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE            : NATURAL := MAXIMUM(L'length, R'length);
    variable LX              : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable RX              : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable result, result1 : STD_ULOGIC;  -- result
  begin  -- ?=
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?="": null detected, returning X"
        severity warning;
      return 'X';
    else
      LX     := RESIZE(XL, SIZE);
      RX     := RESIZE(XR, SIZE);
      result := '1';
      for i in LX'low to LX'high loop
        result1 := LX(i) ?= RX(i);
        if result1 = 'U' then
          return 'U';
        elsif result1 = 'X' or result = 'X' then
          result := 'X';
        else
          result := result and result1;
        end if;
      end loop;
      return result;
    end if;
  end function "?=";

  -- Id: C.75
  function "?=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return TO_UNSIGNED(L, R'length) ?= R;
  end function "?=";

  -- Id: C.76
  function "?=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return TO_SIGNED(L, R'length) ?= R;
  end function "?=";

  -- Id: C.77
  function "?=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC is
  begin
    return L ?= TO_UNSIGNED(R, L'length);
  end function "?=";

  -- Id: C.78
  function "?=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC is
  begin
    return L ?= TO_SIGNED(R, L'length);
  end function "?=";

  -- ============================================================================

  -- Id: C.79
  function "?/=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
    constant L_LEFT          : INTEGER := L'length-1;
    constant R_LEFT          : INTEGER := R'length-1;
    alias XL                 : UNRESOLVED_UNSIGNED(L_LEFT downto 0) is L;
    alias XR                 : UNRESOLVED_UNSIGNED(R_LEFT downto 0) is R;
    constant SIZE            : NATURAL := MAXIMUM(L'length, R'length);
    variable LX              : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable RX              : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable result, result1 : STD_ULOGIC;  -- result
  begin  -- ?=
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?/="": null detected, returning X"
        severity warning;
      return 'X';
    else
      LX     := RESIZE(XL, SIZE);
      RX     := RESIZE(XR, SIZE);
      result := '0';
      for i in LX'low to LX'high loop
        result1 := LX(i) ?/= RX(i);
        if result1 = 'U' then
          return 'U';
        elsif result1 = 'X' or result = 'X' then
          result := 'X';
        else
          result := result or result1;
        end if;
      end loop;
      return result;
    end if;
  end function "?/=";

  -- Id: C.80
  function "?/=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC is
    constant L_LEFT          : INTEGER := L'length-1;
    constant R_LEFT          : INTEGER := R'length-1;
    alias XL                 : UNRESOLVED_SIGNED(L_LEFT downto 0) is L;
    alias XR                 : UNRESOLVED_SIGNED(R_LEFT downto 0) is R;
    constant SIZE            : NATURAL := MAXIMUM(L'length, R'length);
    variable LX              : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable RX              : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable result, result1 : STD_ULOGIC;  -- result
  begin  -- ?=
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.""?/="": null detected, returning X"
        severity warning;
      return 'X';
    else
      LX     := RESIZE(XL, SIZE);
      RX     := RESIZE(XR, SIZE);
      result := '0';
      for i in LX'low to LX'high loop
        result1 := LX(i) ?/= RX(i);
        if result1 = 'U' then
          return 'U';
        elsif result1 = 'X' or result = 'X' then
          result := 'X';
        else
          result := result or result1;
        end if;
      end loop;
      return result;
    end if;
  end function "?/=";

  -- Id: C.81
  function "?/=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return TO_UNSIGNED(L, R'length) ?/= R;
  end function "?/=";

  -- Id: C.82
  function "?/=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return TO_SIGNED(L, R'length) ?/= R;
  end function "?/=";

  -- Id: C.83
  function "?/=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC is
  begin
    return L ?/= TO_UNSIGNED(R, L'length);
  end function "?/=";

  -- Id: C.84
  function "?/=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC is
  begin
    return L ?/= TO_SIGNED(R, L'length);
  end function "?/=";

  -- ============================================================================

  -- Id: S.1
  function SHIFT_LEFT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNRESOLVED_UNSIGNED(XSLL(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function SHIFT_LEFT;

  -- Id: S.2
  function SHIFT_RIGHT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNRESOLVED_UNSIGNED(XSRL(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function SHIFT_RIGHT;

  -- Id: S.3
  function SHIFT_LEFT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL)
    return UNRESOLVED_SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return UNRESOLVED_SIGNED(XSLL(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function SHIFT_LEFT;

  -- Id: S.4
  function SHIFT_RIGHT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL)
    return UNRESOLVED_SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return UNRESOLVED_SIGNED(XSRA(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function SHIFT_RIGHT;

  -- ============================================================================

  -- Id: S.5
  function ROTATE_LEFT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNRESOLVED_UNSIGNED(XROL(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function ROTATE_LEFT;

  -- Id: S.6
  function ROTATE_RIGHT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL)
    return UNRESOLVED_UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNRESOLVED_UNSIGNED(XROR(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function ROTATE_RIGHT;


  -- Id: S.7
  function ROTATE_LEFT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL)
    return UNRESOLVED_SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return UNRESOLVED_SIGNED(XROL(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function ROTATE_LEFT;

  -- Id: S.8
  function ROTATE_RIGHT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL)
    return UNRESOLVED_SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return UNRESOLVED_SIGNED(XROR(STD_ULOGIC_VECTOR(ARG), COUNT));
  end function ROTATE_RIGHT;

  -- ============================================================================

  ------------------------------------------------------------------------------
  -- Note: Function S.9 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.9
  function "sll" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER)
    return UNRESOLVED_UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return SHIFT_RIGHT(ARG, -COUNT);
    end if;
  end function "sll";

  ------------------------------------------------------------------------------
  --   Note: Function S.10 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.10
  function "sll" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return UNRESOLVED_SIGNED(SHIFT_RIGHT(UNRESOLVED_UNSIGNED(ARG), -COUNT));
    end if;
  end function "sll";

  ------------------------------------------------------------------------------
  --   Note: Function S.11 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.11
  function "srl" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER)
    return UNRESOLVED_UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_RIGHT(ARG, COUNT);
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end function "srl";

  ------------------------------------------------------------------------------
  --   Note: Function S.12 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.12
  function "srl" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    if (COUNT >= 0) then
      return UNRESOLVED_SIGNED(SHIFT_RIGHT(UNRESOLVED_UNSIGNED(ARG), COUNT));
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end function "srl";

  ------------------------------------------------------------------------------
  -- Note: Function S.13 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.13
  function "rol" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER)
    return UNRESOLVED_UNSIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_LEFT(ARG, COUNT);
    else
      return ROTATE_RIGHT(ARG, -COUNT);
    end if;
  end function "rol";

  ------------------------------------------------------------------------------
  --   Note: Function S.14 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.14
  function "rol" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_LEFT(ARG, COUNT);
    else
      return ROTATE_RIGHT(ARG, -COUNT);
    end if;
  end function "rol";

  ------------------------------------------------------------------------------
  --   Note: Function S.15 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.15
  function "ror" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER)
    return UNRESOLVED_UNSIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_RIGHT(ARG, COUNT);
    else
      return ROTATE_LEFT(ARG, -COUNT);
    end if;
  end function "ror";

  ------------------------------------------------------------------------------
  -- Note: Function S.16 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.16
  function "ror" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_RIGHT(ARG, COUNT);
    else
      return ROTATE_LEFT(ARG, -COUNT);
    end if;
  end function "ror";

  ------------------------------------------------------------------------------
  -- Note: Function S.17 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.17
  function "sla" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER)
    return UNRESOLVED_UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return SHIFT_RIGHT(ARG, -COUNT);
    end if;
  end function "sla";

  ------------------------------------------------------------------------------
  -- Note: Function S.18 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.18
  function "sla" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return SHIFT_RIGHT(ARG, -COUNT);
    end if;
  end function "sla";

  ------------------------------------------------------------------------------
  -- Note: Function S.19 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.19
  function "sra" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER)
    return UNRESOLVED_UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_RIGHT(ARG, COUNT);
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end function "sra";

  ------------------------------------------------------------------------------
  -- Note: Function S.20 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.20
  function "sra" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER)
    return UNRESOLVED_SIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_RIGHT(ARG, COUNT);
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end function "sra";

  -- ============================================================================

  -- Id: D.1
  function TO_INTEGER (ARG : UNRESOLVED_UNSIGNED) return NATURAL is
    constant ARG_LEFT : INTEGER := ARG'length-1;
    alias XXARG       : UNRESOLVED_UNSIGNED(ARG_LEFT downto 0) is ARG;
    variable XARG     : UNRESOLVED_UNSIGNED(ARG_LEFT downto 0);
    variable RESULT   : NATURAL := 0;
  begin
    if (ARG'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.TO_INTEGER: null detected, returning 0"
        severity warning;
      return 0;
    end if;
    XARG := TO_01(XXARG, 'X');
    if (XARG(XARG'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.TO_INTEGER: metavalue detected, returning 0"
        severity warning;
      return 0;
    end if;
    for I in XARG'range loop
      RESULT := RESULT+RESULT;
      if XARG(I) = '1' then
        RESULT := RESULT + 1;
      end if;
    end loop;
    return RESULT;
  end function TO_INTEGER;

  -- Id: D.2
  function TO_INTEGER (ARG : UNRESOLVED_SIGNED) return INTEGER is
    variable XARG : UNRESOLVED_SIGNED(ARG'length-1 downto 0);
  begin
    if (ARG'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.TO_INTEGER: null detected, returning 0"
        severity warning;
      return 0;
    end if;
    XARG := TO_01(ARG, 'X');
    if (XARG(XARG'left) = 'X') then
      assert NO_WARNING
        report "NUMERIC_STD.TO_INTEGER: metavalue detected, returning 0"
        severity warning;
      return 0;
    end if;
    if XARG(XARG'left) = '0' then
      return TO_INTEGER(UNRESOLVED_UNSIGNED(XARG));
    else
      return (- (TO_INTEGER(UNRESOLVED_UNSIGNED(- (XARG + 1)))) -1);
    end if;
  end function TO_INTEGER;

  -- Id: D.3
  function TO_UNSIGNED (ARG, SIZE : NATURAL) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(SIZE-1 downto 0);
    variable I_VAL  : NATURAL := ARG;
  begin
    if (SIZE < 1) then return NAU;
    end if;
    for I in 0 to RESULT'left loop
      if (I_VAL mod 2) = 0 then
        RESULT(I) := '0';
      else RESULT(I) := '1';
      end if;
      I_VAL          := I_VAL/2;
    end loop;
    if not(I_VAL = 0) then
      assert NO_WARNING
        report "NUMERIC_STD.TO_UNSIGNED: vector truncated"
        severity warning;
    end if;
    return RESULT;
  end function TO_UNSIGNED;

  -- Id: D.4
  function TO_SIGNED (ARG : INTEGER; SIZE : NATURAL) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(SIZE-1 downto 0);
    variable B_VAL  : STD_LOGIC := '0';
    variable I_VAL  : INTEGER   := ARG;
  begin
    if (SIZE < 1) then return NAS;
    end if;
    if (ARG < 0) then
      B_VAL := '1';
      I_VAL := -(ARG+1);
    end if;
    for I in 0 to RESULT'left loop
      if (I_VAL mod 2) = 0 then
        RESULT(I) := B_VAL;
      else
        RESULT(I) := not B_VAL;
      end if;
      I_VAL := I_VAL/2;
    end loop;
    if ((I_VAL /= 0) or (B_VAL /= RESULT(RESULT'left))) then
      assert NO_WARNING
        report "NUMERIC_STD.TO_SIGNED: vector truncated"
        severity warning;
    end if;
    return RESULT;
  end function TO_SIGNED;

  function TO_UNSIGNED (ARG : NATURAL; SIZE_RES : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return TO_UNSIGNED (ARG  => ARG,
                        SIZE => SIZE_RES'length);
  end function TO_UNSIGNED;

  function TO_SIGNED (ARG : INTEGER; SIZE_RES : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return TO_SIGNED (ARG  => ARG,
                      SIZE => SIZE_RES'length);
  end function TO_SIGNED;

  -- ============================================================================

  -- Id: R.1
  function RESIZE (ARG : UNRESOLVED_SIGNED; NEW_SIZE : NATURAL)
    return UNRESOLVED_SIGNED
  is
    alias INVEC     : UNRESOLVED_SIGNED(ARG'length-1 downto 0) is ARG;
    variable RESULT : UNRESOLVED_SIGNED(NEW_SIZE-1 downto 0) :=
      (others => '0');
    constant BOUND  : INTEGER := MINIMUM(ARG'length, RESULT'length)-2;
  begin
    if (NEW_SIZE < 1) then return NAS;
    end if;
    if (ARG'length = 0) then return RESULT;
    end if;
    RESULT := (others => ARG(ARG'left));
    if BOUND >= 0 then
      RESULT(BOUND downto 0) := INVEC(BOUND downto 0);
    end if;
    return RESULT;
  end function RESIZE;

  -- Id: R.2
  function RESIZE (ARG : UNRESOLVED_UNSIGNED; NEW_SIZE : NATURAL)
    return UNRESOLVED_UNSIGNED
  is
    constant ARG_LEFT : INTEGER := ARG'length-1;
    alias XARG        : UNRESOLVED_UNSIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT   : UNRESOLVED_UNSIGNED(NEW_SIZE-1 downto 0) :=
      (others => '0');
  begin
    if (NEW_SIZE < 1) then return NAU;
    end if;
    if XARG'length = 0 then return RESULT;
    end if;
    if (RESULT'length < ARG'length) then
      RESULT(RESULT'left downto 0) := XARG(RESULT'left downto 0);
    else
      RESULT(RESULT'left downto XARG'left+1) := (others => '0');
      RESULT(XARG'left downto 0)             := XARG;
    end if;
    return RESULT;
  end function RESIZE;

  function RESIZE (ARG, SIZE_RES : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return RESIZE (ARG      => ARG,
                   NEW_SIZE => SIZE_RES'length);
  end function RESIZE;

  function RESIZE (ARG, SIZE_RES : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return RESIZE (ARG      => ARG,
                   NEW_SIZE => SIZE_RES'length);
  end function RESIZE;

  -- ============================================================================

  -- Id: L.1
  function "not" (L : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_UNSIGNED(not(STD_ULOGIC_VECTOR(L)));
    return RESULT;
  end function "not";

  -- Id: L.2
  function "and" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_UNSIGNED(STD_ULOGIC_VECTOR(L) and
                                  STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "and";

  -- Id: L.3
  function "or" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_UNSIGNED(STD_ULOGIC_VECTOR(L) or
                                  STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "or";

  -- Id: L.4
  function "nand" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_UNSIGNED(STD_ULOGIC_VECTOR(L) nand
                                  STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "nand";

  -- Id: L.5
  function "nor" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_UNSIGNED(STD_ULOGIC_VECTOR(L) nor
                                  STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "nor";

  -- Id: L.6
  function "xor" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_UNSIGNED(STD_ULOGIC_VECTOR(L) xor
                                  STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.7 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.7
  function "xnor" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
    variable RESULT : UNRESOLVED_UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_UNSIGNED(STD_ULOGIC_VECTOR(L) xnor
                                  STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "xnor";

  -- Id: L.8
  function "not" (L : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_SIGNED(not(STD_ULOGIC_VECTOR(L)));
    return RESULT;
  end function "not";

  -- Id: L.9
  function "and" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_SIGNED(STD_ULOGIC_VECTOR(L) and STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "and";

  -- Id: L.10
  function "or" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_SIGNED(STD_ULOGIC_VECTOR(L) or STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "or";

  -- Id: L.11
  function "nand" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_SIGNED(STD_ULOGIC_VECTOR(L) nand
                                STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "nand";

  -- Id: L.12
  function "nor" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_SIGNED(STD_ULOGIC_VECTOR(L) nor STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "nor";

  -- Id: L.13
  function "xor" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_SIGNED(STD_ULOGIC_VECTOR(L) xor STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.14 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.14
  function "xnor" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
    variable RESULT : UNRESOLVED_SIGNED(L'length-1 downto 0);
  begin
    RESULT := UNRESOLVED_SIGNED(STD_ULOGIC_VECTOR(L) xnor
                                STD_ULOGIC_VECTOR(R));
    return RESULT;
  end function "xnor";

  -- Id: L.15
  function "and" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (L and STD_ULOGIC_VECTOR(R));
  end function "and";

  -- Id: L.16
  function "and" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (STD_ULOGIC_VECTOR(L) and R);
  end function "and";

  -- Id: L.17
  function "or" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (L or STD_ULOGIC_VECTOR(R));
  end function "or";

  -- Id: L.18
  function "or" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (STD_ULOGIC_VECTOR(L) or R);
  end function "or";

  -- Id: L.19
  function "nand" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (L nand STD_ULOGIC_VECTOR(R));
  end function "nand";

  -- Id: L.20
  function "nand" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (STD_ULOGIC_VECTOR(L) nand R);
  end function "nand";

  -- Id: L.21
  function "nor" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (L nor STD_ULOGIC_VECTOR(R));
  end function "nor";

  -- Id: L.22
  function "nor" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (STD_ULOGIC_VECTOR(L) nor R);
  end function "nor";

  -- Id: L.23
  function "xor" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (L xor STD_ULOGIC_VECTOR(R));
  end function "xor";

  -- Id: L.24
  function "xor" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (STD_ULOGIC_VECTOR(L) xor R);
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.25 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.25
  function "xnor" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (L xnor STD_ULOGIC_VECTOR(R));
  end function "xnor";

  ------------------------------------------------------------------------------
  -- Note: Function L.26 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.26
  function "xnor" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC)
    return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED (STD_ULOGIC_VECTOR(L) xnor R);
  end function "xnor";

  -- Id: L.27
  function "and" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (L and STD_ULOGIC_VECTOR(R));
  end function "and";

  -- Id: L.28
  function "and" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (STD_ULOGIC_VECTOR(L) and R);
  end function "and";

  -- Id: L.29
  function "or" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (L or STD_ULOGIC_VECTOR(R));
  end function "or";

  -- Id: L.30
  function "or" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (STD_ULOGIC_VECTOR(L) or R);
  end function "or";

  -- Id: L.31
  function "nand" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (L nand STD_ULOGIC_VECTOR(R));
  end function "nand";

  -- Id: L.32
  function "nand" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (STD_ULOGIC_VECTOR(L) nand R);
  end function "nand";

  -- Id: L.33
  function "nor" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (L nor STD_ULOGIC_VECTOR(R));
  end function "nor";

  -- Id: L.34
  function "nor" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (STD_ULOGIC_VECTOR(L) nor R);
  end function "nor";

  -- Id: L.35
  function "xor" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (L xor STD_ULOGIC_VECTOR(R));
  end function "xor";

  -- Id: L.36
  function "xor" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (STD_ULOGIC_VECTOR(L) xor R);
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.37 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.37
  function "xnor" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (L xnor STD_ULOGIC_VECTOR(R));
  end function "xnor";

  ------------------------------------------------------------------------------
  -- Note: Function L.38 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.38
  function "xnor" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC)
    return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED (STD_ULOGIC_VECTOR(L) xnor R);
  end function "xnor";

  ------------------------------------------------------------------------------
  -- Note: Function L.39 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.39
  function "and" (L : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return and (STD_ULOGIC_VECTOR (L));
  end function "and";

  ------------------------------------------------------------------------------
  -- Note: Function L.40 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.40
  function "and" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return and (STD_ULOGIC_VECTOR (L));
  end function "and";

  ------------------------------------------------------------------------------
  -- Note: Function L.41 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.41
  function "nand" (L : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return nand (STD_ULOGIC_VECTOR (L));
  end function "nand";

  ------------------------------------------------------------------------------
  -- Note: Function L.42 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.42
  function "nand" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return nand (STD_ULOGIC_VECTOR (L));
  end function "nand";

  ------------------------------------------------------------------------------
  -- Note: Function L.43 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.43
  function "or" (L : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return or (STD_ULOGIC_VECTOR (L));
  end function "or";

  ------------------------------------------------------------------------------
  -- Note: Function L.44 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.44
  function "or" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return or (STD_ULOGIC_VECTOR (L));
  end function "or";

  ------------------------------------------------------------------------------
  -- Note: Function L.45 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.45
  function "nor" (L : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return nor (STD_ULOGIC_VECTOR (L));
  end function "nor";

  ------------------------------------------------------------------------------
  -- Note: Function L.46 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.46
  function "nor" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return nor (STD_ULOGIC_VECTOR (L));
  end function "nor";

  ------------------------------------------------------------------------------
  -- Note: Function L.47 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.47
  function "xor" (L : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return xor (STD_ULOGIC_VECTOR (L));
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.48 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.48
  function "xor" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return xor (STD_ULOGIC_VECTOR (L));
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.49 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.49
  function "xnor" (L : UNRESOLVED_SIGNED) return STD_ULOGIC is
  begin
    return xnor (STD_ULOGIC_VECTOR (L));
  end function "xnor";

  ------------------------------------------------------------------------------
  -- Note: Function L.50 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.50
  function "xnor" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC is
  begin
    return xnor (STD_ULOGIC_VECTOR (L));
  end function "xnor";

  -- ============================================================================

  -- support constants for STD_MATCH:

  type BOOLEAN_TABLE is array(STD_ULOGIC, STD_ULOGIC) of BOOLEAN;

  constant MATCH_TABLE : BOOLEAN_TABLE := (
    --------------------------------------------------------------------------
    -- U      X      0      1      Z      W      L      H      -
    --------------------------------------------------------------------------
    (false, false, false, false, false, false, false, false, true),  -- | U |
    (false, false, false, false, false, false, false, false, true),  -- | X |
    (false, false, true,  false, false, false, true,  false, true),  -- | 0 |
    (false, false, false, true,  false, false, false, true,  true),  -- | 1 |
    (false, false, false, false, false, false, false, false, true),  -- | Z |
    (false, false, false, false, false, false, false, false, true),  -- | W |
    (false, false, true,  false, false, false, true,  false, true),  -- | L |
    (false, false, false, true,  false, false, false, true,  true),  -- | H |
    (true,  true,  true,  true,  true,  true,  true,  true,  true)   -- | - |
    );

  -- Id: M.1
  function STD_MATCH (L, R : STD_ULOGIC) return BOOLEAN is
  begin
    return MATCH_TABLE(L, R);
  end function STD_MATCH;

  -- Id: M.2
  function STD_MATCH (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN is
    alias LV : UNRESOLVED_UNSIGNED(1 to L'length) is L;
    alias RV : UNRESOLVED_UNSIGNED(1 to R'length) is R;
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.STD_MATCH: null detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if LV'length /= RV'length then
      assert NO_WARNING
        report "NUMERIC_STD.STD_MATCH: L'LENGTH /= R'LENGTH, returning FALSE"
        severity warning;
      return false;
    else
      for I in LV'low to LV'high loop
        if not (MATCH_TABLE(LV(I), RV(I))) then
          return false;
        end if;
      end loop;
      return true;
    end if;
  end function STD_MATCH;

  -- Id: M.3
  function STD_MATCH (L, R : UNRESOLVED_SIGNED) return BOOLEAN is
    alias LV : UNRESOLVED_SIGNED(1 to L'length) is L;
    alias RV : UNRESOLVED_SIGNED(1 to R'length) is R;
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.STD_MATCH: null detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if LV'length /= RV'length then
      assert NO_WARNING
        report "NUMERIC_STD.STD_MATCH: L'LENGTH /= R'LENGTH, returning FALSE"
        severity warning;
      return false;
    else
      for I in LV'low to LV'high loop
        if not (MATCH_TABLE(LV(I), RV(I))) then
          return false;
        end if;
      end loop;
      return true;
    end if;
  end function STD_MATCH;

  -- Id: M.5
  function STD_MATCH (L, R : STD_ULOGIC_VECTOR) return BOOLEAN is
    alias LV : STD_ULOGIC_VECTOR(1 to L'length) is L;
    alias RV : STD_ULOGIC_VECTOR(1 to R'length) is R;
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_STD.STD_MATCH: null detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if LV'length /= RV'length then
      assert NO_WARNING
        report "NUMERIC_STD.STD_MATCH: L'LENGTH /= R'LENGTH, returning FALSE"
        severity warning;
      return false;
    else
      for I in LV'low to LV'high loop
        if not (MATCH_TABLE(LV(I), RV(I))) then
          return false;
        end if;
      end loop;
      return true;
    end if;
  end function STD_MATCH;

  -- ============================================================================

  -- function TO_01 is used to convert vectors to the
  --          correct form for exported functions,
  --          and to report if there is an element which
  --          is not in (0, 1, H, L).

  -- Id: T.1
  function TO_01 (S : UNRESOLVED_UNSIGNED; XMAP : STD_ULOGIC := '0')
    return UNRESOLVED_UNSIGNED is
  begin
    if (S'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.TO_01: null detected, returning NAU"
        severity warning;
      return NAU;
    end if;
    return UNRESOLVED_UNSIGNED(TO_01(STD_ULOGIC_VECTOR(S), XMAP));
  end function TO_01;

  -- Id: T.2
  function TO_01 (S : UNRESOLVED_SIGNED; XMAP : STD_ULOGIC := '0')
    return UNRESOLVED_SIGNED is
  begin
    if (S'length < 1) then
      assert NO_WARNING
        report "NUMERIC_STD.TO_01: null detected, returning NAS"
        severity warning;
      return NAS;
    end if;
    return UNRESOLVED_SIGNED(TO_01(STD_ULOGIC_VECTOR(S), XMAP));
  end function TO_01;

  -- Id: T.3
  function TO_X01 (S : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED(TO_X01(STD_ULOGIC_VECTOR(S)));
  end function TO_X01;

  -- Id: T.4
  function TO_X01 (S : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED(TO_X01(STD_ULOGIC_VECTOR(S)));
  end function TO_X01;

  -- Id: T.5
  function TO_X01Z (S : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED(TO_X01Z(STD_ULOGIC_VECTOR(S)));
  end function TO_X01Z;

  -- Id: T.6
  function TO_X01Z (S : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED(TO_X01Z(STD_ULOGIC_VECTOR(S)));
  end function TO_X01Z;

  -- Id: T.7
  function TO_UX01 (S : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED is
  begin
    return UNRESOLVED_UNSIGNED(TO_UX01(STD_ULOGIC_VECTOR(S)));
  end function TO_UX01;

  -- Id: T.8
  function TO_UX01 (S : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED is
  begin
    return UNRESOLVED_SIGNED(TO_UX01(STD_ULOGIC_VECTOR(S)));
  end function TO_UX01;

  -- Id: T.9
  function IS_X (S : UNRESOLVED_UNSIGNED) return BOOLEAN is
  begin
    return IS_X(STD_ULOGIC_VECTOR(S));
  end function IS_X;

  -- Id: T.10
  function IS_X (S : UNRESOLVED_SIGNED) return BOOLEAN is
  begin
    return IS_X(STD_ULOGIC_VECTOR(S));
  end function IS_X;

  -- ============================================================================
  -- string conversion and write operations
  -- ============================================================================
  function TO_OSTRING (value : UNRESOLVED_UNSIGNED) return STRING is
  begin
    return TO_OSTRING(STD_ULOGIC_VECTOR (value));
  end function TO_OSTRING;

  function TO_OSTRING (value : UNRESOLVED_SIGNED) return STRING is
    constant result_length : INTEGER := (value'length+2)/3;
    constant pad           : STD_ULOGIC_VECTOR(1 to (result_length*3 -
                                                     value'length))
      := (others => value (value'left));  -- Extend sign bit
  begin
    return TO_OSTRING(pad & STD_ULOGIC_VECTOR (value));
  end function TO_OSTRING;

  function to_hstring (value : UNRESOLVED_UNSIGNED) return STRING is
  begin
    return to_hstring(STD_ULOGIC_VECTOR (value));
  end function to_hstring;

  function to_hstring (value : UNRESOLVED_SIGNED) return STRING is
    constant result_length : INTEGER := (value'length+3)/4;
    constant pad           : STD_ULOGIC_VECTOR(1 to (result_length*4 -
                                                     value'length))
      := (others => value (value'left));  -- Extend sign bit
  begin
    return to_hstring(pad & STD_ULOGIC_VECTOR (value));
  end function to_hstring;

  procedure READ (L    : inout LINE; VALUE : out UNRESOLVED_UNSIGNED;
                  GOOD : out BOOLEAN) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    READ (L     => L,
          VALUE => ivalue,
          GOOD  => GOOD);
    VALUE := UNSIGNED(ivalue);
  end procedure READ;

  procedure READ (L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    READ (L      => L,
           VALUE => ivalue);
    VALUE := UNSIGNED (ivalue);
  end procedure READ;

  procedure READ (L    : inout LINE; VALUE : out UNRESOLVED_SIGNED;
                  GOOD : out BOOLEAN) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    READ (L     => L,
          VALUE => ivalue,
          GOOD  => GOOD);
    VALUE := SIGNED(ivalue);
  end procedure READ;

  procedure READ (L : inout LINE; VALUE : out UNRESOLVED_SIGNED) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    READ (L      => L,
           VALUE => ivalue);
    VALUE := SIGNED (ivalue);
  end procedure READ;

  procedure WRITE (L         : inout LINE; VALUE : in UNRESOLVED_UNSIGNED;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    ivalue := STD_ULOGIC_VECTOR (VALUE);
    WRITE (L          => L,
            VALUE     => ivalue,
            JUSTIFIED => JUSTIFIED,
            FIELD     => FIELD);
  end procedure WRITE;

  procedure WRITE (L         : inout LINE; VALUE : in UNRESOLVED_SIGNED;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    ivalue := STD_ULOGIC_VECTOR (VALUE);
    WRITE (L          => L,
            VALUE     => ivalue,
            JUSTIFIED => JUSTIFIED,
            FIELD     => FIELD);
  end procedure WRITE;

  procedure OREAD (L    : inout LINE; VALUE : out UNRESOLVED_UNSIGNED;
                   GOOD : out BOOLEAN) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    OREAD (L     => L,
           VALUE => ivalue,
           GOOD  => GOOD);
    VALUE := UNSIGNED(ivalue);
  end procedure OREAD;

  procedure OREAD (L    : inout LINE; VALUE : out UNRESOLVED_SIGNED;
                   GOOD : out BOOLEAN) is
    constant ne               : INTEGER := (VALUE'length+2)/3;
    constant pad              : INTEGER := ne*3 - VALUE'length;
    variable ivalue           : STD_ULOGIC_VECTOR(0 to ne*3-1);
    variable ok               : BOOLEAN;
    variable expected_padding : STD_ULOGIC_VECTOR(0 to pad-1);
  begin
    OREAD (L      => L,
            VALUE => ivalue,            -- Read padded STRING
            GOOD  => ok);
    -- Bail out if there was a bad read
    if not ok then
      GOOD := false;
      return;
    end if;
    expected_padding := (others => ivalue(pad));
    if ivalue(0 to pad-1) /= expected_padding then
      GOOD := false;
    else
      GOOD  := true;
      VALUE := UNRESOLVED_SIGNED (ivalue (pad to ivalue'high));
    end if;
  end procedure OREAD;

  procedure OREAD (L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    OREAD (L     => L,
           VALUE => ivalue);
    VALUE := UNSIGNED (ivalue);
  end procedure OREAD;

  procedure OREAD (L : inout LINE; VALUE : out UNRESOLVED_SIGNED) is
    constant ne               : INTEGER := (VALUE'length+2)/3;
    constant pad              : INTEGER := ne*3 - VALUE'length;
    variable ivalue           : STD_ULOGIC_VECTOR(0 to ne*3-1);
    variable expected_padding : STD_ULOGIC_VECTOR(0 to pad-1);
  begin
    OREAD (L      => L,
            VALUE => ivalue);           -- Read padded string
    expected_padding := (others => ivalue(pad));
    if ivalue(0 to pad-1) /= expected_padding then
      assert false
        report "NUMERIC_STD.OREAD Error: Signed vector truncated"
        severity error;
    else
      VALUE := UNRESOLVED_SIGNED (ivalue (pad to ivalue'high));
    end if;
  end procedure OREAD;

  procedure HREAD (L    : inout LINE; VALUE : out UNRESOLVED_UNSIGNED;
                   GOOD : out BOOLEAN) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    HREAD (L     => L,
           VALUE => ivalue,
           GOOD  => GOOD);
    VALUE := UNSIGNED(ivalue);
  end procedure HREAD;

  procedure HREAD (L    : inout LINE; VALUE : out UNRESOLVED_SIGNED;
                   GOOD : out BOOLEAN) is
    constant ne               : INTEGER := (VALUE'length+3)/4;
    constant pad              : INTEGER := ne*4 - VALUE'length;
    variable ivalue           : STD_ULOGIC_VECTOR(0 to ne*4-1);
    variable ok               : BOOLEAN;
    variable expected_padding : STD_ULOGIC_VECTOR(0 to pad-1);
  begin
    HREAD (L      => L,
            VALUE => ivalue,            -- Read padded STRING
            GOOD  => ok);
    if not ok then
      GOOD := false;
      return;
    end if;
    expected_padding := (others => ivalue(pad));
    if ivalue(0 to pad-1) /= expected_padding then
      GOOD := false;
    else
      GOOD  := true;
      VALUE := UNRESOLVED_SIGNED (ivalue (pad to ivalue'high));
    end if;
  end procedure HREAD;

  procedure HREAD (L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    HREAD (L     => L,
           VALUE => ivalue);
    VALUE := UNSIGNED (ivalue);
  end procedure HREAD;

  procedure HREAD (L : inout LINE; VALUE : out UNRESOLVED_SIGNED) is
    constant ne               : INTEGER := (VALUE'length+3)/4;
    constant pad              : INTEGER := ne*4 - VALUE'length;
    variable ivalue           : STD_ULOGIC_VECTOR(0 to ne*4-1);
    variable expected_padding : STD_ULOGIC_VECTOR(0 to pad-1);
  begin
    HREAD (L      => L,
            VALUE => ivalue);           -- Read padded string
    expected_padding := (others => ivalue(pad));
    if ivalue(0 to pad-1) /= expected_padding then
      assert false
        report "NUMERIC_STD.HREAD Error: Signed vector truncated"
        severity error;
    else
      VALUE := UNRESOLVED_SIGNED (ivalue (pad to ivalue'high));
    end if;
  end procedure HREAD;

  procedure OWRITE (L         : inout LINE; VALUE : in UNRESOLVED_UNSIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    ivalue := STD_ULOGIC_VECTOR (VALUE);
    OWRITE (L         => L,
            VALUE     => ivalue,
            JUSTIFIED => JUSTIFIED,
            FIELD     => FIELD);
  end procedure OWRITE;

  procedure OWRITE (L         : inout LINE; VALUE : in UNRESOLVED_SIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    constant ne  : INTEGER := (VALUE'length+2)/3;
    constant pad : STD_ULOGIC_VECTOR(0 to (ne*3 - VALUE'length) - 1)
      := (others => VALUE (VALUE'left));
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    ivalue := STD_ULOGIC_VECTOR (VALUE);
    OWRITE (L         => L,
            VALUE     => pad & ivalue,
            JUSTIFIED => JUSTIFIED,
            FIELD     => FIELD);
  end procedure OWRITE;

  procedure HWRITE (L         : inout LINE; VALUE : in UNRESOLVED_UNSIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
  begin
    ivalue := STD_ULOGIC_VECTOR (VALUE);
    HWRITE (L         => L,
            VALUE     => ivalue,
            JUSTIFIED => JUSTIFIED,
            FIELD     => FIELD);
  end procedure HWRITE;

  procedure HWRITE (L         : inout LINE; VALUE : in UNRESOLVED_SIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable ivalue : STD_ULOGIC_VECTOR(VALUE'range);
    constant ne     : INTEGER := (VALUE'length+3)/4;
    constant pad    : STD_ULOGIC_VECTOR(0 to (ne*4 - VALUE'length) - 1)
      := (others => VALUE(VALUE'left));
  begin
    ivalue := STD_ULOGIC_VECTOR (VALUE);
    HWRITE (L         => L,
            VALUE     => pad & ivalue,
            JUSTIFIED => JUSTIFIED,
            FIELD     => FIELD);
  end procedure HWRITE;

end package body NUMERIC_STD;
